require(tidyverse)

pathways <- readr::read_csv('https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv') %>% 
  rename(date = `Specimen date`) %>% 
  rename(code = `Area code`) %>% 
  rename(count = `Daily lab-confirmed cases`)

# add variables and subsets
day_of_week <- function(date) {
  day_of_week <- weekdays(date)
  out <- dplyr::case_when(
    day_of_week %in% c("Saturday", "Sunday") ~ "weekend",
    day_of_week %in% c("Monday") ~ "monday",
    TRUE ~ "rest_of_week"
  )
  out <- factor(out, levels = c("rest_of_week", "monday", "weekend"))
  out
}

pathways <- pathways %>% 
  mutate(day = as.integer(date - min(date, na.rm = TRUE)),
         weekday = day_of_week(date))
last_date <- as.Date("2020-06-15")
first_date <- last_date - 8*7 #max(pathways$date, na.rm = TRUE) - 6*7

pathways_recent <- pathways %>%
  filter(date >= first_date & date <= last_date)

# define candidate models
models <- list(
  regression = trendbreaker::lm_model(count ~ day),
  poisson_constant = trendbreaker::glm_model(count ~ 1, family = "poisson"),
  negbin_time = trendbreaker::glm_nb_model(count ~ day),
  negbin_time_weekday = trendbreaker::glm_nb_model(count ~ day + weekday),
  negbin_time_weekday2 = trendbreaker::glm_nb_model(count ~ day * weekday)
)

counts_nhs_region <- pathways_recent %>% filter(`Area name` == 'Leicester') %>% 
  group_by(code, date, day, weekday) %>%
  summarise(count = sum(count)) %>%
  complete(date, fill = list(count = 0)) %>% 
  split(.$code)

la <- counts_nhs_region[[1]]


res_nhs_region <- trendbreaker::asmodee(la, models, method = trendbreaker::evaluate_aic, alpha = 0.5, fixed_k=7)
sum(res_nhs_region$results$outlier[36:42])

rm(la, first_date, last_date, res_nhs_region, counts_nhs_region, pathways_recent)


# Convert it to flag those regions which cross a threshold of outliers and on which date
# It will check each LA to see if it's an outlier (and an increasing outlier)

# set up holder
which.regions <- unique(pathways$code)
which.dates <- seq(as.Date("2020-06-01"), as.Date("2020-07-31"), by="day")
results.holder4 <- as.data.frame(matrix(nrow=length(which.dates), ncol=length(which.regions)), row.names=which.dates)
colnames(results.holder4) <- which.regions
results.holder5 <- as.data.frame(matrix(nrow=length(which.dates), ncol=length(which.regions)), row.names=which.dates)
colnames(results.holder5) <- which.regions
results.holder6 <- as.data.frame(matrix(nrow=length(which.dates), ncol=length(which.regions)), row.names=which.dates)
colnames(results.holder6) <- which.regions

# i = date loop
# j = region loop
for(i in 1:length(which.dates)) {
  for(j in 1:length(which.regions)) {
    last_date <- which.dates[i]
    first_date <- last_date - 8*7 #max(pathways$date, na.rm = TRUE) - 6*7
    
    pathways_recent <- pathways %>%
      filter(date >= first_date & date <= last_date)
    
    counts_nhs_region <- pathways_recent %>% filter(code == which.regions[j]) %>% 
      group_by(code, date, day, weekday) %>%
      summarise(count = sum(count)) %>%
      complete(date, fill = list(count = 0)) %>% 
      split(.$code)
    
    la <- counts_nhs_region[[1]]
    
    res_nhs_region <- trendbreaker::asmodee(la, models, method = trendbreaker::evaluate_aic, alpha = 0.5, fixed_k=7)
    if(sum(res_nhs_region$results$outlier[36:42]) >= 4 & sum(res_nhs_region$results$classification[36:42]=="increase") >= 3) {
      results.holder4[i, j] <- "trigger"
    } else {
      results.holder4[i, j] <- "normal"
    }
    if(sum(res_nhs_region$results$outlier[36:42]) >= 5 & sum(res_nhs_region$results$classification[36:42]=="increase") >= 3) {
      results.holder5[i, j] <- "trigger"
    } else {
      results.holder5[i, j] <- "normal"
    }
    if(sum(res_nhs_region$results$outlier[36:42]) >= 6 & sum(res_nhs_region$results$classification[36:42]=="increase") >= 3) {
      results.holder6[i, j] <- "trigger"
    } else {
      results.holder6[i, j] <- "normal"
    }
  }
}

write.csv(results.holder4, "~/Sync/LSHTM/COVID/20200923_Outlier_detection_k4.csv")
write.csv(results.holder5, "~/Sync/LSHTM/COVID/20200923_Outlier_detection_k5.csv")
write.csv(results.holder6, "~/Sync/LSHTM/COVID/20200923_Outlier_detection_k6.csv")

### end

# plot if you want to see the last one
plot(res_nhs_region, "date") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_label = format("%d %b")) +
  labs(x = NULL,
       y = paste0("Daily potential COVID-19 reports\n in ", pathways %>% filter(code %in% la$code) %>% pull(`Area name`) %>% unique()))

par(mfrow=c(2,1))
