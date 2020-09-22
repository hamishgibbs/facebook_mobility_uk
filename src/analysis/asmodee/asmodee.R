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
first_date <- as.Date('2020-06-29') - 6*7
pathways_recent <- pathways %>%
  filter(date >= first_date)
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

res_nhs_region <- trendbreaker::asmodee(la, models, method = trendbreaker::evaluate_aic, alpha = 0.5)

plot(res_nhs_region, "date") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_label = format("%d %b")) +
  labs(x = NULL,
       y = paste0("Daily potential COVID-19 reports\n in ", pathways %>% filter(code %in% la$code) %>% pull(`Area name`) %>% unique()))
