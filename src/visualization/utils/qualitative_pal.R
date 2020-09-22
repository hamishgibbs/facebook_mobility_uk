qualitative_pal <- function(names, rep_n = 3){
  
  qual_col_pals <-  brewer.pal.info[brewer.pal.info$category == 'qual',]
  pal <-  rep(unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))), rep_n)
  
  pal <- pal[1:length(names)]
  names(pal) = names
  
  return(pal)
  
}
