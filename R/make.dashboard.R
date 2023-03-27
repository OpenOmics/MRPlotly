#' Write a Rmd file for flex-dashboard
#' 
#' To run the dashboard open the produced .Rmd file in RStudio and hit 'Run Documents'
#' @author Subrata Paul
#' @param file Filename to shave the dashboard .Rmd File
#' @export make.dashboard
#' 
#' 
#' 


make.dashboard<-function(file = 'dashboard'){
  #Use usethis::use_data(dashboard, internal = T, overwrite = T) 
  # two write internal data
  writeLines(rmd, paste0(file, '.Rmd'))
}
