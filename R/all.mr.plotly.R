#' Create RDS object containing all plots
#' 
#' @author Subrata Paul
#' 
#' bmi_exp_dat <- extract_instruments(outcomes = c('ieu-a-2', 'ieu-a-9', 'ieu-a-12'))
#' chd_out_dat <- extract_outcome_data(snps = bmi_exp_dat$SNP, outcomes = 'ieu-a-7')
#' dat <- harmonise_data(bmi_exp_dat, chd_out_dat)
#' res_single <- mr_singlesnp(dat)
#' all.mr.plotly(res_single)
#' 
#' @param res_single Output data.frame from `mr_singlesnp` or filename containing the result in csv format
#' @param `res` - Output of MR analysis using TwoSampleMR `mr` function
#' @param `dat` - Harmonized data. Output of the `harmonise_data` function from TwoSampleMR package
#' @param `out` - Output file name. If provided the plots will be saved as .rds otherwise a list of all plots will be returned. 
#' @param `include_data` - Logical. Default is FALSE
#' 
#' @export all.rm.plotly
#'
#'@import plotly
#'@import dplyr
#'@import magrittr
#'



all.rm.plotly<-function(res, res_single, dat, out = NA, include_data = F){
  blank_plot <- function(message)
  {
    requireNamespace("ggplot2", quietly=TRUE)
    ggplot2::ggplot(data.frame(a=0,b=0,n=message)) + 
      ggplot2::geom_text(ggplot2::aes(x=a,y=b,label=n)) + 
      ggplot2::labs(x=NULL,y=NULL) + 
      ggplot2::theme(axis.text=ggplot2::element_blank(), 
                     axis.ticks=ggplot2::element_blank())
  }
  
  # Read res data

  if(is.character(res)){
    if(grepl('.csv', res)){
      res = read.csv(res)
    }else{
      res = read.table(res, header = T)
    }
  }else{
    if(!is.data.frame(res)) stop('res has to be either a data.frame or file containing the data')
  }
  
  message('Reading res data completed!')
  
  # Read harmonized data
  if(is.character(dat)){
    if(grepl('.csv', dat)){
      dat = read.csv(dat)
    }else{
      dat = read.table(dat, header = T)
    }
  }else{
    if(!is.data.frame(dat)) stop('res has to be either a data.frame or file containing the data')
  }
  
  message('Reading harmonized data completed!')
  
  # Check harmonized data
  dat_required_col <- c('beta.exposure', 'beta.outcome', 'id.exposure', 'id.outcome', 'exposure', 'outcome', 'se.exposure', 'se.outcome', 'SNP', 'mr_keep')
  missing_fields = setdiff(dat_required_col, names(dat))
  
  if(length(missing_fields)>0){
    stop(paste0('The harmonized data should contain ', paste(missing_fields, sep = ', '),' columns.'))
  }
  
  # Check res data
  res_required_col <- c('method', 'outcome', 'id.outcome', 'exposure', 'id.exposure', 'b')
  
  missing_fields = setdiff(res_required_col, names(res))
  
  if(length(missing_fields)>0){
    stop(paste0('The mr result data should contain ', paste(missing_fields, sep = ', '),' columns.'))
  }
  
  # Read res_single data
  
  if(is.character(res_single)){
    if(grepl('.csv', res_single)){
      res_single = read.csv(res_single)
    }else{
      res_single = read.table(res_single, header = T)
    }
  }else{
    if(!is.data.frame(res_single)) stop('res_single has to be either a data.frame or file containing the data')
  }
  
  message('Reading res_single data completed!')
  # Check res_single data
  required_fields = c('SNP', 'b', 'se', 'id.exposure','id.outcome')
  missing_fields = setdiff(required_fields, names(res_single))
  
  if(length(missing_fields)>0){
    stop(paste0('The res_single output should contain ', paste(missing_fields, sep = ', '),' columns.'))
  }
  
  message('All input looks good! Generating Plots')
  
  # Generate plots
  
  all_plots<-list()
  all_plots$Forest<-forest.plotly(res_single)
  all_plots$LOO = res.loo.plotly(res_loo)
  all_plots$Scatter<- scatter.plotly(res, dat)
  all_plots$Funnel<-funnel.plotly(res_single)
  all_plots$exposures = unique(res$id.exposure)
  all_plots$outcomes = unique(res$id.outcome)
  
  if(include_data){
    all_plots$data.res = res
    all_plots$data.res_single = res_single
    all_plots$data.harmo = dat
  }
  
  if(is.na(out)){
    return(all_plots)
  }else{
    if(endsWith(out, '.rds')){
      outfile = out
    }else{
      outfile = paste0(out, '.rds')
    }
    message(paste0('Saving all plots to ', outfile))
    saveRDS(all_plots, outfile)
  }
  
}



