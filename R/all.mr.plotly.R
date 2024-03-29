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
#' @param res_single Output data.frame from \code{\link[TwoSampleMR]{mr_singlesnp}} or filename containing the result in csv format
#' @param res_loo Output of leave-one-out analyais from \code{\link[TwoSampleMR]{mr_leaveoneout}} function
#' @param res  Output of MR analysis using TwoSampleMR \code{\link[TwoSampleMR]{mr}} function
#' @param dat  Harmonized data. Output of the \code{\link[TwoSampleMR]{harmonise_data}} function from TwoSampleMR package
#' @param out  Output file name. If provided the plots will be saved as .rds otherwise a list of all plots will be returned. 
#' @param include_data  Logical. Default is FALSE
#' @param failed A csv or tsv file containing information about failed runs. THe information will be displayed in the dashboard as a table. 
#' @param version Software versions used to run the analysis to be displayed in the dashboard. Use \code{sessionInfo()} to generate the version information. 
#' @param pheno_data A data frame containing information about the outcome phenotypes that were used in the analysis to be included in the dashboard. 
#' @param avail_pheno A tsv or csv file containing information about the gwas summary statistcs available in the system or database. It can be multiple files in a vector. By default Pan-UKBB and ieu open gwas project databases are used.
#' @param avail_pheno_names A vector of the names of the databases. By default it is Pan-UKBB and ieuGWAS. Please note that avail_pheno and avail_pheno_names are for dashboard report only. If you have a different database please change the names on the dashboard.Rmd created by make.dashboard function. For default values avail_pheno should have file names of Pan-UKBB and ieuGWAS. 
#' @export all.mr.plotly 
#'
#'@import plotly
#'@import dplyr
#'@import magrittr
#'@import TwoSampleMR
#'



all.mr.plotly<-function(res, 
                        res_single, 
                        res_loo, 
                        dat, 
                        out = NULL, 
                        include_data = F, 
                        failed = NULL, 
                        versions = NULL, 
                        heterogeneity = NULL, 
                        pleiotropy = NULL, 
                        pheno_data = NULL,
                        avail_pheno = NULL, 
                        avail_pheno_names = c('Pan-UKBB', 'ieuGWAS')){
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
  all_plots$Forest<-suppressWarnings(forest.plotly(res_single))
  all_plots$LOO = res.loo.plotly(res_loo)
  all_plots$Scatter<- scatter.plotly(res, dat)
  all_plots$Funnel<-funnel.plotly(res_single)
  all_plots$exposures = unique(res$id.exposure)
  all_plots$outcomes = unique(res$id.outcome)
  
  
  if(!is.null(failed)){
    if(grepl('.csv', failed)){
      failed = read.csv(failed)
    }else{
      failed = read.table(failed, header = T)
    }
  }else{
    failed = data.frame(File = c(NA), Error = c(NA))
  }
  
  if(!is.null(versions)){
    versions = readLines(versions)
  }
  
  if(!is.null(heterogeneity)){
    if(grepl('.csv', heterogeneity)){
      heterogeneity = read.csv(heterogeneity)
    }else{
      heterogeneity = read.table(heterogeneity, header = T)
    }
  }else{
    heterogeneity = data.frame(File = c(NA), Error = c(NA))
  }
  
  if(!is.null(pleiotropy)){
    if(grepl('.csv', pleiotropy)){
      pleiotropy = read.csv(pleiotropy)
    }else{
      pleiotropy = read.table(pleiotropy, header = T)
    }
  }else{
    pleiotropy = data.frame(File = c(NA), Error = c(NA))
  }
  

  if(!is.null(pheno_data)){
    if(grepl('.csv', pheno_data)){
      pheno_data = read.csv(pheno_data)
      names(pheno_data) = tolower(names(pheno_data))
    }else{
      pheno_data = read.delim(pheno_data, header = T)
      names(pheno_data) = tolower(names(pheno_data))
    }
  }else{
    pheno_data = data.frame(File = c(NA), Error = c(NA))
  }
  
  
  
  
  all_plots$failed = failed
  all_plots$version = versions
  all_plots$heterogeneity = heterogeneity
  all_plots$pleiotropy = pleiotropy
  all_plots$pheno_data = pheno_data
  
  
  if(include_data){
    all_plots$data.res = res
    all_plots$data.res_single = res_single
    all_plots$data.harmo = dat
  }
  
  
  if(!is.null(avail_pheno)){
    all_phenos <- list()
    if(length(avail_pheno)!=length(avail_pheno_names)){
      warning('Please provide names for all sets of available outcome data.\n Did not include the list of available phenotypes')
      all_phenos[['NA']]<-data.frame(Information = c(NA), Not = c(NA), Provided = c(NA))
    }else{
      for(i in 1:length(avail_pheno)){
        if(grepl('.csv', avail_pheno[i])){
          read_pheno = read.csv(avail_pheno[i])
        }else{
          read_pheno = read.delim(avail_pheno[i], header = T)
        }
        names(read_pheno) = tolower(names(read_pheno))
        all_phenos[[avail_pheno_names[i]]]<-read_pheno 
      }  
    }
  }
  
  all_plots$avail_pheno = all_phenos
  
  if(is.null(out)){
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



