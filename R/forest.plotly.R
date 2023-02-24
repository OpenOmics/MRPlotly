#' List of forest plots for multiple outcome-exposure
#'
#' @author Subrata Paul
#'
#' #' @examples
#' bmi_exp_dat <- extract_instruments(outcomes = c('ieu-a-2', 'ieu-a-9', 'ieu-a-12'))
#' chd_out_dat <- extract_outcome_data(snps = bmi_exp_dat$SNP, outcomes = 'ieu-a-7')
#' dat <- harmonise_data(bmi_exp_dat, chd_out_dat)
#' res_single <- mr_singlesnp(dat)
#' forest.plotly.single(res_single)
#'
#' This function gives plotly object based on the results of `mr_singlesnp`. The forest plot compares the MR estimates using the different MR methods against the single SNP tests.
#' @param res_single Output data.frame from `mr_singlesnp` or filename containing the result in csv format
#' @export
#' @import plotly
#' @import dplyr
#' @import magrittr
#'
#'
#'

forest.plotly<-function(res_single){


  forest.plotly.single<-function(res_single, nticks = 40){
    res_single$Type = factor(ifelse(grepl('All', res_single$SNP), 'All','SNP'), levels = c('SNP','All'))
    res_single = res_single%>%arrange(b)
    snps = unique(res_single$SNP)
    snps = snps[!grepl('All',snps)]
    meta = unique(grep('All', res_single$SNP, value = T))


    fig1 = res_single%>%filter(Type == 'SNP')%>%
      plot_ly(x = ~b, y = ~SNP, color = ~Type,
              type = 'scatter', mode = 'markers',
              error_x = ~list(array = se))%>%
      layout(xaxis = list(title = 'MR effect size'),
             yaxis = list(title = '', categoryorder = 'trace'),
             showlegend = F)

    fig2 = res_single%>%filter(Type == 'All')%>%
      plot_ly(x = ~b, y = ~SNP, color = ~Type,
              type = 'scatter', mode = 'markers',
              error_x = ~list(array = se))%>%
      layout(xaxis = list(title = 'MR effect size'),
             yaxis = list(title = '', categoryorder = 'trace'),
             showlegend = F)

    fig = subplot(fig1, fig2, nrows = 2, shareX = T, heights = c(1-sum(res_single$Type=='All')*0.03/2,sum(res_single$Type=='All')*0.03/2), margin = 0)

    return(fig)
  }

  if(is.character(res_single)){
    if(grepl('.csv', res_single)){
      res_single = read.csv(res_single)
    }else{
      res_single = read.table(res_single, header = T)
    }
  }else{
    if(!is.data.frame(res_single)) stop('res_single has to be either a data.frame or file containing the data')
  }



  if(is.character(res_single)){
    if(grepl('.csv', res_single)){
      res_single = read.csv(res_single)
    }else{
      res_single = read.table(res_single, header = T)
    }
  }else{
    if(!is.data.frame(res_single)) stop('res_single has to be either a data.frame or file containing the data')
  }

  required_fields = c('SNP', 'b', 'se', 'id.exposure','id.outcome')
  missing_fields = setdiff(required_fields, names(res_single))

  if(length(missing_fields)>0){
    stop(paste0('The res_single output should contain ', paste(missing_fields, sep = ', '),' columns.'))
  }

  plot_names = expand.grid(exposures = unique(res_single$id.exposure),
                           outcomes = unique(res_single$id.outcome))
  plot_names$names = paste(plot_names$exposures, plot_names$outcomes, sep = '.')


  if(nrow(plot_names)==1){
    plot_data = res_single%>%dplyr::filter(id.exposure == plot_names$exposures[1] & id.outcome == plot_names$outcomes[1])
    return(forest.plotly.single(plot_data))
  }else{
    all_forest_plotly<-list()
    for(i in 1:nrow(plot_names)){
      plot_data = res_single%>%dplyr::filter(id.exposure == plot_names$exposures[i] & id.outcome == plot_names$outcomes[i])
      all_forest_plotly[[plot_names$names[i]]] <- forest.plotly.single(plot_data)
    }
    return(all_forest_plotly)
  }

}
