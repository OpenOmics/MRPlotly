#' Funnel plot for TwoSampleMR
#'
#' @author Subrata Paul
#'
#' @examples
#'
#' bmi_exp_dat <- extract_instruments(outcomes = c('ieu-a-2', 'ieu-a-9', 'ieu-a-12'))
#' chd_out_dat <- extract_outcome_data(snps = bmi_exp_dat$SNP, outcomes = 'ieu-a-7')
#' dat <- harmonise_data(bmi_exp_dat, chd_out_dat)
#' res_single <- mr_singlesnp(dat)
#' funnel.plotly(res_single)
#'
#' @param `res_single`
#'
#' @import plotly
#' @import dplyr
#' @import magrittr





funnel.plotly<-function(res_single){
  # Check if input a data.frame or a file path. Read if it is a file.
  if(is.character(res_single)){
    if(grepl('.csv', res_single)){
      res_single = read.csv(res_single)
    }else{
      res_single = read.table(res_single, header = T)
    }
  }else{
    if(!is.data.frame(res_single)) stop('res_single has to be either a data.frame or file containing the data')
  }

  # Check if all required columns are available
  required_fields = c('SNP', 'b', 'se', 'id.exposure','id.outcome')
  missing_fields = setdiff(required_fields, names(res_single))
  if(length(missing_fields)>0){
    stop(paste0('The res_single output should contain ', paste(missing_fields, sep = ', '),' columns.'))
  }

  # Function for a single plot
  funnel.plotly.single<-function(plot_data){
    if (sum(!grepl("All", plot_data$SNP)) < 2) {
      return(blank_plot("Insufficient number of SNPs"))
    }
    am = grep('All', plot_data$SNP, value = T)
    plot_data$SNP = gsub('All - ', '', plot_data$SNP)
    am <- gsub("All - ", "", am)

    fig = plotly::ggplotly(ggplot(data = plot_data[!plot_data$SNP%in%am, ],
                                  aes(x = b, y = 1/se,
                                      text = paste0('β: ', round(b,4), '\nSE: ', round(se, 4) ,'\n','SNP: ', SNP)))+
                             geom_point()+
                             geom_vline(data = plot_data[plot_data$SNP%in%am, ], aes(xintercept = b, color = SNP)) +
                             scale_colour_manual(name = 'MR Method',
                                                 values = c("#a6cee3",  "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",  "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99", "#b15928"))+
                             ggpubr::theme_pubr()+
                             labs(x = 'β'), tooltip = 'text'
    )

    return(fig)

  }

  # Make grid for each pair of exposure and outcome
  plot_names = expand.grid(exposures = unique(res_single$id.exposure), outcomes = unique(res_single$id.outcome))
  plot_names$names = paste(plot_names$exposures, plot_names$outcomes, sep = '.')

  # If only one outcome-exposure combination found return a single figure else return a list
  if(nrow(plot_names)==1){
    return(funnel.plotly.single(
      res_single%>%dplyr::filter(id.exposure == plot_names$exposures[1] & id.outcome == plot_names$outcomes[1]))
      )
  }else{
    funnel_plots<-list()
    for(i in 1:nrow(plot_names)){
      funnel_plots[[plot_names$names[i]]] <- funnel.plotly.single(
        res_single%>%dplyr::filter(id.exposure == plot_names$exposures[i] & id.outcome == plot_names$outcomes[i])
      )
    }
    return(funnel_plots)
  }


}
