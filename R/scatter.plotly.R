#' Scatter plot for TwoSampleMR analysis
#'
#' @author Subrata Paul
#' @param `res` - Output of MR analysis using TwoSampleMR `mr` function
#' @param `dat` - Harmonized data. Output of the `harmonise_data` function from TwoSampleMR package
#'
#' @examples
#'
#' bmi_exp_dat <- extract_instruments(outcomes = c('ieu-a-2', 'ieu-a-9', 'ieu-a-12'))
#' chd_out_dat <- extract_outcome_data(snps = bmi_exp_dat$SNP, outcomes = 'ieu-a-7')
#' dat <- harmonise_data(bmi_exp_dat, chd_out_dat)
#' res = mr(dat)
#' scatter.plotly(res, dat)
#'
#' @export
#'
#'
#' @import plotly
#' @import dplyr
#' @import magrittr
#'
#'

scatter.plotly<-function(res, dat){

  scatter.plotly.single<-function(res_dat){
    fig = plot_ly(res_dat,
                  x = ~beta.exposure,
                  y = ~beta.outcome,
                  type = 'scatter',
                  error_x = ~list(array = se.exposure),
                  error_y = ~list(array = se.outcome),
                  text = ~SNP,
                  name = 'SNP',
                  mode = 'markers',
                  hovertemplate = paste(
                    "<b>%{text}</b><br>",
                    "exposure = %{x:$, .0f} <span>&#177;</span> %{error_x.array:$, .0f} </br>",
                    "outcome = %{y:$, .0f} <span>&#177;</span> %{error_y.array:$, .0f}",
                    "<extra></extra>"
                  ))%>%add_segments(x = 0, xend = max(res_dat$beta.exposure),
                                    y = ~a, yend = ~b*max(res_dat$beta.exposure),
                                    color = ~method,
                                    name = ~method,
                                    text = ~SNP,
                                    error_x = ~list(array = zero),
                                    error_y = ~list(array = zero))%>%
      layout(legend = list(x = 0.7, y = 1))
    return(fig)
  }


  if(is.character(res)){
    if(grepl('.csv', res)){
      res = read.csv(res)
    }else{
      res = read.table(res, header = T)
    }
  }else{
    if(!is.data.frame(res)) stop('res has to be either a data.frame or file containing the data')
  }

  if(is.character(dat)){
    if(grepl('.csv', dat)){
      dat = read.csv(dat)
    }else{
      dat = read.table(dat, header = T)
    }
  }else{
    if(!is.data.frame(dat)) stop('res has to be either a data.frame or file containing the data')
  }

  dat_required_col <- c('beta.exposure', 'beta.outcome', 'id.exposure', 'id.outcome', 'exposure', 'outcome', 'se.exposure', 'se.outcome', 'SNP', 'mr_keep')
  missing_fields = setdiff(dat_required_col, names(dat))

  if(length(missing_fields)>0){
    stop(paste0('The harmonized data should contain ', paste(missing_fields, sep = ', '),' columns.'))
  }

  res_required_col <- c('method', 'outcome', 'id.outcome', 'exposure', 'id.exposure', 'b')

  missing_fields = setdiff(res_required_col, names(res))

  if(length(missing_fields)>0){
    stop(paste0('The mr result data should contain ', paste(missing_fields, sep = ', '),' columns.'))
  }

  index = dat$beta.exposure < 0
  dat$beta.exposure[index] = dat$beta.exposure[index] * -1
  dat$beta.outcome[index] = dat$beta.outcome[index] * -1
  xrange = seq(0, max(dat$beta.exposure), length.out = 50)

  res$a <- 0

  for(i in 1:nrow(res)){
    if(res$method[i] == 'MR Egger'){
      d = dat%>%filter(outcome == res$outcome[i] & id.outcome == res$id.outcome[i] & exposure == res$exposure[i] & id.exposure == res$id.exposure[i])
      temp = mr_egger_regression(d$beta.exposure,
                                 d$beta.outcome, d$se.exposure, d$se.outcome,
                                 default_parameters())
      res$a[i]<-temp$b_i
    }else if(res$method[i] == "MR Egger (bootstrap)"){
      d = dat%>%filter(outcome == res$outcome[i] & id.outcome == res$id.outcome[i] & exposure == res$exposure[i] & id.exposure == res$id.exposure[i])
      temp = mr_egger_regression_bootstrap(d$beta.exposure,
                                           d$beta.outcome, d$se.exposure, d$se.outcome,
                                           default_parameters())
      res$a[i]<-temp$b_i
    }
  }

  res = res%>%dplyr::select(id.exposure, id.outcome, exposure, outcome, method, b, a)
  dat = dat%>%filter(mr_keep)%>%dplyr::select(id.exposure, id.outcome, exposure, outcome, beta.exposure, beta.outcome, se.exposure, se.outcome, SNP)
  res_dat = dat%>%left_join(res, by = c("id.outcome", "outcome", "exposure", "id.exposure"))
  index = duplicated(res_dat%>%dplyr::select(method, b, a))
  res_dat$b[index]<-0
  res_dat$a[index]<-0
  res_dat$method[index]<-0
  #res_dat[duplicated(res_dat%>%dplyr::select(method, b, a)),c('method','b','a')]<-0
  res_dat = unique(res_dat)
  res_dat$method[res_dat$method==0]<-NA
  res_dat$zero <- 0
  res_dat$method = factor(res_dat$method)

  plot_names = expand.grid(exposures = unique(res_dat$id.exposure), outcomes = unique(res_dat$id.outcome))
  plot_names$names = paste(plot_names$exposures, plot_names$outcomes, sep = '.')

  print(nrow(plot_names))

  if(nrow(plot_names)==1){
    return(scatter.plotly.single(
      res_dat%>%dplyr::filter(id.exposure == plot_names$exposures[1] & id.outcome == plot_names$outcomes[1])
      )
    )
  }else{
    print('test')
    all_scatter_plotly<-list()
    for(i in 1:nrow(plot_names)){
      all_scatter_plotly[[plot_names$names[i]]] <- scatter.plotly.single(
        res_dat%>%dplyr::filter(id.exposure == plot_names$exposures[i] & id.outcome == plot_names$outcomes[i])
      )
    }
    return(all_scatter_plotly)
  }
}
