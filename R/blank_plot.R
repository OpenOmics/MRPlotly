#' Creates a blank plot with provided message
#' 
#' @author Subrata Paul
#' 
#' This function is used to create a blank plot with a text message for the dashboard for the cases where data were not available or the TwoSampleMR model did not run for lack of SNPs


blank_plot <- function(message)
{
  requireNamespace("ggplot2", quietly=TRUE)
  ggplot2::ggplot(data.frame(a=0,b=0,n=message)) + 
    ggplot2::geom_text(ggplot2::aes(x=a,y=b,label=n)) + 
    ggplot2::labs(x=NULL,y=NULL) + 
    ggplot2::theme(axis.text=ggplot2::element_blank(), 
                   axis.ticks=ggplot2::element_blank())
}