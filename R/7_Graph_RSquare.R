
#' Plot the Rsquare of coefficients
#'
#' Plot the r-squared of own price elasticities explained by region or chain
#' @export
PlotRsquare <- function(dt){
  gg = ggplot(dt, aes(x = value)) +
    geom_histogram(aes(y=..density..),colour = "gray50", fill = "lightskyblue1")  +
    facet_grid(.~variable) +
    theme(panel.background = element_blank())

  #dirty solution to add median vertical line in facet_grid
  med_fac <- dt[, list(x = median(value, na.rm = TRUE)), by = variable]
  med_fac[, label:=round(x, 3)];
  y_range <- ggplot_build(gg)$panel$ranges[[1]]$y.range[2]
  med_fac[, y_pos:=y_range]
  gg = gg + geom_vline(data=med_fac, aes(xintercept=x), col = 'red')+
    geom_text(data = med_fac, aes(x = x+.1, y = y_pos, label = label))
}

