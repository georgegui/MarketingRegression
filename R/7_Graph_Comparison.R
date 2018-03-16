#' @import ggplot2
#' @import CommonFunctions
#' @import data.tree
#' @import GGally
NULL
#'
#' Compare estimations by scatter plot
#'
#' It compares a list of estimation results by plotting scatter plots.
#'
#' @export
#' @param estimation results
#' @param ordered_models a data.table that includes the exact name and
#'  abbreviation of each model. They are ordered into the order they are being
#'  printed.
#' @param parent_folder a directory in which the graphs are to be stored.
CompareModels <- function(dt, ordered_models, parent_folder, ...){
  UseMethod('CompareModels')
}

#' @describeIn CompareModels plot a list of data.table that includes all the
#'  models.
#'
#' @export CompareModels.list
CompareModels.list <- function(dt_list, ...){
  if('data.table' %in% intersect(lapply(dt_list, class))){
    dt_tables <-rbindlist(dt_list)
    CompareModels.data.table(dt_tables, ordered_models, parent_folder, ...)
  } else {
    stop('Wrong model input.')
  }
}


#' @describeIn CompareModels plot data.table that includes all the models
#'
#' @export CompareModels.data.table
CompareModels.data.table <- function(
  dt_long, ordered_models,
  parent_folder = NULL,
  file_name          = NULL,
  sample_percentage = NULL,
  same_range         = FALSE,
  cols_to_compare    = c('significant','coefficient'),
  cols_to_merge      = c('var_name', 'rhs_group', 'group_code', 'store_code_uc'),
  plot_style = CompareHex
){
  cols_to_merge <- intersect(cols_to_merge, names(dt_long))
  # select sample
  Grp_table <- dt_long[, list(.GRP), by = cols_to_merge]
  n_grp <- nrow(Grp_table)
  if(is.null(sample_percentage)){
    n_sample <- ifelse(IsTestMode(),
                       max(n_grp * 0.01, 150),
                       max(n_grp * 0.01, 20000))
  } else {
    n_sample <- max(n_grp * sample_percentage, 150)
  }
  n_sample <- floor(n_sample)
  truncated_range <- quantile(dt_long[, coefficient], c(0.01, 0.99), na.rm = TRUE)

  dt_long <- dt_long[coefficient > truncated_range[[1]] &
                       coefficient < truncated_range[[2]]]
  set.seed(6)
  Grp_table <- Grp_table[sample(.N, n_sample)]
  dt_long <- merge(dt_long, Grp_table[, .SD, .SDcols = cols_to_merge],
                   by = cols_to_merge)
  # dcast
  dt_long <- merge(dt_long, ordered_models, by = 'model_name')
  dcast_formula <- formula(paste(paste(cols_to_merge, collapse = '+'),
                                 'model_abbrev', sep = '~'))
  dt_wide <- dcast(dt_long, dcast_formula, value.var = cols_to_compare)
  coefficient_cols <- paste0('coefficient_', ordered_models$model_abbrev)
  significant_cols <- paste0('significant_', ordered_models$model_abbrev)

  DefaultDiagGraph <- function(data, mapping){
    class(data) <- c('data.table', 'data.frame')
    PrettyPlot(data, toString(mapping), x_full_range = TRUE)
  }
  DefaultLeftGraph <- function(..., func){
    plot_style(...,
               truncated_range = truncated_range,
               same_range = same_range)
  }
  g = ggpairs(dt_wide, columns = coefficient_cols,
              lower = list(continuous = DefaultLeftGraph),
              diag = list(continuous = DefaultDiagGraph),
              upper = list(continuous = DefaultRightGraph),
              columnLabels = ordered_models$model_abbrev)

  # make plot
  if(is.null(file_name)){
    print(g)
  } else {
    pdf(file = paste0(parent_folder, file_name),
        width = combined_graph_width,
        height = combined_graph_width)
    print(g)
    dev.off()
  }
  return(NULL)
}


#' Plot Styles for CompareModels
#'
#' @param data data of ggplot
#' @param mapping mapping of ggplot
#' @param same_range whether all plots should be in the same range
#' @param truncated_range if same_range=TRUE, this truncated range will be
#'  applied
#' @name CompareStyle
NULL

#' @rdname CompareStyle
# a scatter plot comparison
#' @export
CompareScatter <- function(data, mapping, truncated_range, same_range){
  p <- ggplot(data = data, mapping = mapping) + geom_point(size = 0.1, alpha = 0.2)
  FormatComparisonPlot(p, data, mapping, truncated_range, same_range)
}

#' @rdname CompareStyle
#  a density plot comparison
#' @export
CompareDensity <- function(data, mapping, truncated_range, same_range){
  p <- ggplot(data = data, mapping = mapping) +
    stat_density_2d(aes(fill = ..level..), geom = 'polygon') +
    scale_fill_gradient(low = 'white', high = 'black', trans = 'log')
  FormatComparisonPlot(p, data, mapping, truncated_range, same_range)
}

#' @rdname CompareStyle
#  a hex density plot comparison
#' @export
CompareHex <- function(data, mapping, truncated_range, same_range){
  p <- ggplot(data = data, mapping = mapping) + stat_binhex() +
    scale_fill_gradient(low = 'white', high = 'black', trans = 'log')
  FormatComparisonPlot(p, data, mapping, truncated_range, same_range)
}

#' @rdname CompareStyle
#'
# scatter plot with contour
#' @export
CompareScatterDensity <- function(data, mapping, truncated_range, same_range){
  p <- ggplot(data = data, mapping = mapping) +
    stat_density_2d(aes(fill = ..level..)) +
    scale_fill_gradient(low = 'white', high = 'black', trans = 'log')
  FormatComparisonPlot(p, data, mapping, truncated_range, same_range)
}

#' @rdname CompareStyle
#
# Format the Comparison Plot
FormatComparisonPlot <- function(p, data, mapping, truncated_range, same_range){
  if(same_range){
    p <- p +
      scale_x_continuous(limits = truncated_range) +
      scale_y_continuous(limits = truncated_range)
  } else {
    mapping_info <- as.character(mapping)
    x_col <- mapping_info[["x"]]
    y_col <- mapping_info[["y"]]
    p <- p +
      scale_x_continuous(limits = quantile(data[[x_col]], c(0.01, 0.99), na.rm = TRUE)) +
      scale_y_continuous(limits = quantile(data[[y_col]], c(0.01, 0.99), na.rm = TRUE))
  }
  p <- p + geom_abline(slope = 1, color = 'red') +
    geom_smooth(method = 'lm', se = FALSE, linetype = 'dashed',
                color = 'blue', size = 1) +
    theme(axis.line.x      = element_line(),
          axis.line.y      = element_line(),
          panel.background = element_blank(),
          panel.grid.minor = element_line(colour = "grey95"),
          panel.grid.major = element_line(colour = "grey90"))
  p
}


#' @rdname CompareStyle
#
# Report the statistics for each pair of plots
DefaultRightGraph <- function(data, mapping){
  mapping_info <- as.character(mapping)
  class(data) <- c('data.table', 'data.frame')
  x_col <- mapping_info[["x"]]
  y_col <- mapping_info[["y"]]
  revised_cor <- cor(data[[x_col]], data[[y_col]], use = "pairwise.complete.obs")
  revised_cor <- round(revised_cor, digits = 3)
  n_non_NA <- nrow(data) - sum(is.na(data[[x_col]])|is.na(data[[y_col]]))
  text_to_print <- paste0('Correlation: ', format(revised_cor, big.mark=","),
                          '\n Comparable Observations: \n', n_non_NA)

  ggplot() + annotate('text', x = 2, y = 2, label = text_to_print, size = 2)
}



