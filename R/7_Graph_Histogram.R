#' @import ggplot2
#' @import gridExtra
#' @import CommonFunctions
#' @import data.tree
NULL

combined_graph_width  = 8.27;
combined_graph_height = 11.69;
title_size            = (combined_graph_width+combined_graph_height)/21*12;
text_size             = (combined_graph_width+combined_graph_height)/21*8;
label_size            = (combined_graph_width+combined_graph_height)/21*5;


#' Plot a histogram of estimation results.
#'
#' @export
#'
#' @param dt the estimation results
#' @inheritParams CompareModels
#' @param var_type 'price', 'promotion', etc.
#' @param var_group 'own', 'top_1_competing', etc.
#' @param color_legend which column would be used as color column
#' @param graph_settings the specific settings of which column should be printed
#'  and how they should be printed.
PlotSummary <- function(
  dt, ordered_models, parent_folder,
  var_type, var_group, color_legend, graph_settings, ...
){
  cur_dir <- paste(parent_folder, var_type, var_group, sep = '/')
  cur_dir <- paste0(cur_dir, '/')
  MakeDir(cur_dir)
  if(missing(color_legend)) color_legend <- NULL
  for(cur_id in 1:nrow(graph_settings)){
    cur_col    <- graph_settings[cur_id, field]
    full_range <- as.numeric(graph_settings[cur_id, full_range])
    x_axis     <- GenerateAxisX(dt[, get(cur_col)], x_full_range = full_range)
    for(weighted_by in c('brand_revenue', NA)){
      plots <- list()
      if(is.na(weighted_by)) weighted_by <- NULL
      for(model_id in 1:nrow(ordered_models)){
        cur_model_name <- ordered_models[model_id, model_name]
        cur_plot <- PrettyPlot(dt[cur_model_name == model_name],
                               x            = cur_col,
                               scalex       = x_axis,
                               weight       = weighted_by,
                               color_legend = color_legend)
        plots[[model_id]] <- cur_plot +
          xlab(ordered_models[model_id, model_abbrev])
      }
      if(is.null(weighted_by)){
        file_path <-paste0(cur_dir, cur_col, '.pdf')
      } else {
        file_path <- paste0(cur_dir, cur_col, '_weighted.pdf')
      }
      plot_lists <- ArrangeGraphs(plots)
      pdf(file_path,
          width  = combined_graph_width,
          height = combined_graph_height,
          onefile = TRUE)
      for(cur_plot in plot_lists){
        grid.arrange(cur_plot)
      }
      dev.off()
    }
  }

  return(NULL)
}



#' @describeIn PlotSummary Plot the brand level statistics of estimation
#'
#' @export
#'
PlotBrandSummary <- function(...){
  graph_settings <- fread(
    'field         , full_range
    median         , 0
    mean           , 0
    positivePerc   , 1
    negativePerc   , 1
    significantPerc, 1'
  )
  PlotSummary(...,
              graph_settings = graph_settings)
}

#' @describeIn PlotSummary Plot the brand store statistics of estimation
#'
#' @export
#'
PlotBrandStoreSummary <- function(...){
  graph_settings <- fread(
    'field     , full_range
    coefficient, 0'
  )
  PlotSummary(..., graph_settings = graph_settings,
              color_legend = 'significant')
}

