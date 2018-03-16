#' @import data.table
#' @import CommonFunctions
NULL

#' Check if estimation profiles are valid.
#'
#' It checks if a data.table of estimation profiles is valid.
#'  It will stop if there exists any invalid estimation type, time control, or
#'  regional control. It will replace missing values with default values.
#'
#' @export
#' @param DT the data.table that includes the estimation profiles.
#' each row is a model
#' @param default_value_file a csv file specifying the default values, if missing,
#' will use a default file in inst/
#' @return Return a fixed version of the estimation profile. Unqualified or
#' missing values will all be set to default values with warnings. Detailed
#' warnings are available by setting VERBOSE = TRUE
CheckEstimationProfiles <- function(
  DT,
  default_value_file = system.file('', 'Default_Model_Values.csv', package = 'DemandEstimation')
  ){
  necessary_cols <- c(
    'model_name',
    'estimation_type',
    'time_control',
    'regional_control'
    )
  dt_names <- names(DT)
  missing_cols <- setdiff(necessary_cols, dt_names)
  if(length(missing_cols) > 0){
    stop(missing_cols, ' missing\n')
  }
  # check for duplicate columns
  if(max(table(names(DT))) > 1){
    stop('duplicated input columns\n')
  }
  #-----------------------------------------------------------------------------
  # check input type
  valid_estimation_types  <- c('OLS', 'Bayes', 'LASSO', 'IV', 'SparseOLS',
                               'SparseLASSO', 'TimeWindow')
  valid_time_controls     <- c('YearMonth', 'YearQuarter', 'YearWeek', 'None')
  valid_regional_controls <- c('Store', 'County', 'ZIP3', 'DMA', 'Region', 'State', 'National')
  valid_input <- TRUE
  for(cur_col in c('estimation_type', 'time_control', 'regional_control')){
    my_cols    <- DT[, get(cur_col)]
    valid_cols <- get(paste0('valid_', cur_col, 's'))
    diff_cols  <- setdiff(my_cols, valid_cols)
    if(length(diff_cols) > 0){
      cat(Separator(), 'invalid', sub('_', ' ', cur_col), '\n')
      VerboseWarning(diff_cols)
      valid_input <- FALSE
    }
  }
  if(!valid_input) stop('Invalid inputs, please correct it!')
  #-----------------------------------------------------------------------------
  # check settings of character columns
  character_settings <- fread(
    'field        , value
    sales_col     , volume_sold
    price_col     , imputed_price_hybrid
    base_price_col, base_price_hybrid
    promotion_col , promotion_freq_5
    estimate_by_group, National'
    )
  missing_char_cols  <- setdiff(character_settings[, field], dt_names)
  for(cur_col in missing_char_cols) DT[, (cur_col) := as.character(NA)]
  non_complete_cols <- sapply(DT, complete.cases)
  non_complete_cols <- names(non_complete_cols[non_complete_cols == FALSE])
    ## replace missing character settings with default
  for(i in 1:nrow(character_settings)){
    cur_col           <- character_settings[i, field]
    cur_default_value <- character_settings[i, value]
    DT[, (cur_col) := as.character(get(cur_col))]
    DT[is.na(get(cur_col)) | get(cur_col) == '', (cur_col) := cur_default_value]
  }
  #-----------------------------------------------------------------------------
  # check numeric settinsg
  if(!file.exists(default_value_file)){
    default_value_file = system.file("Default_Model_Values.csv", package = 'DemandEstimation')
    warning(sprintf('default value file not found; use %s instead', default_value_file))
  }
  numeric_settings <- fread(default_value_file)
  missing_num_cols <- setdiff(numeric_settings[, field], dt_names)
  for(cur_col in missing_num_cols) DT[, (cur_col) := as.numeric(NA)]
    ## replace missing numeric values with default
  for(i in 1:nrow(numeric_settings)){
    cur_col <- numeric_settings[i, field]
    cur_default_value <- as.numeric(numeric_settings[i, value])
    invalid_numeric_rows <- is.na(DT[[cur_col]]) | (!is.numeric(DT[[cur_col]]))
    DT[, (cur_col) := as.numeric(get(cur_col))]
    DT[invalid_numeric_rows, (cur_col) := cur_default_value]
  }
  #-----------------------------------------------------------------------------
  # add other model names
  for(cur_col in c('model_abbrev', 'model_file_suffix')){
    if(!(cur_col %in% dt_names)){
      DT[, (cur_col) := as.character(NA)]
    }
    DT[is.na(get(cur_col)) | get(cur_col) == '', (cur_col) := model_name]
  }
  #-----------------------------------------------------------------------------
  #----------Warning--------------------
  i <- length(non_complete_cols)
  if(i > 0){
    cat(Separator())
    cat(i, 'columns are non-complete and are replaced with default value\n')
    VerboseWarning(non_complete_cols)
    }
  i <- length(missing_num_cols)
  if(i > 0){
    cat(Separator())
    cat(i, 'numeric columns are missing and are replaced with default value\n')
    VerboseWarning(missing_num_cols)
    }
  i <- length(missing_char_cols)
  if(i > 0){
    cat(Separator())
    cat(i, 'character columns are missing and are replaced with default value\n')
    VerboseWarning(missing_char_cols)
    }

  return(DT)
}
