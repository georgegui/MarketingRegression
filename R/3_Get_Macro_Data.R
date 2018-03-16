#' @import zoo
NULL
#' Get County-Level Macro
#'
#' This function will take in the unemployement and housing data to merge
#'  and simplify them into a county*YearMonth table
#'
#' @export
GetCountyTable <- function(PATHS){
  load(PATHS$input$macro$unemployment$dir)
  load(PATHS$input$macro$housing$dir)

  zillow_DT[, ':='(ZIP3 = as.character(zip_code %/% 100),
                   County = paste(CountyName, State, sep = ', '))]
  employ_table[, County := sub(' County', '', fips_county_descr)]
  setnames(employ_table,
           c('fips_state_code', 'fips_state_name'),
           c('State', 'State_Name'))
  zillow_DT_county <- zillow_DT[, .(zillow_index = mean(zillow_index, na.rm = TRUE)),
                                by = .(date, County)]
  zillow_DT_county[, zillow_index := zillow_index/1e3]
  setnames(zillow_DT_county, c('date'), c('YearMonth'))
  employ_table[, YearMonth := format(as.Date(month), '%Y-%m')]

  employ_table_county <- employ_table[, .(unemployrate = mean(unemployrate, na.rm = TRUE),
                                          popestimate = sum(popestimate_2064, na.rm = TRUE)),
                                      by = .(YearMonth, County, State, State_Name)]
  rm(employ_table)
  employ_table_county[popestimate == 0, popestimate := NA
                      ][, popestimate := mean(popestimate, na.rm = TRUE), by = County]
  employ_table_county <- employ_table_county[!is.na(popestimate)]
  county_macro_table <- merge(employ_table_county,
                              zillow_DT_county, by = c('County', 'YearMonth'))
  setkey(county_macro_table, County, YearMonth)

  county_macro_table <- county_macro_table[, count := sum(!(is.na(unemployrate))),
                                           by = YearMonth][count > 0][
                                             , count := NULL]
  county_macro_table <- county_macro_table[, count := sum(!(is.na(zillow_index))),
                                           by = County][count > 0][
                                             , count := NULL]
  county_macro_table[, State := as.character(State)]
  week_time_table <- GenerateTimeFactorTable()
  county_macro_table <- merge(county_macro_table,
                              unique(week_time_table[, .(YearMonth, YearQuarter)]),
                              by = 'YearMonth')
  county_macro_table[, County := toupper(County)]
  return(county_macro_table)
}
