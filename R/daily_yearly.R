#' Convert Daily Gain to X-year Gain
#' 
#' For example, you can use this function to calculate that an investment that 
#' gains 0.1\% per day would gain approximately 28.5\% in a year (252 trading 
#' days).
#' 
#' 
#' @param daily.gain Numeric value, e.g. 0.001 for 0.1\%.
#' @param years Numeric value.
#' 
#' 
#' @return Numeric value.
#' 
#' 
#' @examples 
#' # Calculate annual gain for an investment that gains 0.1% per day
#' daily_yearly(daily.gain = 0.001)
#' 
#' # Calculate 5-year gains corresponding to various daily gains
#' daily_yearly(daily.gain = seq(0, 0.001, 0.0001), years = 5)
#' 
#' 
#' @export
daily_yearly <- function(daily.gain, years = 1) {
  return((1 + daily.gain)^(252 * years) - 1)
}