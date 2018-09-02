#' Sortino Ratio
#' 
#' Calculates Sortino ratio from vector of gains or prices. The formula is: 
#' \code{(mean(gains) - rf) / sd(gains[gains < rf])}, where \code{rf} is some 
#' (daily) minimum acceptable rate of return (eg. risk-free rate of return).
#' 
#' 
#' @inheritParams metrics
#' @param rf Numeric value.
#' 
#' 
#' @return
#' Numeric value or vector.
#' 
#' 
#' @examples
#' # Simulate daily gains over a 5-year period
#' set.seed(123)
#' stock.gains <- rnorm(252 * 5, 0.0005, 0.01)
#' 
#' # Calculate Sortino ratio using minimum acceptable return of 0
#' sortino(stock.gains)
#' 
#' 
#' @export
sortino <- function(gains = NULL,
                    prices = NULL,
                    rf = 0) {
  
  # Convert from prices to gains if necessary
  if (! is.null(prices)) {
    gains <- prices_gains(prices)
  }
  
  # Calculate and return Sortino ratio
  if (is.vector(gains)) {
    sortino.ratio <- (mean(gains) - rf) / sd(gains[gains < rf])
  } else {
    means <- apply(gains, 2, mean)
    sds <- apply(gains, 2, function(x) sd(x[x < rf]))
    sortino.ratio <- (means - rf) / sds
  }
  
  return(sortino.ratio)
  
}
