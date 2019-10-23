FisherZTransformation <- function(r) {
  # Transforms Pearson’s r to Fisher's Z
  # Source: https://www.statisticshowto.datasciencecentral.com/fisher-z/
  #
  # Args:
  #   r: Pearson's correlation coefficient r
  #
  # Returns:
  #.  z: Fisher's z-score
  
  z = .5 * ( log(1+r) - log(1-r) )
  
  return(z)
}

ComparingIndependentCorrelation <- function(r, n) {
  # Test the significance between two independent correlation coefficients
  # Source: https://www.statisticssolutions.com/comparing-correlation-coefficients/
  #
  # Args:
  #   r: vector of two correlation values
  #   n: vector of two sample size values
  #
  # Returns:
  #   The significance of the correlation
  
  
}
