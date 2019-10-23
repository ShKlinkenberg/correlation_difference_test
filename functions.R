FisherZTransformation <- function(r) {
  # Transforms Pearson’s r to Fisher's Z
  # Source: https://www.statisticshowto.datasciencecentral.com/fisher-z/
  #
  # Args:
  #   r: Pearson's correlation coefficient r
  #
  # Returns:
  #.  z: Fisher's z-score
  
  z = .5 * ( log(1+r) – log(1-r) )
  
  return(z)
}
