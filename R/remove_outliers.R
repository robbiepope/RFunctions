################################################################################
# Detect & remove the outliers
################################################################################
# Required Libraries
library(stats)

# Standard deviations from mean or interquartile range
remove.outliers <- function(v, method=c('sd', 'iqr'), n.sd=3)
  #' Set outliers to NA by two different methods
  #' Standard deviations frmo the mean or by interquartile range
  #' 
  #' @author Adapted from Alessia Visconti
  #' @param v vector of values to process
  #' @param method sd is standard deviation and iqr is interquartile range
  #' @param n.sd number of SD from mean, only relevant if method='sd'
  #' @return vector with outliers set to NA based on selected method
  #' 
{
  if (method == 'sd') 
    {
    avg <- mean(v, na.rm=TRUE)
    stdev <- stats::sd(v, na.rm=TRUE)
    v[v <= (avg - n.sd*stdev) | v >= (avg + n.sd*stdev)] <- NA
    return(v)
  } 
  else if (method == 'iqr') 
    {
    q1 <- quantile(v, probs=0.25, na.rm = TRUE)
    q3 <- quantile(v, probs=0.75, na.rm = TRUE)
    IQR <- q3-q1
    v[v > (q3 + (IQR*1.5)) | v < (q1 - (IQR*1.5))] <- NA
    return(v)
  } 
  else 
    {
    stop("Method not valid. Valid methods are 'sd' and 'iqr'")
  }
}