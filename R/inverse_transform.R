################################################################################
# FUNCTION: Inverse transform vector of data 
################################################################################

# Function to inverse transform a vector of data 
inverse.transform.rank <- function(x) 
  #' Inverse rank transform a vector 
  #'
  #' @param v <- vector of values
  #' @return col_inv <- a vector of values inverse transformed by rank
  #' 
  #' @author adapted from https://yingji15.github.io/2019-08-17-inverse-normal-transformation/
  #' 
{
  col_inv <- qnorm((rank(x,na.last="keep")-0.5)/sum(!is.na(x)))
  return(col_inv)
}
