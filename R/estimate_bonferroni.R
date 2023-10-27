################################################################################
# Estimate bonferroni correction
################################################################################
# Function to estimatr the bonferroni correction
estimate.bonferroni <- function(meff, signific=0.05)
  #' Estimates the bonferroni corrected p-value threshold
  #'
  #' @param meff <- maximum effective number of tests (float)
  #' @param signific <- Researcher set sginificance value, default is 0.05
  #' @return bonferroni corrected p value
  #'
  #' @author Robbie Pope
  #'

{
  m.eff <- 1/meff
  alpha <- 1 - (1-signific)**m.eff
  return(alpha)
}
