################################################################################
# FUNCTION: Compute jaccard similarity
################################################################################
jaccard_similarity <- function (rslt1, rslt2)
  #' Estimates the Jaccard similarity coefficient for two lists
  #'
  #'
  #' @param rslt1 vector one  to compare
  #' @param rslt1 vector two  to compare
  #' @return: Jaccard similarity coefficient
  #'
  #' @author Robbie Pope
  #
  {
  intersection = length(intersect(rslt1, rslt2))
  union = length(rslt1) + length(rslt2) - intersection
  return (intersection/union)
}

