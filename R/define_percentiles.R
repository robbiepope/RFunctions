###############################################################################
# FUNCTION: Define percentiles for each food group
################################################################################
# Function to define percentiles
define_percentiles <- function (df, percentiles=5)
  #' For a df of FoodGroups and meal_portions in grams, split each food group
  #' into percentiles and assign percentile number to row
  #' Highest percentile = TOP
  #' Lowest percentile = BOTTOM
  #'
  #' @param df <- DF with columns 'FoodGroup"' and 'total_meal_portion'
  #' @param percentiles <- Number of percentiles to split into, default=5
  #' @return x
  #'
  #' @author Robbie Pope
  #'
{
  # Group by FoodGroup and determine percentile based on total meal portion
  quintile_df <- df %>%
    group_by(FoodGroup) %>%
    mutate(Quintile = ntile(total_meal_portion, percentiles)) %>%
    ungroup()

  return(quintile_df)
}
