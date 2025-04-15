# utils.R

# compute_score returns the points associated with the final field position (fp).
# - If fp >= 105, we assume a touchdown (7 points).
# - If fp is in field-goal range (here, fp >= 80), return 3.
# - Otherwise, return 0.
# If fp is missing (NA), we return 0.
compute_score <- function(fp) {
  if (is.na(fp)) {
    return(0)
  } else if (fp >= 105) {
    return(7)
  } else if (fp >= 80) {
    return(3)
  } else {
    return(0)
  }
}
