# Standardize continuous covariates
# The user can either:
# Provide the original mean and sd, or
# Provide the original (unstandardized) data so the function can calculate mean and sd
standardize_x <- function(x, mean_val = NULL, sd_val = NULL) {
  # Handle NA input: keep NAs in output
  result <- rep(NA_real_, length(x))

  # Determine mean and sd
  if (is.null(mean_val) || is.null(sd_val)) {
    mean_val <- mean(x, na.rm = TRUE)
    sd_val <- sd(x, na.rm = TRUE)
  }

  if (sd_val == 0) {
    warning("Standard deviation is zero. Returning NaN for all non-NA entries.")
    result[!is.na(x)] <- NaN
  } else {
    result[!is.na(x)] <- (x[!is.na(x)] - mean_val) / sd_val
  }

  return(result)
}

# function to reverse the standardization and transforms standardized variables back
# to their original scale. The user can either:
# Provide the original mean and sd, or
# Provide the original (unstandardized) data so the function can calculate mean and sd
unstandardize_x <- function(x_std, original_x = NULL, mean_val = NULL, sd_val = NULL) {
  # Handle NA input: keep NAs in output
  result <- rep(NA_real_, length(x_std))

  # Determine mean and sd
  if (!is.null(mean_val) && !is.null(sd_val)) {
    result[!is.na(x_std)] <- x_std[!is.na(x_std)] * sd_val + mean_val
    return(result)
  }

  if (!is.null(original_x)) {
    mean_val <- mean(original_x, na.rm = TRUE)
    sd_val <- sd(original_x, na.rm = TRUE)
    result[!is.na(x_std)] <- x_std[!is.na(x_std)] * sd_val + mean_val
    return(result)
  }

  stop("Please provide either 'mean_val' and 'sd_val' or 'original_x'.")
}



