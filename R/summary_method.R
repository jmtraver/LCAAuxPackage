
# ------------------------------ summary.mxGlm() ------------------------------
#'@param object object of class mxGlm
#'@param do.boot logical. do a bootstrap for inference (default = FALSE)
#'@param B number of drawn bootstrap samples (default = 999)
#'@param seed set a seed for the bootstrap
#'@param alpha set alpha value for type I error (default = 0.05)
#'@param show.ci logical. Output a confidence interval for 1 - alpha (default = FALSE)
#'@param check.time logical. Output the time it takes to run bootstrap (only available for bootstrap)
#'@param mean.diff logical. Output the mean differences in the outcome variable depending on class membership

#'@method summary mxGlm
#'@export

## idea set up:
# - default class of mx_BCH is mx_glm, but people can manually set the class to
#   glmmTMB if they want to work with it in a different way
# - then we have a custom summary function that runs the cluster bootstrap and
#   prints results similarly to how summary(glmmTMB_object) would do it

# summary methods needs arguments:
# - object
# - B = 999
# - seed = NULL
# - alpha = 0.5
# - show.ci = FALSE


# Only works when loaded into the environment?                              ####

summary.mxGlm <- function(object,
                          do.boot = FALSE,
                          B = 999,
                          seed = NULL,
                          alpha = 0.05,
                          show.ci = FALSE,
                          check.time = FALSE,
                          mean.diff = FALSE) {


  if (mean.diff == TRUE & do.boot == FALSE) {
    warning("'mean.diff' inference only available for 'do.boot' = TRUE")
  }
  if (check.time == TRUE & do.boot == FALSE) {
    warning("'check.time' only returned for 'do.boot' = TRUE")
  }

  if (do.boot == TRUE) {

    check.time <- TRUE
    mean.diff <- TRUE

    # Fit cluster bootstrap
    boot_results <- cluster_boot(object, B = B, seed = seed,
                                 check.time = check.time)
    # Get summary results
    mx_summary <- boot_summary(boot_results, alpha = alpha, ci = show.ci,
                               mean.diff = mean.diff)

    # add time information to output
    mx_summary$time <- boot_results$time

  } else if (do.boot == FALSE) {

    # Get summary results
    mx_summary <- mxGlm_summary(object, alpha = alpha, ci = show.ci,
                                mean.diff = mean.diff)

  }

  # Append more info to summary
  mx_summary$mean.diff <- mean.diff
  mx_summary$B <- B
  mx_summary$do.boot <- do.boot

  # Show preliminary results
  class(mx_summary) <- 'mxSummary'

  # print(mx_summary$coefficients)
  return(mx_summary)

}

