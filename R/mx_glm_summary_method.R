
# ------------------------------ summary.mx_glm() ------------------------------

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

summary.mx_glm <- function(object,
                           B = 999,
                           seed = NULL,
                           alpha = 0.05,
                           show.ci = FALSE,
                           check.time = TRUE) {

  # Fit cluster bootstrap
  boot_results <- cluster_boot(object, B = B, seed = seed,
                               check.time = check.time)

  # Get summary results
  mx_summary <- boot_summary(boot_results, alpha = alpha, ci = show.ci)

  # Show preliminary results
  class(mx_summary) <- 'mx_summary'
  return(mx_summary)

}

