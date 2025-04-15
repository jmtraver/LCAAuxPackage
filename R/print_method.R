
# ---------------------------- print.mx_summary()  -----------------------------

#'@method print mx_summary
#'@export

## idea:
# - this should print whatever we want the user to see when they call our
#   summary function

# Note: this is a very rudimentary version.


# Only works when loaded into the environment?                              ####

print.mxSummary <- function(m) {

  time_diff <- m$time
  output <- m$coefficients
  mean_diff <- m$mean.diff
  do_boot <- m$do.boot
  B <- m$B

  if(!is.null(time_diff)) {
    print(time_diff)
  }

  if(!is.null(m$conf_int)) {
    results_ci <- m$conf_int
    output <- cbind(output, results_ci)
  }

  if(do_boot == FALSE) {
    res_text <- "BCH analysis (analytic standard errors not implemented yet)"
    diff_text <- "Mean comparisons (inference not implemented yet)"

  } else if (do_boot == TRUE) {
    res_text <- paste0("BCH analysis (with bootstrap standard errors: B = ",
                       B, ")")
    diff_text <- paste0("Mean comparisons (with bootstrap standard errors: B = ",
                        B, ")")
  }

  # Pretty output
  cat(paste0("\n", res_text, "\n"))
  print(output)
  cat("\n")

  if (mean_diff == TRUE) {
    cat(paste0(diff_text, "\n"))
    print(m$difference)
    cat("\n")
  }

}
