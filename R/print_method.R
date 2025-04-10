
# ---------------------------- print.mx_summary()  -----------------------------

## idea:
# - this should print whatever we want the user to see when they call our
#   summary function

# Note: this is a very rudimentary version.


# Only works when loaded into the environment?                              ####

print.mxSummary <- function(m) {

  time_diff <- m$time
  output <- m$coefficients

  if(!is.null(time_diff)) {
    print(time_diff)
  }

  if(!is.null(m$conf_int)) {
    results_ci <- m$conf_int
    output <- cbind(output, results_ci)
  }

  # Pretty output
  cat("\n")
  print(output)
  cat("\n")

}
