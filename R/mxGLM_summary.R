
# ------------------------------ mxGLM_summary() -------------------------------

mxGLM_summary <- function(mxGLM_return, alpha = 0.05, ci = TRUE) {

  if (class(mxGLM_return) != 'mxGlm') {
    stop(paste0("mxGLM_summary not usable on object of class '",
                class(mxGLM_return), "'."))
  }

  # Extract coefficients
  coefs <- mxGLM_return$par[-length(mxGLM_return$par)]
  names(coefs) <- mxGLM_return$class_names

  # Get standard errors - NOT IMPLEMENTED YET
  se <- rep(NA, times = length(coefs))
  # Get test statistic - NOT IMPLEMENTED YET
  z_emp <- coefs/se
  # Get p-value - NOT IMPLEMENTED YET
  p_val <- 2*pnorm(abs(z_emp), lower.tail = FALSE)



  results <- data.frame(`Estimate` = coefs,
                        `Std. Error` = se,
                        `z value` = z_emp,
                        `Pr(>|z|)` = p_val,
                        check.names = FALSE
  )

  summary_return <- list(coefficients = results,
                         alpha = alpha,
                         ci = ci,
                         check.names = FALSE)

  if (ci == TRUE) {
    z_crit <- qnorm(1 - alpha/2)
    LL <- coefs - z_crit*se
    UL <- coefs + z_crit*se

    results_ci <- data.frame(`LL` = LL,
                             `UL` = UL)

    summary_return$conf_int <- results_ci
  }

  return(summary_return)

}
