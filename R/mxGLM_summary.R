
# ------------------------------ mxGlm_summary() -------------------------------

mxGlm_summary <- function(mxGlm_obj, alpha = 0.05, ci = TRUE) {

  if (class(mxGlm_obj) != 'mxGlm') {
    stop(paste0("mxGLM_summary not usable on object of class '",
                class(mxGlm_obj), "'."))
  }

  # Extract coefficients
  coefs <- mxGlm_obj$par[-length(mxGlm_obj$par)]
  names(coefs) <- mxGlm_obj$class_names

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
