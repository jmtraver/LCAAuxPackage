
# ------------------------------ mxGLM_summary() -------------------------------

mxGLM_summary <- function(mxGLM_return, alpha = 0.05, ci = TRUE) {

  # get bootstrap standard error
  boot_se <- apply(boot_coefs, 2, sd)
  # compute test statistic
  z_emp <- orig_coefs/boot_se
  # get p value
  p_val <- 2*pnorm(abs(z_emp), lower.tail = FALSE)

  results <- data.frame(`Estimate` = orig_coefs,
                        `Std. Error` = boot_se,
                        `z value` = z_emp,
                        `Pr(>|z|)` = p_val,
                        check.names = FALSE
  )

  summary_return <- list(coefficients = results,
                         alpha = alpha,
                         ci = ci,
                         boot_means = boot_means,
                         check.names = FALSE)

  if (ci == TRUE) {
    z_crit <- qnorm(1 - alpha/2)
    LL <- orig_coefs - z_crit*boot_se
    UL <- orig_coefs + z_crit*boot_se

    results_ci <- data.frame(`LL` = LL,
                             `UL` = UL)

    summary_return$conf_int <- results_ci
  }

  return(summary_return)

}
