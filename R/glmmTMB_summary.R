
# ----------------------------- glmmTMB_summary() ------------------------------

# ...description...

glmmTMB_summary <- function(glmmTMB_obj, alpha = 0.05, ci = TRUE) {

  summary_res <- summary(glmmTMB_obj)

  results <- data.frame(summary_res$coefficients$cond,
                        check.names = FALSE)

  orig_coefs <- results[, 1]
  orig_se <- results[, 2]
  names(orig_coefs) <- names(orig_se) <- rownames(results)

  summary_return <- list(coefficients = results,
                         alpha = alpha,
                         ci = ci)

  if (ci == TRUE) {
    z_crit <- qnorm(1 - alpha/2)
    LL <- orig_coefs - z_crit*orig_se
    UL <- orig_coefs + z_crit*orig_se

    results_ci <- data.frame(`LL` = LL,
                             `UL` = UL)

    summary_return$conf_int <- results_ci
  }

  return(summary_return)

}
