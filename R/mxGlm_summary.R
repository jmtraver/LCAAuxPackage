
# ------------------------------ mxGlm_summary() -------------------------------
#' @export



mxGlm_summary <- function(mxGlm_obj,
                          alpha = 0.05,
                          ci = TRUE,
                          mean.diff = TRUE) {

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

  if (mean.diff == TRUE) {
    # number of classes
    n_class <- length(coefs)
    # set up all pair comparisons
    all_comp <- combn(1:n_class, 2)
    # number of comparisons
    n_comp <- NCOL(all_comp)

    all_mean_comp <- vector(length = n_comp)
    orig_diffs <- vector(length = n_comp)

    for (comp in 1:n_comp) {
      c1 <- paste0("class", all_comp[1, comp])
      c2 <- paste0("class", all_comp[2, comp])
      mean_comparison <- paste0(c1, "_", c2)
      all_mean_comp[comp] <- mean_comparison

      # mean differences original
      orig_diffs[comp] <- coefs[c1] - coefs[c2]
    }

    # name original mean difference
    names(orig_diffs) <- all_mean_comp

    # MISSING: inference, confidence intervals

    results_diff <- data.frame(`Estimate` = orig_diffs,
                               check.names = FALSE)

    summary_return$difference <- results_diff

  }

  return(summary_return)

}
