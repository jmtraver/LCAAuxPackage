
# ------------------------------- boot_summary() -------------------------------

# ...description...

boot_summary <- function(boot_return, alpha = 0.05, ci = TRUE,
                         mean.diff = TRUE) {

  boot_coefs <- boot_return$boot_results$t    # when using boot to fit the model
  orig_coefs <- boot_return$coefs
  colnames(boot_coefs) <- names(orig_coefs)


  boot_means <- colMeans(boot_coefs)               ####

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


  if (mean.diff == TRUE) {

    # number of classes
    n_class <- length(orig_coefs) - 1
    # set up all pair comparisons
    all_comp <- combn(1:n_class, 2)
    # number of comparisons
    n_comp <- NCOL(all_comp)
    # make data frame
    boot_coefs <- data.frame(boot_coefs)
    colnames(boot_coefs) <- names(orig_coefs)

    for (comp in 1:n_comp) {
      c1 <- paste0("class", all_comp[1, comp])
      c2 <- paste0("class", all_comp[2, comp])
      mean_comparison <- paste0(c1, "_", c2)
      boot_coefs[[mean_comparison]] <- (boot_coefs[, c1] - boot_coefs[, c2])
    }

    # MISSING summarize results for mean comparisons!

  }

  return(summary_return)
}

