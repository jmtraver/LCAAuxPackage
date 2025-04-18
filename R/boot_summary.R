
# ------------------------------- boot_summary() -------------------------------
#' Summarize Cluster Bootstrap Results
#'
#' Computes bootstrap standard errors, test statistics, p-values, and optional confidence intervals
#' from a bootstrap object returned by `cluster_boot()`. Also computes pairwise differences between
#' class estimates if requested.
#'
#' @param boot_return A list returned by `cluster_boot()` containing `boot_results` (a `boot` object)
#'   and `coefs` (original parameter estimates).
#' @param alpha Significance level for confidence intervals. Default is 0.05.
#' @param ci Logical. If TRUE, returns confidence intervals. Default is TRUE.
#' @param mean.diff Logical. If TRUE, computes pairwise mean differences between class estimates.
#'   Default is TRUE.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{coefficients}{Data frame with estimate, bootstrap SE, z-value, and p-value.}
#'   \item{conf_int}{(Optional) Confidence intervals for original estimates.}
#'   \item{difference}{(Optional) Pairwise class comparisons with estimate, SE, z, p, and CI.}
#'   \item{alpha}{Significance level used for CI.}
#'   \item{boot_means}{Bootstrap means of each parameter (for reference).}
#'   \item{B}{Number of bootstrap replicates.}
#' }
#'
#'
#' @export

# ...description...

boot_summary <- function(boot_return,
                         alpha = 0.05,
                         ci = TRUE,
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
                         B = boot_return$B,
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

    all_mean_comp <- vector(length = n_comp)
    orig_diffs <- vector(length = n_comp)

    for (comp in 1:n_comp) {
      c1 <- paste0("class", all_comp[1, comp])
      c2 <- paste0("class", all_comp[2, comp])
      mean_comparison <- paste0(c1, "_", c2)

      # mean differences original
      orig_diffs[comp] <- orig_coefs[c1] - orig_coefs[c2]

      # mean differences bootstrap
      all_mean_comp[comp] <- mean_comparison
      boot_coefs[[mean_comparison]] <- (boot_coefs[, c1] - boot_coefs[, c2])
    }
    # name original mean difference
    names(orig_diffs) <- all_mean_comp

    # bootstrap mean difference dataframe
    mean_diffs <- boot_coefs[, all_mean_comp, drop = FALSE]

    # compute bootstrap SE
    diff_se <- apply(mean_diffs, 2, sd)
    # compute test statistic
    z_emp <- orig_diffs/diff_se
    # get p value
    p_val <- 2*pnorm(abs(z_emp), lower.tail = FALSE)

    results_diff <- data.frame(`Estimate` = orig_diffs,
                               `Std. Error` = diff_se,
                               `z value` = z_emp,
                               `Pr(>|z|)` = p_val,
                                check.names = FALSE)

    if (ci == TRUE) {
      z_crit <- qnorm(1 - alpha/2)
      LL <- orig_diffs - z_crit*diff_se
      UL <- orig_diffs + z_crit*diff_se

      results_diff$LL <- LL
      results_diff$UL <- UL

    }

    summary_return$difference <- results_diff

  }
  return(summary_return)
}

