
# ------------------------------- cluster_boot() -------------------------------
#' @export

# ...description...

# dependency
library(dplyr)
library(boot)

cluster_boot <- function(mxGlm_obj,
                         B = 999,
                         seed = NULL,
                         check.time = TRUE) {

  time_1 <- Sys.time()

  # Initialize data
  y <- mxGlm_obj$y
  class_vec <- mxGlm_obj$class_vec
  wstar_it <- mxGlm_obj$weights
  n_class <- length(mxGlm_obj$class_names)
  data <- mxGlm_obj$data
  row_names <- rownames(data)
  data$id <- floor(as.numeric(row_names))

  # get formula
  frm <- mxGlm_obj$formula
  # name for outcome and class variable
  oc <- as.character(frm[2])
  czv <- as.character(frm[3])

  # get cluster ids
  cluster_id <- unique(data$id)
  n_cluster <- length(cluster_id)

  if(n_cluster >= length(data$id)) {
    stop("No clustering detected.")
  }

  # # get function call, formula and family from model
  # fun_call <- mxGlm_obj$call
  # formula <- mxGlm_obj$call$formula
  # family <- mxGlm_obj$modelInfo$family$family

  # how to incooporate link info?  mxGlm_obj$modelInfo$family$link        #####

  set.seed(seed)

  # model on original data
  # orig_model <- mxGlm_obj
  # class(orig_model) <- 'glmmTMB'
  # orig_param <- summary(orig_model)$coefficients$cond[, "Estimate"]
  # p <- length(orig_param)

  orig_param <- mxGlm_obj$par
  names(orig_param) <- c(mxGlm_obj$class_names, "sigma_maybe")
  p <- length(orig_param)


  # bootstrap with boot package for slightly faster results
  boot_fun <- function(d, i,
                       family = gaussian(),
                       formula = NULL) {

    # sample n ids from the cluster ids, where n = number of clusters
    sampled_ids <- unique(d$id)[i]

    # make sure the bootstrap data file is of the same as the original
    id_counts <- table(sampled_ids)
    data_boot <- do.call(rbind, lapply(names(id_counts), function(i) {
      subset_rows <- d[d$id == as.numeric(i), ]
      subset_rows[rep(1:nrow(subset_rows), times = id_counts[i]), ]
    }))

    # fit model
    # boot_model <- glmmTMB::glmmTMB(formula = formula, data = data_boot,
    #                                family = family, weights = wstar_it)

    # fit model
    boot_model <- mxBCHfit(n_class = n_class, w = data_boot$wstar_it,
                           y = data_boot[[oc]], cls = data_boot[[czv]])

    boot_results <- boot_model$par
    return(boot_results)
  }


  boot_results <- boot::boot(data = data, statistic = boot_fun,
                             R = B,
                             #formula = formula,
                             #family = gaussian()
                             )

  # OLD BOOTSTRAP: took longer
  # # initiate bootstrap results matrix
  # boot_results <- matrix(NA, nrow = B, ncol = p)
  # colnames(boot_results) <- names(orig_param)
  #
  # for (b in 1:B) {
  #
  #   # sample from clusters
  #   new_ids <- sample(cluster_id, size = n_cluster, replace = TRUE)
  #
  #   # initiate data file without any rows
  #   data_boot <- data %>% dplyr::filter(id == -1)
  #   # select all datapoints from each sampled cluster and build new dataframe
  #   for (i in sort(new_ids)) {  # sort is not necessary but nice for debug
  #     one_cluster <- data %>% dplyr::filter(id == i)
  #     data_boot <- rbind(data_boot, one_cluster)
  #   }
  #
  #   # fit model
  #   boot_model <- glmmTMB(formula = formula, data = data_boot, family = family)
  #   boot_results[b, ] <- summary(boot_model)$coefficients$cond[, "Estimate"]
  #
  # }

  boot_return <- list(
    boot_results = boot_results,
    coefs = orig_param,
    # formula = formula,
    B = B,
    family = family
  )

  if (check.time == TRUE) {
    time_2 <- Sys.time()
    boot_return$time <- time_2 - time_1
  }

  return(boot_return)
}
