
# ------------------------------- cluster_boot() -------------------------------

# ...description...

# dependency
library(dplyr)

cluster_boot <- function(mx_glm_obj,
                         B = 999,
                         seed = NULL,
                         check.time = TRUE) {

  time_1 <- Sys.time()

  # get data from model
  data <- mx_glm_obj$frame
  row_names <- rownames(data)
  data$id <- floor(as.numeric(row_names))
  # copy weights column to usable name
  data$wstar_it <- data[, "(weights)"]

  # get cluster ids
  cluster_id <- unique(data$id)
  n_cluster <- length(cluster_id)

  if(n_cluster >= length(data$id)) {
    stop("No clustering detected.")
  }

  # get function call, formula and family from model
  fun_call <- mx_glm_obj$call
  formula <- mx_glm_obj$call$formula
  family <- mx_glm_obj$modelInfo$family$family

  # how to incooporate link info?  mx_glm_obj$modelInfo$family$link        #####

  set.seed(seed)

  # model on original data
  orig_model <- mx_glm_obj
  class(orig_model) <- 'glmmTMB'
  orig_param <- summary(orig_model)$coefficients$cond[, "Estimate"]
  p <- length(orig_param)

  # initiate bootstrap results matrix
  boot_results <- matrix(NA, nrow = B, ncol = p)
  colnames(boot_results) <- names(orig_param)

  for (b in 1:B) {

    # sample from clusters
    new_ids <- sample(cluster_id, size = n_cluster, replace = TRUE)

    # initiate data file without any rows
    data_boot <- data %>% dplyr::filter(id == -1)
    # select all datapoints from each sampled cluster and build new dataframe
    for (i in sort(new_ids)) {  # sort is not necessary but nice for debug
      one_cluster <- data %>% dplyr::filter(id == i)
      data_boot <- rbind(data_boot, one_cluster)
    }

    # fit model
    boot_model <- glmmTMB(formula = formula, data = data_boot, family = family)
    boot_results[b, ] <- summary(boot_model)$coefficients$cond[, "Estimate"]

  }

  boot_return <- list(
    boot_results = boot_results,
    coefs = orig_param,
    formula = formula,
    B = B,
    family = family
  )

  if (check.time == TRUE) {
    time_2 <- Sys.time()
    boot_return$time <- time_2 - time_1
  }

  return(boot_return)
}
