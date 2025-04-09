
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

  if(n_cluster == length(data$id)) {
    stop("No clustering detected.")
  }

  # get function call, formula and family from model
  fun_call <- mx_glm_obj$call
  formula <- mx_glm_obj$call$formula
  family <- mx_glm_obj$modelInfo$family$family

  # how to incooporate link info?  mx_glm_obj$modelInfo$family$link        #####

  set.seed(seed)

  # model on original data
  # this is not necessary I believe, but it shouldn't add too much runtime
  orig_model <- glmmTMB(formula = formula, data = data, family = family,
                        weights = wstar_it)
  orig_param <- summary(orig_model)$coefficients$cond[, "Estimate"]
  p <- length(orig_param)
  # alternative is to extract the parameter information INCLUDING the rownames
  # of all effects from the mx_glm_obj directly

  # initiate bootstrap results matrix
  boot_results <- matrix(NA, nrow = B, ncol = p)
  colnames(boot_results) <- names(orig_param)

  for (b in 1:B) {

    # sample from clusters
    new_ids <- sample(cluster_id, size = n_cluster, replace = TRUE)

    # initiate data file without any rows
    data_boot <- data %>% dplyr::filter(id == -1)
    # select all datapoints from each sampled cluster and build new dataframe
    for (i in sort(new_ids)) {                             # sort is not necessary
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
    print(time_2 - time_1)            ##### THIS TAKES FOREVER
  }

  return(boot_return)
}
