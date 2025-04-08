
# --------------------------------- get_frm() ----------------------------------

# This function takes the user defined formula and exchanges "latent_class" for
# the dummie variables class1, class2, ... classn_class.

# dependency
library(stringr)

# function
get_frm <- function(frm_original, n_class, reference_group = 1) {

  # extract outcome information
  outcome <- as.character(frm_original)[2]

  # extract predictor information
  pred_info <- as.character(frm_original)[3]

  # extract other predictor(s) that are not latent_class
  other_preds <- stringr::str_split(pred_info, "latent_class")[[1]]

  # create dummies for latent class
  class_numbers <- (1:n_class)[-reference_group]
  classes <- paste0("class", class_numbers, collapse = " + ")

  # paste predictors back together with class dummies
  new_preds <- paste0(other_preds[1], classes, other_preds[2])

  # recompile formula
  new_frm <- as.formula(paste0(outcome, " ~ ", new_preds))
  return(new_frm)
}

# # Test function
# my_frm <- outcome ~ pred1 + latent_class + pred2
# new_frm <- get_frm(my_frm, n.class = 3)
# new_frm




