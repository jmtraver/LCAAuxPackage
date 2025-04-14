
# --------------------------------- get_frm() ----------------------------------

# This function takes the user defined formula and exchanges "latent_class" for
# the dummie variables class1, class2, ... classn_class.

# dependency
library(stringr)

# function
get_frm <- function(frm_original, n_class, reference_group = 1) {

  # check if latent_class was included in formula
  fc <- as.character(frm_original)
  frm_char <- paste0(c(fc[2], fc[1], fc[3]), collapse = " ")
  check_LC <- stringr::str_detect(frm_char, "latent_class")
  if (check_LC == FALSE) {
    stop(paste0("'latent_class' not included in formula: ",
                frm_char,
                "\nCheck spelling or different analytic method."))
  }

  # check if reference group is realistic
  if (reference_group > n_class) {
    stop(paste0("The reference group (",
                reference_group,
                ") exceeds the total number of latent classes"))
  }

  # check if ":" or "*" is in formula - if yes stop for now
  # if (reference_group > n_class) {
  #   stop(paste0("The reference group (",
  #               reference_group
  #               ,") exceeds the total number of latent classes"))
  # }

  # extract outcome information
  outcome <- fc[2]

  # extract predictor information
  pred_info <- fc[3]

  # extract other predictor(s) that are not latent_class
  other_preds <- stringr::str_split(pred_info, "latent_class")[[1]]

  # # create dummies for latent class
  # class_numbers <- (1:n_class)[-reference_group]
  # classes <- paste0("class", class_numbers, collapse = " + ")

  # rename latent_class to modal assigned class
  classes <- "ass_class"

  # paste predictors back together with class dummies
  new_preds <- paste0(other_preds[1], classes, other_preds[2])

  # recompile formula
  new_frm <- as.formula(paste0(outcome, " ~ ", new_preds))
  return(new_frm)
}

# # Test function
# my_frm <- y ~  latent_class*x
# new_frm <- get_frm(my_frm, n_class = 5, reference_group = 1)
# new_frm

