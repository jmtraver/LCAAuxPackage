
# ---------------------------------- mx_BCH() ----------------------------------

# This function uses the BCH approach to predict distal outcomes by latent class
# membership in a glm model framework.

# dependency
library(glmmTMB)


mx_BCH <- function(formula.tmb = NULL,
                   data = NULL,
                   post.prob = NULL,
                   prior.prob = NULL,
                   family = gaussian(),
                   robust.se = "none",
                   id = NULL,     # Do we still need?
                   reference_group = 1   # reference group when latent class is predictor
                   ) {
  # Stop if formula is NULL
  if (is.null(formula.tmb)) {
    stop("mx_BCH Error: formula cannot be NULL. Specify a formula object.")
  }
  # Stop if data is NULL
  if (is.null(data)) {
    stop("mx_BCH Error: data cannot be NULL. Specify a data frame.")
  }
  # Stop if post probabilities are NULL
  if (is.null(post.prob)) {
    stop("mx_BCH Error: post.prob cannot be NULL. Indicate a list of class probability columns.")
  }

  # Initialize values
  n_class <- length(post.prob)

  # Get modal class assignment
  data <- get_class_dummies(data = data, post.prob = post.prob)

  # Get formula
  new_formula <- get_frm(frm_original = formula.tmb, n_class = n_class,    ## Was causing error - JE
                         reference_group = reference_group)
  # new_formula <- formula.tmb

  # Calculate prior probabilities if NULL
  if (is.null(prior.prob)) {
    prior.prob <- colMeans(data[ , post.prob])
  }
  # Save prior probabilities
  prior_probs <- prior.prob

  # Initialize D matrix
  D <- matrix(NA, nrow = n_class, ncol = n_class)

  # Compute D matrix
  for (t in 1:n_class) {         # iterate over X = t
    for (s in 1:n_class) {       # iterate over W = s

      #                  P(X = t | Y)      *         P(W = s | Y)       /    P(X = t)
      D[t, s] <- mean(data[, post.prob[t]] * data[, paste0("class", s)] / prior_probs[t])

    }
  }
  # Solve D matrix
  weights <- solve(D)      # Should we transpose as well??? - Probably, let's check with Dans Code! - HW

  # Pivot longer
  # Pivoting with rep() is necessary to recognize clustering in the rownames
  data_long <- data[rep(1:nrow(data), each = n_class), ]
  data_long$class_new <- rep(1:n_class, times = nrow(data))

  # Save individual weights to each row
  for (i in 1:n_class) {
    # # For every row that belongs to class_new = i in the column wstar_i
    # data_long[data_long$class_new == i, "wstar_it"] <-
    #   # Take the sum of the probabilities in the columns class1, 2 and 3 times their
    #   # respective weights (= their values in the t(Dstar) matrix)
    #   rowSums(data_long[data_long$class_new == i, paste0("class", 1:n_class)] * weights[i, ])

    # ---- There was an error with matrix*vector multiplication
    # This is why we now expand the weights to have equal dimension to filter_class
    filter_class <- data_long[data_long$class_new == i, paste0("class", 1:n_class)]
    repeated_weights <- matrix(rep(weights[i, ], each = NROW(filter_class)),
                               nrow = NROW(filter_class))

    # For every row that belongs to class_new = i in the column wstar_i
    data_long[data_long$class_new == i, "wstar_it"] <-
      # Take the sum of the probabilities in the columns class1, 2 and 3 times their
      # respective weights (= their values in the t(Dstar) matrix)
      rowSums(filter_class * repeated_weights)

  }
  # fit1 <- glmmTMB(formula.tmb, weights = wstar_it, contrasts=NULL, data = data_long)

  fit1 <- glmmTMB(new_formula,
                  weights = wstar_it,
                  # contrasts = NULL,
                  data = data_long,
                  family = family)

  # class(fit1) <- 'mxGlm'

  return(fit1)
}
