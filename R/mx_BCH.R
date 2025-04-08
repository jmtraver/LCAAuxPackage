
# ---------------------------------- mx_BCH() ----------------------------------

# This function takes

mx_BCH <- function(formula.tmb = NULL,
                   data = NULL,
                   post.prob = NULL,
                   prior.prob = NULL,
                   family = gaussian(),
                   robust.se = "none",
                   id = NULL,     # Do we still need?
                   reference_group = 1   # reference group when latent class is predictor
                   ) {

  if (is.null(formula.tmb)) {
    stop("mx_BCH Error: formula cannot be NULL. Specify a formula object.")
  }

  if (is.null(data)) {
    stop("mx_BCH Error: data cannot be NULL. Specify a data frame.")
  }

  if (is.null(post.prob)) {
    stop("mx_BCH Error: post.prob cannot be NULL. Indicate a list of class probabilities.")
  }

  # Initialize values
  n_class <- length(post.prob)

  # Get modal class assignment
  data <- get_class_dummies(data = data, post.prob = post.prob)

  # Get formula
  new_formula <- get_frm(frm_original = formula.tmb, n_class = n_class,
                         reference_group = reference_group)

  if (is.null(prior.prob)) {
    prior_probs <- colMeans(data[ , post.prob])
  } else {
  prior_probs <- prior.prob
  }

  D <- matrix(NA, nrow = n_class, ncol = n_class)

  # Compute D matrix
  for (t in 1:n_class) {         # iterate over X = t
    for (s in 1:n_class) {       # iterate over W = s

      #                           P(X = t | Y)        *         P(W = s | Y)        /    P(X = t)
      D[t, s] <- mean(data[, paste0("Cprob", t)] * data[, paste0("class", s)] / prior_probs[t])
    }
  }
  # Solve D matrix
  weights <- solve(D)

  # Pivot longer
  data_long <- data[rep(1:nrow(data), each = n_class), ]
  data_long$class_new <- rep(1:n_class, times = nrow(data))

  # Save individual weights to each row
  for (i in 1:n_class) {
    # For every row that belongs to class_new = i in the column wstar_i
    data_long[data_long$class_new == i, "wstar_it"] <-
      # Take the sum of the probabilities in the columns class1, 2 and 3 times their
      # respective weights (= their values in the t(Dstar) matrix)
      rowSums(data_long[data_long$class_new == i, paste0("class", 1:n_class)] * weights[i, ])

  }
  # fit1 <- glmmTMB(formula.tmb, weights = wstar_it, contrasts=NULL, data = data_long)
  fit1 <- glmmTMB(new_formula,
                  weights = wstar_it,
                  # contrasts = NULL,
                  data = data_long,
                  family = family)

  return(fit1)
}
