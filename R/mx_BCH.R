mx_BCH <- function(model = NULL, formula.tmb = NULL, n.class = NULL, data = NULL,
                   post.prob = NULL, prior.prob = NULL, family = gaussian(),
                   robust.se = "none", id = NULL) {

  # Initialize values
  n_class <- n.class

  # Get modal class assignment
  data <- get_class_dummies(data, post.prob)    # not tested yet

  # Get formula
  new_formula <- get_frm(frm.original = formula.tmb, n.class = n_class)

  if (is.null(prior.prob)) {
    prior_probs <- colMeans(data[ , post.prob])
  } #else {
  #prior_probs <- prior.prob
  # }
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
      rowSums(data_long[data_long$class_new == i, c("class1", "class2", "class3")] * weights[i, ])
  }
  fit1 <- glmmTMB(formula.tmb, weights = wstar_it, contrasts=NULL, data = data_long)
  return(fit1)
}
