
# ---------------------------------- mx_BCH() ----------------------------------

# This function uses the BCH approach to predict distal outcomes by latent class
# membership in a glm model framework.

#' mx_BCH
#'
#' This function uses BCH to predict distal outcomes by latent class membership in a GLM framework.
#'
#' @param formula.bch Specify the formula object: "latent_class"
#' @param data Your data
#' @param post.prob A list of latent class probability columns
#' @param prior.prob Prior probabilities; if NULL, will be calculated
#' @param family A GLM object specifying the error distribution. "Gaussian" assumes normal error distribution
#' @param class.order A vector of integers specifying the class order for the posterior probability columns supplied. The default is to number the classes in the order the posterior probabilties are supplied.
#' @param robust.se Robust standard error correction. Currently set to "none", as it is not available
#' @param id A column in data that has original ID numbers
#' @param reference_group The reference group when latent class is the predictor
#'
#' @return A list of class \code{"mxGlm"} containing:
#' \describe{
#'   \item{\code{model}}{The fitted model object returned by \code{mxBCHfit}.}
#'   \item{\code{data}}{A long-format data frame with class assignment and BCH weights.}
#'   \item{\code{formula}}{The final formula used in model estimation.}
#' }
#' @export

mx_BCH <- function(formula.bch = NULL,
                   data = NULL,
                   post.prob = NULL,
                   prior.prob = NULL,
                   family = gaussian(),
                   class.order = NULL,
                   robust.se = "none",
                   id = NULL,     # Do we still need?
                   reference_group = 1   # reference group when latent class is predictor
                   ) {
  # Stop if formula is NULL
  if (is.null(formula.bch)) {
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

  # FOR NOW: Stop if family is not gaussian
  if (is.null(post.prob)) {
    stop("mx_BCH Error: post.prob cannot be NULL. Indicate a list of class probability columns.")
  }

  # Force data to be a dataframe
  data <- data.frame(data)

  # Initialize values
  n_class <- length(post.prob)

  # Gets dummy variables modal class assignment
  data <- get_class_dummies(data = data, post.prob = post.prob)
  # Get modal class assignment
  data <- assigned_class(data = data, post.prob = post.prob)


  # Get formula
  new_formula <- get_frm(frm_original = formula.bch, n_class = n_class,
                         reference_group = reference_group)
  #new_formula <- formula.bch
  # frm <- formula.bch
  frm <- new_formula

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
  # weights <- solve(D)      # Should we transpose as well??? - Probably, let's check with Dans Code! - HW
  weights <- t(solve(D))     # Yes, we need the transpose in order for the weights to work correctly (HW)

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

  # fit1 <- glmmTMB(formula.bch, weights = wstar_it, contrasts=NULL, data = data_long)
  oc <- as.character(frm[2])
  czv <- as.character(frm[3])

  fit1 <- mxBCHfit(n_class = n_class, w = data_long$wstar_it, y= data_long[[oc]],
                   cls = data_long[[czv]])

  # append long format data file to return list
  fit1$data <- data_long
  fit1$formula <- frm
  fit1$orig_formula <- formula.bch

  # If class order is supplied to function, change class ordering in output
  if (!is.null(class.order)) {
    if (length(class.order) != n_class) {
      warning("Class order argument ignored, because the length of class.order does not match the number of classes.")
      class_names_new <- 1:n_class
    } else {
      class_names <- fit1$class_names
      class_names_new <- vector(length = n_class)
      for (i in 1:n_class) {
        class_names_new[class.order[i]] <- class_names[i]
      }
      fit1$class_names <- class_names_new
    }
  }

  class(fit1) <- 'mxGlm'

  return(fit1)
}


# Extract results
#mu_hat <- fit1$par[1:n_class]
#sigma_hat <- exp(fit1$par[n_class + 1])

#cat("Estimated class means (mu_t):\n")
#print(mu_hat)
#cat("Estimated residual SD (sigma):", sigma_hat, "\n")
