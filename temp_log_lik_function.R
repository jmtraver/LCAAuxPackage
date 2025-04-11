# ~-~-~-~-~-~-~-~-~-~-~- loglik distal outcome function -~-~-~-~-~-~-~-~-~-~-~-#
## This code estimates parameters by maximizing the long-format (clustered) data
## per Vermunt (2010) eq 17. It is only tested on our NLYS data, estimating
## Alc12 ~ Class. It uses the already-calculated wstar_it from the previous fx.
## It unfortunately only produces parameter estimates: no SE, z, p, AIC, etc.
## These data/model produce a singular Hessian matrix, possibly due to the
## class means (initialization values) being too similar. We need the Hessian
## to be non-singular to calculate SEs using sandwich cluster robust.
## Need to update this code to integrate into our function, aka; to be useful
## in other data/models!



#T <- length(unique(data_long$Class))
#y <- data_long$Alc12
#w <- data_long$wstar_it
#class_vec <- data_long$Class

loglik_distal <- function(params) {
  mu <- params[1:T]
  log_sigma <- params[T + 1]
  sigma <- exp(log_sigma)

  # Safety: if sigma is too small or non-finite, return large value
  if (!is.finite(sigma) || sigma <= 1e-6) return(1e10)

  # Predicted mean for each row in the long-format data
  mu_i <- mu[as.integer(data_long$class)]  # Ensure proper indexing

  # Check for any bad values
  if (any(!is.finite(mu_i))) return(1e10)
  if (any(!is.finite(data_long$Alc12))) return(1e10)

  # Compute weighted log-likelihood
  ll_vec <- dnorm(data_long$Alc12, mean = mu_i, sd = sigma, log = TRUE)
  if (any(!is.finite(ll_vec))) return(1e10)

  ll <- sum(w * ll_vec)  # Use 'w' here instead of 'w_star' if needed
  return(-ll)  # we minimize
}

### Informed initialization:
# Calculate initial values for mu based on class means
#class_means <- tapply(data_long$Alc12, data_long$Class, mean, na.rm = TRUE)
#class_stddevs <- tapply(data_long$Alc12, data_long$Class, sd, na.rm = TRUE)

# Initialize mu as the class means
#mu_init <- class_means
# Add small random perturbation to initial values
#set.seed(123)  # For reproducibility
#mu_init <- mu_init + rnorm(T, mean = 0, sd = 0.5)  # Adjust the sd as needed


# Initialize sigma as the overall standard deviation of Alc12
#sigma_init <- sd(data_long$Alc12, na.rm = TRUE)

# Log-transform the initial sigma (since the parameter sigma is log-transformed)
#log_sigma_init <- log(sigma_init)

# Combine into a single vector for initial parameters
#init <- c(mu_init, log_sigma_init)

### Basic intialization
#init <- c(rep(mean(data_long$Alc12, na.rm = TRUE), T), log(1))

# Maximize log-likelihood
#fit <- optim(init, loglik_distal, method = "BFGS", control = list(maxit = 1000), hessian=TRUE)

# Extract results
#mu_hat <- fit$par[1:T]
#sigma_hat <- exp(fit$par[T + 1])

#cat("Estimated class means (mu_t):\n")
#print(mu_hat)
#cat("Estimated residual SD (sigma):", sigma_hat, "\n")

################# Singular, cant use
# Calculate Hessian for Sandwich Estimator
#hessian_matrix <- fit$hessian
#if (any(abs(eigen(hessian_matrix)$values)<1e-6)) {
#  warning("The Hessian matrix is singular or near-singular. There is potentially an issue with the model or data.\n")
#}
