# ~-~-~-~-~-~-~-~-~-~-~- loglik distal outcome function -~-~-~-~-~-~-~-~-~-~-~-#


mxBCHfit <- function(n_class = n_class,
                     y = outcome,
                     w = data_long$wstar_it,
                     cls) {


# Set values for the loglik fx
#n_class <- length(unique(data_long$Class))
#y <- data_long$Alc12
#w <- data_long$wstar_it
#class_vec <- data_long$Class
n_class <- n_class
y <- y
w <- w
class_vec <- cls

loglik_distal <- function(params, outcome = y, cv = class_vec) {
  mu <- params[1:n_class]
  log_sigma <- params[n_class + 1]
  sigma <- exp(log_sigma)

  # Safety: if sigma is too small or non-finite, return large value
  if (!is.finite(sigma) || sigma <= 1e-6) return(1e10)

  # Predicted mean for each row in the long-format data
  mu_i <- mu[as.integer(cv)]  # Ensure proper indexing

  # Check for any bad values, replace with very large number
  if (any(!is.finite(mu_i))) return(1e10)
  if (any(!is.finite(y))) return(1e10)

  # Compute weighted log-likelihood
  ## Log-likelihood of each value of Y, given mean mu and sd sigma
  ll_vec <- dnorm(y, mean = mu_i, sd = sigma, log = TRUE)
  ## Return large value for bad observations
  if (any(!is.finite(ll_vec))) return(1e10)
  ## Weight each loglik with wstar_it
  ll <- sum(w * ll_vec)
  ## Minimize for optim
  return(-ll)
}

### Informed initialization:
# Calculate initial values for mu based on class means
class_means <- tapply(y, cls, mean, na.rm = TRUE)
class_stddevs <- tapply(y, cls, sd, na.rm = TRUE)

# Initialize mu as the class means
mu_init <- class_means

## OR add small random perturbation to initial values
#set.seed(123)  # For reproducibility
#mu_init <- mu_init + rnorm(n_class, mean = 0, sd = 0.5)  # Adjust the sd as needed

# Initialize sigma as the overall standard deviation of Alc12
sigma_init <- sd(y, na.rm = TRUE)

# Log-transform the initial sigma (since the parameter sigma is log-transformed)
log_sigma_init <- log(sigma_init)

# Combine into a single vector for initial parameters
init <- c(mu_init, log_sigma_init)

### OR JUST USE-- Basic intialization, no information
#init <- c(rep(mean(data_long$Alc12, na.rm = TRUE), n_class), log(1))

# Maximize log-likelihood
fit <- optim(init, loglik_distal, method = "BFGS", control = list(maxit = 1000), hessian=TRUE)

# append information about outcome, classes etc
fit$y <- y
fit$class_names <- paste0("class", 1:n_class)
fit$class_vec <- class_vec
fit$weights <- w

return(fit)
}
