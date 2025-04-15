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


# Set values for the loglik fx
T <- length(unique(data_long$Class))
y <- data_long$Alc12
w <- data_long$wstar_it
class_vec <- data_long$Class

loglik_distal <- function(params) {
  mu <- params[1:T]
  log_sigma <- params[T + 1]
  sigma <- exp(log_sigma)

  # Safety: if sigma is too small or non-finite, return large value
  if (!is.finite(sigma) || sigma <= 1e-6) return(1e10)

  # Predicted mean for each row in the long-format data
  mu_i <- mu[as.integer(data_long$class)]  # Ensure proper indexing

  # Check for any bad values, replace with very large number
  if (any(!is.finite(mu_i))) return(1e10)
  if (any(!is.finite(data_long$Alc12))) return(1e10)

  # Compute weighted log-likelihood
  ## Log-likelihood of each value of Y, given mean mu and sd sigma
  ll_vec <- dnorm(data_long$Alc12, mean = mu_i, sd = sigma, log = TRUE)
  ## Return large value for bad observations
  if (any(!is.finite(ll_vec))) return(1e10)
  ## Weight each loglik with wstar_it
  ll <- sum(w * ll_vec)
  ## Minimize for optim
  return(-ll)
}

### Informed initialization:
# Calculate initial values for mu based on class means
class_means <- tapply(data_long$Alc12, data_long$Class, mean, na.rm = TRUE)
class_stddevs <- tapply(data_long$Alc12, data_long$Class, sd, na.rm = TRUE)

# Initialize mu as the class means
mu_init <- class_means

## OR add small random perturbation to initial values
#set.seed(123)  # For reproducibility
#mu_init <- mu_init + rnorm(T, mean = 0, sd = 0.5)  # Adjust the sd as needed

# Initialize sigma as the overall standard deviation of Alc12
sigma_init <- sd(data_long$Alc12, na.rm = TRUE)

# Log-transform the initial sigma (since the parameter sigma is log-transformed)
log_sigma_init <- log(sigma_init)

# Combine into a single vector for initial parameters
init <- c(mu_init, log_sigma_init)

### OR JUST USE-- Basic intialization, no information
#init <- c(rep(mean(data_long$Alc12, na.rm = TRUE), T), log(1))

# Maximize log-likelihood
fit <- optim(init, loglik_distal, method = "BFGS", control = list(maxit = 1000), hessian=TRUE)

# Extract results
mu_hat <- fit$par[1:T]
sigma_hat <- exp(fit$par[T + 1])

cat("Estimated class means (mu_t):\n")
print(mu_hat)
cat("Estimated residual SD (sigma):", sigma_hat, "\n")

################# Singular, cant use (way to fix this?)
# Calculate Hessian for Sandwich Estimator
hessian_matrix <- fit$hessian
if (any(abs(eigen(hessian_matrix)$values)<1e-6)) {
  warning("The Hessian matrix is singular or near-singular. There is potentially an issue with the model or data.\n")
}

###################################################### Any?
n_class <- 3
y <- Alc12
w <- wstar_it
class_vec <- dat$Class
dm <- dat$Math98:dat$RComp98

loglik_distal <- function(params, outcome = y, cv = class_vec, dm) {
  mu <- params[1:n_class]
  log_sigma <- params[n_class + 1]
  sigma <- exp(log_sigma)
  n_preds <- ncol(dm)
  beta <- params[(n_class + 1):(n_class + n_preds)]

  # Safety: if sigma is too small or non-finite, return large value
  if (!is.finite(sigma) || sigma <= 1e-6) return(1e10)

  # Predicted mean for each row in the long-format data
  mu_i <- mu[as.integer(cv)]  # Ensure proper indexing
  lin <- mu_i + dm %*% beta

  # Check for any bad values, replace with very large number
  if (any(!is.finite(mu_i))) return(1e10)
  if (any(!is.finite(y))) return(1e10)

  # Compute weighted log-likelihood
  ## Log-likelihood of each value of Y, given mean mu and sd sigma
  ll_vec <- dnorm(y, mean = lin, sd = sigma, log = TRUE)
  ## Return large value for bad observations
  if (any(!is.finite(ll_vec))) return(1e10)
  ## Weight each loglik with wstar_it
  ll <- sum(w * ll_vec)
  ## Minimize for optim
  return(-ll)
}
# ~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-#
####                        Variable distributions                     ####
# ~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-#
mxBCHfit <- function(n_class = n_class,
                     y = outcome,
                     w = data_long$wstar_it,
                     cls,
                     fam) {


  # Set values for the loglik fx
  n_class <- n_class
  y <- y
  w <- w
  class_vec <- cls

if (fam == "gaussian") {
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
  class_means <- tapply(y, class_vec, mean, na.rm = TRUE)
  class_stddevs <- tapply(y, class_vec, sd, na.rm = TRUE)

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
}

if (fam == "binomial") {
  loglik_distal <- function(params, outcome = y, cv = class_vec) {
    mu <- params[1:n_class]  # These are logits
    mu_i <- mu[as.integer(cv)]  # Assign logit to each row

    # Safety check
    if (any(!is.finite(mu_i))) return(1e10)
    if (any(!is.finite(outcome))) return(1e10)

    # Convert logits to probabilities
    p_i <- 1 / (1 + exp(-mu_i))

    # Safety: Clamp extreme probabilities
    eps <- 1e-8
    p_i <- pmin(pmax(p_i, eps), 1 - eps)

    # Log-likelihood for Bernoulli
    ll_vec <- outcome * log(p_i) + (1 - outcome) * log(1 - p_i)

    if (any(!is.finite(ll_vec))) return(1e10)

    # Weighted sum of log-likelihoods
    ll <- sum(w * ll_vec)

    return(-ll)  # Negative log-likelihood for minimization
  }


    ### Informed initialization:
    # Calculate initial values for mu based on class means
    class_means <- tapply(y, class_vec, mean, na.rm = TRUE)
    #class_stddevs <- tapply(y, class_vec, sd, na.rm = TRUE)

    # Initialize mu as the class means
    mu_init <- (1 / (1 + exp(class_means)))

    ## OR add small random perturbation to initial values
    #set.seed(123)  # For reproducibility
    #mu_init <- mu_init + rnorm(n_class, mean = 0, sd = 0.5)  # Adjust the sd as needed

    # Initialize sigma as the overall standard deviation of Alc12
    #sigma_init <- sd(y, na.rm = TRUE)

    # Log-transform the initial sigma (since the parameter sigma is log-transformed)
    #log_sigma_init <- log(sigma_init)

    # Combine into a single vector for initial parameters
    init <- c(mu_init)

    ### OR JUST USE-- Basic intialization, no information
    #init <- c(rep(mean(data_long$Alc12, na.rm = TRUE), n_class), log(1))

    # Maximize log-likelihood
    fit <- optim(init, loglik_distal, method = "BFGS", control = list(maxit = 1000), hessian=TRUE)

}


  # append information about outcome, classes etc
  fit$y <- y
  fit$class_names <- paste0("class", 1:n_class)
  fit$class_vec <- class_vec
  fit$weights <- w

  return(fit)
}
