# Testing the functions

#----------------------------------------------------#
#### Continuous outcomes with two classes dataset (N = 100) ####
#----------------------------------------------------#
# simulate example data
set.seed(5)

var1 <- rnorm(100, mean = 0, sd = 1)
var2 <- rnorm(100, mean = 5, sd = 2)

# Simulate class probabilities
class1_prob <- runif(100, min = 0, max = 1)
class2_prob <- 1 - class1_prob

# generate data frame
df <- data.frame(
  var1 = var1,
  var2 = var2,
  class1_prob = class1_prob,
  class2_prob = class2_prob
)

# estimate lca model
mod1_1 <- mx_BCH(var1 ~ latent_class,
               data = df,
               post.prob = c("class1_prob", "class2_prob"))

# output
summary(mod1_1)
summary(mod1_1, do.boot = TRUE, show.ci = TRUE, check.time = TRUE, mean.diff = TRUE)

# estimate lca model
mod1_2 <- mx_BCH(var2 ~ latent_class,
               data = df,
               post.prob = c("class1_prob", "class2_prob"))

# output
summary(mod1_2)
summary(mod1_2, do.boot = TRUE, show.ci = TRUE, check.time = TRUE, mean.diff = TRUE)

#----------------------------------------------------#
#### Continuous outcomes with three classes dataset (N=100) ####
#----------------------------------------------------#
# simulate example data
set.seed(5)

var1 <- rnorm(100, mean = 0, sd = 2.5)
var2 <- rnorm(100, mean = 5, sd = 3)

# Generate class probabilities from a Dirichlet distribution
library(MCMCpack)
class_probs <- rdirichlet(100, alpha = c(1, 1, 1))

# generate data frame
df2 <- data.frame(
  var1 = var1,
  var2 = var2,
  class1_prob = class_probs[,1],
  class2_prob = class_probs[,2],
  class3_prob = class_probs[,3]
)

# estimate lca model
mod2_1 <- mx_BCH(var1 ~ latent_class,
               data = df2,
               post.prob = c("class1_prob", "class2_prob", "class3_prob"))

# output
summary(mod2_1)
summary(mod2_1, do.boot = TRUE, show.ci = TRUE, check.time = TRUE, mean.diff = TRUE)

# estimate lca model
mod2_2 <- mx_BCH(var2 ~ latent_class,
               data = df2,
               post.prob = c("class1_prob", "class2_prob", "class3_prob"))

# output
summary(mod2_2)
summary(mod2_2, do.boot = TRUE, show.ci = TRUE, seed = 5, check.time = TRUE, mean.diff = TRUE)

# estimate lca model (change the order of class prob & set seed )
mod2_3 <- mx_BCH(var2 ~ latent_class,
                 data = df2,
                 post.prob = c("class2_prob", "class3_prob", "class1_prob"))

# output
summary(mod2_3)
summary(mod2_3, do.boot = TRUE, show.ci = TRUE, seed = 5,  check.time = TRUE, mean.diff = TRUE)

#----------------------------------------------------#
#### Continuous outcomes with three classes dataset (N=300) ####
#----------------------------------------------------#
# simulate example data
set.seed(5)

var1 <- rnorm(300, mean = 0, sd = 2.5)
var2 <- rnorm(300, mean = 5, sd = 3)

# Generate class probabilities from a Dirichlet distribution
class_probs <- rdirichlet(100, alpha = c(1, 1, 1))

# generate data frame
df3 <- data.frame(
  var1 = var1,
  var2 = var2,
  class1_prob = class_probs[,1],
  class2_prob = class_probs[,2],
  class3_prob = class_probs[,3]
)

# estimate lca model
mod3_1 <- mx_BCH(var1 ~ latent_class,
                 data = df3,
                 post.prob = c("class1_prob", "class2_prob", "class3_prob"))

# output
summary(mod3_1)
summary(mod3_1, do.boot = TRUE, show.ci = TRUE, seed = 123, check.time = TRUE, mean.diff = TRUE)

# estimate lca model (change the order of class prob & change the number of boots)
mod3_2 <- mx_BCH(var2 ~ latent_class,
                 data = df3,
                 post.prob = c("class1_prob", "class2_prob", "class3_prob"))

# output
summary(mod3_2)
summary(mod3_2, do.boot = TRUE, show.ci = TRUE, seed = 123, B = 1000, check.time = TRUE, mean.diff = TRUE)

#----------------------------------------------------#
#### Continuous outcomes with four classes dataset (N=300) ####
#----------------------------------------------------#
# simulate example data
set.seed(5)

var1 <- rnorm(300, mean = 0, sd = 2.5)
var2 <- rnorm(300, mean = 5, sd = 3)

# Generate class probabilities from a Dirichlet distribution
class_probs <- rdirichlet(100, alpha = c(1, 1, 1, 1))

# generate data frame
df4 <- data.frame(
  var1 = var1,
  var2 = var2,
  class1_prob = class_probs[,1],
  class2_prob = class_probs[,2],
  class3_prob = class_probs[,3],
  class4_prob = class_probs[,4]
)

# estimate lca model
mod4_1 <- mx_BCH(var1 ~ latent_class,
                 data = df4,
                 post.prob = c("class1_prob", "class2_prob", "class3_prob", "class4_prob"))

# output
summary(mod4_1)
summary(mod4_1, do.boot = TRUE, show.ci = TRUE, seed = 123, check.time = TRUE, mean.diff = TRUE)
