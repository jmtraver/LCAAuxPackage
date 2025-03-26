
# ------------------------ ChatGPT conversion SAS to R -------------------------


# Reproducing Mplus BCH results manually, for modal assignment

library(MASS) # For matrix inversion

D <- matrix(c(0.923, 0.003, 0.074,
              0.064, 0.620, 0.316,
              0.051, 0.000, 0.949), nrow = 3, byrow = TRUE)

Dstar <- solve(D)
print(D)
print(Dstar)
print(t(Dstar)) # Transpose for ease of input

# Load dataset
modal <- read.table("/Users/hannaw/Documents/Arbeit Uni/USA Chapel Hill/Research Ideas/LCA Auxiliary Variable/NLSYclass.dat", 
                    header = FALSE, 
                    na.strings = ".")

colnames(modal) <- c("Bed", "TV", "Shy", "HrdClm", "MClose", "MLeave", "CID", "Male", 
                     "MEd", "Math98", "RComp98", "Alc12", "Cprob1", "Cprob2", "Cprob3", "Class")

modal$class1 <- ifelse(modal$Class == 1, 1, 0)
modal$class2 <- ifelse(modal$Class == 2, 1, 0)
modal$class3 <- ifelse(modal$Class == 3, 1, 0)

# --- Test ---
library(dplyr)
D_matrix_maybe <- modal %>% 
  group_by(Class) %>%
  summarise(mean_C1 = mean(Cprob1),
            mean_C2 = mean(Cprob2),
            mean_C3 = mean(Cprob3))
D_matrix_maybe
# This is something else!



# Compute D matrix
prior_probs <- c(0.46801, 0.19546, 0.33652)
# PriorProb1-PriorProb3 are the overall class proportions.
# Could also compute them with greater precision as the means of Cprob1-Cprob3.
# Cprob1-Cprob3 are posterior probabilities output by Mplus, Class indicates 
# modal class assignment (1, 2 or 3) which is used to make the indicator 
# variables class1-class3


# modal$Cprob1[2] * modal$class1[2] / prior_probs[1]
# modal$Cprob1[2] * modal$class2[2] / prior_probs[1]
# modal$Cprob2[1] * modal$class1[1] / prior_probs[1]
# modal$Cprob2[1] * modal$class2[1] / prior_probs[1]

computeD <- transform(modal,
                      P11 = Cprob1 * class1 / prior_probs[1],
                      # Probability of person i of belonging to class 1 times
                      # the probability of person i being sorted into class 1
                      # divided by the overall probability of belonging to 
                      # class 1.
                      P12 = Cprob1 * class2 / prior_probs[1],
                      P13 = Cprob1 * class3 / prior_probs[1],
                      P21 = Cprob2 * class1 / prior_probs[2],
                      P22 = Cprob2 * class2 / prior_probs[2],
                      P23 = Cprob2 * class3 / prior_probs[2],
                      P31 = Cprob3 * class1 / prior_probs[3],
                      P32 = Cprob3 * class2 / prior_probs[3],
                      P33 = Cprob3 * class3 / prior_probs[3]
)

# Format to D matrix
D_elements <- colMeans(computeD[, c("P11", "P12", "P13", "P21", "P22", "P23", "P31", "P32", "P33")])
D_new <- matrix(D_elements, nrow = 3, byrow = TRUE)
round(D_new, 3)
#       [,1]  [,2]  [,3]
# [1,] 0.923 0.003 0.074
# [2,] 0.064 0.620 0.316
# [3,] 0.051 0.000 0.949

# # Compute means
# summary(computeD[, c("P11", "P12", "P13", "P21", "P22", "P23", "P31", "P32", "P33")])
# summary(computeD[, c("Cprob1", "Cprob2", "Cprob3")])
# summary(modal[, c("Cprob1", "Cprob2", "Cprob3", "class1", "class2", "class3")])
# table(modal$Class)
# # table(modal$Class)/416
# head(modal, 20)


# --- Get D directly without building new columns ---

n_class <- 3
D_new <- matrix(NA, nrow = n_class, ncol = n_class)
for (t in 1:n_class) {         # iterate over X = t
  for (s in 1:n_class) {       # iterate over W = s
    
    #                           P(X = t | Y)        *         P(W = s | Y)        /    P(X = t)
    D_new[t, s] <- mean(modal[, paste0("Cprob", t)] * modal[, paste0("class", s)] / prior_probs[t])
  }
}
D_new
# round(D_new, 3)
#       [,1]  [,2]  [,3]
# [1,] 0.923 0.003 0.074
# [2,] 0.064 0.620 0.316
# [3,] 0.051 0.000 0.949

# P(X = t | Y): posterior probability class membership; each person has a 
#               probability (between 0 & 1, all of which sum up to 1) of
#               belonging to class t
# P(W = s | Y): class membership based on LCA results; e.g. with modal 
#               assignment each person has a probability of 1 of belonging to 
#               the class P(X = t | Y) showed the highest probability. All other 
#               classes will get assigned a probability of 0.
# P(X = t):     mixing probabilities
#               -> The relative frequency of each subgroup in the overall 
#               population
# Each element of the D matrix is the probability P(W = s | X = t), meaning
# the probability of being sorted into class s given the probability of being in 
# class t.
# For example: 
# D[1, 1]: is the probability of being sorted into class 1 given the probability
#          of being in class 1

# D[3, 1]: is the probability of being sorted into class 3 given the probability
#          of being in class 1
# OR?
# D[3, 1]: is the probability of being sorted into class 1 given the probability
#          of being in class 3

# The larger the probability of being sorted into a class that corresponds to 
# the class each person has the highest probability of belonging to, the clearer
# class separation. Equivalently, we would want values close to zero for the 
# probability of being assigned a class membership that does not correspond to
# the class the person has the highest probability of belonging to.

# Element D[2, 3] shows that the probability of being sorted into class 2 
# given the probability of belonging to class 3 is actually quite high (~32%).
# OR?
# Element D[2, 3] shows that the probability of being sorted into class 3 
# given the probability of belonging to class 2 is actually quite high (~32%).

# I believe, the latter would make more sense given the mixing probabilities
# and seeing how the element P23 is computed: 
# P23 = Cprob2 * class3 / prior_probs[2]
# This means we take the probability of person i of belonging to class 2 
# (Cprob2), multiply that by the probability of person i being sorted into 
# class 3 (class3; 1 versus 0) and dividing by the probability of any person in 
# the population belonging to class 2.

modal %>% 
  group_by(Class) %>%
  summarise(n()/416)
# Class  rel_freq
# 1      0.462
# 2      0.123
# 3      0.416
# There are more people sorted into class 3 than would be expected by the mixing
# probabilities (prior_probs = c(0.46801, 0.19546, 0.33652)).


# ---------------------- Expanding data for BCH weighting ---------------------- 

modal_long <- modal[rep(1:nrow(modal), each = 3), ]
modal_long$class_new <- rep(1:3, times = nrow(modal))

weights <- matrix(c(1.0883813, -0.082538, -0.05849,
                    -0.005266, 1.6133026, 0.000283,
                    -0.083115, -0.530765, 1.0582074), nrow = 3, byrow = TRUE)
# This is just the transpose of the inverse of the D matrix read in above:
# t(Dstar)

n_class <- 3
for (i in 1:n_class) {
  # For every row that belongs to class_new = i in the column wstar_i
  modal_long[modal_long$class_new == i, "wstar_it"] <- 
  # Take the sum of the probabilities in the columns class1, 2 and 3 times their
  # respective weights (= their values in the t(Dstar) matrix)
    rowSums(modal_long[modal_long$class_new == i, c("class1", "class2", "class3")] * weights[i, ])
}
# w*_it = sum(w_is * d*_st),
# where w_is = P(W = s | Y), and the d*_st is the corresponding weight in the 
# transposed inverse of the D matrix
# (P(W = s | Y) are variables class1, class2 and class3)



# --------------------------- Naive class assignment ---------------------------

lm1 <- lm(Alc12 ~ as.factor(Class), data = modal)
summary(lm1)

# N = 327

# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         5.2361     0.1917  27.309   <2e-16 ***
# as.factor(Class)2  -0.3361     0.4112  -0.817    0.414    
# as.factor(Class)3  -0.5298     0.2716  -1.950    0.052 .
# Multiple R-squared:  0.01171,	Adjusted R-squared:  0.005609
# F-statistic: 1.919 on 2 and 324 DF,  p-value: 0.1483

# ------ Mplus results ------
# EQUALITY TESTS OF MEANS ACROSS CLASSES USING THE BCH PROCEDURE
# WITH 2 DEGREE(S) OF FREEDOM FOR THE OVERALL TEST
# 
# ALC12
#                         Mean       S.E.
# Class 1                4.894      0.368
# Class 2                4.617      0.251
# Class 3                5.274      0.207
# 
#                    Chi-Square   P-Value
# Overall test           3.870      0.144
# Class 1 vs. 2          0.319      0.572
# Class 1 vs. 3          0.775      0.379
# Class 2 vs. 3          3.634      0.057

# --- In long format no correction for clustering, no weights ---
lm2 <- lm(Alc12 ~ as.factor(Class), data = modal_long)
summary(lm2)
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         5.2361     0.1104  47.446  < 2e-16 ***
# as.factor(Class)2  -0.3361     0.2367  -1.420  0.15592    
# as.factor(Class)3  -0.5298     0.1563  -3.389  0.00073 ***
# Multiple R-squared:  0.01171,	Adjusted R-squared:  0.009689
# F-statistic: 5.794 on 2 and 978 DF,  p-value: 0.003151

# --- In long format no correction for clustering, with weights ---
lm3 <- lm(Alc12 ~ as.factor(Class), data = modal_long, weights = wstar_it)
summary(lm3)
# Error in lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok,  : 
#    missing or negative weights not allowed

# By hand with weights
X <- model.matrix(lm2)
colnames(X) <- c("(Intercept)", "class2", "class3")

new_dat <- modal_long %>% 
  select(Alc12, Class, class2, class3, wstar_it) %>% 
  na.omit()
# W <- matrix(new_dat$wstar_it, ncol = 1)
W <- diag(new_dat$wstar_it)                                # is that right?
y <- matrix(new_dat$Alc12, ncol = 1)
# dim(t(X)); dim(W); dim(X)

# estimate beta_hat
XtWX <- t(X) %*% W %*% X
beta_hat <- solve(XtWX) %*% t(X) %*% W %*% y
beta_hat
#                         [,1]
# (Intercept)        5.2361112
# as.factor(Class)2 -0.3361112
# as.factor(Class)3 -0.5298177

coef(lm1)
#  (Intercept) as.factor(Class)2 as.factor(Class)3 
#    5.2361111        -0.3361111        -0.5298174
coef(lm2)
#  (Intercept) as.factor(Class)2 as.factor(Class)3 
#    5.2361111        -0.3361111        -0.5298174 



# ---------------------------- Fixed effects model -----------------------------
fm1 <- lm(Alc12 ~ as.factor(class_new) + as.factor(CID), data = modal_long)
summary(fm1)
fm1 <- lm(Alc12 ~ as.factor(Class) + as.factor(CID), data = modal_long)
summary(fm1)
# weird results

# ---------------------------- Mixed model analysis ---------------------------- 
library(lme4)

# # Linear mixed model without weighting
# modal_long$class_new <- as.factor(modal_long$class_new)
# lmer1 <- lmer(Alc12 ~ class_new + (1 | CID), data = modal_long)
# summary(lmer1)
# # convergence issues!!!


# Linear mixed model without weighting
lmer1 <- lmer(Alc12 ~ Class + (1 | CID), data = modal_long)
summary(lmer1)
# convergence issues!!!


# # Linear mixed model with weighting
# lmer2 <- lmer(Alc12 ~ as.factor(class_new) + (1 | CID), weights = wstar_it, data = modal_long)
# summary(lmer2)

# Linear mixed model with weighting
lmer2 <- lmer(Alc12 ~ as.factor(Class) + (1 | CID), weights = wstar_it, data = modal_long)
summary(lmer2)
# Error in mkRespMod(fr, REML = REMLpass) : all(weights >= 0) is not TRUE



# proc nlmixed data=modal_long empirical;
# parms d1=5, d2=5, d3=5, s2=5;
# mu = d1*class_new1 + d2*class_new2 + d3*class_new3 + u;
# ll = -.5*log(2*(22/7))-.5*log(s2)-((Alc12-mu)**2)/(2*s2);
# wll =  wstar_it*ll;
# model Alc12 ~ general(wll);
# random u ~ normal(0,0) subject=CID;
# contrast "overall test" .5*d2 + .5*d3 - d1, d2-d3;
# run;

# ---------------------------- Using BCH function ---------------------------- 

# Fit the model in tidySEM
dat_clean <- modal %>% 
  select(Bed, TV, Shy, HrdClm, MClose, MLeave)

# order data
dat_clean <- data.frame(lapply(dat_clean, ordered))


set.seed(1987) # setting seed 
new_mod <- mx_lca(data = dat_clean, classes = 3) 
# Error in update_thresholds(zscore) : 
# Could not complete thresholds; either specify all thresholds by hand, or remove constraints.

# Treat it as numeric
new_mod <- mx_profiles(data = dat_clean, classes = 3)  
summary(new_mod)

# table_prob(new_mod)
# plot_prob(new_mod)

res <- BCH(new_mod, "y ~ 1", data = data.frame(y = modal$Alc12))
summary(res)

# free parameters:
#            name   matrix row col Estimate Std.Error A
# 1 class1.S[1,1] class1.S   y   y 5.159031 0.5292959  
# 2 class1.M[1,1] class1.M   1   y 5.121053 0.1647788  
# 3 class2.S[1,1] class2.S   y   y 5.477949 1.0005275  
# 4 class2.M[1,1] class2.M   1   y 4.939550 0.3022894  
# 5 class3.S[1,1] class3.S   y   y 5.342011 0.8606089  
# 6 class3.M[1,1] class3.M   1   y 4.592799 0.2633020 


