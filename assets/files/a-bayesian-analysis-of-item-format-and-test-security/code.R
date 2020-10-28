# Setup ------------------------------------------------------------------------

library(BMA)    # bicreg
library(coda)   # HPDinterval, summary
library(rjags)  # coda.samples, jags.model, update

dat <- read.csv("dat.csv")

# DATA
# dat           47 x 6 data frame: rows are persons, columns are variables
# 
# VARIABLES
# com_score     0-25 points
# item_format   0 = MC, 1 = DOMC
# sec_score     0-25 points
# source_rt     response time in minutes
# major         0 = Human Development or Psychology, 1 = Other
# gender        0 = Male, 1 = Female

# Part 1: Multiple Linear Regression -------------------------------------------

out1 <- lm(com_score ~ ., dat)
summary(out1)

# Part 2: Bayesian Linear Regression -------------------------------------------

dat_list <- list(
  Y = dat$com_score,
  X1 = dat$item_format,
  X2 = dat$sec_score,
  X3 = dat$source_rt,
  X4 = dat$major,
  X5 = dat$gender,
  J = nrow(dat)
)
vars <- c("a", "b1", "b2", "b3", "b4", "b5", "tau")

mod <- "model {
  for (j in 1:J) {
    Y[j] ~ dnorm(mu[j], tau)
    mu[j] <- a + b1*X1[j] + b2*X2[j] + b3*X3[j] + b4*X4[j] + b5*X5[j]
  }
  
  # Priors
  tau ~ dgamma(0.01, 0.01)
  a ~ dnorm(5, 0.2)
  b1 ~ dnorm(-3, 0.2)
  b2 ~ dnorm(0.5, 1)
  b3 ~ dnorm(0.5, 1)
  b4 ~ dnorm(-3, 0.2)
  b5 ~ dnorm(0, 0.2)
}"

writeLines(mod, con = "mod")
out2 <- jags.model("mod", data = dat_list, n.chains = 2, n.adapt = 1000)
update(out2, n.iter = 5000)
coda <- coda.samples(out2, variable.names = vars, n.iter = 50000, thin = 10)

summary(coda)
HPDinterval(coda)

# Part 3: Bayesian Model Averaging ---------------------------------------------

out3 <- bicreg(
  x = dat[c("item_format", "sec_score", "source_rt", "major", "gender")],
  y = dat$com_score
)
summary(out3)