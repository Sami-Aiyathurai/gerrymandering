## Summary statistics

library(rio)
library(foreign)
library(tidyverse)
library(bayesplot)
library(rstanarm)
library(MCMCpack)
library(parameters)
library(loo)
library(bayestestR)
library(coda)

egs_mod <- import("egs_mod.csv")

summary(egs_mod)

egs_mod$State <- as.factor(egs_mod$State)
egs_mod$Trifecta <- as.factor(egs_mod$Trifecta)
egs_mod$Institution <- as.factor(egs_mod$Institution)
egs_mod$State_Supreme_method <- as.factor(egs_mod$State_Supreme_method)


ggplot(egs_mod, aes(x=Efficiency_gap)) +
  geom_histogram()
ggplot(egs_mod, aes(x=Institution)) +
  geom_bar()
ggplot(egs_mod, aes(x=State_Supreme_method)) +
  geom_bar()

## Need to prove that MLM is needed

m1 <- stan_glm(Efficiency_gap~Institution+State_Supreme_method+State+Year+Governor+Legislature_Control+Trifecta,
                 family=gaussian, data=egs_mod, seed=349)
summary(m1, digits=3)
print(m1, digits=3)

## Building Bayesian Multilevel Models

## Building the VPC

# - no independent variables, estimate the extnt to which clustering occurs within a group
# - estimate the intercept value, interclass correlation (the proportion of the DV attributed to clustering)

vpc1 <- stan_glmer(formula=Efficiency_gap~1+(1|State), family=gaussian, data=egs_mod, seed=349)

print(vpc1, digits=3)
summary(vpc1, digits=3)
mcmc_acf(vpc1, pars=c("sigma", "(Intercept)"))
mcmc_hist(vpc1)
mcmc_trace(vpc1)
mcmc_dens_overlay(vpc1)

vpc2 <- stan_glmer(formula=Efficiency_gap~1+(1|State), family=gaussian, data=egs_mod, seed=349, iter=4000,
                   warmup=2000, thin=5)
print(vpc2, digits=3)
summary(vpc2, digits=3)
mcmc_acf(vpc2, pars=c("sigma", "(Intercept)"))
mcmc_hist(vpc2)
mcmc_trace(vpc2)
mcmc_dens_overlay(vpc2)

# summary gives intercept for each country, print gives the relationships

vpc3 <- stan_glmer(formula=Efficiency_gap~1+(1|State), family=gaussian, data=egs_mod, seed=349,
                   iter=10000, warmup=2500, thin=5)
print(vpc3, digits=3)
summary(vpc3,digits=3)
mcmc_acf(vpc3, pars=c("sigma", "(Intercept)"))
mcmc_hist(vpc3)
mcmc_trace(vpc3)
mcmc_dens_overlay(vpc3)

vpc4 <- stan_glmer(formula=Efficiency_gap~1+(1|State), family=gaussian, data=egs_mod, seed=349,
                   iter=20000, warmup=5000, thin=5, chains=5)
print(vpc4, digits=3)
summary(vpc4,digits=3)
mcmc_acf(vpc4, pars=c("sigma", "(Intercept)"))
mcmc_hist(vpc4)
mcmc_trace(vpc4)
mcmc_dens_overlay(vpc4) # fits a lot better

vpc5 <- stan_glmer(formula=Efficiency_gap~1+(1|State), family=gaussian, data=egs_mod, seed=349,
                   iter=25000, warmup=5000, thin=5, chains=5)
print(vpc5, digits=3)
summary(vpc5,digits=3)
mcmc_acf(vpc5, pars=c("sigma", "(Intercept)"))
mcmc_hist(vpc5)
mcmc_trace(vpc5)
mcmc_dens_overlay(vpc5) # these are all pretty good

ri1 <- stan_glmer(formula=Efficiency_gap~Trifecta+(1|State), family=gaussian, data=egs_mod,
                  seed=349, iter=25000, warmup=5000, thin=5, chains=5)
# 2 divergent transitions after warmup
print(ri1, digits=3)
summary(ri1,digits=3)
mcmc_acf(ri1, pars=c("sigma", "(Intercept)"))
mcmc_hist(ri1)
mcmc_trace(ri1)
mcmc_dens_overlay(ri1)
plot(ranef(ri1))

ri2 <- stan_glmer(formula=Efficiency_gap~Institution+(1|State), family=gaussian, data=egs_mod,
                  seed=349, iter=25000, warmup=5000, thin=5, chains=5)
print(ri2, digits=3)
summary(ri2,digits=3)
mcmc_acf(ri2, pars=c("sigma", "(Intercept)"))
mcmc_hist(ri2) # these don't look great
mcmc_trace(ri2)
mcmc_dens_overlay(ri2) # these look good


ri3 <- stan_glmer(formula=Efficiency_gap~State_Supreme_method+(1|State), family=gaussian, data=egs_mod,
                  seed=349, iter=30000, warmup=5000, thin=5, chains=5)
## 36 divergent transitions
print(ri3, digits=3)
summary(ri3,digits=3)
mcmc_acf(ri3, pars=c("sigma", "(Intercept)"))
mcmc_hist(ri3) # these don't look great
mcmc_trace(ri3)
mcmc_dens_overlay(ri3)
plot(ranef(ri3))
