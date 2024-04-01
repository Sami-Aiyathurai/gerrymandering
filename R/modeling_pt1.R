## Summary statistics
library(rio)
library(foreign)
library(ggplot2)
library(tidyverse)
library(broom)
library(bayesplot)
library(rstanarm)
library(MCMCpack)
library(dplyr)
library(parameters)
library(bayestestR)
library(loo)
library(coda)
library(sjPlot)
library(augment)
library(sjmisc)
library(sjlabelled)
library(tidyverse)

egs_mod <- import("egs_mod2.csv")

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

plot(ranef(vpc5))


model_parameters(vpc5, centrality="median", ci=0.95, ci_method="hdi", digits=3)
model_parameters(ri3, centrality="median", ci=0.95, ci_method="hdi", digits=3)
model_parameters(ri1, centrality="median", ci=0.95, ci_method="hdi", digits=3)

## don't forget waic(vpc5) and how to interpret it

## Interpreting the VPC
print(vpc5, digits=3)
summary(vpc5, digits=3)
dotplot(ranef(vpc5))

## 7.164% of the variance is explained by state level differences, 4.207% by individuals
## VPC = 7.164/(7.164+4.207) = 63.00%
# this means that 63% of the variance appears to be associated with differences between states

# - print(vpc7)
# - country std dev 1.765
# - 1.765% of the variance is explained by country level differences, 2.606% is explained by individual level differences.
# - residual: 2.606
# - VPC = 1.765/(1.765+2.606) = 0.4038
# - 40.38% of variance appears to be associated with differences between countries.
# - amount of variation in the outcome that exists between individuals
# - country intercept as proportion of country + residual (STD add)

## Interpreting RI
print(ri1, digits=3)

summary(ri3, digits=3)

ranefri1 <- as.data.frame(ranef(ri1))

ggplot(ranefri1, aes(x=condval, y=grp)) +
  geom_point()


## 6.435/(4.305+6.435) = 59.9162% of the variation is associated with differences between countries

# - remember: random intercept implies that each country has the same relationship between the two variables, they just have different starting points
# - VPC 1.733 / (1.733+2.603) = 39.97% variation is associated with differences between countries
# - 1.733 is the variation explained by country level
# - 2.603 is the variation explained by individual level
# rn.b.ri2 <- ranef(b.ri2)
# df.rn.b.ri2 <- as.data.frame(rn.b.ri2)
#
# ggplot(euro.short, aes(x=fairelcc, y=accalaw)) +
#   geom_jitter() +
#   geom_path(x=0, y=-1.656, color="red")
#
# ggplot(df.rn.b.ri2, aes(y=condval, color=grp)) +
#   geom_line()

