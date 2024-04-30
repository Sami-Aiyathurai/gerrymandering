## final modeling attempts

## creating a VPC

### rerun using egs_mod w/ OH


# egs_mod <- import("egs_mod_fin.csv")

egs_mod$gov_bin <- ifelse(egs_mod$gov_control == "D", 0, 1)

vpc1 <- stan_glmer(eg~1+(1|state), family=gaussian, data=egs_mod, seed=1546)
vpc1pct <- stan_glmer(eg_pct~1+(1|state), family=gaussian, data=egs_mod, seed=12345,
                      iter=2000)

mcmc_acf(vpc1pct)
mcmc_trace(vpc1pct)
mcmc_hist(vpc1pct)
mcmc_dens_overlay(vpc1pct)

print(vpc1, digits=2)
print(vpc1pct, digits=2)

summary(vpc1, digits=2)
summary(vpc1pct, digits=2)

mcmc_acf(vpc1pct)

vpc5 <- stan_glmer(formula=eg~1+(1|state), family=gaussian, data=egs_mod, seed=12345,
                   iter=20000, warmup=2500, thin=5, chains=4)
vpc5pct <- stan_glmer(formula=eg_pct~1+(1|state), family=gaussian, data=egs_mod, seed=12345,
                      iter=20000, warmup=2500, thin=5, chains=4)
vpc5pct <- stan_glmer(formula=eg_pct~1+(1|state), family=gaussian, data=egs_mod, seed=12345,
                      iter=30000, warmup=2500, thin=4, chains=4)

mcmc_acf(vpc5pct)
mcmc_hist(vpc5pct)
mcmc_trace(vpc5pct)
mcmc_dens_overlay(vpc5pct)

## making vpc plot to visualize

coef(vpc5pct)$state

ggplot(egs_mod, aes(x=year, y=eg_pct)) +
  geom_point(aes(color=state)) +
  geom_abline(intercept=4.462635) +
  geom_abline(intercept=)



# changing the settings so maybe it converges??
print(vpc5, digits=2)
print(vpc5pct, digits=2)

summary(vpc5, digits=2)
summary(vpc5pct, digits=2)

## RANDOM INTERCEPTS
# when I ran it with gov_bin it didn't work & family = gaussian
ri_gov <- stan_glmer(formula=eg_pct~gov_control+(1|state), family=gaussian, data=egs_mod,
                     seed=12345, iter=30000, warmup=2500, thin=4, chains=4)
print(ri_gov)
summary(ri_gov)

ri_gov2 <- stan_glmer(formula=eg_pct~gov_control+(1|state), family=gaussian, data=egs_mod,
                      seed=12345, iter=20000, warmup=2500, thin=5, chains=4)
ranef(ri_gov2)

model_parameters(ri_gov2, centrality="median", ci_method="hdi")
## this is a BINARY variable dumbass

model_parameters(ri_gov, ci=0.95, ci_method="hdi")

ri_leg <- stan_glmer(formula=eg_pct~leg_control+(1|state), family=gaussian, data=egs_mod,
                     seed=12345, iter=20000, warmup=2500, thin=5, chains=4)
model_parameters(ri_leg, ci=0.95, ci_method="hdi")
ranrileg <- ranef(ri_leg)$state
plot(ranef(ri_leg))

random_intercepts_df <- data.frame(Group = rownames(ranrileg), Intercept = ranrileg)

# Plot the random intercepts
ggplot(random_intercepts_df, aes(x = Group, y = X.Intercept.)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +  # Add a dashed line at y = 0 for reference
  xlab("Grouping Variable") +
  ylab("Random Intercept") +
  ggtitle("Random Intercept Plot")

## 5 divergences, but no divergences when thin=5

## seed is 12345

ri_tri1 <- stan_glmer(formula=eg_pct~trifecta+(1|state), family=gaussian, data=egs_mod,
                      seed=12345, iter=20000, warmup=2500, thin=5, chains=4)
ri_tri <- stan_glmer(formula=eg_pct~trifecta+(1|state), family=gaussian, data=egs_mod,
                     seed=12345, iter=30000, warmup=2500, thin=4, chains=4)

model_parameters(ri_tri1, ci=0.95, ci_method="hdi")

loo1 <- loo(vpc5pct, save_psis=TRUE)

yrep1 <- posterior_predict(ri_gov)
ppc_loo_pit_overlay(
  y=egs_mod$gov_control,
  yrep=yrep1,
  lw=weights(loo2$psis_object)
)

loo_vpc <- loo(vpc5pct, save_psis=TRUE)
loo_gov <- loo(ri_gov2, save_psis=TRUE)
loo_leg <- loo(ri_leg, save_psis=TRUE)
loo_tri <- loo(ri_tri1, save_psis=TRUE)

ri_inst <- stan_glmer(formula=eg_pct~institution+(1|state), family=gaussian, data=egs_mod,
                      seed=12345, iter=20000, warmup=2500, thin=5, chains=4) # no div
loo_inst <- loo(ri_inst, k_threshold = 0.7, save_psis=TRUE) # one pareto k > 0.7
## 1 divergence
loo_inst
model_parameters(ri_inst, ci=0.95, ci_method="hdi")
summary(ri_inst)

model_parameters(ri_simpinst, ci=0.95, ci_method="hdi")

ranefrileg <- ranef(ri_leg)

ri_simpinst <- stan_glmer(formula=eg_pct~simp_inst+(1|state), family=gaussian, data=egs_mod,
                          seed=12345, iter=20000, warmup=2500, thin=5, chains=4)
## converges!!

ri_sup <- stan_glmer(formula=eg_pct~state_supreme+(1|state), family=gaussian, data=egs_mod,
                     seed=12345, iter=20000, warmup=2500, thin=5, chains=4)
model_parameters(ri_sup, ci=0.95, ci_method="hdi")
model_parameters(ri_simpsup, ci=0.95, ci_method="hdi")

ri_simpsup <- stan_glmer(formula=eg_pct~state_supcat+(1|state), family=gaussian, data=egs_mod,
                         seed=12345, iter=20000, warmup=2500, thin=5, chains=4)

ri_pri <- stan_glmer(formula=eg_pct~primary+(1|state), family=gaussian, data=egs_mod,
                     seed=12345, iter=20000, warmup=2500, thin=5, chains=4)
## everything ran but state sup and inst normal
## do an RI with inst, leg
#
# ri_inst_leg <- stan_glmer(formula=eg_pct~institution*leg_control+(1|state), family=gaussian, data=egs_mod,
#                       seed=12345, iter=20000, warmup=2500, thin=5, chains=4)

loo(vpc5pct) #224
loo(ri_gov2) #219
loo(ri_leg) #212 !!
loo(ri_tri1) # 221
loo(ri_inst, k_threshold=0.7) # pareto k thing
loo(ri_simpinst) # 222
loo(ri_sup) #225
loo(ri_pri) #224
loo(ri3) # 213
loo(ri4) # 215

## Graph for GOV EFFECT




# ri <- c(-2.8, 7.40, -0.38, 1.25, -3.16, -5.05)
# rep <- c(-6.6, 3.6, -3.98, -2.55, -6.96, -8.85)
# slope <- -3.80

#
# df1 <- data.frame(group = group,
#                   random_intercept=ri,
#                   ends=rep,
#                   x=1)
#
# ggplot(df1, aes(x=rep, y=ri)) +
#   geom_point(aes(color=group))
#
# ggplot(df1, aes(x = group, y = ri)) +
#   geom_abline(intercept = 11.2, slope = slope, linetype = "dashed", color = "gray") +
#   geom_abline(intercept = 14, slope=slope, linetype="dashed", color="gray") +
#   geom_abline(intercept = 12.9, slope=slope, linetype="dashed", color="gray") +
#   geom_abline(intercept = 12.1, slope=slope, linetype="dashed", color="gray") +
#   geom_abline(intercept = 7.2, slope=slope, linetype="dashed", color="gray") +
#   geom_point(aes(color=group, size=3)) +
#   geom_text(aes(label = ri), vjust = -0.5, size = 3) +
#   labs(x = "Group", y = "Random Intercept", title = "Random Intercept Model Plot") +
#   theme_minimal()
#
# plot(ranef(ri))

loo(rs1) ## no additional explanatory power, 214.7 (7.4)

print(ri_leg)
summary(ri_leg)

summary(rs1, digits=2)
model_parameters(rs1, ci=0.95, ci_method="hdi", digits=2)
model_parameters(rsinst, ci=0.95, ci_method="hdi")
model_parameters(rs2, ci=0.95, ci_method="hdi")
summary(rsinst, digits=2)
ranef(rsinst)
ranef(rs2)

loo(rs1)
loo(rsinst)
loo(rs2, k_threshold=0.7)

rs1 <- stan_glmer(eg_pct~leg_control+(1+leg_control|state), family=gaussian,
                  data=egs_mod, seed=12345, iter=20000, warmup=2500, thin=5, chains=4)

rsinst <- stan_glmer(eg_pct~institution+(1+institution|state), family=gaussian,
                     data=egs_mod, seed=12345, iter=20000, warmup=2500, thin=5, chains=4)

rs2 <- stan_glmer(eg_pct~leg_control+institution+(1+leg_control+institution|state), family=gaussian,
                  data=egs_mod, seed=12345, iter=20000, warmup=2500, thin=5, chains=4)

rs3 <- stan_glmer(eg_pct~leg_control+gov_control+institution+(1+leg_control+institution|state),
                  data=egs_mod, seed=12345, iter=20000, warmup=2500, thin=5, chains=4)

##
## effect now that leg_control has on each state

ri3 <- stan_glmer(eg_pct~leg_control+simp_inst+(1|state), family=gaussian,
                  data=egs_mod, seed=12345, iter=20000, warmup=2500, thin=5, chains=4)
ri4 <- stan_glmer(eg_pct~gov_control+leg_control+simp_inst+(1|state), family=gaussian,
                  data=egs_mod, seed=12345, iter=20000, warmup=2500, thin=5, chains=4)

coef(ri_sup)

plot(coef(ri_sup)$state)

summary(ri_gov2, digits=2)

# testing the interaction between the two
loo(ri_inst_leg)

model_parameters(ri_simpinst, ci=0.95, ci_method="hdi")
model_parameters(ri_inst, ci=0.95, ci_method="hdi")
model_parameters(ri_pri, ci=0.95, ci_method="hdi")
model_parameters(ri_sup, ci=0.95, ci_method="hdi")

model_parameters(ri_inst_leg, ci=0.95, ci_method="hdi")
# ri_tri1 has seed of 12345
#ri_tri has seed of 123456
# converges for 12345 does not converge for 123456

states <- c("CO", "MI", "OH", "PA", "WI")

X1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

## LEG

leg1 <- 0.12
leg2 <- -7.05
leg3 <- -0.70

X1 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)

Z2 <- (4.63, -0.24, 1.19, -1.93, -3.56, 4.63, -0.24, 1.19, -1.93, -3.56,
       4.63-sqrt(48.7025), -0.24-sqrt(48.7025), 1.19-sqrt(48.7025), -1.93-sqrt(48.7025), -3.56-sqrt(48.7025),
       4.63-sqrt(48.7025), -0.24, 1.19, -1.93, -3.56,
)


govs1 <- -2.82
govs2 <- -3.8

Z1 <- c(7.40, -0.38, 1.25, -3.16, -5.05, 7.40, -0.38, 1.25, -3.16, -5.05,
        (7.4-sqrt(13.44)-0.1), (-0.38-sqrt(13.44)-0.1),
        (1.25-sqrt(13.44)-0.1), (-3.16-sqrt(13.44)-0.1), (-5.05-sqrt(13.44)-0.1),
        (7.4-sqrt(6.9524)-0.1), (-0.38-sqrt(6.9524)-0.1),
        (1.25-sqrt(6.9524)-0.1), (-3.16-sqrt(6.9524)-0.1), (-5.05-sqrt(6.9524)-0.1))

## MAKING GRAPH FOR BOTH SETS OF VALUES (GOV) ## do not use ##

govdf <- data.frame(x=X1, y=Z1, group=states)

ggplot(govdf, aes(x=X1, y=Z1)) +
  geom_abline(intercept=7.40, slope=govs2, linetype="solid", color="red") +
  geom_abline(intercept=-0.38, slope=govs2, linetype="solid", color="red") +
  geom_abline(intercept=1.25, slope=govs2, linetype="solid", color="red") +
  geom_abline(intercept=-3.16, slope=govs2, linetype="solid", color="red") +
  geom_abline(intercept=-5.05, slope=govs2, linetype="solid", color="red") +
  geom_abline(intercept=7.40, slope=govs1, linetype="dashed", color="blue") +
  geom_abline(intercept=-0.38, slope=govs1, linetype="dashed", color="blue") +
  geom_abline(intercept=1.25, slope=govs1, linetype="dashed", color="blue") +
  geom_abline(intercept=-3.16, slope=govs1, linetype="dashed", color="blue") +
  geom_abline(intercept=-5.05, slope=govs1, linetype="dashed", color="blue") +
  geom_text(aes(x=0, y=7, label="CO")) +
  geom_text(aes(x=0, y=1.7, label="OH")) +
  geom_text(aes(x=0, y=0.1, label="MI")) +
  geom_text(aes(x=0, y=-2.7, label="PA")) +
  geom_text(aes(x=0, y=-4.6, label="WI")) +
  #geom_text(aes(x=0.5, y=-4, label="Effect of Republican control = -3.6"), size=3, color="black", angle=-18) +
  xlab("Governor party") +
  ylab("Efficiency gap (%)") +
  geom_point() +
  scale_y_continuous(breaks=seq(-8, 8, 2),
                     labels=c("-8", "-6", "-4", "-2", "0", "2", "4", "6", "8")) +
  scale_x_continuous(breaks=seq(0, 1, 1),
                     labels=c("Democrat", "Republican")) +
  labs(col="State") +
  theme_minimal()

## graphing!!

## summary stats

ggplot(egs_mod, aes(x=year, y=eg_pct)) +
  #facet_wrap(~state) +
  geom_point(aes(color=state)) +
  geom_smooth() +
  geom_hline(yintercept=8, linetype="dashed") +
  geom_hline(yintercept=-8, linetype="dashed") +
  xlab("Year") +
  ylab("Efficiency gap") +
  scale_color_discrete("State") +
  scale_y_continuous(breaks=seq(-20, 16, 4),
                     labels=c("-20", "-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")) +
  theme_bw()

ggplot(egs_mod, aes(x=state, y=eg_pct)) +
  geom_boxplot() +
  geom_hline(yintercept=8, linetype="dashed") +
  geom_hline(yintercept=-8, linetype="dashed") +
  scale_y_continuous(breaks=seq(-16, 12, 4),
                     labels=c("-16", "-12", "-8", "-4", "0",
                              "4", "8", "12")) +
  xlab("State") +
  ylab("Efficiency gap (%)") +
  theme_bw()


ggplot(egs_mod, aes(x=leg_control, y=eg_pct)) +
  #facet_wrap(~state) +
  geom_point(aes(color=state)) +
  geom_smooth() +
  xlab("Year") +
  ylab("Efficiency gap") +
  scale_color_discrete("State") +
  scale_y_continuous(breaks=seq(-20, 16, 4),
                     labels=c("-20", "-16", "-12", "-8", "-4", "0", "4", "8", "12", "16")) +
  theme_bw()
## replicate this for each explanatory

ggplot(egs_mod, aes(x=gov_control)) +
  geom_bar() +
  facet_wrap(~state) +
  xlab("Party controlling governorship") +
  ylab("Count") +
  theme_bw()

ggplot(egs_mod, aes(x=leg_control)) +
  geom_bar() +
  facet_wrap(~state) +
  xlab("Party controlling both legislative chambers") +
  ylab("Count") +
  theme_light()

ggplot(egs_mod, aes(x=trifecta)) +
  geom_bar() +
  facet_wrap(~state) +
  xlab("Trifecta status") +
  ylab("Count") +
  theme_bw()

## need to remove the labs

lab1 <- c("Independent \ncommission", "Legislature",
          "Politician \ncommission")
ggplot(egs_mod, aes(x=institution)) +
  geom_bar() +
  facet_wrap(~state) +
  xlab("Institution controlling redistricting") +
  ylab("Count") +
  scale_x_discrete(labels=lab1) +
  theme_bw()

lab2 <- c("Governor \nappointment", "Nonpartisan \nelection", "Partisan \nelection")
ggplot(egs_mod, aes(x=state_supreme)) +
  geom_bar() +
  facet_wrap(~state) +
  xlab("Appointment method for state supreme court judges") +
  ylab("Count") +
  scale_x_discrete(labels=lab2) +
  theme_bw()

lab3 <- c( "Closed", "Open","Semi")
ggplot(egs_mod, aes(x=primary)) +
  geom_bar() +
  facet_wrap(~state) +
  xlab("Primary system") +
  ylab("Count") +
  scale_x_discrete(labels=lab3) +
  theme_bw()

## EFFECT OF GOV PARTY

group <- c("CO", "MI", "OH", "PA", "WI")

X1 <- c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
Y1 <- c(7.40, -0.38, 1.25, -3.16, -5.05,
        (7.4-sqrt(13.44)-0.1), (-0.38-sqrt(13.44)-0.1),
        (1.25-sqrt(13.44)-0.1), (-3.16-sqrt(13.44)-0.1), (-5.05-sqrt(13.44)-0.1)) ## where Y2 is result of Y1 fudged a little
Y2 <- c( 7.40, -0.38, 1.25, -3.16, -5.05,
         (-2.8-3.46-0.4), (7.4-3.46-0.4), (-0.38-3.46-0.4),
         (1.25-3.46-0.4), (-3.16-3.46-0.4), (-5.05-3.46-0.4))


df2 <- data.frame(group=group,
                  x=X1, y=Y1)

ggplot(df2, aes(x=X1, y=Y1)) +
  geom_abline(intercept=7.40, slope=slope, linetype="solid", color="gray") +
  #geom_abline(intercept=-2.8, slope=slope, linetype="solid", color="gray") +
  geom_abline(intercept=-0.38, slope=slope, linetype="dashed", color="gray") +
  geom_abline(intercept=1.25, slope=slope, linetype="dashed", color="gray") +
  geom_abline(intercept=-3.16, slope=slope, linetype="dashed", color="gray") +
  geom_abline(intercept=-5.05, slope=slope, linetype="solid", color="gray") +
  #geom_text(aes(x=0.5, y=-4, label="Effect of Republican control = -3.6"), size=3, color="black", angle=-18) +
  xlab("Governor party") +
  ylab("Efficiency gap (%)") +
  geom_point(aes(color=group)) +
  scale_y_continuous(breaks=seq(-8, 8, 2),
                     labels=c("-8", "-6", "-4", "-2", "0", "2", "4", "6", "8")) +
  scale_x_continuous(breaks=seq(0, 1, 1),
                     labels=c("Democrat", "Republican")) +
  labs(col="State") +
  theme_minimal()

## version without MEDIAN


ggplot(df2, aes(x=X1, y=Y1)) +
  geom_abline(intercept=7.40, slope=slope, linetype="solid", color="gray") +
  geom_abline(intercept=-2.8, slope=slope, linetype="solid", color="gray") +
  geom_abline(intercept=-0.38, slope=slope, linetype="dashed", color="gray") +
  geom_abline(intercept=1.25, slope=slope, linetype="dashed", color="gray") +
  geom_abline(intercept=-3.16, slope=slope, linetype="dashed", color="gray") +
  geom_abline(intercept=-5.05, slope=slope, linetype="solid", color="gray") +
  #geom_text(aes(x=0.5, y=-4, label="Effect of Republican control = -3.6"), size=3, color="black", angle=-18) +
  xlab("Governor party") +
  ylab("Efficiency gap (%)") +
  geom_point(aes(color=group)) +
  scale_y_continuous(breaks=seq(-8, 8, 2),
                     labels=c("-8", "-6", "-4", "-2", "0", "2", "4", "6", "8")) +
  scale_x_continuous(breaks=seq(0, 1, 1),
                     labels=c("Democrat", "Republican")) +
  labs(col="State") +
  theme_minimal()


## graph for LEG EFFECT
slope3 <- -7.05
X3 <- c(0, 0, 0, 0, 0, 0,
        1, 1, 1, 1, 1, 1)
Y3 <- c(0.12, 4.63, -0.24, 1.19, -1.93, -3.56,
        0.12-sqrt(48.7025)-0.1, 4.63-sqrt(48.7025)-0.1, -0.24-sqrt(48.7025)-0.1,
        1.19-sqrt(48.7025)-0.1, -1.93-sqrt(48.7025)-0.1, -3.56-sqrt(48.7025)-0.1)
df3 <- data.frame(group=group,
                  x=X3, y=Y3)
ggplot(df3, aes(x=X3, y=Y3)) +
  geom_abline(intercept=4.63, slope=slope3, linetype="solid", color="gray") +
  geom_abline(intercept=0.12, slope=slope3, linetype="solid", color="gray") +
  geom_abline(intercept=-0.24, slope=slope3, linetype="dashed", color="gray") +
  geom_abline(intercept=1.19, slope=slope3, linetype="dashed", color="gray") +
  geom_abline(intercept=-1.93, slope=slope3, linetype="dashed", color="gray") +
  geom_abline(intercept=-3.56, slope=slope3, linetype="solid", color="gray") +
  #geom_text(aes(x=0.5, y=-4, label="Effect of Republican control = -3.6"), size=3, color="black", angle=-18) +
  xlab("Legislature party") +
  ylab("Efficiency gap (%)") +
  geom_point(aes(color=group)) +
  scale_y_continuous(breaks=seq(-12, 6, 2),
                     labels=c("-12", "-10", "-8", "-6", "-4", "-2", "0", "2", "4", "6")) +
  scale_x_continuous(breaks=seq(0, 1, 1),
                     labels=c("Democrat", "Republican")) +
  labs(col="State") +
  theme_minimal()

### EFFECT OF REP TRIFECTA

slope4 <- -5.82
slope5 <- -3.24

X4 <- c(0, 0, 0, 0, 0, 0,
        1, 1, 1, 1, 1, 1)
Y4 <- c(-0.54, 6.70, -0.11, 1.17, -2.88, -4.97,
        -0.54-sqrt(32.8724)-0.1, 6.70-sqrt(32.8724)-0.1, -0.11-sqrt(32.8724)-0.1,
        1.17-sqrt(32.8724)-0.1, -2.88-sqrt(32.8724)-0.1, -4.97-sqrt(32.8724)-0.1)

df4 <- data.frame(group=group,
                  x=X4, y=Y4)

ggplot(df4, aes(x=X4, y=Y4)) +
  geom_abline(intercept=6.70, slope=slope4, linetype="solid", color="gray") +
  geom_abline(intercept=-0.54, slope=slope4, linetype="solid", color="gray") +
  geom_abline(intercept=-0.11, slope=slope4, linetype="dashed", color="gray") +
  geom_abline(intercept=1.17, slope=slope4, linetype="dashed", color="gray") +
  geom_abline(intercept=-2.88, slope=slope4, linetype="dashed", color="gray") +
  geom_abline(intercept=-4.97, slope=slope4, linetype="solid", color="gray") +
  #geom_text(aes(x=0.5, y=-4, label="Effect of Republican control = -3.6"), size=3, color="black", angle=-18) +
  xlab("Trifecta") +
  ylab("Efficiency gap (%)") +
  geom_point(aes(color=group)) +
  scale_y_continuous(breaks=seq(-12, 6, 2),
                     labels=c("-12", "-10", "-8", "-6", "-4", "-2", "0", "2", "4", "6")) +
  scale_x_continuous(breaks=seq(0, 1, 1),
                     labels=c("Democrat", "Republican")) +
  labs(col="State") +
  theme_minimal()
