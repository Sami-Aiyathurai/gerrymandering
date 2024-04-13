## egs pos & modeling

library(stargazer)
library(knitr)


yb2014 <- year_baseline_data(2014, wi_data)
yb2016 <- year_baseline_data(2016, wi_data)

## scott wanted to see contested vs uncontested EGs
## maybe even contested vs uncontested estimations vs 75/25 split
## one guy said something about showing competition

kable(yb2014)

## MPSA SUMMARY STATS AND STUFF

## eg_pos

summary(egs$EG_pos, digits=2)

egs$EG_P_shift <- (egs$EG_pos - min(egs$EG_pos))

egs$EG_P_shift <- sort(egs$EG_P_shift)
egs$EG_cat[egs$EG_P_shift < 0.08] <- "Acceptable"
egs$EG_cat[egs$EG_P_shift >= 0.08] <- "Gerrymander"

egs$EG_cat <- factor(egs$EG_cat, levels=c("Acceptable", "Gerrymander")) # EG_cat is for positive EGs only

ggplot(egs, aes(x=EG_cat)) +
  geom_bar() +
  ylab("Count") +
  xlab("Centered absolute value efficiency gaps") +
  ggtitle("Distribution of acceptable and gerrymandered plans across all states and years")


egs$EG_cat1[egs$EG <= -0.08] <- "Republican gerrymander"
egs$EG_cat1[egs$EG > -0.08 & egs$EG < 0.08] <- "Acceptable"
egs$EG_cat1[egs$EG >= 0.08] <- "Democratic gerrymander"

egs$EG_cat1 <- factor(egs$EG_cat1, levels=c("Republican gerrymander", "Acceptable", "Democratic gerrymander"))

ggplot(egs, aes(x=EG_cat1)) +
  geom_bar() +
  xlab("Efficiency gap categories") +
  ylab("Count") +
  ggtitle("Distribution of efficiency gaps")

ggplot(egs, aes(x=State, fill=EG_cat1)) +
  geom_bar(position="dodge") +
  xlab("Efficiency gap categories") +
  ylab("Count") +
  ggtitle("Distribution of efficiency gaps")

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=State)) +
  facet_wrap(~EG_cat1) +
  scale_y_continuous(breaks=seq(-0.20, 0.12, 0.04),
                     label=c("-0.20", "-0.16",
                             "-0.12", "-0.08", "-0.04",
                             "0", "0.04", "0.08", "0.12")) +
  theme_light()
#  scale_y_continuous(breaks=seq(-0.24, 0.12, 0.4))

# should shift this so 0 is the minimum

egs$State <- as.factor(egs$State)

ggplot(egs, aes(x=State, y=EG)) +
  geom_boxplot() +
  scale_y_continuous(breaks=seq(-0.24, 0.12, 0.04)) +
  ggtitle("Boxplots of efficiency gaps by state for 2008-2022") +
  ylab("Efficiency gap") +
  geom_hline(yintercept=0.08, linetype="dashed") +
  geom_hline(yintercept=-0.08, linetype="dashed")

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=State)) +
  scale_y_continuous(breaks=seq(-0.24, 0.12, 0.04)) +
  ylab("Efficiency gap") +
  geom_hline(yintercept=0.08, linetype="dashed") +
  geom_hline(yintercept=-0.08, linetype="dashed") +
  ggtitle("State house efficiency gaps from 2008-2022") +
  scale_x_continuous(breaks=seq(2008, 2022, 2)) +
  theme_light()


## showing contested and uncontested districts per state


summary(wi_egs)

egs_c <- egs %>%
  filter(State == "CO")

egs_p <- egs %>%
  filter(State == "PA")

egs_w <- egs %>%
  filter(State == "WI")

egs_m <- egs %>%
  filter(State == "MI")
summary(egs_w$EG)
summary(egs_m$EG)
summary(egs_c$EG)
summary(egs_p$EG)

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=Institution)) +
  facet_wrap(~State)

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=State_Supreme_method)) +
  facet_wrap(~State)

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=Legislature_Control)) +
  facet_wrap(~State)

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=Governor)) +
  facet_wrap(~State)

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=Trifecta)) +
  facet_wrap(~State)

ggplot(egs, aes(x=Year, y=EG)) +
  geom_point(aes(color=primary)) +
  facet_wrap(~State)



ggplot(egs, aes(x=primary)) +
  geom_histogram(stat="count")

ggplot(egs, aes(x=State_Supreme_method)) +
  geom_bar()

ggplot(egs, aes(x=Year, y=EG_P_shift)) +
  geom_point(aes(color=)) +
  facet_wrap(~State)


## observing the massive model to prove if it's MLM or not (this is with EG_pos)
m1 <- stan_glm(EG_pos~Institution+State_Supreme_method+State+Governor+primary+Legislature_Control+Trifecta,
               family=gaussian, data=egs, seed=349)
print(m1, digits=3)
summary(m1, digits=3)
mcmc_trace(m1)
waic(m1)

vp1 <- stan_glmer(formula=EG_pos~1+(1|State), family=gaussian, data=egs, seed=349, iter=5000,
                  adapt_delta=)

print(vp1, digits=3) # has one divergence
print(vpc1, digits=3)


