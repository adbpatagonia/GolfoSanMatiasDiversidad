# Code to carry out GAMM modelling presented in Cuesta-Nunez et al (2025)
# "Disentangling the Effects of Haul Duration on Species Richness in Trawl Surveys at Local and Regional Scales"

# Section 1 fits the models presented in the ms, and performs model checking
# Section 2 performs model selection
# Section 3 calculates deviance partition for the most parsimonious model

# libraries -----
library(mgcv)
library(gratia)
library(gam.hp)
library(tidyverse)
library(data.table)

# GAM models ----
## full model -----
# include Year, depth, towing time, and longitude (distance from the mouth of the Gulf)
# HaulID as a random effect
m.1 <- gam(
  richness ~
    s(Depth, bs = "tp") +
    s(longitude, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    s(towing.time, bs = "tp") +
    s(HaulID, k = length(levels(mod.dat$HaulID)), bs = "re") ,
  method = "REML",
  data = mod.dat
)

### model check -----
mod <- m.1
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.1")

summary(mod)


## add triple interaction -----
m.2 <- gam(
  richness ~
    s(Depth, bs = "tp") +
    s(longitude, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    s(towing.time, bs = "tp") +
    ti(Depth, longitude, towing.time) +
    s(HaulID, k = length(levels(mod.dat$HaulID)), bs = "re") ,
  method = "REML",
  data = mod.dat
)

### model check -----
mod <- m.2
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.2")

summary(mod)

## add double interaction longitude:towing.time -----
m.3 <- gam(
  richness ~
    s(Depth, bs = "tp") +
    s(towing.time, bs = "tp") +
    s(longitude, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    ti(towing.time, longitude) +
    s(HaulID, k = length(levels(mod.dat$HaulID)), bs = "re") ,
  method = "REML",
  data = mod.dat
)


### model check -----
mod <- m.3
k.check(mod)
appraise(mod)
draw(mod, residuals = TRUE) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.3")

summary(mod)


## add double interaction depth:towing.time -----
m.4 <- gam(
  richness ~
    s(Depth, bs = "tp") +
    s(longitude, bs = "tp") +
    s(towing.time, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    ti(towing.time, Depth) +
    s(HaulID, k = length(levels(mod.dat$HaulID)), bs = "re") ,
  method = "REML",
  data = mod.dat
)


### model check -----
mod <- m.4
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.4")

summary(mod)


# Model selection -----
ms <- AIC(m.1, m.2, m.3,  m.4) %>%
  data.table(keep.rownames = TRUE) %>%
  mutate(LL = c(
    logLik(m.1), logLik(m.2), logLik(m.3),  logLik(m.4)
  )) %>%
  mutate(dev = c(
    deviance(m.1), deviance(m.2), deviance(m.3),  deviance(m.4)
  )) %>%
  mutate(deltaAIC = AIC - min(AIC))

# akaike weights
ms$wi <- exp(-0.5 * ms$deltaAIC) / sum(exp(-0.5 * ms$deltaAIC))
# evidence ratio
ms$er <- max(ms$wi) / ms$wi
# Round numeric columns to 2 decimal places
ms <- ms[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]

ms$model <- c(
  "Main effects",
  "Main effects + triple interaction",
  "Main effects + double interaction (longitude, towing.time)",
  "Main effects + double interaction (depth, towing.time)"
)

ms <- ms[order(AIC), .(model, df, LL, dev, deltaAIC, er)]



# Deviance partition ----
p.var.part <- plot.gamhp(gam.hp(m.3), plot.perc = TRUE)

p.var.part$data %>% arrange(desc(value))
