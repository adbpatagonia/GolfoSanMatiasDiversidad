# ADB
# 2025-03-14
# analisis para analizar la riqueza en el golfo san matias como funcion del Year de pesca y del esfuerzo pesquero
# datos provistos por el Dr Guillermo Svendsen (CONICET - Centro Almirante Storni),
# como parte de la tesis de Josefina Cuesta Nunez (Universidad del Comahue, CONICET - Centro Almirante Storni)

# this script sources the scripts that carries out data exploration
# and fits models


# source data -----
source(paste0(here::here(), "/analysis/0_ExplorarDatos.R"))
# source("analysis/0_ExplorarDatos.R")

# libraries -----
library(gam.hp)



# GAM models -----
## standardize x values -----
mod.dat$Depth_st <- standardize_x(mod.dat$Depth)
mod.dat$area_barrida_st <- standardize_x(mod.dat$area_barrida)
mod.dat$long_st <- standardize_x(mod.dat$long)
# I fit the model with the standardized values, and the fit was identical
# to the model fit without standardizing
# given that the fit is the same, I decided to use the variables in the
# original scale, because it is easier to interpret

# Year as random effect (random intercept)
mod.dat$Year_fac <- as.factor(mod.dat$Year)

## fit full model -----
# include Year as a random effect
# include depth, longitude (distance from the mouth of the Gulf, and towing time)
m.1 <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    s(tiempo_arrastre2, bs = "tp") +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re"),
  method = "ML",
  data = mod.dat
)

### model check -----
mod <- m.1
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)


## add triple interaction -----
m.2 <- gam(
  riqueza ~
    s(Depth) +
    s(long) +
    s(tiempo_arrastre2) +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(Depth, long, tiempo_arrastre2),
  method = "ML",
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

## add double interaction long -----
m.3 <- gam(
  riqueza ~
    s(Depth) +
    s(tiempo_arrastre2) +
    s(long) +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, long),
  method = "ML",
  data = mod.dat
)


### model check -----
mod <- m.3
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.3")

summary(mod)

## add double interaction depth -----
m.4 <- gam(
  riqueza ~
    s(Depth) +
    s(long) +
    s(tiempo_arrastre2) +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, Depth),
  method = "ML",
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

## Model selection -----
# most parsimonious model is model with no interaction
ms <- AIC(m.1, m.2, m.3, m.4) %>%
  data.table(keep.rownames = TRUE) %>%
  mutate(LL = c(
    logLik(m.1), logLik(m.2), logLik(m.3), logLik(m.4)
  )) %>%
  mutate(dev = c(
    deviance(m.1), deviance(m.2), deviance(m.3), deviance(m.4)
  )) %>%
  mutate(deltaAIC = AIC - min(AIC))
# Round numeric columns to 2 decimal places
ms$wi <- exp(-0.5 * ms$deltaAIC) / sum(exp(-0.5 * ms$deltaAIC))
ms$er <- max(ms$wi) / ms$wi
ms$rsq <- c(
  summary(m.1)$r.sq,
  summary(m.2)$r.sq,
  summary(m.3)$r.sq,
  summary(m.4)$r.sq
)
ms <- ms[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]
ms$model <- c(
  "Main effects",
  "Main effects + triple interaction",
  "Main effects + double interaction (longitud, tiempo)",
  "Main effects + double interaction (depth, tiempo)"
)

ms <- ms[order(AIC), .(model, df, LL, dev, rsq, deltaAIC, er)]

# Visualizze flexibility of each smooth term
# Extract EDF for each term in the model
edf_data <- edf(m.3)

# Visualize EDFs
ggplot(edf_data, aes(x = reorder(.smooth, .edf), y = .edf)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Effective Degrees of Freedom per Smooth Term",
    x = "Smooth Term", y = "Effective Degrees of Freedom (EDF)"
  )



vis.gam(m.3, view = c("Depth", "long"))
vis.gam(m.3, view = c("tiempo_arrastre2", "Depth"))
vis.gam(m.3, view = c("tiempo_arrastre2", "long"))


# partition of deviance ----
p.var.part <- plot.gamhp(gam.hp(m.3), plot.perc = TRUE)
