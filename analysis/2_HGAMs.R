library("ungeviz")
library(gam.hp)

# source data -----
source(paste0(here::here(), "/analysis/1_Modelado.R"))

## full model G ----
# include Depth and  area barrida as continuous variables
# Year as random effect (random intercept)
mod.dat$Year_fac <- as.factor(mod.dat$Year)

m.rich.full_modG <- gam(
  riqueza ~ s(Depth, k = 20, bs = "tp") +
    s(area_barrida, k = 20, bs = "tp") +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re"),
  data = mod.dat,
  method = "REML", family = "gaussian"
)

mod <- m.rich.full_modG
### model check -----
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)
### replace  area by time
m.rich.full.time_modG <- gam(
  riqueza ~ s(Depth, k = 20, bs = "tp") +
    s(tiempo_arrastre2, k = 20, bs = "tp") +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re"),
  data = mod.dat,
  method = "REML", family = "gaussian"
)

draw(m.rich.full.time_modG)
summary(m.rich.full.time_modG)

## model G ----
# A single common (global) smoother for all observations
# two smoothers:
# 1. a Thin plate regression spline of depth, and
# 2. a random effect for treatment to model year-specific intercepts
# The random effect smoother (bs="re") that we used for the treatment factor
# always has a k value equal to the number of levels in the grouping variable
m.rich_modG <- gam(
  riqueza ~ s(Depth, k = 20, bs = "tp") +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re"),
  data = mod.dat,
  method = "REML", family = "gaussian"
)

mod <- m.rich_modG
### model check -----
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)

logLik(m.rich_modG, REML = TRUE)
logLik(m.rich.full_modG, REML = TRUE)

## model GS ----
# A single common smoother plus group-level smoothers that have the same wiggliness
# analogue to a GLMM with varying slopes
# two smoothers:
# 1. a Thin plate regression spline of depth, and
# 2.  group-level smoother (year-specific)

m.rich_modGS <- gam(
  riqueza ~ s(Depth, k = 20, bs = "tp") +
    s(Depth, Year_fac, k = 20, bs = "fs", m = 2),
  data = mod.dat,
  method = "REML", family = "gaussian"
)

mod <- m.rich_modGS
### model check -----
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)

## model GI ----
# A single common smoother plus group-level smoothers that have their own level of wiggliness
# three smoothers:
# 1. a Thin plate regression spline of depth, and
# 2.  group-level smoother (year-specific). Diffrence from model GS: m = 1
# 3. a random effect for year to model year-specific intercepts
# The random effect smoother (bs="re") that we used for the year factor
# always has a k value equal to the number of levels in the grouping variable

m.rich_modGI <- gam(
  riqueza ~ s(Depth, k = 20, m = 2, bs = "tp") +
    s(Depth, Year_fac, k = 20, bs = "fs", m = 1) +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re"),
  data = mod.dat,
  method = "REML", family = "gaussian"
)
mod <- m.rich_modGI
### model check -----
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)

## model S ----
# Model S (shared smoothers) is model GS without the global smoother term
# This model assumes all groups have the same smoothness, but that the individual shapes of the smooth terms are not related
# If in a study there are very few data points in each grouping level (relative to the strength of the functional relationship of interest), estimates from model S will typically be much more variable than from model GS
# one smoother:
# 1.  group-level smoother (year-specific)

m.rich_modS <- gam(riqueza ~ s(Depth, Year_fac, k = 20, bs = "fs", m = 2),
  data = mod.dat,
  method = "REML", family = "gaussian"
)


mod <- m.rich_modS
### model check -----
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)
## model I ----
# Model I is model GI without the first term
#  group-level smoothers that have their own level of wiggliness
# two smoothers:
# 1.  group-level smoother (year-specific).
# 2. a random effect for year to model year-specific intercepts
# The random effect smoother (bs="re") that we used for the year factor
# always has a k value equal to the number of levels in the grouping variable

m.rich_modI <- gam(
  riqueza ~ s(Depth, Year_fac, k = 20, bs = "fs", m = 2) +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re"),
  data = mod.dat,
  method = "REML", family = "gaussian"
)
mod <- m.rich_modI
### model check -----
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)

# model selection ----
AIC(
  # m.rich,
  #   m.rich_red,
  m.rich.full_modG,
  m.rich_modG,
  m.rich_modGS,
  m.rich_modGI,
  m.rich_modS,
  m.rich_modI
) %>%
  arrange(AIC) %>%
  mutate(deltaAIC = round(AIC - min(AIC), 2))

bestmod <- m.rich_modG

# model fit multiple models ------
new.dat <- expand.grid(
  Year_fac = unique(mod.dat$Year_fac),
  Depth = seq(min(mod.dat$Depth), max(mod.dat$Depth), 1)
)
predictions <- bind_rows(
  predict(bestmod,
    newdata = new.dat,
    se.fit = TRUE,
    type = "response"
  ) %>%
    data.frame() %>%
    bind_cols(new.dat) %>%
    mutate(modelo = "G"),
  predict(m.rich_modGS,
    newdata = new.dat,
    se.fit = TRUE,
    type = "response"
  ) %>%
    data.frame() %>%
    bind_cols(new.dat) %>%
    mutate(modelo = "GS")
) %>%
  bind_rows(
    predict(m.rich_modGS,
      newdata = new.dat,
      se.fit = TRUE,
      type = "response"
    ) %>%
      data.frame() %>%
      bind_cols(new.dat) %>%
      mutate(modelo = "GI")
  )

p.fit.mods <-
  ggplot(predictions) +
  geom_ribbon(
    aes(
      x = Depth,
      y = fit,
      fill = modelo,
      ymin = (fit - 2 * se.fit),
      ymax = (fit + 2 * se.fit)
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  # geom_line(data = sample_df, aes(x = Depth, y = riqueza,
  #                                 group = .draw, color = Year_fac),
  #           size = 0.3)  +
  geom_line(aes(x = Depth, y = fit, color = modelo, linetype = modelo), size = 1.3) +
  geom_point(
    data = mod.dat,
    aes(x = Depth, y = riqueza),
    alpha = 0.4
  ) +
  facet_grid(. ~ Year_fac) +
  # facet_wrap(.~Year_fac)  +
  ylab("Species Richness") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE, title = "Year")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = "Year"))


# model fit G ----
sample_df <- sample_outcomes(bestmod, newdata = new.dat, 20, unconditional = TRUE)
conf <- confidence_band(bestmod, newdata = new.dat, unconditional = TRUE)

p.fit.bestmod <- predict(bestmod,
  newdata = new.dat,
  se.fit = TRUE,
  type = "response"
) %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(
    aes(
      x = Depth,
      y = fit,
      fill = Year_fac,
      ymin = (fit - 2 * se.fit),
      ymax = (fit + 2 * se.fit)
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  # geom_line(data = sample_df, aes(x = Depth, y = riqueza,
  #                                 group = .draw, color = Year_fac),
  #           size = 0.3)  +
  geom_line(aes(x = Depth, y = fit, color = Year_fac), size = 1.3) +
  geom_point(
    data = mod.dat,
    aes(
      x = Depth, y = riqueza,
      color = Year_fac,
      label = area_barrida
    ),
    alpha = 0.4
  ) +
  facet_grid(. ~ Year_fac) +
  # facet_wrap(.~Year_fac)  +
  ylab("Species Richness") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE, title = "Year")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = "Year"))



parameters::simulate_parameters(bestmod)
# 2016 > 2018 > 2009 > 2006 > 2022 > 2007

p.var.part <- plot.gamhp(gam.hp(bestmod), plot.perc = TRUE)

p.var.part$data[which(grepl(x = p.var.part$data$variable, pattern = "Depth")), "value"]
p.var.part$data[which(grepl(x = p.var.part$data$variable, pattern = "Year")), "value"]

m <- gam(
  riqueza ~ -1 + s(Depth, k = 20, bs = "tp") +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re"),
  data = mod.dat,
  method = "REML", family = "gaussian"
)
parameters::simulate_parameters(m, centrality = "mean")

# model fit GI -----
p.fit.modGI <- predict(m.rich_modGI,
  newdata = new.dat,
  se.fit = TRUE,
  type = "response"
) %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(
    aes(
      x = Depth,
      y = fit,
      fill = Year_fac,
      ymin = (fit - 2 * se.fit),
      ymax = (fit + 2 * se.fit)
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  # geom_line(data = sample_df, aes(x = Depth, y = riqueza,
  #                                 group = .draw, color = Year_fac),
  #           size = 0.3)  +
  geom_line(aes(x = Depth, y = fit, color = Year_fac), size = 1.3) +
  geom_point(
    data = mod.dat,
    aes(x = Depth, y = riqueza, color = Year_fac),
    alpha = 0.4
  ) +
  facet_grid(. ~ Year_fac) +
  # facet_wrap(.~Year_fac)  +
  ylab("Species Richness") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE, title = "Year")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = "Year"))

# model fit GS -----
p.fit.modGS <- predict(m.rich_modGS,
  newdata = new.dat,
  se.fit = TRUE,
  type = "response"
) %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(
    aes(
      x = Depth,
      y = fit,
      fill = Year_fac,
      ymin = (fit - 2 * se.fit),
      ymax = (fit + 2 * se.fit)
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  # geom_line(data = sample_df, aes(x = Depth, y = riqueza,
  #                                 group = .draw, color = Year_fac),
  #           size = 0.3)  +
  geom_line(aes(x = Depth, y = fit, color = Year_fac), size = 1.3) +
  geom_point(
    data = mod.dat,
    aes(x = Depth, y = riqueza, color = Year_fac),
    alpha = 0.4
  ) +
  facet_grid(. ~ Year_fac) +
  # facet_wrap(.~Year_fac)  +
  ylab("Species Richness") +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE, title = "Year")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = "Year"))
