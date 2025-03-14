# ADB
# 2025-03-14
# analisis para analizar la riqueza en el golfo san matias como funcion del Year de pesca y del esfuerzo pesquero
# datos provistos por el Dr Guillermo Svendsen (CONICET - Centro Almirante Storni),
# como parte de la tesis de Josefina Cuesta Nunez (Universidad del Comahue, CONICET - Centro Almirante Storni)

# this script sources the scripts that carries out data exploration
# and fits models


# source data -----
source("analysis/0_ExplorarDatos.R")


# GAM models -----
## standardize x values -----
mod.dat$Depth_st <- standardize_x(mod.dat$Depth)

# I fit the model with the standardized values, and the fit was identical
# to the model fit without standardizing
# given that the fit is the same, I decided to use the variables in the
# original scale, because it is easier to interpret

## fit full model -----
mod.dat$area_barrida_st <- standardize_x(mod.dat$area_barrida)

m.rich <- gam(riqueza ~ s(Year, bs = "cr",  k = 6) +
                s(Depth, bs = "cr") +
                s(area_barrida, bs = "cr"),
              method="REML",
              data = mod.dat)

### model check -----
k.check(m.rich)
appraise(m.rich)
draw(m.rich)

summary(m.rich)

## reduced model -----
# drop the term area_barrida
m.rich_red <-  gam(riqueza ~ s(Year, bs = "cr",  k = 6) +
                     s(Depth, bs = "cr") ,
                     method = "REML",
                   data = mod.dat)

### model check -----
k.check(m.rich_red)
appraise(m.rich_red)
draw(m.rich_red)

summary(m.rich_red)

## model comparison -----
anova(m.rich, m.rich_red)

# the reduced model has a more parsimonious fit
# smaller AIC
AIC(m.rich, m.rich_red)
# similar fit
logLik(m.rich)
logLik(m.rich_red)
deviance(m.rich)
deviance(m.rich_red)

vis.gam(m.rich_red, view=c( "Depth", "Year"))
vis.gam(m.rich_red, view=c( "Year", "Depth"))

## plot fits ------
wd <- 0.4

### riqueza ~ f(Year) -----
new.dat <- expand.grid(Year = seq(min(mod.dat$Year), max(mod.dat$Year), 1),
                       # area_barrida = mean(mod.dat$area_barrida),
                       Depth = mean(mod.dat$Depth))

p.fit.Year <- predict(m.rich_red,
                      newdata = new.dat,
                      se.fit = TRUE,
                      type="response") %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(aes(x = Year,
                  y = fit,
                  ymin = (fit - 2 * se.fit),
                  ymax = (fit + 2 * se.fit)),
              alpha = 0.15,
              color = 'transparent') +
  geom_line(aes(x = Year, y = fit)) +
  geom_point(data = mod.dat,
             aes(x = Year, y = riqueza),
             alpha = 0.4,
             position = position_dodge2(width = wd)) +
  ylab("Species Richness")

### riqueza ~ f(Depth) -----
new.dat <- expand.grid(Depth = seq(min(mod.dat$Depth), max(mod.dat$Depth), 1),
                       # area_barrida = mean(mod.dat$area_barrida),
                       Year = mean(mod.dat$Year))

p.fit.Depth <- predict(m.rich_red,
                       newdata = new.dat,
                       se.fit = TRUE,
                       type="response") %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(aes(x = Depth,
                  y = fit,
                  ymin = (fit - 2 * se.fit),
                  ymax = (fit + 2 * se.fit)),
              alpha = 0.15,
              color = 'transparent') +
  geom_line(aes(x = Depth, y = fit)) +
  geom_point(data = mod.dat,
             aes(x = Depth, y = riqueza),
             alpha = 0.4,
             position = position_dodge2(width = wd)) +
  ylab("Species Richness")

### riqueza ~ f(Year | Depth) -----
new.dat <- expand.grid(Year = seq(min(mod.dat$Year), max(mod.dat$Year), 1),
                       # area_barrida = mean(mod.dat$area_barrida),
                       Depth_bin = cut(mod.dat$Depth, 5)) %>%
  merge(mod.dat %>%
          mutate(Depth_bin = cut(Depth, 5)) %>%
          group_by(Depth_bin) %>%
          reframe(Depth = mean(Depth))) %>%
  unique() %>%
  arrange(Year, Depth)
mod.dat <- mod.dat %>%
  mutate(Depth_bin = cut(Depth, 5))


p.fit.Year_Depth <- predict(m.rich_red,
                            newdata = new.dat,
                            se.fit = TRUE,
                            type="response") %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(aes(x = Year,
                  y = fit,
                  ymin = (fit - 2 * se.fit),
                  ymax = (fit + 2 * se.fit)),
              alpha = 0.15,
              color = 'transparent') +
  geom_line(aes(x = Year, y = fit)) +
  geom_point(data = mod.dat,
             aes(x = Year, y = riqueza),
             alpha = 0.4,
             position = position_dodge2(width = wd)) +
  facet_grid(.~Depth_bin) +
  ylab("Species Richness")

### riqueza ~ f(Depth | Year) -----
new.dat <- expand.grid(Year = unique(mod.dat$Year),
                       # area_barrida = mean(mod.dat$area_barrida),
                       Depth = seq(min(mod.dat$Depth), max(mod.dat$Depth), .1))

p.fit.Depth_Year <- predict(m.rich_red,
                            newdata = new.dat,
                            se.fit = TRUE,
                            type="response") %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(aes(x = Depth,
                  y = fit,
                  ymin = (fit - 2 * se.fit),
                  ymax = (fit + 2 * se.fit)),
              alpha = 0.15,
              color = 'transparent') +
  geom_line(aes(x = Depth, y = fit)) +
  geom_point(data = mod.dat,
             aes(x = Depth, y = riqueza),
             alpha = 0.4,
             position = position_dodge2(width = wd)) +
  facet_wrap(.~Year)  +
  ylab("Species Richness")

# extra stuff ----
# https://github.com/wilkelab/ungeviz
library('ungeviz')
sample_df <- sample_outcomes(m.rich_red, newdata = new.dat, 30, unconditional = TRUE)
conf <- confidence_band(m.rich_red, newdata = new.dat, unconditional = TRUE)
ggplot(mod.dat, aes(Year, riqueza)) +
  facet_grid(.~Depth_bin) +
  geom_ribbon(data = conf, aes(ymin = lo, ymax = hi), fill="#80808040", color = NA) +
  geom_point(alpha = 0.4,
             position = position_dodge2(width = wd)) +
  geom_line(data = sample_df, aes(group = .draw), color = "#0072B2", size = 0.3) +
  geom_line(data = conf, size = 1, color = "darkred") +
  theme_bw()

# output -----
