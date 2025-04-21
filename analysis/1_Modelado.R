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
    ti(Depth, long, tiempo_arrastre2) ,
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
    s(long) +
    s(tiempo_arrastre2) +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(long, tiempo_arrastre2) ,
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
    ti( Depth, tiempo_arrastre2) ,
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
    logLik(m.1),logLik(m.2),logLik(m.3),logLik(m.4)
  )) %>%
  mutate(dev = c(
    deviance(m.1),deviance(m.2),deviance(m.3),deviance(m.4)
  )) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(AIC)
# Round numeric columns to 2 decimal places
ms <- ms[, lapply(.SD, function(x) if(is.numeric(x)) round(x, 2) else x)]


# Visualizze flexibility of each smooth term
# Extract EDF for each term in the model
edf_data <- edf(m.3)

# Visualize EDFs
ggplot(edf_data, aes(x = reorder(.smooth, .edf), y = .edf)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(title = "Effective Degrees of Freedom per Smooth Term",
       x = "Smooth Term", y = "Effective Degrees of Freedom (EDF)")



vis.gam(m.3, view = c("Depth", "long"))
vis.gam(m.3, view = c("tiempo_arrastre2", "Depth"))
vis.gam(m.3, view = c("tiempo_arrastre2", "long"))

## Predictions ----
# effect of longitude is linear - simply look at the mean
# effect of tiempo de arrastre: bin it
new.dat <- expand.grid(
  Year_fac = unique(mod.dat$Year_fac),
  Depth = seq(min(mod.dat$Depth), max(mod.dat$Depth),  1),
  long = mean(mod.dat$long),
  tiempo_arrastre2 = seq(15,45,1)
  # tiempo_arrastre2 = c(15,30,40)

) %>%
  mutate(time_bin = cut(tiempo_arrastre2, 3))

mod.dat[, time_bin := factor(ifelse(tiempo_arrastre2 > 35, "(35,45]",
                                    ifelse(tiempo_arrastre2 <= 25, "(15,25]",
                                           "(25,35]")))]

pred.bestmod <- predict(m.3,
                        newdata = new.dat,
                        se.fit = TRUE,
                        type = "response"
) %>%
  data.frame() %>%
  mutate(lcl = fit - 2 * se.fit,
         ucl = fit + 2 * se.fit) %>%
  bind_cols(new.dat)
## plot fits ------
wd <- 0.4

### bestmod -----
p.bestmod <-   ggplot(pred.bestmod) +
  geom_ribbon(
    aes(
      x = Depth,
      y = fit,
      ymin = lcl,
      ymax = ucl
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  geom_line(aes(x = Depth, y = fit)) +
  geom_point(
    data = mod.dat,
    aes(x = Depth, y = riqueza),
    alpha = 0.4,
    position = position_dodge2(width = wd)
  ) +
  ylab("Species Richness") +
  facet_grid(Year_fac ~ time_bin)

### riqueza ~ f(Depth) -----
new.dat <- expand.grid(
  Depth = seq(min(mod.dat$Depth), max(mod.dat$Depth), 1),
  # area_barrida = mean(mod.dat$area_barrida),
  Year = mean(mod.dat$Year)
)

p.fit.Depth <- predict(m.rich_red,
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
      ymin = (fit - 2 * se.fit),
      ymax = (fit + 2 * se.fit)
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  geom_line(aes(x = Depth, y = fit)) +
  geom_point(
    data = mod.dat,
    aes(x = Depth, y = riqueza),
    alpha = 0.4,
    position = position_dodge2(width = wd)
  ) +
  ylab("Species Richness")

### riqueza ~ f(Year | Depth) -----
new.dat <- expand.grid(
  Year = seq(min(mod.dat$Year), max(mod.dat$Year), 1),
  # area_barrida = mean(mod.dat$area_barrida),
  Depth_bin = cut(mod.dat$Depth, 5)
) %>%
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
                            type = "response"
) %>%
  data.frame() %>%
  bind_cols(new.dat) %>%
  ggplot(.) +
  geom_ribbon(
    aes(
      x = Year,
      y = fit,
      ymin = (fit - 2 * se.fit),
      ymax = (fit + 2 * se.fit)
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  geom_line(aes(x = Year, y = fit)) +
  geom_point(
    data = mod.dat,
    aes(x = Year, y = riqueza),
    alpha = 0.4,
    position = position_dodge2(width = wd)
  ) +
  facet_grid(. ~ Depth_bin) +
  ylab("Species Richness")

### riqueza ~ f(Depth | Year) -----
new.dat <- expand.grid(
  Year = unique(mod.dat$Year),
  # area_barrida = mean(mod.dat$area_barrida),
  Depth = seq(min(mod.dat$Depth), max(mod.dat$Depth), .1)
)

p.fit.Depth_Year <- predict(m.rich_red,
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
      ymin = (fit - 2 * se.fit),
      ymax = (fit + 2 * se.fit)
    ),
    alpha = 0.15,
    color = "transparent"
  ) +
  geom_line(aes(x = Depth, y = fit)) +
  geom_point(
    data = mod.dat,
    aes(x = Depth, y = riqueza),
    alpha = 0.4,
    position = position_dodge2(width = wd)
  ) +
  facet_wrap(. ~ Year) +
  ylab("Species Richness")

# extra stuff ----
# https://github.com/wilkelab/ungeviz
library("ungeviz")
sample_df <- sample_outcomes(m.rich_red, newdata = new.dat, 30, unconditional = TRUE)
conf <- confidence_band(m.rich_red, newdata = new.dat, unconditional = TRUE)
ggplot(mod.dat, aes(Year, riqueza)) +
  facet_grid(. ~ Depth_bin) +
  geom_ribbon(data = conf, aes(ymin = lo, ymax = hi), fill = "#80808040", color = NA) +
  geom_point(
    alpha = 0.4,
    position = position_dodge2(width = wd)
  ) +
  geom_line(data = sample_df, aes(group = .draw), color = "#0072B2", size = 0.3) +
  geom_line(data = conf, size = 1, color = "darkred") +
  theme_bw()

# output -----

library(RColorBrewer)
library(plotly)

### riqueza ~ f(Depth & Year) -----
new.dat <- expand.grid(
  tiempo_arrastre2 = seq(15, 45, 1),
  # long = seq(min(mod.dat$long), max(mod.dat$long), length.out = 15),
  long = mean(mod.dat$long),
  Year_fac = unique(mod.dat$Year_fac),
  Depth = seq(32, 176, 2)
)
preds <- predict(m.3,
                 newdata = new.dat,
                 se.fit = FALSE,
                 exclude = "s(Year_fac)",
                 type = "response"
) %>%
  data.frame() %>%
  rename(fit = ".") %>%
  bind_cols(new.dat)

Year_obs <- mod.dat$Year
Depth_obs <- mod.dat$Depth
time_obs <- mod.dat$tiempo_arrastre2
long_obs <- mod.dat$long
sprich <- mod.dat$riqueza



p.fit.3d <- plot_ly(preds,
                    x = ~tiempo_arrastre2,
                    y = ~Depth,
                    z = ~fit,
                    intensity = ~fit,
                    colors = rev(colorRampPalette(brewer.pal(10, "Spectral"))(41)),
                    type = "mesh3d",
                    opacity = 0.6,
                    showscale = FALSE,
                    showlegend = FALSE
) %>%
  # layout(
  #   xaxis = list(range = c(2005, 2022)),
  #   yaxis = list(range = c(30, 185)))

  add_trace(
    x = ~time_obs, y = ~Depth_obs, z = ~sprich,
    type = "scatter3d",
    mode = "markers",
    # intensity = ~recruits,
    marker = list(
      color = "rgb(0, 0, 0)",
      size = 3
    ),
    opacity = 0.6,
    # size = 2,
    # colors = 'black',
    showlegend = FALSE
  ) %>%
  layout(
    title = "",
    # xaxis = list(range = c(2005, 2022)),
    # yaxis = list(range = c(30, 185)),
    scene = list(
      xaxis = list(title = "Tiempo de arrastre"),
      yaxis = list(title = "Depth (m)"),
      zaxis = list(title = "Species Richness")
    )
  ) # %>%
# config(
#   toImageButtonOptions = list(
#     format = "svg",
#     filename = "PinkOdd_SR_Flow",
#     width = 600,
#     height = 700
#   )
# )
