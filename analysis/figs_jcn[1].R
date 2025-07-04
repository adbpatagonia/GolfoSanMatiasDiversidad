source(paste0(here::here(), "/analysis/1_Modelado.R"))

mod.dat <-
  mod.dat %>%
  mutate(
    lance = factor(lance)
  )

# modelo final -----

m.3.y <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(tiempo_arrastre2, bs = "tp") +
    s(long, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, long),
  method = "REML",
  data = mod.dat
)


### model check -----

mod <- m.3.y
k.check(mod)
appraise(mod)
summary(mod)

sm <- smooth_estimates(mod) %>%
  add_confint()

# add partial residuals to data

mod.dat <-
  mod.dat %>%
  add_partial_residuals(mod)

names(mod.dat)

## graficos finales ----

depth <-
  sm %>%
  filter(.smooth == "s(Depth)") %>%
  ggplot() +
  geom_rug(
    aes(x = Depth),
    data = mod.dat,
    sides = "b",
    length = grid::unit(0.02, "npc")
  ) +
  geom_ribbon(
    aes(
      ymin = .lower_ci,
      ymax = .upper_ci,
      x = Depth
    ),
    alpha = 0.2
  ) +
  geom_point(
    aes(
      x = Depth,
      y = `s(Depth)`
    ),
    data = mod.dat,
    shape = 21,
    cex = 1.5,
    fill = "grey",
    colour = "grey50",
    alpha = .7
  ) +
  geom_line(
    aes(
      x = Depth,
      y = .estimate
    ),
    lwd = 1.2
  ) +
  labs(
    y = "Partial effect",
    x = "Depth"
  )

time <-
  sm %>%
  filter(.smooth == "s(tiempo_arrastre2)") %>%
  ggplot() +
  geom_rug(
    aes(x = tiempo_arrastre2),
    data = mod.dat,
    sides = "b",
    length = grid::unit(0.02, "npc")
  ) +
  geom_ribbon(
    aes(
      ymin = .lower_ci,
      ymax = .upper_ci,
      x = tiempo_arrastre2
    ),
    alpha = 0.2
  ) +
  geom_point(
    aes(
      x = tiempo_arrastre2,
      y = `s(tiempo_arrastre2)`
    ),
    data = mod.dat,
    shape = 21,
    cex = 1.5,
    fill = "grey",
    colour = "grey50",
    alpha = .7
  ) +
  geom_line(
    aes(
      x = tiempo_arrastre2,
      y = .estimate
    ),
    lwd = 1.2,
    linetype = "dashed"
  ) +
  labs(
    y = "Partial effect",
    x = "Trawled Time"
  )


long <-
  sm %>%
  filter(.smooth == "s(long)") %>%
  ggplot() +
  geom_rug(
    aes(x = long),
    data = mod.dat,
    sides = "b",
    length = grid::unit(0.02, "npc")
  ) +
  geom_ribbon(
    aes(
      ymin = .lower_ci,
      ymax = .upper_ci,
      x = long
    ),
    alpha = 0.2
  ) +
  geom_point(
    aes(
      x = long,
      y = `s(long)`
    ),
    data = mod.dat,
    shape = 21,
    cex = 1.5,
    fill = "grey",
    colour = "grey50",
    alpha = .7
  ) +
  geom_line(
    aes(
      x = long,
      y = .estimate
    ),
    lwd = 1.2
  ) +
  labs(
    y = "Partial effect",
    x = "Longitude"
  )

year <-
  sm %>%
  filter(.smooth == "s(Year)") %>%
  ggplot() +
  geom_rug(
    aes(x = Year),
    data = mod.dat,
    sides = "b",
    length = grid::unit(0.02, "npc")
  ) +
  geom_ribbon(
    aes(
      ymin = .lower_ci,
      ymax = .upper_ci,
      x = Year
    ),
    alpha = 0.2
  ) +
  geom_point(
    aes(
      x = Year,
      y = `s(Year)`
    ),
    data = mod.dat,
    shape = 21,
    cex = 1.5,
    fill = "grey",
    colour = "grey50",
    alpha = .7
  ) +
  geom_line(
    aes(
      x = Year,
      y = .estimate
    ),
    linetype = "dashed",
    lwd = 1.2
  ) +
  labs(
    y = "Partial effect",
    x = "Year"
  )

# Crear grilla de predicción

grid_pred <-
  expand.grid(
    tiempo_arrastre2 = seq(
      min(mod.dat$tiempo_arrastre2),
      max(mod.dat$tiempo_arrastre2),
      length.out = 30
    ),
    long = seq(
      min(mod.dat$long),
      max(mod.dat$long),
      length.out = 30
    )
  )

# Agregar columnas faltantes

grid_pred$Depth <-
  median(mod.dat$Depth)
grid_pred$Year <-
  median(mod.dat$Year)

grid_pred$lance <-
  mod.dat$lance[1]  # valor arbitrario

# Predicción del modelo

grid_pred$pred_richness <-
  predict(
    m.3.y,
    newdata = grid_pred,
    type = "response"
  )


## término de la interacción (efecto parcial) ----

pred_terms <-
  predict(
    m.3.y,
    newdata = grid_pred,
    type = "terms"
  )

# Extraer efecto parcial de la interacción

grid_pred$partial_effect <-
  pred_terms[, "ti(tiempo_arrastre2,long)"]

# Gráfico centrado en cero

interaccion <-
  grid_pred %>%
  ggplot() +
  aes(
    x = tiempo_arrastre2,
    y = long,
    fill = partial_effect
  ) +
  geom_tile() +
  geom_contour(
    aes(
      z = partial_effect
    ),
    color = "grey10",
    alpha = 0.6
  ) +
  scale_fill_gradient2(
    name = "Partial effect",
    low = "darkblue",
    mid = "white",
    high = "darkred",
    midpoint = 0
  ) +
  theme_bw()+
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 8,
      barheight = 1
    )
  ) +
  theme_bw() +
  labs(
    x = "Trawling time",
    y = "Longitude"
  ) +
  theme(
    legend.position = "bottom",
    legend.justification = "center",legend.margin = margin(t = -5)
  )


library(patchwork)

(year + time + depth + long + interaccion) +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')
