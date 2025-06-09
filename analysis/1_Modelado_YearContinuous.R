source(paste0(here::here(), "/analysis/1_Modelado.R"))

## fit full model -----
# include Year as a random effect
# include depth, longitude (distance from the mouth of the Gulf, and towing time)
m.1.y <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    s(tiempo_arrastre2, bs = "tp") ,
  # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") ,
  # s(lance, k = length(levels(mod.dat$lance)), bs = "re"),
  method = "ML",
  data = mod.dat
)

### model check -----
mod <- m.1.y
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)

### remove year ----
m.1.ny <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    # s(Year, bs = "cr", k = 6) +
    s(tiempo_arrastre2, bs = "tp") ,
  # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") ,
  # s(lance, k = length(levels(mod.dat$lance)), bs = "re"),
  method = "ML",
  data = mod.dat
)

#### model check -----
mod <- m.1.ny
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)

## add triple interaction -----
m.2.y <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    s(tiempo_arrastre2, bs = "tp") +
    # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    # s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    ti(Depth, long, tiempo_arrastre2),
  method = "ML",
  data = mod.dat
)

### model check -----
mod <- m.2.y
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.2")

summary(mod)

### remove year ----
m.2.ny <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    # s(Year, bs = "cr", k = 6) +
    s(tiempo_arrastre2, bs = "tp") +
    # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    # s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    ti(Depth, long, tiempo_arrastre2),
  method = "ML",
  data = mod.dat
)

#### model check -----
mod <- m.2.ny
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.2")

summary(mod)


## add double interaction long -----
m.3.y <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(tiempo_arrastre2, bs = "tp") +
    s(long, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    # s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, long),
  method = "REML",
  data = mod.dat
)


### model check -----
mod <- m.3.y
k.check(mod)
appraise(mod)
draw(mod, residuals = TRUE) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.3")

summary(mod)

### remove year ----
m.3.ny <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(tiempo_arrastre2, bs = "tp") +
    s(long, bs = "tp") +
    # s(Year, bs = "cr", k = 6) +
    # s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, long),
  method = "ML",
  data = mod.dat
)


#### model check -----
mod <- m.3.ny
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.3.ny")

summary(mod)

## add double interaction depth -----
m.4.y <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    s(tiempo_arrastre2, bs = "tp") +
    s(Year, bs = "cr", k = 6) +
    # s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, Depth),
  method = "ML",
  data = mod.dat
)


### model check -----
mod <- m.4.y
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.4")

summary(mod)

### remove year ----
m.4.ny <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    s(tiempo_arrastre2, bs = "tp") +
    # s(Year, bs = "cr", k = 6) +
    # s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, Depth),
  method = "ML",
  data = mod.dat
)


#### model check -----
mod <- m.4.ny
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.4")

summary(mod)

## Model selection -----
# most parsimonious model is model with no interaction
ms.y <- AIC(m.1.y, m.2.y, m.3.y,  m.4.y) %>%
  data.table(keep.rownames = TRUE) %>%
  mutate(LL = c(
    logLik(m.1.y), logLik(m.2.y), logLik(m.3.y),  logLik(m.4.y)
  )) %>%
  mutate(dev = c(
    deviance(m.1.y), deviance(m.2.y), deviance(m.3.y),  deviance(m.4.y)
  )) %>%
  mutate(deltaAIC = AIC - min(AIC))
# Round numeric columns to 2 decimal places
ms.y$wi <- exp(-0.5 * ms.y$deltaAIC) / sum(exp(-0.5 * ms.y$deltaAIC))
ms.y$er <- max(ms.y$wi) / ms.y$wi
ms.y$rsq <- c(
  summary(m.1.y)$r.sq,
  summary(m.2.y)$r.sq,
  summary(m.3.y)$r.sq,
  # summary(m.3)$r.sq,
  # summary(m.3.ny)$r.sq,
  summary(m.4.y)$r.sq
)
ms.y <- ms.y[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]
ms.y$model <- c(
  "Main effects",
  "Main effects + triple interaction",
  "Main effects + double interaction (longitud, tiempo)",
  # "Main effects + double interaction (longitud, tiempo) - random year",
  # "Main effects + double interaction (longitud, tiempo) - remove year",
  "Main effects + double interaction (depth, tiempo)"
)

ms.y <- ms.y[order(AIC), .(model, df, LL, dev, rsq, deltaAIC, er)]


# plots -----
# Visualizze flexibility of each smooth term
# Extract EDF for each term in the model
edf_data <- edf(m.3.y)


# Visualize EDFs
# ggplot(edf_data, aes(x = reorder(.smooth, .edf), y = .edf)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   theme_minimal(base_size = 14) +
#   labs(
#     title = "Effective Degrees of Freedom per Smooth Term",
#     x = "Smooth Term", y = "Effective Degrees of Freedom (EDF)"
#   )



## 3D plots -----
# vis.gam(m.3.y, view = c("Depth", "long"))
# vis.gam(m.3.y, view = c("tiempo_arrastre2", "Depth"))
# vis.gam(m.3.y, view = c("tiempo_arrastre2", "long"))

# vis.gam(m.3,
#         view = c("long", "tiempo_arrastre2"),
#         plot.type = "persp")
# vis.gam(m.3,
#         view = c("tiempo_arrastre2", "long"),
#         plot.type = "persp")


new.dat <- expand.grid(
  tiempo_arrastre2 = seq(min(mod.dat$tiempo_arrastre2), max(mod.dat$tiempo_arrastre2), length.out = 15),
  long = seq(min(mod.dat$long), max(mod.dat$long), length.out = 15),
  Depth = c(60, 90, 170)
)
preds <- predict(m.3.ny,
                 newdata = new.dat,
                 se.fit = FALSE,
                 # exclude = "s(Year_fac)",
                 type = "response"
) %>%
  data.frame() %>%
  rename(fit = ".") %>%
  bind_cols(new.dat)

# Plot
ggplot() +
  geom_tile(data = preds,
            aes(x = tiempo_arrastre2, y = long, fill = fit)) +
  geom_point(data = mod.dat,
             aes(x = tiempo_arrastre2, y = long), color = "black", alpha = 0.6) +
  scale_fill_viridis_c() +
  theme_minimal()


Year_obs <- mod.dat$Year
Depth_obs <- mod.dat$Depth
time_obs <- mod.dat$tiempo_arrastre2
long_obs <- mod.dat$long
sprich <- mod.dat$riqueza


# # Split the data by group
groups <- unique(preds$Depth)

# Add mesh trace (with legend turned off)
p <- plot_ly()
# Loop through each group
for (g in groups) {
  data_g <- subset(preds, Depth == g)

  # Triangulate the surface
  triangles <- delaunayn(data_g[, c("long", "tiempo_arrastre2")])
  i <- triangles[, 1] - 1
  j <- triangles[, 2] - 1
  k <- triangles[, 3] - 1


  hover_text <- paste0(
    "Tiempo: ", g, "<br>",
    "Longitude: ", data_g$long, "<br>",
    "Depth: ", data_g$Depth, "<br>",
    "Species Richness: ", round(data_g$fit, 2)
  )


  p <- add_trace(
    p,
    type = "mesh3d",
    x = data_g$tiempo_arrastre2,
    y = data_g$long,
    z = data_g$fit,
    i = i, j = j, k = k,
    intensity = preds$fit,
    text = hover_text,
    hoverinfo = "text",
    opacity = 0.6,
    colors = rev(colorRampPalette(brewer.pal(10, "Spectral"))(41)),
    # name = paste("Group", g),
    showlegend = FALSE, # This hides the mesh from the legend
    flatshading = TRUE,
    showscale = FALSE
  )
}

# Add the shared marker trace with legend
ht <- paste0(
  "Longitude_obs: ", mod.dat$long, "<br>",
  "Tiempo_obs: ", mod.dat$tiempo_arrastre2, "<br>",
  "Depth_obs: ", mod.dat$Depth, "<br>",
  "Species Richness_obs: ", round(mod.dat$riqueza, 2)
)
p <- add_trace(
  p,
  data = mod.dat,
  x = ~tiempo_arrastre2,
  y = ~long,
  z = ~riqueza,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    color = ~Depth_obs,
    colorscale = "Viridis",
    colorbar = list(
      title = "Depth",
      len = 0.4
    ),
    size = 3
  ),
  name = "Observations", # Legend label
  text = ht,
  hoverinfo = "text",
  showlegend = TRUE, # Show this in the legend
  opacity = 0.6
)

# Final layout
p <- layout(
  p,
  title = "",
  scene = list(
    xaxis = list(title = "Tiempo de arrastre"),
    yaxis = list(title = "Longitude"),
    zaxis = list(title = "Species Richness")
  ))

