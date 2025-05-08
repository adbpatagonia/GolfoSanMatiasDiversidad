# source modelling -----
source(paste0(here::here(), "/analysis/2_plot_fits.R"))
# libraries -----
library(glmmTMB)
library(DHARMa)



# fit glzm ----
m.3.lm <- glmmTMB(
  riqueza ~
    Depth_st +
    tiempo_arrastre2_st +
    long_st +
    tiempo_arrastre2_st * long_st +
    (1|Year_fac) ,
  data = mod.dat, family = gaussian(link = "log")
)


## model check -----
mod <- m.3.lm
testDispersion(mod)
simulationOutput <- simulateResiduals(fittedModel = mod, plot = F)
plot(simulationOutput)

summary(mod)
logLik(mod)
parameters::parameters(mod)
parameters::parameters(m.3)

# prepare data ----

standardize_x(x = 176, mean_val = mean(mod.dat$Depth), sd_val = sd(mod.dat$Depth))

new.dat.lm <- expand.grid(
  tiempo_arrastre2 =  seq(15, 45, length.out = 10),
  tiempo_arrastre2_st =  seq(-2.097858, 2.581987, length.out = 10),
  # long = seq(min(mod.dat$long), max(mod.dat$long), length.out = 15),
  # long = mean(mod.dat$long),
  long = c(-64, -64.4, -64.8),
  long_st = c(1.761123, 0.201424, -1.358275),
  Year_fac = unique(mod.dat$Year_fac),
  Depth_st = seq(-3.155327, 176, length.out = 70),
  Depth = seq(32, 1.709766, length.out = 70)
)

preds <- predict(m.3.lm,
                 newdata = new.dat.lm,
                 se.fit = FALSE,
                 re.form = NA,
                 # exclude = "s(Year_fac)",
                 type = "response"
) %>%
  data.frame() %>%
  rename(fit = ".") %>%
  bind_cols(new.dat.lm)

# unstandardize x vars


wd <- 0.4
# ggplot(preds.lm) +
#   geom_ribbon(
#     aes(
#       x = Depth,
#       y = fit,
#       ymin = (fit - 2 * se.fit),
#       ymax = (fit + 2 * se.fit)
#     ),
#     alpha = 0.5,
#     color = "transparent"
#   ) +
#   geom_line(aes(x = Depth, y = fit)) +
#   geom_point(data = mod.dat,
#        aes(x = Depth, y = riqueza),
#     alpha = 0.4,
#     position = position_dodge2(width = wd)
#   ) +
#   facet_wrap(. ~ Year_fac) +
#   ylab("Species Richness")


# 4th var: longitude ----
# Split the data by group
groups <- unique(preds$long)

# Base plot
p <- plot_ly()

# Loop through each group
for (g in groups) {
  data_g <- subset(preds, long == g)

  # Triangulate the surface
  triangles <- delaunayn(data_g[, c("tiempo_arrastre2", "Depth")])
  i <- triangles[, 1] - 1
  j <- triangles[, 2] - 1
  k <- triangles[, 3] - 1


  hover_text <- paste0(
    "Longitude: ", g, "<br>",
    "Tiempo: ", data_g$tiempo_arrastre2, "<br>",
    "Depth: ", data_g$Depth, "<br>",
    "Species Richness: ", round(data_g$fit, 2)
  )

  # Add mesh trace (with legend turned off)
  p <- add_trace(
    p,
    type = "mesh3d",
    x = data_g$tiempo_arrastre2,
    y = data_g$Depth,
    z = data_g$fit,
    # i = i, j = j, k = k,
    intensity = data_g$fit,
    text = hover_text,
    hoverinfo = "text",
    opacity = 0.6,
    colors = rev(colorRampPalette(brewer.pal(10, "Spectral"))(41)),
    name = paste("Group", g),
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
  y = ~Depth,
  z = ~riqueza,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    color = ~long_obs,
    colorscale = "Viridis",
    colorbar = list(
      title = "Longitude",
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
    yaxis = list(title = "Depth (m)"),
    zaxis = list(title = "Species Richness")
  )
)

p.fit.4d.long.lm <- p

# 4th var: tiempo de arrastre ----
# Split the data by group
groups <- unique(preds$tiempo_arrastre2)
groups <- c(20, 30, 40)


# Base plot
p <- plot_ly()

# Loop through each group
for (g in groups) {
  data_g <- subset(preds, tiempo_arrastre2 == g)

  # Triangulate the surface
  triangles <- delaunayn(data_g[, c("long", "Depth")])
  i <- triangles[, 1] - 1
  j <- triangles[, 2] - 1
  k <- triangles[, 3] - 1


  hover_text <- paste0(
    "Tiempo: ", g, "<br>",
    "Longitude: ", data_g$long, "<br>",
    "Depth: ", data_g$Depth, "<br>",
    "Species Richness: ", round(data_g$fit, 2)
  )

  # Add mesh trace (with legend turned off)
  p <- add_trace(
    p,
    type = "mesh3d",
    x = data_g$long,
    y = data_g$Depth,
    z = data_g$fit,
    i = i, j = j, k = k,
    intensity = data_g$fit,
    text = hover_text,
    hoverinfo = "text",
    opacity = 0.6,
    colors = rev(colorRampPalette(brewer.pal(10, "Spectral"))(41)),
    name = paste("Group", g),
    showlegend = FALSE, # This hides the mesh from the legend
    flatshading = TRUE,
    showscale = FALSE
  )
}

# Add the shared marker trace with legend
ht <- paste0(
  "Tiempo_obs: ", mod.dat$tiempo_arrastre2, "<br>",
  "Longitude_obs: ", mod.dat$long, "<br>",
  "Depth_obs: ", mod.dat$Depth, "<br>",
  "Species Richness_obs: ", round(mod.dat$riqueza, 2)
)
p <- add_trace(
  p,
  data = mod.dat,
  x = ~long,
  y = ~Depth,
  z = ~riqueza,
  type = "scatter3d",
  mode = "markers",
  marker = list(
    color = ~time_obs,
    colorscale = "Viridis",
    colorbar = list(
      title = "Tiempo",
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
    xaxis = list(title = "Longitude"),
    yaxis = list(title = "Depth (m)"),
    zaxis = list(title = "Species Richness")
  )
)

p.fit.4d.time.lm <- p

