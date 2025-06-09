library(patchwork)
library(gratia)
library(dplyr)
library(purrr)

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

m <- m.1.y


sm_plt <- draw(m, residuals = TRUE)
sm_plt

# First derivative -----
# The 1st derivative at a specified value of the
# covariate is the GAM equivalent of the
# β in a (G)LM

# If you want to estimate the the nth derivative, need to have a
# n+1 derivative penalty
fd <- derivatives(m, type = "central", unconditional = TRUE)
fd

fd_plt <- draw(fd)
sm_plt[[1]] + fd_plt[[1]] + plot_layout(ncol =  2 )


# Second derivative ----
m2 <- gam(
  riqueza ~
    s(Depth, bs = "tp", m = 3) + # m = 3 because we want to get the 2nd derivative
    s(long, bs = "tp", m = 3) +
    s(Year, bs = "cr", k = 6, m = 3) +
    s(tiempo_arrastre2, bs = "tp", m = 3) ,
  # s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") ,
  # s(lance, k = length(levels(mod.dat$lance)), bs = "re"),
  method = "ML",
  data = mod.dat
)


fd2 <- derivatives(m2, order = 2, eps = 0.1,
                   type = "central",
                   unconditional = TRUE)
fd2_plt <- draw(fd2)
sm_plt[[1]] +
  (fd_plt[[1]]  + ylab("First derivative")) +
  (fd2_plt[[1]] + ylab("Second derivative")) +
  plot_layout(ncol = 3)
sm_plt[[2]] +
  (fd_plt[[2]]  + ylab("First derivative")) +
  (fd2_plt[[2]] + ylab("Second derivative")) +
  plot_layout(ncol = 3)
sm_plt[[3]] +
  (fd_plt[[3]]  + ylab("First derivative")) +
  (fd2_plt[[3]] + ylab("Second derivative")) +
  plot_layout(ncol = 3)
sm_plt[[4]] +
  (fd_plt[[4]]  + ylab("First derivative")) +
  (fd2_plt[[4]] + ylab("Second derivative")) +
  plot_layout(ncol = 3)


# Change indicators -----
# Significant change is where the simultaneous interval on derivative excludes 0
## Depth ----
fds <- derivatives(m, select = "s(Depth)",
                   type = "central", unconditional = TRUE,
                   interval = "simultaneous")

m |>                                               # take the model, and
  smooth_estimates(select = "s(Depth)") |>         # evaluate the smooth
  add_sizer(derivatives = fds, type = "change") |> # add change indicator
  draw()                                           # plot

m |>                                               # take the model, and
  smooth_estimates(select = "s(Depth)") |>         # evaluate the smooth
  add_sizer(derivatives = fds, type = "sizer") |> # add change indicator
  draw()                                           # plot

m |>                                               # take the model, and
  smooth_estimates(select = "s(Depth)") |>         # evaluate the smooth
  add_sizer(derivatives = fds, type = "sizer") %>%
  filter(!is.na(.decrease))

draw(fds)

## tiempo arrastre ----
fds <- derivatives(m, select = "s(tiempo_arrastre2)",
                   type = "central", unconditional = TRUE,
                   interval = "simultaneous")

m |>                                               # take the model, and
  smooth_estimates(select = "s(tiempo_arrastre2)") |>         # evaluate the smooth
  add_sizer(derivatives = fds, type = "change") |> # add change indicator
  draw()                                           # plot


draw(fds)

## Year ----
fds <- derivatives(m, select = "s(Year)",
                   type = "central", unconditional = TRUE,
                   interval = "simultaneous")

m |>                                               # take the model, and
  smooth_estimates(select = "s(Year)") |>         # evaluate the smooth
  add_sizer(derivatives = fds, type = "change") |> # add change indicator
  draw()                                           # plot


draw(fds)

## long ----
fds <- derivatives(m, select = "s(long)",
                   type = "central", unconditional = TRUE,
                   interval = "simultaneous")

m |>                                               # take the model, and
  smooth_estimates(select = "s(long)") |>         # evaluate the smooth
  add_sizer(derivatives = fds, type = "change") |> # add change indicator
  draw()                                           # plot


draw(fds)

# Best model -----
## remove random term ----
# its derivative is alwayz zero
# Get all smooths and filter out random effects
smooth_info <- gratia::smooths(m.3)
valid_terms <- smooth_info[!grepl(pattern = "Year", x = smooth_info)]

## derivatives for univariate smooths -----
# Split out interaction term
uni_terms <- valid_terms[!grepl("^ti\\(", valid_terms)]
interaction_terms <- valid_terms[grepl("^ti\\(", valid_terms)]

# Compute derivatives for univariate smooths
first_uni <- map_dfr(uni_terms, ~
                       derivatives(m.3, term = .x,
                                   order = 1,
                                   interval = "confidence") %>%
                       mutate(term = .x)
)

second_uni <- map_dfr(uni_terms, ~
                        derivatives(m.3, term = .x,
                                    order = 2,
                                    interval = "confidence") %>%
                        mutate(term = .x)
)

## derivatives for interaction smooth -----
# === Function: Numerical derivatives for tensor smooth ===
numerical_smooth_derivatives <- function(model,
                                         var1, var2,
                                         data,
                                         fixed_vars = list(),
                                         grid_length = 100) {
  # Create grid as before
  grid <- expand.grid(
    seq(min(data[[var1]], na.rm = TRUE), max(data[[var1]], na.rm = TRUE), length.out = grid_length),
    seq(min(data[[var2]], na.rm = TRUE), max(data[[var2]], na.rm = TRUE), length.out = grid_length)
  )
  names(grid) <- c(var1, var2)

  if(length(fixed_vars) > 0) {
    for (v in names(fixed_vars)) {
      grid[[v]] <- fixed_vars[[v]]
    }
  }

  smooth_idx <- which(sapply(model$smooth, function(s) grepl(var1, s$label) & grepl(var2, s$label)))
  if(length(smooth_idx) == 0) stop("Tensor smooth not found in model.")
  smooth_info <- model$smooth[[smooth_idx]]
  cols <- smooth_info$first.para:smooth_info$last.para

  Xp <- predict(model, newdata = grid, type = "lpmatrix")
  Xp_ti <- Xp[, cols, drop = FALSE]
  beta <- coef(model)[cols]
  smooth_vals <- as.vector(Xp_ti %*% beta)

  grid_var1 <- sort(unique(grid[[var1]]))
  grid_var2 <- sort(unique(grid[[var2]]))
  smooth_mat <- matrix(smooth_vals, nrow = length(grid_var1), ncol = length(grid_var2), byrow = FALSE)

  d_var1 <- diff(grid_var1)[1]
  d_var2 <- diff(grid_var2)[1]

  finite_diff <- function(y, dx) {
    n <- length(y)
    dy <- numeric(n)
    dy[1] <- (y[2] - y[1]) / dx
    dy[n] <- (y[n] - y[n-1]) / dx
    for(i in 2:(n-1)) {
      dy[i] <- (y[i+1] - y[i-1]) / (2*dx)
    }
    dy
  }

  # Compute derivatives
  d1_var1 <- t(apply(smooth_mat, 2, finite_diff, dx = d_var1))
  d1_var2 <- t(apply(t(smooth_mat), 2, finite_diff, dx = d_var2))

  d2_var1 <- t(apply(d1_var1, 2, finite_diff, dx = d_var1))
  d2_var2 <- t(apply(t(d1_var2), 2, finite_diff, dx = d_var2))

  results <- expand.grid(grid_var1, grid_var2)
  names(results) <- c(var1, var2)
  results$smooth <- as.vector(smooth_mat)
  results[[paste0("d1_", var1)]] <- as.vector(d1_var1)
  results[[paste0("d1_", var2)]] <- as.vector(d1_var2)
  results[[paste0("d2_", var1)]] <- as.vector(d2_var1)
  results[[paste0("d2_", var2)]] <- as.vector(d2_var2)

  return(results)
}


# === Example usage ===

# Define fixed variables for other covariates in your model
fixed_vars <- list(
  Depth = median(mod.dat$Depth, na.rm = TRUE),
  Year_fac = factor(levels(mod.dat$Year_fac)[1], levels = levels(mod.dat$Year_fac))
)

# Run function to get derivatives for tensor smooth of tiempo_arrastre2 and long
deriv_results <- numerical_smooth_derivatives(
  model = m.3,
  var1 = "tiempo_arrastre2",
  var2 = "long",
  data = mod.dat,
  fixed_vars = fixed_vars,
  grid_length = 100
)

# === Plot: First derivative wrt tiempo_arrastre2 ===

ggplot(deriv_results, aes(x = tiempo_arrastre2, y = long, fill = d1_tiempo_arrastre2)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c(option = "plasma") +
  labs(
    title = "Numerical first derivative of tensor smooth wrt tiempo_arrastre2",
    fill = "d/d tiempo_arrastre2"
  ) +
  theme_minimal()
