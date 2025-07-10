 source(paste0(here::here(), "/analysis/1_Modelado.R"))

## fit full model -----
# include lance as a random effect
# include depth, longitude (distance from the mouth of the Gulf, and towing time)
m.1.lance <- gam(
  riqueza ~
    s(Depth) +
    s(tiempo_arrastre2) +
    s(long) +
    s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    s(Year, bs = "cr", k = 6) ,
  # ti(tiempo_arrastre2, long),
  method = "ML",
  data = mod.dat
)

### model check -----
mod <- m.1.lance
k.check(mod)
appraise(mod)
draw(mod)

summary(mod)

## add triple interaction -----
m.2.lance <- gam(
  riqueza ~
    s(Depth, bs = "tp") +
    s(long, bs = "tp") +
    s(tiempo_arrastre2, bs = "tp") +
    s(Year, bs = "cr", k = 6)  +
    s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    ti(Depth, long, tiempo_arrastre2),
  method = "ML",
  data = mod.dat
)

### model check -----
mod <- m.2.lance
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.2")

summary(mod)

## add double interaction long -----
m.3.lance <- gam(
  riqueza ~
    s(Depth) +
    s(tiempo_arrastre2) +
    s(long) +
    s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    s(Year, bs = "cr", k = 6)  +
    ti(tiempo_arrastre2, long),
  method = "ML",
  data = mod.dat
)

### model check -----
mod <- m.3.lance
k.check(mod)
appraise(mod)
draw(mod, residuals = TRUE) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.3")

summary(mod)


## add double interaction depth -----
m.4.lance <- gam(
  riqueza ~
    s(Depth) +
    s(tiempo_arrastre2) +
    s(long) +
    s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    s(Year, bs = "cr", k = 6)  +
    ti(tiempo_arrastre2, Depth),
  method = "ML",
  data = mod.dat
)

### model check -----
mod <- m.4.lance
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.4")

summary(mod)


## Model selection -----
# most parsimonious model is model with no interaction
ms.lance <- AIC(m.1.lance, m.2.lance, m.3.lance,   m.4.lance) %>%
  data.table(keep.rownames = TRUE) %>%
  mutate(LL = c(
    logLik(m.1.lance), logLik(m.2.lance), logLik(m.3.lance), logLik(m.4.lance)
  )) %>%
  mutate(dev = c(
    deviance(m.1.lance), deviance(m.2.lance), deviance(m.3.lance),  deviance(m.4.lance)
  )) %>%
  mutate(deltaAIC = AIC - min(AIC))
# Round numeric columns to 2 decimal places
ms.lance$wi <- exp(-0.5 * ms.lance$deltaAIC) / sum(exp(-0.5 * ms.lance$deltaAIC))
ms.lance$er <- max(ms.lance$wi) / ms.lance$wi
ms.lance$rsq <- c(
  summary(m.1.lance)$r.sq,
  summary(m.2.lance)$r.sq,
  summary(m.3.lance)$r.sq,
  # summary(m.3)$r.sq,
  # summary(m.3.ny)$r.sq,
  summary(m.4.lance)$r.sq
)
ms.lance <- ms.lance[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)]
ms.lance$model <- c(
  "Main effects",
  "Main effects + triple interaction",
  "Main effects + double interaction (longitud, tiempo)",
  # "Main effects + double interaction (longitud, tiempo) - random year",
  # "Main effects + double interaction (longitud, tiempo) - remove year",
  "Main effects + double interaction (depth, tiempo)"
)

ms.lance <- ms.lance[order(AIC), .(model, df, LL, dev, rsq, deltaAIC, er)]



kable(ms.lance) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover"),
    position = "center", full_width = FALSE
  )
