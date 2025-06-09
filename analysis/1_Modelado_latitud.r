source(paste0(here::here(), "/analysis/1_Modelado.R"))

mod.dat <- fish.dat[, .(riqueza, lance, Year, area_barrida, tiempo_arrastre2, Depth, long, lat)]
mod.dat$Year_fac <- as.factor(mod.dat$Year)

## add double interaction long -----
m.3.latitud <- gam(
  riqueza ~
    s(Depth) +
    s(tiempo_arrastre2) +
    s(long) +
    s(lat) +
    # s(lance, k = length(levels(mod.dat$lance)), bs = "re") +
    s(Year_fac, k = length(levels(mod.dat$Year_fac)), bs = "re") +
    ti(tiempo_arrastre2, long),
  method = "ML",
  data = mod.dat
)


### model check -----
mod <- m.3.latitud
k.check(mod)
appraise(mod)
draw(mod) +
  theme_minimal() +
  scale_fill_viridis_c() +
  labs(title = "Partial Effects in m.3")

summary(mod)
