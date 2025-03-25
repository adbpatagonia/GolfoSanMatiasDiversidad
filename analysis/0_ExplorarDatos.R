# ADB
# 2025-03-14
# analisis para analizar la riqueza en el golfo san matias como funcion del ano de pesca y del esfuerzo pesquero
# datos provistos por el Dr Guillermo Svendsen (CONICET - Centro Almirante Storni),
# como parte de la tesis de Josefina Cuesta Nunez (Universidad del Comahue, CONICET - Centro Almirante Storni)

# this script reads the data and carries out data exploration

# libraries -----
library(data.table)
library(tidyverse)
library(janitor)
library(GGally)
library(mgcv)
library(sf)
library(raster)
library(tidyterra)
library(terra)
library(rnaturalearth)
library(gratia)

ggplot2::theme_set(theme_bw())

# functions -----
source(paste0(here::here(), "/R/standardize_x.r"))
source(paste0(here::here(), "/R/HighstatLibV7.R"))
source(paste0(here::here(), "/R/lat-lon-labels.R"))


# data -----
fish.dat <- fread(paste0(here::here(), "/data-raw/datos_modelos_corregido.csv"))



# wrangle data ----
## drop first colmn ----
fish.dat[,V1 := NULL]
## column names ----
fish.dat <- janitor::clean_names(fish.dat)

fish.dat <- fish.dat %>%
  rename(Year = ano,
         Depth = prof)

# explore data -----
ggplot(fish.dat[Year < 2022]) +
  geom_histogram(aes(x = tiempo_arrastre2))

ggplot(fish.dat[Year < 2022]) +
  geom_histogram(aes(x = area_barrida ))

ggplot(fish.dat[Year == 2022]) +
  geom_histogram(aes(x = area_barrida ))


ggplot(fish.dat) +
  geom_histogram(aes(x = Depth))

ggplot(fish.dat) +
  geom_histogram(aes(x = riqueza))

ggplot(fish.dat) +
  geom_histogram(aes(x = tiempo_arrastre2))

ggplot(fish.dat) +
  geom_point(aes(y = riqueza, x = tiempo_arrastre2))

ggplot(fish.dat) +
  geom_point(aes(y = riqueza, x = area_barrida ))

ggplot(fish.dat) +
  geom_point(aes(y = riqueza, x = area_barrida )) +
  geom_smooth(aes(y = riqueza, x = area_barrida)) +
  facet_wrap(.~Year)

ggplot(fish.dat) +
  geom_point(aes(y = riqueza, x = tiempo_arrastre2 )) +
  geom_smooth(aes(y = riqueza, x = tiempo_arrastre2)) +
  facet_wrap(.~Year)


ggplot(fish.dat) +
  geom_point(aes(y = riqueza, x = Depth))

ggplot(fish.dat) +
  geom_point(aes(y = riqueza, x = long))


## assess correlation among explanatory vars ----

mod.dat <- fish.dat[,.(riqueza, Year, area_barrida, tiempo_arrastre2 , Depth, long)]

#correlation between:
# 1. area_barrida y tiempo de arrastre (r = 0.692). Podemos usar solo una - desde un punto de vista logico, creo que area_barrida debe ser mejor variable para explicar la cantidad de spp
# 2. Depthundidad y longitud (r = 0.422). Podemos usar solo una - desde un punto de vista biologico, creo que Depthundidad debe ser mejor variable para explicar la cantidad de spp
# 3. Año y area barrida: esto esta dado porque en 2022 se muestreo de manera diferente....
# 4. Año y tiempo de arrastre: esto esta dado porque en 2022 se muestreo de manera diferente....
p.corr.mod.dat <-
ggpairs(mod.dat[,.( Year, area_barrida, tiempo_arrastre2 , Depth, long)])
p.corr.mod.rest <-
ggpairs(mod.dat[,.( Year, area_barrida, Depth)])

# multicollinearity
corvif(mod.dat[,.( Year, area_barrida, tiempo_arrastre2 , Depth, long)])
multico <- corvif(mod.dat[,.( Year, area_barrida,  Depth)])


# map ----
# plotting bathymetry - took code from
# https://heima.hafro.is/~einarhj/groftp/maps.html


bg  <-  ne_countries(scale = 10,
                     country = "Argentina",
                     type = "countries",
                     continent = NULL,
                     returnclass = "sf")


bathy <- rast(paste0(here::here(), "/data/bathy_DEM_90.img"))
bb <- st_bbox(c(xmin = -66, xmax = -63,
                ymin = -43, ymax = -40))

z <- raster::crop(bathy, bb)

# values above sealevel set to NA
i <- values(z) > 5
values(z)[i] <- NA

bathy <- as.data.frame(z, xy = TRUE) %>%
  gather(-x, -y, key = "var", value = "value", factor_key = TRUE)

p.map <-
  ggplot() +
  # theme_void() +
  # geom_spatraster(data = hill, show.legend = FALSE) +
  # Note the scale, grey colours
  # scale_fill_gradientn(colours = grey(0:100 / 100), na.value = NA) +
  # ggnewscale::new_scale_fill() +
  # geom_spatraster(data = z, alpha = 0.5, show.legend = TRUE) +
  # geom_spatraster_contour(data = z, colour = "gray", linewidth = 0.4,
  #                         linetype = 2,
  #                         breaks = seq(0, -200, -10)) +
  geom_raster(data = bathy, aes(x = x, y = y, fill = value),
              interpolate = TRUE) +
  geom_contour(data = bathy, aes(x = x, y = y, z = value),
               breaks = seq(1, -200, -10),
               linetype = 3, colour = "gray50") +
  geom_sf(data = bg, fill = "gray70") +
  coord_sf(xlim = c(-65.5,-63.8),
           ylim = c(-42.3, -40.5),
           expand = TRUE,
           crs = st_crs(bg)) +
  geom_point(data = fish.dat, aes(x = long, y = lat, color = as.factor(Year)),
             alpha = 0.8) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE, title = ""))


# output -----

# ggsave(filename = "output/map.png",
#        plot = p.map, height = 8, width = 10)
