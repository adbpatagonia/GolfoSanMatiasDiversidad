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
library(gratia)

ggplot2::theme_set(theme_bw())

# functions -----
source("R/standardize_x.r")
source("R/HighstatLibV7.R")

# data -----
fish.dat <- fread("data-raw/datos_modelos_jcn.csv")


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
ggpairs(mod.dat[,.( Year, area_barrida, tiempo_arrastre2 , Depth, long)])
ggpairs(mod.dat[,.( Year, area_barrida, Depth)])

# multicollinearity
corvif(mod.dat[,.( Year, area_barrida, tiempo_arrastre2 , Depth, long)])
corvif(mod.dat[,.( Year, area_barrida,  Depth)])


