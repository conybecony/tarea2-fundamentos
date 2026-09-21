
# Constanza Pinilla

# Septiembre 2026

# 1 responde a la pregunta: 
# ¿En qué sector económico las personas mayores de 20 años tienen el mayor 
# ingreso promedio?

# cargar paquetes
library(dplyr)
library(tidyverse)

# 2 Cargar datos
casen <- read.csv("data/raw/casen_reducido.csv") 
ingresos <- read.csv("data/raw/casen_ingresos.csv") 

# Dimensiones, tipos y cuántos `NA` hay en `ingreso`

dim(casen)      # 60  6

dim(ingresos)   # 60 10

names(casen)    # "region" "sector" "educ" "edad" "ingreso" "genero" 

names(ingresos) #"region" "sector" "genero" "educ" "edad" "horas" "ing_trabajo" 
# "ing_capital"  "ing_subsidios" "ing_total"    

sum(is.na(casen$ingreso)) # 5 NA

# 2b Columnas por patrón
names(select(ingresos, starts_with("ing"))) # 4  
names(select(ingresos, where(is.numeric)))  # 7
# no dan lo mismo porque en la segunda se busca cuando solo son las columnas
# númericas y en el primero solo las que empiecen con la palabra ing que hay 4.

# 3
casen_resultado <- casen |> 
  filter(edad > 20 & !is.na(ingreso)) |> 
  select(ingreso, sector, edad, educ) |> 
  mutate(
    experiencia = pmax(edad - educ - 6, 0)) |> 
   group_by(sector) |> 
   summarise(
    n   = n(), 
    ingreso_promedio = mean(ingreso, na.rm = TRUE)) |> 
    arrange(desc(ingreso_promedio)) 

# 3b
casen <- casen |>
  mutate( casen,
          experiencia = pmax(edad - educ - 6, 0),
          grupo_etario = case_when(
          edad  < 20  ~ "Menores a 20 años",
          edad == 20  ~ "Igual a 20 años",
          TRUE        ~ "mayores a 20 años"
))

table(casen$grupo_etario, useNA = "ifany")

# 4a
casen_sector <- casen |> 
  group_by(sector) |> 
  summarise(
  n = n(),
  sector_ingreso = mean(ingreso, na.rm = T)
  )

casen_sector_edad <- casen |> 
  group_by(sector, edad) |> 
  summarise(
  n = n(),
  sector_edad = mean(ingreso, na.rm = T)
  )

# 4b
casen |> 
  group_by(sector) |> 
  mutate()

# 5
mean(casen$ingreso, na.rm = TRUE) # 655290.9
mean(casen$ingreso)               # NA
sum(is.na(casen$ingreso))         # 5 datos NA
# Sin ocupar na.rm = TRUE, el resultado es NA y ocupándolo me da 655290.9, la
# sumatoria de los NA es 5 y en este caso si es aceptable perderlos ya que la 
# cifra de datos perdidos no es muy grande respecto a la base de datos total.

# 6
casen_resultado <- casen |> 
  filter(grupo_etario == "mayores a 20 años"  & !is.na(ingreso)) |> 
  select(ingreso, sector, edad, educ) |> 
  mutate(
    experiencia = pmax(edad - educ - 6, 0)) |> 
  group_by(sector) |> 
  summarise(
    n   = n(), 
    ingreso_promedio = mean(ingreso, na.rm = TRUE)) |> 
  arrange(desc(ingreso_promedio)) 
  
# 7 Interpretación

# El sector donde las personas mayores de 20 años presenta un mayor ingreso promedio
# es Educación, con $908857, le sigue Servicios, con $806786, en tercer lugar
# Industria, con $697889 y en el último lugar está Agricultura, con $446250.
# Estos resultados muestran que el sector se asocia con el ingreso promedio,
# ya que las personas que trabajan en Educación presentan ingresos promedio más 
# altos que quienes trabajan en Agricultura. Esto podría relacionarse con las 
# diferencias en las condiciones laborales y las remuneraciones propias de cada 
# sector.
# Sin embargo una limitación de los datos es el pequeño número de de personas
# en algunos sectores, como Construcción, que cuenta con solo 5 observaciones.














