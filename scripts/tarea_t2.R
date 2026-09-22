
# Constanza Pinilla
# Septiembre 2026
# calcula en qué sector económico las personas mayores de 30 años tienen el mayor 
# ingreso promedio, sacando promedios y agregando nuevas columnas que nos ayudarán
# a este calculo.

# 1 responde a la pregunta: 
# ¿En qué sector económico las personas mayores de 30 años tienen el mayor 
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

str(casen) 
str(ingresos)

names(casen)    # "region" "sector" "educ" "edad" "ingreso" "genero" 

names(ingresos) #"region" "sector" "genero" "educ" "edad" "horas" "ing_trabajo" 
# "ing_capital"  "ing_subsidios" "ing_total"    

sum(is.na(casen$ingreso)) # hay 5 NA

# 2b Columnas por patrón
names(select(ingresos, starts_with("ing"))) # 4  
names(select(ingresos, where(is.numeric)))  # 7
# no dan lo mismo porque en la segunda se busca cuando solo son las columnas
# númericas y en el primero solo las que empiecen con la palabra ing que hay 4.

# 3a
casen |> 
  filter(edad > 30 & !is.na(ingreso)) |> 
  select(ingreso, sector, edad, educ) |> 
  mutate(
    experiencia = pmax(edad - educ - 6, 0)) |> 
  summarise(
    n   = n(), 
    ingreso_promedio = mean(ingreso, na.rm = TRUE)) |> 
    arrange(desc(ingreso_promedio)) 

# 3b
casen <- casen |>
  mutate( casen,
          experiencia = pmax(edad - educ - 6, 0),
          grupo_etario = case_when(
          edad  < 30  ~ "Menores a 30 años",
          edad == 30  ~ "Igual a 30 años",
          TRUE        ~ "Mayores a 30 años"
))

table(casen$grupo_etario, useNA = "ifany")

# 4a
casen_sector <- casen |>  # tabla de 6x3
  group_by(sector) |> 
  summarise(
  n = n(),
  sector_ingreso = mean(ingreso, na.rm = T)
  )

casen_sector_edad <- casen |> #tabla de 54x4
  group_by(sector, edad) |> 
  summarise(
  n = n(),
  sector_edad = mean(ingreso, na.rm = T)
  )

# 4b
casen |>
  group_by(sector) |>
  mutate(experiencia = pmax(edad - educ - 6, 0)) |>
  ungroup()

# cuando uno usa summarise() con group_by (), este agrupa las filas y se hace 
# la operación en esa fila, pero no ocupando la formula y con mutate () 
# la operación va fila por fila.

# 5
mean(casen$ingreso, na.rm = TRUE) # 655290.9
mean(casen$ingreso)               # NA
sum(is.na(casen$ingreso))         # 5 datos NA
# Sin ocupar na.rm = TRUE, el resultado es NA y ocupándolo me da 655290.9, la
# sumatoria de los NA es 5 y en este caso si es aceptable perderlos ya que la 
# cifra de datos perdidos no es muy grande respecto a la base de datos total.

# 6
casen_resultado <- casen |> 
   mutate( 
     experiencia = pmax(edad - educ - 6, 0),
     grupo_etario = case_when(
       edad  < 30  ~ "Menores a 30 años",
       edad == 30  ~ "Igual a 30 años",
       TRUE        ~ "Mayores a 30 años"
     )) |> 
  filter(grupo_etario == "Mayores a 30 años"  & !is.na(ingreso)) |> 
  select(ingreso, sector, edad, educ) |> 
  group_by(sector) |> 
  summarise(
    n   = n(), 
    ingreso_promedio = mean(ingreso, na.rm = TRUE)) |> 
  arrange(desc(ingreso_promedio)) 
  
casen_resultado

# 7 Interpretación

# El sector donde las personas mayores de 30 años presenta un mayor ingreso promedio
# es Educación, con $931250, le sigue Servicios, con $820364, en tercer lugar
# Industria, con $682833 y en el último lugar está Agricultura, con $461727
# Estos resultados muestran que el sector se asocia con el ingreso promedio,
# ya que las personas que trabajan en Educación presentan ingresos promedio más 
# altos que quienes trabajan en Agricultura. Esto podría relacionarse con las 
# diferencias en las condiciones laborales y las remuneraciones propias de cada 
# sector. Sin embargo una limitación de los datos es el pequeño número de personas
# en algunos sectores, como Educación y Construcción, que cuenta con solo 4 observaciones.

# Guardar casen_resultado en la carpeta  
dir.create("data/processed", showWarnings = FALSE)
write.csv(casen_resultado, "data/processed/casen_resultado_t2.csv", row.names = FALSE)

# Revisar si se guardó
file.exists("data/processed/casen_resultado_t2.csv")










