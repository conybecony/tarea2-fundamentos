
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
grupo_etario = case_when(
  edad  < 20  ~ "Menores a 20 años",
  edad == 20  ~ "Igual a 20 años",
  TRUE        ~ "mayores a 20 años"
)

table(casen$grupo_etario, useNA = "ifany")






