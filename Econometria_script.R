#leamos la base de datos

library(readxl)
library(dplyr)

data <- read.csv("Video Games Sales.csv",sep = ",")

View(data)
# nrow(data) #numero de datos 

#filtremos los datos NA y eligamos 4 tipos de consolas y generos
tipos_consola <- c("PS3", "PS4", "X360","Wii")

tipos_genero <- c("Shooter", "Sports", "Action","Misc")

#construyamos la data

data_new <- data |>
  filter(Platform %in% tipos_consola & Genre %in% tipos_genero)
 

#View(data_new)

######____ HACER 
# intervalos de confianza para los betas TODOS
# son significativas o no las betas cada uno
#diferencias significativas entre los betas ??
# prueba ML (prueba conjunta) no robusto 





