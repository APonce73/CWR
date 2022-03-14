library(shiny)
library(shinyjs)
library(shinyWidgets)
#library(grid)
#library(vcd)
library(plyr)
library(tidyverse)
library(readxl)
library(xlsx)
library(leaflet)
library(janitor)

TablaZ <- read_csv("database/_DBfinal_Darwin_ID.csv", col_names = T) %>% 
    clean_names()

TablaZ1 <- TablaZ %>% 
    rename(especie = "binomial") %>% 
    rename(lat = "dec_lat") %>% 
    rename(long = "dec_long") %>% 
    rename(subespecie = "subspecies") %>% 
    mutate(genero = especie) %>% 
    separate(genero, into = c("genero", "especie11"), sep = " ") %>% 
    select("id_darwin", "genero", "especie", "subespecie","lat", "long", "source","year", 
           "compiler") %>% 
    drop_na("lat") %>% 
    drop_na("long") %>% 
    mutate(RatingCol = as.character(genero))
    

#TablaZ2 <- TablaZ1 %>% 
#    select(genero) %>% 
#    mutate(genero = as.factor(genero)) %>% 
#    mutate(val = 1) %>% 
#    group_by(genero) %>% 
#    summarise_all(sum)

Lat_negativa <- TablaZ1 %>% 
    filter(lat < 0)

writexl::write_xlsx(Lat_negativa , path = "database/lat_negativa.xlsx")

Long_positiva <- TablaZ1 %>% 
    filter(long > 0)

writexl::write_xlsx(Long_positiva , path = "database/Long_positiva.xlsx")



TablaZ1 <- TablaZ1 %>% 
    mutate(RatingCol = case_when(RatingCol == "Capsicum" ~ "#41ae76",
                                 RatingCol == "Cucurbita" ~ "#00441b",
                                 RatingCol == "Gossypium" ~ "#0868ac",
                                 RatingCol == "Persea" ~ "#4d004b",
                                 RatingCol == "Phaseolus" ~ "#7f0000",
                                 RatingCol == "Physalis" ~ "#67001f",
                                 RatingCol == "Solanum" ~ "#081d58",
                                 RatingCol == "Tripsacum" ~ "#8c6bb1",
                                 RatingCol == "Vanilla" ~ "#67000d",
                                 RatingCol == "Zea" ~ "#99d8c9",
                                 ))  %>% 
    filter(lat > 0) %>% 
    filter(long < 0)

#summary(TablaZ1)

##TableP <- read_excel("PGM_update2017.xlsx", sheet = "PGM_maices_Alex", col_names = T)
##
##TTabla <- TableP %>%
##  dplyr::filter(!is.na(Raza_primaria)) %>%
##  dplyr::filter(!is.na(Latitud)) %>%
##  dplyr::filter(Estado != "ND") %>%
##  dplyr::filter(Raza_primaria != "ND") %>%
## # dplyr::filter(PeriodoColecta == "Periodo III") %>% 
##  distinct()
##
##TTabla <- droplevels.data.frame(TTabla)
##TTabla$Anhio_Colecta <- base::as.factor(TTabla$Anhio_Colecta)
##
##TTabla <- TTabla %>%
##  dplyr::mutate(Estado = revalue(Estado,c("AGUASCALIENTES" = "Aguascalientes"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("BAJA CALIFORNIA" = "Baja California"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("BAJA CALIFORNIA SUR" = "Baja California Sur"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("CAMPECHE" = "Campeche"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("CHIAPAS" = "Chiapas"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("CHIHUAHUA" = "Chihuahua"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("COAHUILA DE ZARAGOZA" = "Coahuila"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("COLIMA" = "Colima"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("DISTRITO FEDERAL" = "Ciudad de México"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("DURANGO" = "Durango"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("MEXICO" = "Estado de México"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("GUANAJUATO" = "Guanajuato"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("GUERRERO" = "Guerrero"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("HIDALGO" = "Hidalgo"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("JALISCO" = "Jalisco"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("MICHOACAN DE OCAMPO" = "Michoacán"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("MORELOS" = "Morelos"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("NAYARIT" = "Nayarit"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("NUEVO LEON" = "Nuevo León"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("OAXACA" = "Oaxaca"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("PUEBLA" = "Puebla"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("QUERETARO DE ARTEAGA" = "Querétaro"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("QUINTANA ROO" = "Quintana Roo"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("SAN LUIS POTOSI" = "San Luis Potosí"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("SINALOA" = "Sinaloa"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("SONORA" = "Sonora"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("TABASCO" = "Tabasco"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("TAMAULIPAS" = "Tamaulipas"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("TLAXCALA" = "Tlaxcala"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("VERACRUZ DE IGNACIO DE LA LLAVE" = "Veracruz"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("YUCATAN" = "Yucatán"))) %>%
##  dplyr::mutate(Estado = revalue(Estado,c("ZACATECAS" = "Zacatecas"))) %>%
##  dplyr::mutate(Complejo_racial = revalue(Complejo_racial,c("Chapalote" = "Chapalotes"))) %>%
##  dplyr::mutate(Complejo_racial = revalue(Complejo_racial,c("Cónico" = "Cónicos"))) %>%
##  dplyr::mutate(Raza_primaria = revalue(Raza_primaria,c("Nal-Tel de Altura" = "Nal-tel de Altura"))) %>%
##  dplyr::mutate(Raza_primaria = revalue(Raza_primaria,c("Complejo Serrano de Jalisco" = "Serrano de Jalisco")))
##
##names(TTabla)[20] <- c("longitude")
##names(TTabla)[21] <- c("latitude")
##
###Ventana 1 Mapa
##TableL <- TTabla
##
##RatingCol <- as.character(TableL$Raza_primaria)
##
##TableL <- data.frame(TableL, RatingCol)
##TableL <- TableL %>%
##  #Conicos
##  dplyr::mutate(RatingCol = revalue(RatingCol,c("Arrocillo Amarillo" = "#00441b", "Cacahuacintle" = "#006d2c",
##                                                "Chalqueño" = "#238b45", "Cónico" = "#41ae76", "Cónico Norteño" = "#66c2a4",
##                                                "Dulce" = "#99d8c9", "Elotes Cónicos" = "#004529", "Mixteco" = "#006837",
##                                                "Mushito" = "#238443", "Negrito" = "#41ab5d", "Palomero de Chihuahua" = "#78c679",
##                                                "Palomero de Jalisco" = "#addd8e", "Palomero Toluqueño" = "#d9f0a3", "Uruapeño" = "#f7fcb9"))) %>%
##  #Chapalotes
##  dplyr::mutate(RatingCol = revalue(RatingCol,c("Chapalote" = "#0868ac", "Dulcillo del Noroeste" = "#2b8cde",
##                                                "Elotero de Sinaloa" = "#4eb3d3", "Reventador" = "#7bccc4"))) %>%
##  #Tropicales precoces
##  dplyr::mutate(RatingCol = revalue(RatingCol,c("Conejo" = "#4d004b", "Nal-tel" = "#810f7c", "Ratón" = "#88419d",
##                                                "Zapalote Chico" = "#8c6bb1"))) %>%
##  #Ocho hileras
##  dplyr::mutate(RatingCol = revalue(RatingCol,c("Ancho" = "#7f0000", "Blando" = "#b30000", "Bofo" = "#d7301f",
##                                                "Bolita" = "#ef6548", "Elotes Occidentales" = "#fc8d59",
##                                                "Harinoso de Ocho" = "#fdbb84", "Jala" = "#662506", "Onaveño" = "#993404",
##                                                "Tablilla de Ocho" = "#cc4c02", "Tabloncillo" = "#ec7014", "Tabloncillo Perla" = "#fe9929",
##                                                "Zamorano Amarillo" = "#fec44f"))) %>%
##  #Sierra de Chihuahua
##  dplyr::mutate(RatingCol = revalue(RatingCol,c("Apachito" = "#67001f", "Azul" = "#980043", "Serrano de Jalisco" = "#ce1256",
##                                                "Cristalino de Chihuahua" = "#e7298a", "Gordo" = "#df65b0", "Mountain Yellow" = "#c994c7"))) %>%
##  #Maduración tardía
##  dplyr::mutate(RatingCol = revalue(RatingCol,c( "Comiteco" = "#081d58", "Coscomatepec" = "#253494", "Dzit Bacal" = "#225ea8", "Mixeño" = "#1d91c0",
##                                                 "Motozinteco" = "#41b6c4",  "Negro de Chimaltenango" = "#7fcdbb", "Olotillo" = "#2171b5",
##                                                 "Olotón" = "#4292c6", "Quicheño" = "#6baed6", "Serrano" = "#9ecae1",
##                                                 "Serrano Mixe" = "#c6dbef", "Tehua" = "#deebf7"))) %>%
##  #Dentados Tropicales
##  dplyr::mutate(RatingCol = revalue(RatingCol,c("Celaya" = "#67000d", "Chiquito" = "#a50f15", "Choapaneco" = "#cb181d",
##                                                "Cubano Amarillo" = "#ef3b2c", "Nal-tel de Altura" = "#fb6a4a",
##                                                "Pepitilla" = "#fc9272", "Tepecintle" = "#fcbba1", "Tuxpeño" = "#fed976",
##                                                "Tuxpeño Norteño" = "#bd0026", "Vandeño" = "#e31a1c", 
##                                                "Zapalote Grande" = "#fc4e2a"))) %>% 
##  dplyr::filter(AltitudProfundidad < 5000)




#Cargar los datos de Teocintle
##Teocintle <- read.csv("Teocintle.csv", header = T, sep = ",")
##
##Teocintle <- Teocintle %>%
##  filter(!is.na(Latitud)) %>%
##  distinct()
##
##names(Teocintle)[15] <- c("longitude")
##names(Teocintle)[16] <- c("latitude")
##names(Teocintle)[14] <- c("Altitud")
##
###Cargar los datos de Teocintle
##Tripsacum <- read.delim("Tripsacum.csv", header = T, sep = ",")
##Tripsacum <- Tripsacum %>%
##  dplyr::filter(!is.na(Latitud)) %>%
##  distinct()
##names(Tripsacum)[12] <- c("longitude")
##names(Tripsacum)[13] <- c("latitude")
##Tripsacum <- data.frame(Tripsacum, Tipo = "Tripsacum")
##Teocintle <- data.frame(Teocintle, Tipo = "Teocintle")
##Tripsacum <- Tripsacum %>%
##  select(Tipo, longitude, latitude, Fuente, Taxa, Estado, Municipio)
##Teocintle <- Teocintle %>%
##  select(Tipo, longitude, latitude, Fuente, Taxa, Estado, Municipio)
##Parientes <- rbind(Tripsacum, Teocintle)
##
##
##maize_icon <- makeIcon(iconUrl = "icons8-corn-50.png",
##                       iconWidth = 5,
##                       iconHeight = 5)
##
