############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################# SCRIPT GENERAL ###############################################
############################################################################################################

#### PAQUETES A UTILIZAR ####

library(tidyr)      # Versión 0.8.1
library(dplyr)      # Versión 0.8.0.1
library(readr)      # Versión 1.1.1
library(stringr)    # Versión 1.3.1
library(purrr)      # Versión 0.2.4
library(magrittr)   # Versión 1.5

#### DATOS GENERALES ####

print.noquote("###############################################################")
print.noquote("###################### Cargando datos #########################")
print.noquote("###############################################################")

# Familias políticas
print.noquote("#################### Familias Políticas #######################")
familias_politicas <- c("FN", "Derecha", "Izquierda", "Centro",
                        "Otras derechas", "Otras izquierdas", "Otros")
familias_politicas_todas <- read_csv("DATOS/GENERALES/FAMILIAS_POLITICAS_TODAS.csv", 
                                     col_types = c("cccc"),locale = locale(encoding = "latin1"))

# Paleta de colores
print.noquote("##################### Paleta de colores #######################")
paleta_tesis_fn <- read_csv("DATOS/GENERALES/PALETA_TESIS_FN.csv",col_types = "cc")

# Departamentos
DEPARTAMENTOS <- read_tsv(file = "DATOS/GENERALES/depts2007.txt",
                    locale = locale(encoding = "latin1")) %>%
  transmute(COD_REG = REGION,COD_DPTO = DEP, NOM_DPTO = NCCENR)

# Regiones
REGIONES <- read_delim(file = "DATOS/GENERALES/reg2007.txt",
                            delim = "\t",locale = locale(encoding = "latin1")) %>%
  transmute(COD_REG = REGION, NOM_REG = NCCENR) 

orden_regiones <- c(31,22,42,41,21,11,23,25,53,52,24,26,43,82,83,74,54,72,73,91,93,94) %>% as.character()
orden_departamentos <- DEPARTAMENTOS %>% 
  mutate(COD_REG = factor(COD_REG,levels = orden_regiones,ordered = T)) %>% 
  arrange(COD_REG) %>% 
  extract2("COD_DPTO")

# Auxiliares grandes ciudades
aux_paris <- tibble(CODGEO = paste("751",str_pad(1:20,2,pad = 0),sep = ""), 
                    NOM_COMUNA = paste("Paris", 1:20),
                    COD_DPTO = "75",
                    COD_REG = "11",
                    NOM_DPTO = "Paris",
                    NOM_REG = "Île-de-France")

aux_lyon <- tibble(CODGEO = paste("6938",1:9,sep = ""), 
                   NOM_COMUNA = paste("Lyon", 1:9),
                   COD_DPTO = "69",
                   COD_REG = "82",
                   NOM_DPTO = "Rhône",
                   NOM_REG = "Rhône-Alpes")

aux_marsella <- tibble(CODGEO = paste("132",str_pad(1:16,2,pad = 0),sep = ""), 
                       NOM_COMUNA = paste("Marseille", 1:16),
                       COD_DPTO = "13",
                       COD_REG = "93",
                       NOM_DPTO = "Bouches-du-Rhône",
                       NOM_REG = "Provence-Alpes-Côte d'Azur")

# Comunas
COMUNAS_2007 <- read_tsv(file = "DATOS/GENERALES/comsimp2007.txt", 
                         locale = locale(encoding = "latin1")) %>% 
  transmute(CODGEO = paste(DEP,COM,sep=""), NOM_COMUNA = NCCENR, ART = ARTMIN) %>% 
  # Añadimos el artículo al nombre
  mutate(NOM_COMUNA = if_else(is.na(ART),
                              NOM_COMUNA,
                              paste(str_remove_all(ART,"\\(|\\)"),NOM_COMUNA,sep=" "))) %>% 
  # Corregimos los espacios después del apóstrofe y las ligaduras (OE,oe)
  # Sugerencia de https://github.com/tidyverse/stringr/issues/295#issuecomment-485763694
  transmute(CODGEO,
            NOM_COMUNA = str_replace_all(NOM_COMUNA,
                                         set_names(c("'","OE","oe"),
                                                   c("' ","\u008c","(\u009c)")))) %>% 
  # Pegamos datos administrativos superiores y grandes ciudades
  mutate(COD_DPTO = str_sub(CODGEO,1,2)) %>% 
  left_join(DEPARTAMENTOS, by = c("COD_DPTO")) %>% 
  left_join(REGIONES, by = c("COD_REG")) %>% 
  bind_rows(aux_paris) %>% 
  bind_rows(aux_lyon) %>% 
  bind_rows(aux_marsella)

COMUNAS_2012 <- read_tsv(file = "DATOS/GENERALES/comsimp2012.txt", 
                         locale = locale(encoding = "latin1")) %>% 
  transmute(CODGEO = paste(DEP,COM,sep=""), NOM_COMUNA = NCCENR, ART = ARTMIN) %>% 
  # Añadimos el artículo al nombre
  mutate(NOM_COMUNA = if_else(is.na(ART),
                              NOM_COMUNA,
                              paste(str_remove_all(ART,"\\(|\\)"),NOM_COMUNA,sep=" "))) %>% 
  # Corregimos los espacios después del apóstrofe y las ligaduras (OE,oe)
  # Sugerencia de https://github.com/tidyverse/stringr/issues/295#issuecomment-485763694
  transmute(CODGEO,
            NOM_COMUNA = str_replace_all(NOM_COMUNA,
                                         set_names(c("'","OE","oe"),
                                                   c("' ","\u008c","(\u009c)")))) %>% 
  # Pegamos datos administrativos superiores y grandes ciudades
  mutate(COD_DPTO = str_sub(CODGEO,1,2)) %>% 
  left_join(DEPARTAMENTOS, by = c("COD_DPTO")) %>% 
  left_join(REGIONES, by = c("COD_REG")) %>% 
  bind_rows(aux_paris) %>% 
  bind_rows(aux_lyon) %>% 
  bind_rows(aux_marsella)

# Eliminamos auxiliares de las grandes ciudades
remove(aux_paris,aux_lyon,aux_marsella)

#### PRELIMPIEZA DE DATOS ####

print.noquote("###############################################################")
print.noquote("################## Prelimpieza de datos #######################")
print.noquote("###############################################################")

# Prelimpieza de datos de delitos
source("LIMPIEZA_DATOS/00_PRELIMPIEZA_DATOS_DELITOS.R")

#### LIMPIEZA DE DATOS ####

print.noquote("###############################################################")
print.noquote("#################### Limpieza de datos ########################")
print.noquote("###############################################################")

# Limpieza de datos electorales
source("LIMPIEZA_DATOS/01_LIMPIEZA_DATOS_ELECTORALES.R")
# Limpieza de datos censales
source("LIMPIEZA_DATOS/02_LIMPIEZA_DATOS_CENSALES.R")
# Limpieza de otros datos comunales
source("LIMPIEZA_DATOS/03_LIMPIEZA_OTROS_DATOS_COMUNALES.R")
# Limpieza de datos a nivel departamentos
source("LIMPIEZA_DATOS/04_LIMPIEZA_DATOS_NIVEL_DEPARTAMENTOS.R")
