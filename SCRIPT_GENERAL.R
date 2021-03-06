############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################# SCRIPT GENERAL ###############################################
############################################################################################################

#### PAQUETES A UTILIZAR ####

library(tidyr)      # Versi�n 0.8.3
library(dplyr)      # Versi�n 0.8.3
library(readr)      # Versi�n 1.3.1
library(stringr)    # Versi�n 1.4.0
library(purrr)      # Versi�n 0.3.2
library(magrittr)   # Versi�n 1.5
library(ggplot2)    # Versi�n 3.2.0
library(geofacet)   # Versi�n 0.1.10

#### DATOS GENERALES ####

print.noquote("###############################################################")
print.noquote("###################### Cargando datos #########################")
print.noquote("###############################################################")

# Variables explicativas 
print.noquote("################ Equivalencias variables #######################")
equivalencia_variables <- read_csv("DATOS/GENERALES/Variables.csv", 
                                   locale = locale(encoding = "latin1"))

# Familias pol�ticas
print.noquote("#################### Familias Pol�ticas #######################")
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

# Geofacet grid antiguas regiones
fr_anc_reg <- read_csv(file = "DATOS/GENERALES/fr_anc_reg_grid.csv",
                       locale = locale(encoding = "latin1"))

# Auxiliares grandes ciudades
aux_paris <- tibble(CODGEO = paste("751",str_pad(1:20,2,pad = 0),sep = ""), 
                    NOM_COMUNA = paste("Paris", 1:20),
                    COD_DPTO = "75",
                    COD_REG = "11",
                    NOM_DPTO = "Paris",
                    NOM_REG = "�le-de-France")

aux_lyon <- tibble(CODGEO = paste("6938",1:9,sep = ""), 
                   NOM_COMUNA = paste("Lyon", 1:9),
                   COD_DPTO = "69",
                   COD_REG = "82",
                   NOM_DPTO = "Rh�ne",
                   NOM_REG = "Rh�ne-Alpes")

aux_marsella <- tibble(CODGEO = paste("132",str_pad(1:16,2,pad = 0),sep = ""), 
                       NOM_COMUNA = paste("Marseille", 1:16),
                       COD_DPTO = "13",
                       COD_REG = "93",
                       NOM_DPTO = "Bouches-du-Rh�ne",
                       NOM_REG = "Provence-Alpes-C�te d'Azur")

# Comunas
COMUNAS_2007 <- read_tsv(file = "DATOS/GENERALES/comsimp2007.txt", 
                         locale = locale(encoding = "latin1")) %>% 
  transmute(CODGEO = paste(DEP,COM,sep=""), NOM_COMUNA = NCCENR, ART = ARTMIN) %>% 
  # A�adimos el art�culo al nombre
  mutate(NOM_COMUNA = if_else(is.na(ART),
                              NOM_COMUNA,
                              paste(str_remove_all(ART,"\\(|\\)"),NOM_COMUNA,sep=" "))) %>% 
  # Corregimos los espacios despu�s del ap�strofe y las ligaduras (OE,oe)
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
  bind_rows(aux_marsella) %>% 
  filter(!{CODGEO %in% c(13055,69123,75056)})

COMUNAS_2012 <- read_tsv(file = "DATOS/GENERALES/comsimp2012.txt", 
                         locale = locale(encoding = "latin1")) %>% 
  transmute(CODGEO = paste(DEP,COM,sep=""), NOM_COMUNA = NCCENR, ART = ARTMIN) %>% 
  # A�adimos el art�culo al nombre
  mutate(NOM_COMUNA = if_else(is.na(ART),
                              NOM_COMUNA,
                              paste(str_remove_all(ART,"\\(|\\)"),NOM_COMUNA,sep=" "))) %>% 
  # Corregimos los espacios despu�s del ap�strofe y las ligaduras (OE,oe)
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
  bind_rows(aux_marsella) %>% 
  filter(!{CODGEO %in% c(13055,69123,75056)})

# Eliminamos auxiliares de las grandes ciudades
remove(aux_paris,aux_lyon,aux_marsella)

#### PRELIMPIEZA DE DATOS ####

print.noquote("###############################################################")
print.noquote("################## Prelimpieza de datos #######################")
print.noquote("###############################################################")

# Prelimpieza de datos de delitos
# TARDADO
#source("SCRIPTS_R/00_PRELIMPIEZA_DATOS_DELITOS.R")

#### LIMPIEZA DE DATOS ####

print.noquote("###############################################################")
print.noquote("#################### Limpieza de datos ########################")
print.noquote("###############################################################")

# Limpieza de datos electorales
#source("SCRIPTS_R/01_LIMPIEZA_DATOS_ELECTORALES.R")
# Limpieza de datos censales
#source("SCRIPTS_R/02_LIMPIEZA_DATOS_CENSALES.R")
# Limpieza de otros datos comunales
#source("SCRIPTS_R/03_LIMPIEZA_OTROS_DATOS_COMUNALES.R")
# Limpieza de datos a nivel departamentos
#source("SCRIPTS_R/04_LIMPIEZA_DATOS_NIVEL_DEPARTAMENTOS.R")

#### AN�LISIS EXPLORATORIO DE DATOS ####

print.noquote("###############################################################")
print.noquote("############### An�lisis exploratorio de datos ################")
print.noquote("###############################################################")

# AED electoral
# TARDA
# source("SCRIPTS_R/05_AED_ELECTORAL.R") 
# source("SCRIPTS_R/06_AED_CENSAL.R")
# source("SCRIPTS_R/07_AED_OTROS_COMUNALES.R")

#### MODELOS INDIVIDUALES ####

print.noquote("###############################################################")
print.noquote("################### Modelos individuales ######################")
print.noquote("###############################################################")
