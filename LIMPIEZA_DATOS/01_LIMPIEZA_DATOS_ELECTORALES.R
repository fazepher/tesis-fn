############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ LIMPIEZA DE DATOS #############################################
############################################################################################################
############################################ DATOS ELECTORALES #############################################
############################################################################################################

#### FUNCIÓN PARTICULAR ####

limpieza_basica <- function(datos, 
                            columnas_base = c("CODGEO","CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                              "ETIQUETA", "APELLIDO_CANDIDATO", "VOT_CANDIDATO"), 
                            filtro_basico = TRUE, ajuste_gr_comunas = TRUE, cod_casilla = TRUE){
  
  if(filtro_basico){
    
    print.noquote("Solo se conservan los datos de la metrópoli francesa, incluida Córsega, y la primera vuelta.")
    
    datos %<>% 
      filter(!{str_sub(COD_DEPARTAMENTO,1,1) == "Z"} & VUELTA == 1)
  }
  
  if(ajuste_gr_comunas){
    
    print.noquote("Se modifican ciertos datos (NOM_COMUNA Y CODGEO) para París, Marsella y Lyon.")
    
    datos %<>% 
      # Modificamos los Nombres de las comunas cuando son Arrondisements de París, Lyon y Marsella
      mutate(NOM_COMUNA = case_when(NOM_COMUNA == "Paris" ~ paste("Paris",str_sub(CASILLA,1,2), sep = " "),
                                    NOM_COMUNA == "Lyon" ~ paste("Lyon",str_sub(CASILLA,2,2), sep = " "),
                                    NOM_COMUNA == "Marseille" ~ paste("Marseille",str_sub(CASILLA,1,2), sep = " "),
                                    TRUE ~ NOM_COMUNA)) %>%
      # Agregamos el CODGEO del INSEE para las comunas (ajustando para los Arrondisements de París, Lyon y Marsella)
      mutate(CODGEO = paste(COD_DEPARTAMENTO,COD_COMUNA,sep="")) %>%
      mutate(CODGEO = case_when(CODGEO == "75056" ~ paste("751",str_sub(CASILLA,1,2),sep=""),
                                CODGEO == "69123" ~ paste("6938",str_sub(CASILLA,2,2),sep=""),
                                CODGEO == "13055" ~ paste("132",str_sub(CASILLA,1,2),sep=""),
                                TRUE ~ CODGEO))
  }
  
  if(cod_casilla){
    
    print.noquote("Se modifica el código de casillas para hacerlo único.")
    
    datos %<>% mutate(CASILLA = paste(CODGEO,CASILLA,sep="_"))
    
  }
  
  print.noquote("Conservando solo las columnas indicadas.")
  datos %<>% select(one_of(columnas_base)) 
  
  return(datos)
  
}

#### CARGANDO DATOS ####

print.noquote("###############################################################")
print.noquote("###################### Cargando datos #########################")
print.noquote("###############################################################")

# Presidenciales 2007
print.noquote("###################  Presidenciales 2007 ######################")
print.noquote("#####################  Limpieza básica ########################")

presidenciales_2007 <- read_csv2(file = "DATOS/PRELIMPIEZA/RESULTADOS_ELECTORALES/PR07_Bvot_T1T2_FAZH.txt", 
                                 col_names = c("VUELTA",
                                               "COD_DEPARTAMENTO","COD_COMUNA","NOM_COMUNA",
                                               "CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                               "NUM_CANDIDATO","APELLIDO_CANDIDATO","NOMBRE_CANDIDATO","ETIQUETA",
                                               "VOT_CANDIDATO"),
                                 col_types = paste(c(rep("c",5),rep("i",3),rep("c",4),"i"), collapse = ""),
                                 locale = locale(encoding = "latin1")) %>% 
  limpieza_basica %>% 
  mutate(ELECCION = "Presidenciales 2007")


# Legislativas 2007
print.noquote("####################  Legislativas 2007 ######################")
print.noquote("#####################  Limpieza básica #######################")

legislativas_2007 <- read_csv2(file = "DATOS/PRELIMPIEZA/RESULTADOS_ELECTORALES/LG07_Bvot_T1T2_FAZH.txt", 
                               col_names = c("VUELTA",
                                             "COD_DEPARTAMENTO","COD_COMUNA","NOM_COMUNA",
                                             "CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                             "NUM_CANDIDATO","APELLIDO_CANDIDATO","NOMBRE_CANDIDATO","ETIQUETA",
                                             "VOT_CANDIDATO"),
                               col_types = paste(c(rep("c",5),rep("i",3),rep("c",4),"i"), collapse = ""),
                               locale = locale(encoding = "latin1")) %>% 
  limpieza_basica %>% 
  mutate(ELECCION = "Legislativas 2007")

# Presidenciales 2012
print.noquote("################  Presidenciales 2012 #########################")
print.noquote("#####################  Limpieza básica #######################")

presidenciales_2012 <- read_csv2(file = "DATOS/PRELIMPIEZA/RESULTADOS_ELECTORALES/PR12_Bvot_T1T2_FAZH.txt", 
                                 col_names = c("VUELTA",
                                               "COD_DEPARTAMENTO","COD_COMUNA","NOM_COMUNA",
                                               "CIRC_LEGIS","COD_CANT",
                                               "CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                               "NUM_CANDIDATO","APELLIDO_CANDIDATO","NOMBRE_CANDIDATO","ETIQUETA",
                                               "VOT_CANDIDATO"),
                                 col_types = c("ccccccciiicccci"),
                                 locale = locale(encoding = "latin1")) %>% 
  limpieza_basica %>% 
  mutate(ELECCION = "Presidenciales 2012")

# Legislativas 2012
print.noquote("####################  Legislativas 2012 ######################")
print.noquote("#####################  Limpieza básica #######################")

legislativas_2012 <- read_csv2(file = "DATOS/PRELIMPIEZA/RESULTADOS_ELECTORALES/LG12_Bvot_T1T2_FAZH.txt", 
                               col_names = c("VUELTA",
                                             "COD_DEPARTAMENTO","COD_COMUNA","NOM_COMUNA",
                                             "CIRC_LEGIS","COD_CANT",
                                             "CASILLA","INSCRITOS","VOTANTES","VOT_EF",
                                             "NUM_CANDIDATO","APELLIDO_CANDIDATO","NOMBRE_CANDIDATO","ETIQUETA",
                                             "VOT_CANDIDATO"),
                               col_types = c("ccccccciiicccci"),
                               locale = locale(encoding = "latin1")) %>% 
  limpieza_basica %>% 
  mutate(ELECCION = "Legislativas 2012")

#### CONSTRUYENDO BASE ÚNICA ####
print.noquote("###############################################################")
print.noquote("################## Construyendo base única ####################")
print.noquote("###############################################################")

print.noquote("Uniendo todas las bases.")

auxiliar_datos_comuna <- list(presidenciales_2007,legislativas_2007,presidenciales_2012,legislativas_2012) %>% 
  bind_rows %>% 
  distinct(CODGEO,ELECCION,CASILLA,INSCRITOS,VOTANTES,VOT_EF) %>% 
  group_by(CODGEO,ELECCION) %>% 
  summarise_if(is.numeric,list(~sum(.,na.rm=TRUE))) %>% 
  ungroup
  

resultados_electorales <- list(presidenciales_2007,legislativas_2007,presidenciales_2012,legislativas_2012) %>% 
  bind_rows %T>%
  {print.noquote("Agregando datos a nivel comuna.")} %>% 
  group_by(CODGEO,ELECCION,ETIQUETA,APELLIDO_CANDIDATO) %>% 
  summarise(VOT_CANDIDATO = sum(VOT_CANDIDATO,na.rm=TRUE)) %>% 
  ungroup %T>%
  {print.noquote("Conservando solo las columnas:")} %T>%
  {print.noquote("CODGEO, ELECCION, VOT_CANDIDATO Y CANDIDATURA.")} %>% 
  transmute(CODGEO, ELECCION, VOT_CANDIDATO, 
            CANDIDATURA = if_else(ELECCION %in% c("Presidenciales 2007", "Presidenciales 2012"), 
                                  APELLIDO_CANDIDATO,ETIQUETA)) %T>%
  {print.noquote("Clasificando y agregando por familia política.")} %>% 
  left_join(familias_politicas_todas, by = c("CANDIDATURA","ELECCION")) %>% 
  group_by(ELECCION,CODGEO,FAMILIA) %>% 
  summarise_at("VOT_CANDIDATO",list(~sum(.,na.rm=TRUE))) %>% 
  ungroup %>% 
  left_join(auxiliar_datos_comuna) %>% 
  spread(FAMILIA,VOT_CANDIDATO,fill=0) %>% 
  gather(FAMILIA,VOT_CANDIDATO,-c(ELECCION:VOT_EF)) %T>%
  {print.noquote("Calculando porcentaje de votos respecto al número de inscritos.")} %>% 
  mutate(PCT_VOTOS_BR = VOT_CANDIDATO/INSCRITOS)

print.noquote("Guardando base única de resultados electorales.")
write.csv(resultados_electorales,"DATOS/LIMPIOS/RESULTADOS_ELECTORALES.csv",row.names = FALSE)

#### BORRAMOS LO CREADO EN ESTE SCRIPT ####
remove(presidenciales_2007, legislativas_2007,
       presidenciales_2012, legislativas_2012,
       resultados_electorales,
       limpieza_basica,
       auxiliar_datos_comuna)









