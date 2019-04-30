############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ LIMPIEZA DE DATOS #############################################
############################################################################################################
######################################### DATOS NIVEL DEPARTAMENTOS ########################################
############################################################################################################


#### CARGAMOS DATOS CENSALES ####
print.noquote("###############################################################")
print.noquote("##################### Preparando datos ########################")
print.noquote("###############################################################")

DATOS_CENSALES <- read_csv("DATOS/LIMPIOS/DATOS_CENSALES.csv",locale = locale(encoding = "latin1")) %>% 
  separate(CODGEO,c("COD_DPTO","COD_COMUNA"),sep = 2) %>% 
  group_by(COD_DPTO,AÑO) %>% 
  summarise_at("Pob",list(~sum(.))) 
  

#### DELITOS ####
print.noquote("########################  Delitos #############################")
print.noquote("###### Calculando conteo acumulado de 5 años anteriores #######")
print.noquote("##### Por cada 100 mil habitantes al año de la elección #######")

DELITOS_07 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/DELITOS_FAZH.csv",
                       locale = locale(encoding = "latin1")) %>% 
  filter(AÑO %in% 2002:2006) %>% 
  group_by(COD_DPTO,TIPO_CUENTA) %>% 
  summarise_at("CUENTA",list(~sum(.))) %>% 
  ungroup %>% 
  mutate(AÑO = 2007) %>% 
  left_join(DATOS_CENSALES) %>% 
  transmute(COD_DPTO,AÑO,TIPO_CUENTA,CUENTA = CUENTA*(100000/Pob)) %>% 
  filter(!is.na(CUENTA)) %>% 
  spread(TIPO_CUENTA,CUENTA,fill=0)

DELITOS_12 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/DELITOS_FAZH.csv",
                       locale = locale(encoding = "latin1")) %>% 
  filter(AÑO %in% 2007:2011) %>% 
  group_by(COD_DPTO,TIPO_CUENTA) %>% 
  summarise_at("CUENTA",list(~sum(.))) %>% 
  ungroup %>% 
  mutate(AÑO = 2012) %>% 
  left_join(DATOS_CENSALES) %>% 
  transmute(COD_DPTO,AÑO,TIPO_CUENTA,CUENTA = CUENTA*(100000/Pob)) %>% 
  filter(!is.na(CUENTA)) %>% 
  spread(TIPO_CUENTA,CUENTA,fill=0)

#### GINI ####
print.noquote("#######################  Índice Gini ##########################")

GINI_07 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/GINI_DPTOS_2006_FAZH.csv",
                       locale = locale(encoding = "latin1")) %>% 
  mutate(AÑO = 2007)

GINI_12 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/GINI_DPTOS_2011_FAZH.csv",
                    locale = locale(encoding = "latin1")) %>% 
  mutate(AÑO = 2012)


#### CONSTRUYENDO BASE ÚNICA ####
print.noquote("###############################################################")
print.noquote("################## Construyendo base única ####################")
print.noquote("###############################################################")

DATOS_DPTOS <- full_join(DELITOS_07,GINI_07, by=c("COD_DPTO","AÑO")) %>% 
  filter_all(any_vars(!is.na(.)))

DATOS_DPTOS <- full_join(DELITOS_12,GINI_12, by=c("COD_DPTO","AÑO")) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  bind_rows(DATOS_DPTOS)

print.noquote("Guardando base única de datos censale.")
write.csv(DATOS_DPTOS,"DATOS/LIMPIOS/DATOS_NIVEL_DEPARTAMENTOS.csv",row.names = FALSE)

#### BORRAMOS LO CREADO EN ESTE SCRIPT ####
remove(DELITOS_07,GINI_07,
       DELITOS_12,GINI_12,
       DATOS_DPTOS,
       DATOS_CENSALES)
