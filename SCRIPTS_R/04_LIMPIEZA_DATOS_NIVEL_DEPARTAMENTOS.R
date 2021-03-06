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
  group_by(COD_DPTO,A�O) %>% 
  summarise_at("Pob",list(~sum(.))) 
  

#### DELITOS ####
print.noquote("########################  Delitos #############################")
print.noquote("###### Calculando conteo acumulado de 5 a�os anteriores #######")
print.noquote("##### Por cada 100 mil habitantes al a�o de la elecci�n #######")

DELITOS_07 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/DELITOS_FAZH.csv",
                       locale = locale(encoding = "latin1")) %>% 
  filter(A�O %in% 2002:2006) %>% 
  group_by(COD_DPTO,TIPO_CUENTA) %>% 
  summarise_at("CUENTA",list(~sum(.))) %>% 
  ungroup %>% 
  mutate(A�O = 2007) %>% 
  left_join(DATOS_CENSALES) %>% 
  transmute(COD_DPTO,A�O,TIPO_CUENTA,CUENTA = CUENTA*(100000/Pob)) %>% 
  filter(!is.na(CUENTA)) %>% 
  spread(TIPO_CUENTA,CUENTA,fill=0)

DELITOS_12 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/DELITOS_FAZH.csv",
                       locale = locale(encoding = "latin1")) %>% 
  filter(A�O %in% 2007:2011) %>% 
  group_by(COD_DPTO,TIPO_CUENTA) %>% 
  summarise_at("CUENTA",list(~sum(.))) %>% 
  ungroup %>% 
  mutate(A�O = 2012) %>% 
  left_join(DATOS_CENSALES) %>% 
  transmute(COD_DPTO,A�O,TIPO_CUENTA,CUENTA = CUENTA*(100000/Pob)) %>% 
  filter(!is.na(CUENTA)) %>% 
  spread(TIPO_CUENTA,CUENTA,fill=0)

#### GINI ####
print.noquote("#######################  �ndice Gini ##########################")

GINI_07 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/GINI_DPTOS_2006_FAZH.csv",
                       locale = locale(encoding = "latin1")) %>% 
  mutate(A�O = 2007)

GINI_12 <- read_csv(file = "DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/GINI_DPTOS_2011_FAZH.csv",
                    locale = locale(encoding = "latin1")) %>% 
  mutate(A�O = 2012)


#### CONSTRUYENDO BASE �NICA ####
print.noquote("###############################################################")
print.noquote("################## Construyendo base �nica ####################")
print.noquote("###############################################################")

DATOS_DPTOS <- full_join(DELITOS_07,GINI_07, by=c("COD_DPTO","A�O")) %>% 
  filter_all(any_vars(!is.na(.)))

DATOS_DPTOS <- full_join(DELITOS_12,GINI_12, by=c("COD_DPTO","A�O")) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  bind_rows(DATOS_DPTOS)

print.noquote("Guardando base �nica de datos censale.")
write.csv(DATOS_DPTOS,"DATOS/LIMPIOS/DATOS_NIVEL_DEPARTAMENTOS.csv",row.names = FALSE)

#### BORRAMOS LO CREADO EN ESTE SCRIPT ####
remove(DELITOS_07,GINI_07,
       DELITOS_12,GINI_12,
       DATOS_DPTOS,
       DATOS_CENSALES)
