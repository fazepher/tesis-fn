############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ LIMPIEZA DE DATOS #############################################
############################################################################################################
########################################## OTROS DATOS COMUNALES ###########################################
############################################################################################################


#### CARGAMOS DATOS CENSALES ####
print.noquote("###############################################################")
print.noquote("##################### Preparando datos ########################")
print.noquote("###############################################################")

DATOS_CENSALES <- read_csv("DATOS/LIMPIOS/DATOS_CENSALES.csv",locale = locale(encoding = "latin1"))

#### DATOS DE EMPLEO ####
print.noquote("#########################  Empleo #############################")
print.noquote("###############  Transformando a Porcentajes ##################")

EMPLEO_07 <- read_csv(file = "DATOS/PRELIMPIEZA/OTRAS_BASES_COMUNALES/EMPLEO_2007_FAZH.csv", 
                   col_types = paste(c("c",rep("d",9)),collapse = "")) %>% 
  gather(VARIABLE,PERSONAS,-CODGEO) %>% 
  mutate(AÑO = 2007) %>% 
  separate(VARIABLE,c("AUX1","EMPLEO"),extra="merge") %>% 
  group_by(CODGEO,AÑO,EMPLEO) %>% 
  summarise(PERSONAS = sum(PERSONAS)) %>% 
  mutate(EMPLEO = factor(EMPLEO,
                         levels = c("1524","2554","5564",
                                    "DES_1524","DES_2554","DES_5564"),
                         labels = c("Ocu1","Ocu2","Ocu3",
                                    "Des1","Des2","Des3")),
         EDAD = factor(str_sub(EMPLEO,-1,-1))) %>% 
  group_by(CODGEO,AÑO,EDAD,EMPLEO) %>% 
  summarise(PERSONAS = sum(PERSONAS)) %>% 
  mutate(PCT = PERSONAS/sum(PERSONAS)) %>% 
  ungroup %>% 
  select(-PERSONAS,-EDAD) %>% 
  spread(EMPLEO,PCT,fill=0)

EMPLEO_12 <- read_csv(file = "DATOS/PRELIMPIEZA/OTRAS_BASES_COMUNALES/EMPLEO_2012_FAZH.csv", 
                      col_types = paste(c("c",rep("d",9)),collapse = "")) %>% 
  gather(VARIABLE,PERSONAS,-CODGEO) %>% 
  mutate(AÑO = 2012) %>% 
  separate(VARIABLE,c("AUX1","EMPLEO"),extra="merge") %>% 
  group_by(CODGEO,AÑO,EMPLEO) %>% 
  summarise(PERSONAS = sum(PERSONAS)) %>% 
  mutate(EMPLEO = factor(EMPLEO,
                         levels = c("1524","2554","5564",
                                    "DES_1524","DES_2554","DES_5564"),
                         labels = c("Ocu1","Ocu2","Ocu3",
                                    "Des1","Des2","Des3")),
         EDAD = factor(str_sub(EMPLEO,-1,-1))) %>% 
  group_by(CODGEO,AÑO,EDAD,EMPLEO) %>% 
  summarise(PERSONAS = sum(PERSONAS)) %>% 
  mutate(PCT = PERSONAS/sum(PERSONAS)) %>% 
  ungroup %>% 
  select(-PERSONAS,-EDAD) %>% 
  spread(EMPLEO,PCT,fill=0)

#### ESCOLARIDAD ####
print.noquote("#######################  Escolaridad ##########################")
print.noquote("###############  Transformando a Porcentajes ##################")

ESCOL_07 <- read_csv(file = "DATOS/PRELIMPIEZA/OTRAS_BASES_COMUNALES/ESCOLARIDAD_2007_FAZH.csv", 
                     col_types = paste(c("c",rep("d",14)),collapse = "")) %>% 
  gather(VARIABLE,PERSONAS,-CODGEO) %>% 
  mutate(AÑO = 2007) %>% 
  separate(VARIABLE,c("AUX1","ESCOL"),extra="merge") %>% 
  select(-AUX1) %>% 
  separate(ESCOL,c("TIPO","ESCOL"),fill="right") %>% 
  mutate(ESCOL = case_when(is.na(ESCOL) ~ "Esc",
                           ESCOL %in% c("DIPL0") ~ "Dip1",
                           ESCOL %in% c("CEP","BEPC") ~ "Dip2",
                           ESCOL %in% c("CAPBEP","BAC","BACP2") ~ "Dip3",
                           ESCOL %in% c("SUP") ~ "Dip4")) %>% 
  select(-TIPO) %>% 
  group_by(CODGEO,AÑO,ESCOL) %>% 
  summarise(PERSONAS = sum(PERSONAS)) %>% 
  mutate(PCT = PERSONAS/sum(PERSONAS)) %>% 
  ungroup %>% 
  select(-PERSONAS) %>% 
  spread(ESCOL,PCT,fill=0)

ESCOL_12 <-read_csv(file = "DATOS/PRELIMPIEZA/OTRAS_BASES_COMUNALES/ESCOLARIDAD_2012_FAZH.csv", 
                   col_types = paste(c("c",rep("d",14)),collapse = "")) %>% 
  gather(VARIABLE,PERSONAS,-CODGEO) %>% 
  mutate(AÑO = 2012) %>% 
  separate(VARIABLE,c("AUX1","ESCOL"),extra="merge") %>% 
  select(-AUX1) %>% 
  separate(ESCOL,c("TIPO","ESCOL"),fill="right") %>% 
  mutate(ESCOL = case_when(is.na(ESCOL) ~ "Esc",
                           ESCOL %in% c("DIPL0") ~ "Dip1",
                           ESCOL %in% c("CEP","BEPC") ~ "Dip2",
                           ESCOL %in% c("CAPBEP","BAC","BACP2") ~ "Dip3",
                           ESCOL %in% c("SUP") ~ "Dip4")) %>% 
  select(-TIPO) %>% 
  group_by(CODGEO,AÑO,ESCOL) %>% 
  summarise(PERSONAS = sum(PERSONAS)) %>% 
  mutate(PCT = PERSONAS/sum(PERSONAS)) %>% 
  ungroup %>% 
  select(-PERSONAS) %>% 
  spread(ESCOL,PCT,fill=0)


#### CONSTRUYENDO BASE ÚNICA ####
print.noquote("###############################################################")
print.noquote("################## Construyendo base única ####################")
print.noquote("###############################################################")

OTROS_DATOS_COMUNALES <- full_join(EMPLEO_07,ESCOL_07, by=c("CODGEO","AÑO")) %>% 
  filter_all(any_vars(!is.na(.)))

OTROS_DATOS_COMUNALES <- full_join(EMPLEO_12,ESCOL_12, by=c("CODGEO","AÑO")) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  bind_rows(OTROS_DATOS_COMUNALES)

print.noquote("Guardando base única de otros datos comunales.")
write.csv(OTROS_DATOS_COMUNALES,"DATOS/LIMPIOS/OTROS_DATOS_COMUNALES.csv",row.names = FALSE)

#### BORRAMOS LO CREADO EN ESTE SCRIPT ####
remove(EMPLEO_07,ESCOL_07,
       EMPLEO_12,ESCOL_12,
       OTROS_DATOS_COMUNALES,
       DATOS_CENSALES)
