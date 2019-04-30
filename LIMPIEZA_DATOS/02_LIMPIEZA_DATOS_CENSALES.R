############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ LIMPIEZA DE DATOS #############################################
############################################################################################################
############################################## DATOS CENSALES ##############################################
############################################################################################################


#### FUNCIONES LOCALES ####

transforma_pct <- function(datos,variable,valores){
  
  datos %>% group_by_at(c("CODGEO","AÑO",variable)) %>% 
    summarise(PERSONAS = sum(PERSONAS)) %>% 
    mutate(PCT = PERSONAS/sum(PERSONAS)) %>% 
    ungroup %>% 
    select(-PERSONAS) %>% 
    spread(UQ(variable),PCT,fill = 0) %>% 
    return()
  
}

#### CARGANDO DATOS ####

print.noquote("###############################################################")
print.noquote("##################### Preparando datos ########################")
print.noquote("###############################################################")

#### POBLACIÓN TOTAL ####
print.noquote("#####################  Población Total ########################")

POB_07 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_POP1B_2007_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO",
                                   "EDAD","SEXO",
                                   "PERSONAS"),
                     col_types = "ccicd", 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2007,
            Pob = PERSONAS) %>% 
  group_by_at(c("CODGEO","AÑO")) %>% 
  summarise(Pob = sum(Pob)) %>% 
  ungroup

POB_12 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_POP1B_2012_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO","LIBGEO",
                                   "SEXO","EDAD",
                                   "PERSONAS"),
                     col_types = "ccccid", 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2012,
            Pob = PERSONAS) %>% 
  group_by_at(c("CODGEO","AÑO")) %>% 
  summarise(Pob = sum(Pob)) %>% 
  ungroup

#### SEXO ####
print.noquote("##########################  Sexo ##############################")
print.noquote("###############  Transformando a Porcentajes ##################")

SEXO_07 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_POP1B_2007_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO",
                                   "EDAD","SEXO",
                                   "PERSONAS"),
                     col_types = "ccicd", 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2007,
            SEXO = factor(SEXO,levels = c("1","2"), labels = c("Hom","Muj")),
            PERSONAS) %>% 
  transforma_pct("SEXO")

SEXO_12 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_POP1B_2012_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO","LIBGEO",
                                   "SEXO","EDAD",
                                   "PERSONAS"),
                     col_types = "ccccid", 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2012,
            SEXO = factor(SEXO,levels = c("1","2"), labels = c("Hom","Muj")),
            PERSONAS) %>% 
  transforma_pct("SEXO")

#### EDADES ####
print.noquote("####################  Grupos de Edades #######################")
print.noquote("###############  Transformando a Porcentajes ##################")

EDAD_07 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_POP1B_2007_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO",
                                   "EDAD","SEXO",
                                   "PERSONAS"),
                     col_types = "ccicd", 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2007,
            EDAD = cut(EDAD,c(0,17,24,39,54,64,100),include.lowest = TRUE) %>% 
              factor(.,levels = levels(.), labels = paste("Ed",c(1:6),sep="")),
            PERSONAS) %>% 
  transforma_pct("EDAD")

EDAD_12 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_POP1B_2012_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO","LIBGEO",
                                   "SEXO","EDAD",
                                   "PERSONAS"),
                     col_types = "ccccid", 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2012,
            EDAD = cut(EDAD,c(0,17,24,39,54,64,100),include.lowest = TRUE) %>% 
              factor(.,levels = levels(.), labels = paste("Ed",c(1:6),sep="")),
            PERSONAS) %>% 
  transforma_pct("EDAD")

#### NACIONALIDAD ####
print.noquote("######################  Nacionalidad ##########################")
print.noquote("###############  Transformando a Porcentajes ##################")

NAT_07 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_NAT3A_2007_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO",
                                   "SEXO","NACIONALIDAD","CSP",
                                   "PERSONAS"),
                     col_types = paste(c(rep("c",5),"d"), collapse = ""), 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2007,
            NACIONALIDAD = factor(NACIONALIDAD,levels=c("1","2"),labels=c("Fra","Ext")),
            PERSONAS) %>% 
  transforma_pct("NACIONALIDAD")

NAT_12 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_NAT3A_2012_FAZH.txt", 
                    col_names = c("NIVEL","CODGEO","LIBGEO",
                                  "SEXO","NACIONALIDAD","CSP",
                                  "PERSONAS"),
                    col_types = paste(c(rep("c",6),"d"), collapse = ""), 
                    locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2012,
            NACIONALIDAD = factor(NACIONALIDAD,levels=c("1","2"),labels=c("Fra","Ext")),
            PERSONAS) %>% 
  transforma_pct("NACIONALIDAD")

#### CAT SOCIOPROFESIONAL ####
print.noquote("################  Categoría Socioprofesional ##################")
print.noquote("###############  Transformando a Porcentajes ##################")

CSP_07 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_NAT3A_2007_FAZH.txt", 
                    col_names = c("NIVEL","CODGEO",
                                  "SEXO","NACIONALIDAD","CSP",
                                  "PERSONAS"),
                    col_types = paste(c(rep("c",5),"d"), collapse = ""), 
                    locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2007,
            CSP = factor(CSP,levels=as.character(1:8),labels=paste("CSP",1:8,sep = "")),
            PERSONAS) %>% 
  transforma_pct("CSP")

CSP_12 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_NAT3A_2012_FAZH.txt", 
                    col_names = c("NIVEL","CODGEO","LIBGEO",
                                  "CSP","SEXO","NACIONALIDAD",
                                  "PERSONAS"),
                    col_types = paste(c(rep("c",6),"d"), collapse = ""), 
                    locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2012,
            CSP = factor(CSP,levels=as.character(1:8),labels=paste("CSP",1:8,sep = "")),
            PERSONAS) %>% 
  transforma_pct("CSP")

#### MIGRACIÓN ####
print.noquote("###################  Condición Migratoria #####################")
print.noquote("###############  Transformando a Porcentajes ##################")

MIGR_07 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_IMG1_2007_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO",
                                   "SEXO","EDAD","TIPO_ACT","C_MIGRATORIA",
                                   "PERSONAS"),
                     col_types = paste(c(rep("c",6),"d"), collapse = ""), 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2007,
            C_MIGRATORIA = factor(C_MIGRATORIA,levels=c("1","2"),labels=c("Inm","Loc")),
            PERSONAS) %>% 
  transforma_pct("C_MIGRATORIA")

MIGR_12 <- read_csv2(file = "DATOS/PRELIMPIEZA/BASES_CENSALES/BTT_TD_IMG1A_2012_FAZH.txt", 
                     col_names = c("NIVEL","CODGEO","LIBGEO",
                                   "EDAD","C_MIGRATORIA","SEXO",
                                   "PERSONAS"),
                     col_types = paste(c(rep("c",6),"d"), collapse = ""), 
                     locale = locale(decimal_mark = ",")) %>% 
  transmute(CODGEO,
            AÑO = 2012,
            C_MIGRATORIA = factor(C_MIGRATORIA,levels=c("1","2"),labels=c("Inm","Loc")),
            PERSONAS) %>% 
  transforma_pct("C_MIGRATORIA")

#### CONSTRUYENDO BASE ÚNICA ####
print.noquote("###############################################################")
print.noquote("################## Construyendo base única ####################")
print.noquote("###############################################################")

DATOS_CENSALES <- list(POB_07,SEXO_07,EDAD_07,NAT_07,CSP_07,MIGR_07) %>% 
  reduce(full_join,by=c("CODGEO","AÑO")) %>% 
  filter_all(any_vars(!is.na(.)))

DATOS_CENSALES <- list(POB_12,SEXO_12,EDAD_12,NAT_12,CSP_12,MIGR_12) %>% 
  reduce(full_join,by=c("CODGEO","AÑO")) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  bind_rows(DATOS_CENSALES)

print.noquote("Guardando base única de datos censale.")
write.csv(DATOS_CENSALES,"DATOS/LIMPIOS/DATOS_CENSALES.csv",row.names = FALSE)

#### BORRAMOS LO CREADO EN ESTE SCRIPT ####
remove(POB_07,SEXO_07,EDAD_07,NAT_07,CSP_07,MIGR_07,
       POB_12,SEXO_12,EDAD_12,NAT_12,CSP_12,MIGR_12,
       DATOS_CENSALES,
       transforma_pct)
