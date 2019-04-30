############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
########################################## PRELIMPIEZA DE DATOS ############################################
############################################################################################################
############################################## DATOS DELITOS ###############################################
############################################################################################################


print.noquote("###############################################################")
print.noquote("################# Prelimpieza datos delitos ###################")
print.noquote("###############################################################")

# Leemos archivo auxiliar de delitos que determina los delitos a considerarse
aux_delitos <- read_csv("DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/AUXILIAR_DELITOS.csv",
                        locale = locale(encoding = "latin1")) %>% 
  filter(CONSIDERADO == 1)

# Leemos libro de datos de delitos por departamento (omitiendo Francia entera y metropolitana)
# ¡Tarda un poquito!
datos_delitos <- openxlsx::getSheetNames("DATOS/BRUTOS/BASES_NIVEL_DEPARTAMENTOS/Tableaux_4001_TS.xlsx") %>% 
  extract(-c(1,2)) %>% 
  map_dfr(~ openxlsx::read.xlsx("DATOS/BRUTOS/BASES_NIVEL_DEPARTAMENTOS/Tableaux_4001_TS.xlsx", 
                                sheet = .x) %>% 
            as_tibble %>% 
            gather(AUX,n,-Index,-Libellé.index) %>% 
            rename(NUM_DELITO = Index, DELITO = Libellé.index) %>% 
            separate(AUX,c("AÑO","MES")) %>% 
            mutate_at(c("AÑO","MES"),funs(as.integer(.))) %>% 
            filter(AÑO %in% 2002:2012) %>% 
            # Conservando solo los delitos seleccionados
            right_join(aux_delitos,by=c("NUM_DELITO","DELITO")) %>% 
            # Agrupando por tipo de conteo 
            group_by(TIPO_CUENTA,AÑO,MES) %>% 
            summarise(CUENTA = sum(n)) %>% 
            # Identificando el departamento
            mutate(COD_DPTO = .x) %>% 
            select(COD_DPTO, everything()))

print.noquote("##################### Guardando datos ########################")

write.csv(datos_delitos,"DATOS/PRELIMPIEZA/BASES_NIVEL_DEPARTAMENTOS/DELITOS_FAZH.csv",row.names = FALSE)

remove(aux_delitos,datos_delitos)
