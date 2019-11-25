############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
########################################## GENERACIÓN DE  MUESTRA ##########################################
############################################################################################################


#### CARGANDO DATOS ####

print.noquote("###############################################################")
print.noquote("###################### Cargando datos #########################")
print.noquote("###############################################################")

datos_electorales_completos <- read_csv("DATOS/LIMPIOS/RESULTADOS_ELECTORALES.csv", 
                                        locale = locale(encoding = "latin1"))

datos_censales <- read_csv("DATOS/LIMPIOS/DATOS_CENSALES.csv", 
                           locale = locale(encoding = "latin1"))

otros_datos_comunales <- read_csv("DATOS/LIMPIOS/OTROS_DATOS_COMUNALES.csv", 
                                  locale = locale(encoding = "latin1"))
#### Diseño muestral ####

marco_muestral <- datos_electorales_completos %>% 
  filter(FAMILIA == "FN") %>% 
  group_by(CODGEO) %>% 
  # Filtramos a que tenga datos en las 4 elecciones
  filter(n()==4) %>% 
  ungroup %>% 
  distinct(CODGEO) %>% 
  left_join(datos_censales) %>% 
  left_join(otros_datos_comunales) %>% 
  gather(Cats,Pct,-CODGEO,-AÑO,-Pob) %>% 
  left_join(equivalencia_variables) %>% 
  group_by(CODGEO,AÑO,Variable) %>% 
  # Filtramos a que tengan valores en todas las variables en ambos años
  summarise(Aux = sum(Pct)) %>% 
  filter(Aux>0,!is.na(Aux)) %>% 
  summarise(Aux = n()) %>% 
  filter(Aux == 9) %>% 
  ungroup %>% 
  distinct(CODGEO,AÑO) %>% 
  group_by(CODGEO) %>% 
  filter(n()==2) %>% 
  ungroup %>% 
  # Nos quedamos con los datos administrativos
  distinct(CODGEO) %>% 
  left_join(COMUNAS_2012)

tam_tentativo <- 4000

tam_muestra_por_dpto <- marco_muestral %>% 
  group_by(COD_DPTO) %>% 
  summarise(N = n()) %>% 
  mutate(n_prop = round(tam_tentativo*(N/sum(N))),
         n_final = case_when(N < 50 ~ N,
                             n_prop < 20 ~ 20L,
                             T ~ as.integer(n_prop)))

set.seed(51295)
muestra <- marco_muestral %>% 
  split(.$COD_DPTO) %>% 
  map_dfr(~tam_muestra_por_dpto %>% 
        filter(COD_DPTO == unique(.x$COD_DPTO)) %>% 
        extract2("n_final") %>% 
        {sample_n(.x,size=.)}) 

write.csv(muestra, file = "DATOS/Muestra.csv", row.names = FALSE)

muestra_previa <- datos_censales %>% 
  filter(AÑO == 2012) %>% 
  select(CODGEO,Pob) %>% 
  right_join(anti_join(marco_muestral,muestra), by = "CODGEO") %>% 
  sample_n(size = round(nrow(muestra)*.1), weight = Pob) %>% 
  select(-Pob)

write.csv(muestra_previa, file = "DATOS/Muestra_previa.csv", row.names = FALSE)

#### Comparados muestras ####
# comparados_muestra <- muestra %>% 
#   left_join(datos_electorales_completos) %>% 
#   filter(FAMILIA == "FN") %>% 
#   group_by(ELECCION) %>% 
#   summarise_at(c("VOT_EF","VOT_CANDIDATO"),sum) %>% 
#   transmute(Cat = ELECCION, 
#             TIPO = "EST",
#             Pct = VOT_CANDIDATO/VOT_EF) %>% 
#   separate(Cat,c("Cat","AÑO"))
# 
# comparados_muestra <- marco_muestral %>% 
#   left_join(datos_electorales_completos) %>% 
#   filter(FAMILIA == "FN") %>% 
#   group_by(ELECCION) %>% 
#   summarise_at(c("VOT_EF","VOT_CANDIDATO"),sum) %>% 
#   transmute(Cat = ELECCION, 
#             TIPO = "MM",
#             Pct = VOT_CANDIDATO/VOT_EF)  %>% 
#   separate(Cat,c("Cat","AÑO")) %>% 
#   bind_rows(comparados_muestra)
# 
# comparados_muestra <- muestra %>% 
#   left_join(datos_censales) %>% 
#   mutate_at(vars(Hom:Loc),list(~.*Pob)) %>% 
#   group_by(AÑO) %>% 
#   summarise_if(is.numeric,sum) %>% 
#   gather(Cat,Personas,-AÑO,-Pob) %>% 
#   group_by(AÑO,Cat) %>% 
#   transmute(TIPO = "EST",
#             Pct = Personas/Pob) %>% 
#   ungroup %>% 
#   mutate(AÑO = as.character(AÑO)) %>% 
#   bind_rows(comparados_muestra)
# 
# comparados_muestra <- marco_muestral %>% 
#   left_join(datos_censales) %>% 
#   mutate_at(vars(Hom:Loc),list(~.*Pob)) %>% 
#   group_by(AÑO) %>% 
#   summarise_if(is.numeric,sum) %>% 
#   gather(Cat,Personas,-AÑO,-Pob) %>% 
#   group_by(AÑO,Cat) %>% 
#   transmute(AÑO, 
#             TIPO = "MM",
#             Pct = Personas/Pob) %>% 
#   ungroup %>% 
#   mutate(AÑO = as.character(AÑO)) %>% 
#   bind_rows(comparados_muestra)
# 
# comparados_muestra %>% 
#   spread(TIPO,Pct) %>% 
#   mutate(Error = EST - MM) %>% 
#   ggplot(aes(x=Cat,y=Error,fill=factor(AÑO))) + 
#   geom_col(position = position_dodge()) +
#   scale_y_continuous(breaks = seq(-.06,.06,by=.02)) + 
#   theme_minimal() + 
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank())

