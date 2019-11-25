############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################# MODELOS COMPUESTOS ###########################################
############################################################################################################
############################################ ANALIZA PREDICCIONES ##########################################
############################################################################################################

library(rstan)
library(sf)
library(cartogram)
library(cowplot)
library(loo) 

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

muestra <- read_csv("DATOS/Muestra.csv", 
                    locale = locale(encoding = "latin1"))

prueba_datos_unificados <- datos_electorales_completos %>% 
  filter(FAMILIA == "FN") %>% 
  mutate(AÑO = str_extract(ELECCION,"\\d{4}$") %>% as.integer) %>% 
  inner_join(datos_censales, by = c("CODGEO","AÑO")) %>% 
  inner_join(otros_datos_comunales, by = c("CODGEO","AÑO")) %>% 
  left_join(full_join(COMUNAS_2007,COMUNAS_2012)) %>% 
  select(CODGEO,NOM_COMUNA:NOM_REG,AÑO,ELECCION,everything())

datos_P12 <- prueba_datos_unificados %>% 
  filter(ELECCION == "Presidenciales 2012") %>% 
  mutate(Int = 1) %>% 
  select(CODGEO,COD_DPTO,INSCRITOS,VOT_CANDIDATO,Int,Hom:Esc) %>% 
  distinct()

#### MAPAS ####

set.seed(51295)
mapa_comunas <- st_read("DATOS/GENERALES/MAPA_COMUNAS_14") %>% 
  st_transform(crs = 25832)
mapa_dptos <- st_read("DATOS/GENERALES/MAPA_DPTOS") %>% 
  mutate(centro = st_centroid(geometry), 
         lat_centro = map_dbl(centro, ~ .x[[1]]), 
         lon_centro = map_dbl(centro, ~ .x[[2]])) %>% 
  filter(lon_centro > 40) %>% 
  st_transform(crs = 25832)

fronteras_reg <-  mapa_dptos %>% 
  left_join(DEPARTAMENTOS, by = c("code_insee"="COD_DPTO")) %>% 
  group_by(COD_REG) %>% 
  summarise(cod_reg=unique(COD_REG))

dorling_dptos <- mapa_dptos %>% 
  mutate(Iguales = 1) %>% 
  cartogram_dorling("Iguales",0.35,0.1)

fronteras_reg_dorling <- dorling_dptos %>% 
  right_join(DEPARTAMENTOS, by = c("code_insee" = "COD_DPTO")) %>% 
  group_by(COD_REG) %>% 
  summarise(cod_reg = unique(COD_REG)) %>% 
  st_convex_hull(.)

cuantiles_votos <- datos_P12 %>% 
  transmute(Pct = VOT_CANDIDATO/INSCRITOS) %>% 
  extract2(1) %>% 
  quantile(c(0,.25,.45,.5,.55,.75,0.95,1))

etiquetas_votos <- round(100*cuantiles_votos[c(1,4,8)], 1) %>% 
{paste(c("Mín. real: ", "Mediana real: ", "Máx. real: "),.,"%",sep="")}

#### ANALIZA ####

analiza_pred_modelo <- function(modelo, genera_mapas = T){
  
  print.noquote("Extrayendo predicciones para modelo:")
  print.noquote(modelo)
  
  pred_file_test <-  paste("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto",modelo,"PRED.rds", sep = "_") %>% 
    read_rds %>% 
    extract2("Predicciones")
  
  ajuste_medio <- pred_file_test %>% 
    as.data.frame(pars = c("pct_votos_media"),include = TRUE) %>% 
    as_tibble %>% 
    gather(Param,Pct) %>% 
    separate(Param,c("Param","CODGEO"),sep="\\[") %>% 
    select(-Param) %>% 
    mutate(CODGEO = str_remove_all(CODGEO,"\\]") %>% 
             as.integer %>% 
             {datos_P12$CODGEO[.]}) %>% 
    left_join(transmute(datos_P12,CODGEO,Pct_Real = VOT_CANDIDATO/INSCRITOS), by = "CODGEO") %>% 
    mutate(Error = 100*(Pct - Pct_Real))  %>% 
    left_join(mapa_comunas, by = c("CODGEO" = "insee")) %>%
    st_as_sf() %>% 
    st_transform(crs = 25832) %>% 
    select(CODGEO:Error)
  
  print.noquote("Errores del ajuste a nivel comuna")
  print(ajuste_medio$Error %>% summary)
  print.noquote("Generando y guardando mapas")
  
  if(genera_mapas){
    mapa_pred <- ggplot(ajuste_medio) + 
      geom_sf(aes(fill = Pct), color = "transparent", size = rel(0.25)) + 
      geom_sf(data = mapa_dptos, fill = "transparent", color = "gray85", size = rel(0.25)) + 
      scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                       paleta_tesis_fn$COLOR[c(5,2)],"#0c1740"),
                           values = scales::rescale(cuantiles_votos),
                           breaks = cuantiles_votos[c(1,4,8)],
                           labels = etiquetas_votos,
                           limits = cuantiles_votos[c(1,8)]) + 
      theme_void() + 
      theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
            legend.text = element_text(size = rel(0.75)),
            legend.title = element_text(size = rel(0.75)),
            legend.position = c(0.1,0.525))
    
    
    mapa_error <- ggplot(ajuste_medio) + 
      geom_sf(aes(fill = Error), color = "transparent", size = rel(0.25)) + 
      geom_sf(data = mapa_dptos, fill = "transparent", color = "gray85", size = rel(0.25)) + 
      scale_fill_gradientn(colours = paleta_tesis_fn$COLOR[c(1,6,4,7,4,6,1)],
                           values = scales::rescale(c(-50,-20,-5,0,5,20,50)),
                           limits = c(-50,50),
                           breaks = c(-50,-20,-5,5,20,50), 
                           labels = function(x){paste(x,"pp")}) +
      theme_void() + 
      theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
            legend.text = element_text(size = rel(1)),
            legend.title = element_text(size = rel(1)),
            legend.position = c(0.1,0.5))
    
    {ggdraw() + 
        draw_label(paste("Modelo compuesto",modelo),
                   x = 0.5, y = 0.95) + 
        draw_plot(mapa_pred,
                  x = 0.025, width = 0.475, height = 0.9) + 
        draw_plot(mapa_error,
                  x = 0.525, width = 0.475, height = 0.9)} %>% 
                  {ggsave(filename = paste("MODELOS_STAN/Mapas_Pred_Comunas/Modelo_Compuesto_",modelo,".pdf",sep=""), 
                          plot = ., 
                          device = cairo_pdf, width = 22.5/2, height = 17/3)}
    
  }
  
  print.noquote("Extrayendo medidas de error del modelo")
  
  waic <- paste("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_",modelo,".rds", sep = "") %>% 
    read_rds %T>% 
    {get_elapsed_time(.) ->> tiempo} %>% 
    extract_log_lik %>% 
    waic %>% 
    {.$estimates["waic","Estimate"]}
  
  tiempo %<>% 
    as_tibble %>% 
    transmute(Tiempo = warmup + sample) %>% 
    top_n(1,Tiempo) %>% 
    extract2("Tiempo")
  
  medidas_error <- pred_file_test %>% 
    as.data.frame(pars = c("PCMC","PAMC","PTMC",
                           "PCMD","PAMD","PTMD",
                           "PCN","PAN","PTN"),include = TRUE) %>% 
    as_tibble %>% 
    mutate(Modelo = modelo,
           WAIC = waic,
           Tiempo = tiempo) %>% 
    select(Modelo,everything())
  
  return(medidas_error)
}

medidas_errores_modelos <- LETTERS[1:8] %>% 
  map_dfr(~ analiza_pred_modelo(.x))

write.csv(medidas_errores_modelos,file = "MODELOS_STAN/Medidas_Errores_Modelos_Comp.csv",row.names = FALSE)

