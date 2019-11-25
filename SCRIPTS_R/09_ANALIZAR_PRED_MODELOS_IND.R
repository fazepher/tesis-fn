############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ MODELOS INDIVIDUALES ##########################################
############################################################################################################
############################################ ANALIZA PREDICCIONES ##########################################
############################################################################################################

library(rstan)
library(sf)
library(cartogram)
library(cowplot)
library(loo) 

aux_var_nombres_modelos <- function(variable){
  str_replace_all(variable," ","_") %>% 
    str_remove_all("\\.") %>% 
    str_remove_all("pación")
}

genera_nombre_archivo_modelo <- function(variable,tipo,tabla_equiv = equivalencia_variables){
  
  var_archivo <- tabla_equiv %>% 
    filter(Variable == variable) %>% 
    mutate(Variable = aux_var_nombres_modelos(Variable)) %>% 
    extract2(1) %>% 
    unique
  
  case_when(tipo == "Nacional" ~ "MODELOS_STAN/Modelos_Nal_Ind/Modelo_Nal_",
            tipo == "Departamental" ~ "MODELOS_STAN/Modelos_Dep_Ind/Modelo_Dep_",
            tipo == "Jerárquico" ~ "MODELOS_STAN/Modelos_Jer_Ind/Modelo_Jer_") %>% 
    paste(var_archivo, sep = "") %>% 
    paste(".rds", sep = "")
}

gen_nombre_map_com <- function(variable,tipo,tabla_equiv = equivalencia_variables){
  
  var_archivo <- tabla_equiv %>% 
    filter(Variable == variable) %>% 
    mutate(Variable = aux_var_nombres_modelos(Variable)) %>% 
    extract2(1) %>% 
    unique
  
  case_when(tipo == "Nacional" ~ "MODELOS_STAN/Mapas_Pred_Comunas/Modelo_Nal_",
            tipo == "Departamental" ~ "MODELOS_STAN/Mapas_Pred_Comunas/Modelo_Dep_",
            tipo == "Jerárquico" ~ "MODELOS_STAN/Mapas_Pred_Comunas/Modelo_Jer_") %>% 
    paste(var_archivo, sep = "") %>% 
    paste(".pdf", sep = "")
}


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
  quantile(c(0,0.25,.45,.5,.55,0.75,0.95,1))

etiquetas_votos <- round(100*cuantiles_votos[c(1,4,8)], 1) %>% 
{paste(c("Mín. real: ", "Mediana real: ", "Máx. real: "),.,"%",sep="")}

#### ANALIZA ####

analiza_pred_modelo <- function(variable, tipo, genera_mapas = T){
  
  print.noquote("Extrayendo predicciones para modelo:")
  print.noquote(tipo)
  print.noquote(variable)
  
  pred_file_test <- genera_nombre_archivo_modelo(variable,tipo) %>% 
    str_replace(".rds","_PRED.rds") %>% 
    readRDS %>% 
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
        draw_label(paste("Modelo individual",tipo,"para",variable),
                   x = 0.5, y = 0.95) + 
        draw_plot(mapa_pred,
                  x = 0.025, width = 0.475, height = 0.9) + 
        draw_plot(mapa_error,
                  x = 0.525, width = 0.475, height = 0.9)} %>% 
      {ggsave(filename = gen_nombre_map_com(variable,tipo), 
              plot = ., 
              device = cairo_pdf, width = 22.5/2, height = 17/3)}
    
  }
  
  print.noquote("Extrayendo medidas de error del modelo")
  
  # waic <- genera_nombre_archivo_modelo(variable,tipo) %>% 
  #   readRDS %T>% 
  #   {get_elapsed_time(.) ->> tiempo} %>% 
  #   extract_log_lik %>% 
  #   waic %>% 
  #   {.$estimates["waic","Estimate"]}
  
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
    mutate(Variable = variable, 
           Tipo = tipo,
           #WAIC = waic,
           Tiempo = tiempo) %>% 
    select(Variable,Tipo,everything())
  
  return(medidas_error)
}

medidas_errores_modelos <- equivalencia_variables %>%
  distinct(Variable) %>%
  extract2(1) %>%
  map_dfr(~ tibble(Variable = .x,
                   Tipo = c("Nacional","Departamental","Jerárquico"))) %>%
  slice(18:19) %>% #OJO SE ESTÁN GENERANDO SOLO ALGUNOS MAPAS
  pmap_dfr(~ analiza_pred_modelo(..1,..2))

write.csv(medidas_errores_modelos,file = "MODELOS_STAN/Medidas_Errores_Modelos.csv",row.names = FALSE)

datos_graf_medidas <- medidas_errores_modelos %>%
  mutate(Variable = reorder(Variable,WAIC)) %>%
  select(-Tiempo) %>%
  gather(Medida,Valor,-Variable,-Tipo) %>%
  group_by(Variable,Medida) %>%
  spread(Tipo,Valor) %>%
  mutate(Dif = Nacional - Jerárquico) %>%
  gather(Tipo,Valor,Jerárquico,Nacional,Departamental) %>%
  ungroup %>%
  mutate(Tipo = factor(Tipo,levels = c("Jerárquico","Departamental","Nacional"), ordered = T)) %>% 
  arrange(Variable,desc(Tipo)) %>%
  mutate(Nivel = str_sub(Medida,-1),
         Pérdida = if_else(Medida == "WAIC", "WAIC", str_sub(Medida,2,2)))

datos_graf_medidas %>%
  filter(Tipo != "Departamental",Pérdida != "WAIC") %>% #OJO NO SE MUESTRA EL WAIC EN EL GRÁFICO
  {ggplot(.,aes(y=Variable,x=Valor,color = Nivel)) +
      geom_point(data = filter(.,Tipo == "Nacional"),
                 size=rel(2)) +
      geom_path(data = filter(.,Dif>0),
                aes(group = Variable),arrow = arrow(length = unit(0.075, "inches"))) +
      facet_grid(Nivel~Pérdida,
                 scales="free_x",
                 labeller = labeller(Pérdida = c(A = "Absoluto",
                                                 C = "Cuadrático",
                                                 "T" = "Tolerancia 1.5 pp",
                                                 WAIC = "WAIC"),
                                     Nivel = c(C = "Comunas",
                                               D = "Departamentos",
                                               N = "Nacional"))) +
      scale_color_manual(values = paleta_tesis_fn$COLOR[c(2,3,1)]) +
      labs(title = "Pasar de modelado Nacional a Jerárquico disminuye los errores",
           x = "Valor de la medida de error",
           y = "Variable explicativa del modelo") +
      theme_linedraw() +
      theme(legend.position = "none",
            panel.grid = element_blank(),
            strip.background = element_rect(fill="transparent"),
            strip.text = element_text(color = "black", size = rel(1.7)),
            axis.text.y = element_text(size = rel(1.3)),
            axis.text.x = element_text(size = rel(1.4)),
            plot.margin = margin(r = 10),
            plot.title = element_text(margin = margin(t = 10, b = 10), size = rel(1.9)),
            axis.title.x = element_text(margin = margin(t = 15, b = 7), size = rel(1.7)),
            axis.title.y = element_text(margin = margin(r = 15, l = 7), size = rel(1.7)))} %>%
  ggsave(filename = "MODELOS_STAN/Graf_Errores_Modelos_Individuales.pdf",plot = .,
         device = cairo_pdf, width = 12, height = 8)
