
library(loo) 
# CUIDADO CON LAS OPCIONES!!
#options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
library(sf)
library(cartogram)
library(rstan) # Versión 2.18.2

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

# pred_post_dptal <- predictiva_posterior %>%
#   mutate(Pct_Comuna_Obs = VOT_CANDIDATO/INSCRITOS,
#          Pct_Comuna_Est = VOT_SIMUL/INSCRITOS) %>% 
#   group_by(COD_DPTO,Simul) %>% 
#   summarise(INSCRITOS = sum(INSCRITOS),
#             VOT_CANDIDATO = sum(VOT_CANDIDATO),
#             VOT_SIMUL = sum(VOT_SIMUL),
#             Q025_Obs = quantile(Pct_Comuna_Obs,0.025),
#             Q025_Est = quantile(Pct_Comuna_Est, 0.025),
#             Q975_Obs = quantile(Pct_Comuna_Obs,0.975),
#             Q975_Est = quantile(Pct_Comuna_Est, 0.975)) %>% 
#   mutate(Pct_Obs = VOT_CANDIDATO/INSCRITOS,
#          Pct_Est = VOT_SIMUL/INSCRITOS) %>% 
#   ungroup %>% 
#   select(-c(INSCRITOS:VOT_SIMUL))
# 
# pred_post_resumen <- pred_post_dptal %>% 
#   group_by(COD_DPTO) %>% 
#   summarise_at(c("Pct_Obs","Q025_Obs","Q975_Obs"), unique) 

# ggplot(data = pred_post_dptal %>% mutate(COD_DPTO = reorder(COD_DPTO,Pct_Obs))) + 
#   geom_histogram(aes(x=Pct_Est,group = COD_DPTO, stat(density)),
#                  binwidth = .001, fill = paleta_tesis_fn$COLOR[3]) +
#   geom_vline(data = pred_post_resumen,
#              aes(xintercept = Pct_Obs), 
#              color = paleta_tesis_fn$COLOR[2]) + 
#   facet_wrap(~COD_DPTO, scales = "free", nrow = 8) + 
#   theme_bw() +
#   theme(axis.text.y = element_blank(),
#         axis.title.y = element_blank())



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

### Mapas votos ####

shape_votos_P12 <- left_join(datos_P12,mapa_comunas, by = c("CODGEO" = "insee")) %>% 
  mutate(Pct = VOT_CANDIDATO/INSCRITOS) %>% 
  left_join(mutate(muestra, Muestra = TRUE)) %>% 
  st_as_sf %>% 
  st_transform(crs = 25832)

dorling_dptos_votos_P12 <- datos_P12 %>% 
  group_by(COD_DPTO) %>% 
  summarise(Pct_P12_Dpto_Real = sum(VOT_CANDIDATO)/sum(INSCRITOS)) %>% 
  {left_join(dorling_dptos,.,
             by = c("code_insee"="COD_DPTO"))}
            
dorling_dptos_votos_P12 <- shape_votos_P12 %>% 
  filter(Muestra) %>% 
  as_tibble %>% 
  group_by(COD_DPTO) %>% 
  summarise(Pct_P12_Dpto_Muestra = sum(VOT_CANDIDATO)/sum(INSCRITOS)) %>% 
  {left_join(dorling_dptos_votos_P12,.,
             by = c("code_insee"="COD_DPTO"))}

cuantiles_votos <- quantile(shape_votos_P12$Pct,c(0,.45,.5,.55,0.95,1))
cuantiles_votos_dptos <- datos_P12 %>% 
  group_by(COD_DPTO) %>% 
  summarise(Pct = sum(VOT_CANDIDATO)/sum(INSCRITOS)) %>% 
  extract2("Pct") %>% 
  quantile(c(0,.45,.5,.55,1))

mapa_votos_br_p12_mediana <- ggplot(shape_votos_P12) + 
  geom_sf(aes(fill = Pct), color = "transparent") + 
  geom_sf(data = mapa_dptos, fill = "transparent", color = "gray85") + 
  scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                   paleta_tesis_fn$COLOR[c(5,2)],"#0c1740"),
                       values = scales::rescale(cuantiles_votos),
                       breaks = cuantiles_votos[c(1,3,6)],
                       labels = round(100*cuantiles_votos[c(1,3,6)], 1) %>% 
                       {paste(c("Min: ", "Mediana: ", "Max: "),.,"%",sep="")}) + 
  labs(title = "% bruto de votos por comuna") + 
  theme_void() + 
  theme(legend.position = "left")

mapa_votos_br_p12_muestra <- ggplot() + 
  geom_sf(data = filter(shape_votos_P12, Muestra), aes(fill = Pct), color = "transparent") + 
  geom_sf(data = mapa_dptos, fill = "transparent", color = paleta_tesis_fn$COLOR[1]) + 
  scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                   paleta_tesis_fn$COLOR[c(5,2)],"#0c1740"),
                       values = scales::rescale(cuantiles_votos),
                       breaks = cuantiles_votos[c(1,3,6)],
                       labels = round(100*cuantiles_votos[c(1,3,6)], 1) %>% 
                       {paste(c("Min: ", "Mediana: ", "Max: "),.,"%",sep="")},
                       limits = cuantiles_votos[c(1,6)]) + 
  labs(title = "Solo comunas muestreadas") +
  theme_void() + 
  theme(legend.position = "left") 


mapa_votos_br_p12_dptos <- prueba_pred %>% 
  group_by(COD_DPTO) %>% 
  summarise(Pct = sum(VOT_CANDIDATO)/sum(INSCRITOS)) %>% 
  {left_join(mapa_dptos,.,
             by = c("code_insee"="COD_DPTO"))} %>% 
  ggplot() + 
  geom_sf(aes(fill = Pct), color = "transparent") + 
  geom_sf(data = fronteras_reg, fill = "transparent", color = "gray80") + 
  scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                   paleta_tesis_fn$COLOR[c(5,2)]),
                       values = scales::rescale(cuantiles_votos_dptos),
                       breaks = cuantiles_votos_dptos[c(1,3,5)],
                       labels = round(100*cuantiles_votos_dptos[c(1,3,5)], 1) %>% 
                       {paste(c("Min: ", "Mediana: ", "Max: "),.,"%",sep="")}) +
  labs(title = "% bruto de votos por departamento") + 
  theme_void() + 
  theme(legend.position = "left")

dorling_errores_muestra_P12 <- dorling_dptos_votos_P12 %>%
  mutate(Error_P12 = Pct_P12_Dpto_Real - Pct_P12_Dpto_Muestra) %>% 
  ggplot() + 
  geom_sf(data = fronteras_reg_dorling, fill = "transparent", color = paleta_tesis_fn$COLOR[1]) + 
  geom_sf(aes(fill = Error_P12)) + 
  geom_sf_text(aes(label = code_insee), color = "gray95") + 
  annotate("text", y = 5550000, x = 500000,
           label = "Máximo error en \n Bas-Rhin (+4.2 pp)", color = paleta_tesis_fn$COLOR[1]) + 
  geom_curve(y = 5500000, x = 500000, yend = 5400000, xend = 440000, 
             curvature = -0.3, arrow = arrow(), 
             color = paleta_tesis_fn$COLOR[1]) + 
  scale_fill_gradientn(colours = paleta_tesis_fn$COLOR[c(6,3,2,3,6)],
                       values = scales::rescale(c(-.05,-.01,0,.01,.05)),
                       limits = c(-.05,.05),
                       labels = function(x){paste(100*x,"pp")}) +
  labs(title = "Errores departamentales de estimación con muestra") + 
  theme_void()

{ggplot(tibble(x=0:1,y=0:1),aes(x,y)) + 
    annotation_custom(ggplotGrob(mapa_votos_br_p12_mediana),xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1) + 
    annotation_custom(ggplotGrob(mapa_votos_br_p12_dptos),xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1) + 
    annotation_custom(ggplotGrob(mapa_votos_br_p12_muestra),xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5) + 
    annotation_custom(ggplotGrob(dorling_errores_muestra_P12),xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5) + 
    theme_void()} %>% 
  ggsave("Mapa_FN_P12.pdf",.,device = cairo_pdf,
         width = 30, height = 17)


errores_biscale <- medidas_errores %>% 
  filter(Medida == "Error") %>% 
  ungroup %>% 
  right_join(mutate(dorling_dptos_votos_P12, 
                    Error_muestral = Pct_P12_Dpto_Real - Pct_P12_Dpto_Muestra), 
             by = c("COD_DPTO"="code_insee")) %>% 
  st_as_sf %>% 
  st_transform(crs = 25832) %>% 
  mutate(Dif_Error_Medio = Media - Error_muestral) %>% 
  select(COD_DPTO,Error_muestral,Dif_Error_Medio) %>% 
  bi_class(Error_muestral,Dif_Error_Medio,style = "fisher") %>% 
  ggplot() + 
  geom_sf(data = fronteras_reg_dorling, fill = "transparent", color = paleta_tesis_fn$COLOR[1]) + 
  geom_sf(aes(fill = bi_class), show.legend = FALSE) + 
  geom_sf_text(aes(label = COD_DPTO), color = "gray10") + 
  bi_scale_fill(pal = "Brown", dim = 3) + 
  theme_void()

legend_biscale <- bi_pal(pal = "Brown", dim = 3)

fronteras_reg <-  pred_post_dptal %>% 
  distinct(COD_DPTO) %>% 
  left_join(DEPARTAMENTOS) %>% 
  left_join(mapa_dptos, by = c("COD_DPTO"="code_insee")) %>% 
  st_as_sf() %>%
  st_transform(crs = 25832) %>% 
  group_by(COD_REG) %>% 
  summarise(cod_reg = unique(COD_REG))

dorling_dptos <- pred_post_dptal %>% 
  distinct(COD_DPTO) %>% 
  mutate(Iguales = 1) %>% 
  left_join(mapa_dptos, by = c("COD_DPTO"="code_insee")) %>% 
  st_as_sf() %>%
  st_transform(crs = 25832) %>% 
  cartogram::cartogram_dorling("Iguales",0.35,0.1)

fronteras_reg_dorling <- dorling_dptos %>% 
  left_join(DEPARTAMENTOS) %>% 
  group_by(COD_REG) %>% 
  summarise(cod_reg = unique(COD_REG)) %>% 
  st_convex_hull(.)

mapa_pBayes_dptos <- medidas_errores %>% 
  filter(Medida == "pBayes") %>% 
  left_join(mapa_dptos, by = c("COD_DPTO"="code_insee")) %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = Media)) + 
  geom_sf() + 
  geom_sf(data = fronteras_reg, color = "gray85", fill = "transparent") + 
  scale_fill_gradient2(low = paleta_tesis_fn$COLOR[3], 
                       mid = paleta_tesis_fn$COLOR[2], 
                       high = paleta_tesis_fn$COLOR[3], midpoint = 0.5) + 
  theme_minimal() + 
  theme(panel.grid = element_line(color = "transparent"), 
        axis.text = element_blank())

dorling_pBayes_dptos <- medidas_errores %>% 
  filter(Medida == "pBayes") %>% 
  left_join(dorling_dptos, "COD_DPTO") %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = Media)) + 
  geom_sf(data = fronteras_reg_dorling, color = paleta_tesis_fn$COLOR[1], fill = "transparent") + 
  geom_sf() + 
  geom_sf_text(color = "white", aes(label = COD_DPTO), size = rel(5)) +
  scale_fill_gradient2(low = paleta_tesis_fn$COLOR[3], 
                       mid = paleta_tesis_fn$COLOR[2], 
                       high = paleta_tesis_fn$COLOR[3], midpoint = 0.5) + 
  theme_minimal() + 
  theme(panel.grid = element_line(color = "transparent"), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "none")

{ggplot(tibble(x=0:1,y=0:1),aes(x,y)) + 
  annotation_custom(ggplotGrob(mapa_pBayes_dptos),xmin = 0, xmax = 0.5, ymin = 0, ymax = 1) + 
  annotation_custom(ggplotGrob(dorling_pBayes_dptos),xmin = 0.5, xmax = 1, ymin = 0.1, ymax = 0.9) + 
  theme_void()} %>% 
  ggsave("Mapa_pBayes_Dptos.pdf",.,device = cairo_pdf,
         width = 30, height = 17)

mapa_error_dptos <- medidas_errores %>% 
  filter(Medida == "Error") %>% 
  left_join(mapa_dptos, by = c("COD_DPTO"="code_insee")) %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = Media)) + 
  geom_sf() + 
  geom_sf(data = fronteras_reg, color = "gray85", fill = "transparent") + 
  scale_fill_gradient2(low = paleta_tesis_fn$COLOR[3], 
                       mid = paleta_tesis_fn$COLOR[2], 
                       high = paleta_tesis_fn$COLOR[3], midpoint = 0) + 
  theme_minimal() + 
  theme(panel.grid = element_line(color = "transparent"), 
        axis.text = element_blank())

dorling_error_dptos <- medidas_errores %>% 
  filter(Medida == "Error") %>% 
  left_join(dorling_dptos, "COD_DPTO") %>% 
  st_as_sf() %>% 
  ggplot(aes(fill = Media)) + 
  geom_sf(data = fronteras_reg_dorling, color = paleta_tesis_fn$COLOR[1], fill = "transparent") + 
  geom_sf() + 
  geom_sf_text(color = "white", aes(label = COD_DPTO), size = rel(5)) +
  scale_fill_gradient2(low = paleta_tesis_fn$COLOR[3], 
                       mid = paleta_tesis_fn$COLOR[2], 
                       high = paleta_tesis_fn$COLOR[3], midpoint = 0) + 
  theme_minimal() + 
  theme(panel.grid = element_line(color = "transparent"), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.position = "none")

{ggplot(tibble(x=0:1,y=0:1),aes(x,y)) + 
    annotation_custom(ggplotGrob(mapa_error_dptos),xmin = 0, xmax = 0.5, ymin = 0, ymax = 1) + 
    annotation_custom(ggplotGrob(dorling_error_dptos),xmin = 0.5, xmax = 1, ymin = 0.1, ymax = 0.9) + 
    theme_void()} %>% 
  ggsave("Mapa_Error_Dptos.pdf",.,device = cairo_pdf,
         width = 30, height = 17)

