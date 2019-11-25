############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
#################################################### AED ###################################################
############################################################################################################
############################################ DATOS ELECTORALES #############################################
############################################################################################################

library(sf)
library(cartogram)
library(cowplot)

#### TEMA BASE GGPLOT2 ####
lucify_basics <- function(){
  theme(line = element_line(colour = rgb(red = 102, green = 102, blue = 102, maxColorValue = 255)),
        rect = element_rect(colour = rgb(red = 198, green = 198, blue = 198, maxColorValue = 255)),
        text = element_text(#family = "AvantGarde Bk BT", 
                            colour = rgb(red = 60, green = 60, blue = 60, maxColorValue = 255),
                            size = 17),
        axis.title.x = element_text(size = rel(1.2),
                                    margin = margin(t = 15)),
        axis.title.x.top = element_text(size = rel(1.2),
                                        margin = margin(b = 15)),
        axis.title.y = element_text(size = rel(1.2),
                                    margin = margin(r = 30)),
        axis.title.y.right = element_text(size = rel(1.2),
                                          margin = margin(l = 30)),
        axis.text.x = element_text(margin = margin(t = 8)),
        axis.text.x.top = element_text(margin = margin(b = 8)),
        axis.text.y = element_text(margin = margin(r = 8)),
        axis.text.y.right = element_text(margin = margin(l = 8)),
        axis.ticks = element_blank(),
        legend.margin = margin(t = 15, r = 15, b = 15, l = 15),
        legend.title = element_text(size = rel(1.15)),
        panel.spacing = unit(15,"pt"),
        plot.title = element_text(size = rel(1.3),
                                  hjust = 0.5,
                                  margin = margin(b = 20)),
        plot.subtitle = element_text(hjust = 0.5, 
                                     margin = margin(t = -12.5, b = 20)),
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
        strip.text = element_text(size = rel(1.1),
                                  margin = margin(t = 8, r = 8, b = 8, l = 8)))
}
extrafont::loadfonts(device = "win")
theme_set(theme_minimal() + lucify_basics())

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

datos_P12 <- filter(prueba_datos_unificados, ELECCION == "Presidenciales 2012")

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

escala_mapas <- c(paleta_tesis_fn$COLOR[3], "white", paleta_tesis_fn$COLOR[2])

#### Mapa Comunas

shape_votos_P12 <- left_join(datos_P12,mapa_comunas, by = c("CODGEO" = "insee")) %>% 
  mutate(Pct = VOT_CANDIDATO/INSCRITOS) %>% 
  left_join(mutate(muestra, Muestra = TRUE,COD_REG = as.character(COD_REG))) %>% 
  st_as_sf %>% 
  st_transform(crs = 25832)

cuantiles_votos <- quantile(shape_votos_P12$Pct,c(0,.25,.45,.5,.55,.75,0.95,1))

distr_votos_br_p12 <- shape_votos_P12 %>% 
  ggplot() + 
  geom_histogram(aes(x=Pct), size = rel(0.1),
                 binwidth = 0.0025, fill = paleta_tesis_fn$COLOR[5], color = paleta_tesis_fn$COLOR[2]) + 
  annotate("rect",
           xmin=cuantiles_votos[2], xmax = cuantiles_votos[6], ymin = 0, ymax = 700,
           color = paleta_tesis_fn$COLOR[1], fill = paleta_tesis_fn$COLOR[1], alpha = 0.05) + 
  annotate("segment",
           x=cuantiles_votos[4], xend = cuantiles_votos[4], y = 0, yend = 700,
           color = paleta_tesis_fn$COLOR[3], linetype = 1) + 
  annotate("text",x=cuantiles_votos[2]-.03,y=700,
           label=paste("Q1 \n",round(100*cuantiles_votos[2],1) %>% paste0("%"))) + 
  annotate("text",x=cuantiles_votos[6]+.03,y=700,
           label=paste("Q3 \n",round(100*cuantiles_votos[6],1) %>% paste0("%"))) + 
  scale_x_continuous(breaks = cuantiles_votos[c(1,4,8)],
                     labels = function(x) round(100*x,1) %>% paste("%",sep = "")) + 
  labs(y = "Número de comunas",
       x = "% bruto de votos") + 
  theme_half_open() + 
  theme(axis.ticks.x = element_blank(),
        axis.title = element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1)))

ggsave(plot = distr_votos_br_p12, width = 22.5/3, height = 17/3, device = cairo_pdf,
       filename = "AED/ELECTORALES/Distr_Votos_Br_P12_FN.pdf")

distr_votos_br_p12_muestra <- shape_votos_P12 %>% 
  mutate(Muestra = case_when(Muestra ~ "En muestra", 
                             T ~ "Fuera de muestra")) %>% 
  {ggplot(.,aes(x=Pct,stat(density),fill=Muestra,color=Muestra)) + 
      geom_histogram(binwidth = 0.0025, alpha = 0.5, size = rel(0.001)) + 
      facet_wrap(~Muestra,nrow = 1, scales = "free_x") + 
      scale_x_continuous(labels = function(x) round(100*x,2) %>% paste("%",sep = "")) + 
      scale_fill_manual(values = paleta_tesis_fn$COLOR[c(7,4)]) + 
      scale_color_manual(values = paleta_tesis_fn$COLOR[c(7,4)]) +
      labs(y = "Densidad",
           x = "% bruto de votos") + 
      theme_classic() + 
      theme(axis.ticks.x = element_blank(),
            strip.text = element_text(size = rel(1.5)),
            axis.title = element_text(size = rel(1.5)),
            axis.text = element_text(size = rel(1.15)),
            legend.position = "none")}

ggsave(plot = distr_votos_br_p12_muestra, width = 22.5/3, height = 17/3, device = cairo_pdf,
       filename = "AED/ELECTORALES/Distr_Votos_Br_P12_FN_MUESTRA.pdf")

mapa_votos_br_p12_mediana <- shape_votos_P12 %>% 
  ggplot() + 
  geom_sf(aes(fill = Pct), color = "transparent", size = rel(0.25)) + 
  geom_sf(data = mapa_dptos, fill = "transparent", color = "gray85", size = rel(0.25)) + 
  scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                   paleta_tesis_fn$COLOR[c(5,2)],"#0c1740"),
                       values = scales::rescale(cuantiles_votos),
                       breaks = cuantiles_votos[c(1,4,8)],
                       labels = round(100*cuantiles_votos[c(1,4,8)], 1) %>% 
                       {paste(c("Min: ", "Mediana: ", "Max: "),.,"%",sep="")}) + 
  labs(title = "% bruto de votos por comuna") + 
  theme_void() + 
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.text = element_text(size = rel(1.25)),
        legend.title = element_text(size = rel(1.25)),
        legend.position = c(0.07,0.5))

ggsave(plot = mapa_votos_br_p12_mediana, width = 22.5/3, height = 17/3, device = cairo_pdf,
       filename = "AED/ELECTORALES/Mapa_Votos_Br_P12_FN.pdf")


mapa_votos_br_p12_muestra <- shape_votos_P12 %>% 
  filter(Muestra) %>% 
  ggplot() + 
  geom_sf(aes(fill = Pct), color = "transparent", size = rel(0.25)) + 
  geom_sf(data = mapa_dptos, fill = "transparent", color = "gray85", size = rel(0.3)) + 
  scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                   paleta_tesis_fn$COLOR[c(5,2)],"#0c1740"),
                       values = scales::rescale(cuantiles_votos),
                       breaks = cuantiles_votos[c(1,4,8)],
                       labels = round(100*cuantiles_votos[c(1,4,8)], 1) %>% 
                       {paste(c("Mín. real: ", "Mediana real: ", "Máx. real: "),.,"%",sep="")},
                       limits = cuantiles_votos[c(1,8)]) + 
  labs(title = "Solo comunas muestreadas") + 
  theme_void() + 
  theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
        legend.text = element_text(size = rel(1.15)),
        legend.title = element_text(size = rel(1.15)),
        legend.position = c(0.06,0.5))

ggsave(plot = mapa_votos_br_p12_muestra, width = 22.5/3, height = 17/3, device = cairo_pdf,
       filename = "AED/ELECTORALES/Mapa_Votos_Br_P12_FN_MUESTRA.pdf")


#### GEOFACET DISTRIBUCIONES ####

print.noquote("###############################################################")
print.noquote("########### Generando geofacets distribuciones votos ##########")
print.noquote("###############################################################")


datos_distr_por_anc_reg <- datos_electorales_completos %>% 
    filter(FAMILIA == "FN", ELECCION == "Presidenciales 2012") %>% 
    inner_join(COMUNAS_2012) %>% 
    group_by(COD_REG) %>% 
    mutate(MEDIANA = median(PCT_VOTOS_BR)) %>% 
    ungroup() %>% 
    mutate(MEDIANA_NAL = median(PCT_VOTOS_BR)) %>% 
    mutate(NOM_REG = reorder(NOM_REG,MEDIANA),code=COD_REG)
  
mapa_pct_anc_reg <- datos_distr_por_anc_reg %>% 
  group_by(COD_REG,NOM_REG) %>%
  summarise_at(c("INSCRITOS","VOT_CANDIDATO"),sum) %>% 
  mutate(Pct = VOT_CANDIDATO/INSCRITOS) %>% 
  left_join(fronteras_reg) %>% 
  st_as_sf %>% 
  st_transform(crs = 25832)
  
{ggplot(mapa_pct_anc_reg, aes(fill = Pct, label = paste(round(100*Pct,1),"%",sep=""))) + 
    geom_sf(color = "gray85", size = rel(0.25)) + 
    geom_sf_text(color = "gray25") + 
    annotate("text", y = 5625000, x = 400000,
             label = "Máximo en \n Picardie", color = paleta_tesis_fn$COLOR[2]) + 
    geom_curve(y = 5625000, x = 300000, yend = 5550000, xend = 165000, 
               curvature = 0.2, arrow = arrow(), 
               color = paleta_tesis_fn$COLOR[2]) + 
    annotate("text", y = 5625000, x = -275000, 
             label = "Mínimo en \n Île de France", color = paleta_tesis_fn$COLOR[3]) + 
    geom_curve(y = 5625000, x = -160000, yend = 5430000, xend = 0, 
               curvature = -0.2, arrow = arrow(), 
               color = paleta_tesis_fn$COLOR[3]) + 
    annotate("text", y = 5125000, x = 455000,
             label = "Cerca del \n promedio en \n Rhône-Alpes", color = paleta_tesis_fn$COLOR[1]) + 
    scale_fill_gradient(low = paleta_tesis_fn$COLOR[5], high = paleta_tesis_fn$COLOR[1],
                        breaks = c(0,.1,.15,.2), labels = c("0%","10%","15%","20%"), limits = c(0,.2)) + 
    labs(title = "% bruto de votos por región") + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.1,0.5))} %>% 
  ggsave(plot = ., width = 22.5/3, height = 17/3, device = cairo_pdf,
         filename = "AED/ELECTORALES/Pct_Br_Reg_P12_FN.pdf")
  

{ggplot(datos_distr_por_anc_reg, aes(y=stat(density))) + 
    geom_histogram(binwidth = 0.01, size = rel(0.25),
                   aes(x = PCT_VOTOS_BR, fill = MEDIANA/MEDIANA_NAL)) +
    geom_density(data = select(datos_distr_por_anc_reg,-COD_REG),
                 fill="transparent",color="black", size = rel(0.25),
                 aes(x = PCT_VOTOS_BR, fill = MEDIANA/MEDIANA_NAL, stat(density))) +
    geom_vline(xintercept = unique(datos_distr_por_anc_reg$MEDIANA_NAL)) + 
    geom_text(data = distinct(datos_distr_por_anc_reg,NOM_REG,COD_REG),
              x=0.5, aes(y = 3, label = str_replace_all(NOM_REG,"-","\n"), group = COD_REG), 
              size = rel(2.3)) + 
    facet_geo(~COD_REG, grid = filter(fr_anc_reg, col <= 6), 
              label = "name", labeller = label_wrap_gen(10)) + 
    scale_fill_gradientn(colours =  c(paleta_tesis_fn$COLOR[3], "white", paleta_tesis_fn$COLOR[2]), 
                         values = c(0,0.47,0.5,.53,1), 
                         breaks = c(0.5,1,1.5), limits = c(0.5,1.5)) + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_x_continuous(breaks = seq(0,.6,0.2),
                       labels = paste(seq(0,60,20),"%",sep=""),trans = "log1p",
                       limits = c(0,.9)) + 
    scale_y_continuous(trans = "log1p") + 
    labs(title = "% de votos por comuna para la metrópoli entera y cada región",
         fill = "Cociente de medianas") + 
    theme(legend.direction = "horizontal",
          legend.position = c(0.5,0.03),
          strip.text = element_blank(),
          #strip.text.x = element_text(size = rel(0.25), margin = margin(10,0,0,0)),
          plot.title = element_text(margin = margin(b = 20), size = rel(1)),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = rel(0.5)))} %>% 
  ggsave(plot = ., width = 22.5/3, height = 17/3, device = cairo_pdf,
         filename = "AED/ELECTORALES/Geofacet_Reg_P12_FN.pdf")


#### Por Departamentos ####

datos_distr_dptos <- datos_electorales_completos %>% 
  filter(FAMILIA == "FN", ELECCION == "Presidenciales 2012") %>% 
  inner_join(COMUNAS_2012) %>% 
  group_by(COD_DPTO) %>% 
  mutate(MEDIANA = median(PCT_VOTOS_BR)) %>% 
  ungroup() %>% 
  mutate(MEDIANA_NAL = median(PCT_VOTOS_BR)) %>% 
  mutate(NOM_DPTO = reorder(NOM_DPTO,MEDIANA),code_insee=COD_DPTO)

mapa_pct_dptos <- datos_distr_dptos %>% 
  group_by(COD_DPTO,NOM_DPTO,code_insee) %>%
  summarise_at(c("INSCRITOS","VOT_CANDIDATO"),sum) %>% 
  mutate(Pct = VOT_CANDIDATO/INSCRITOS) %>% 
  left_join(mapa_dptos) %>% 
  st_as_sf %>% 
  st_transform(crs = 25832)

{ggplot(mapa_pct_dptos, aes(fill = Pct, label = paste(round(100*Pct,1),"%",sep=""))) + 
    geom_sf(color = "gray85", size = rel(0.25)) + 
    scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                     paleta_tesis_fn$COLOR[c(5,2)],"#0c1740"),
                         values = scales::rescale(cuantiles_votos[c(1:7)]),
                         labels = function(x) round(100*x, 1) %>% paste("%",sep="")) + 
    labs(title = "% bruto de votos por departamento") + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.1,0.5))} %>% 
  ggsave(plot = ., width = 22.5/3, height = 17/3, device = cairo_pdf,
         filename = "AED/ELECTORALES/Pct_Br_Dpto_P12_FN.pdf")

lim_y <- max(shape_votos_P12$Pct)
  
pct_comuna_sub <- shape_votos_P12 %>% 
    ggplot(aes(x="Metrópoli entera", y= Pct)) + 
    geom_hline(yintercept = cuantiles_votos[4],color="gray20", size = rel(0.25)) + 
    geom_hline(yintercept = cuantiles_votos[2],color = "gray50", size = rel(0.25)) + 
    geom_hline(yintercept = cuantiles_votos[6],color = "gray50", size = rel(0.25)) + 
    geom_violin(fill = "transparent", size = rel(0.3)) +
    scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"),  
                       limits = c(0,lim_y)) + 
    scale_x_discrete(position = "top") +
    theme_minimal() + 
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(margin = margin(30,0,10,0), size = rel(1)),
          axis.text.y = element_text(size = rel(0.75)))
  
pct_comuna_dpto <- shape_votos_P12 %>% 
  mutate(MEDIANA_NAL = cuantiles_votos[4]) %>% 
  group_by(COD_REG) %>% 
  mutate(MEDIANA_REG = median(Pct)) %>% 
  ungroup %>% 
  mutate(NOM_REG = reorder(NOM_REG,MEDIANA_REG)) %>% 
  group_by(COD_DPTO) %>% 
  mutate(MEDIANA_DPTO = median(Pct)) %>% 
  ungroup %>% 
  mutate(COD_DPTO = reorder(COD_DPTO,MEDIANA_DPTO)) %>% 
  {ggplot(data = .) + 
      geom_hline(yintercept = cuantiles_votos[4],color="gray20", size = rel(0.1)) + 
      geom_hline(yintercept = cuantiles_votos[2],color = "gray50", size = rel(0.1)) + 
      geom_hline(yintercept = cuantiles_votos[6],color = "gray50", size = rel(0.1)) + 
      geom_violin(aes(x = COD_DPTO, y = Pct, fill = MEDIANA_DPTO/MEDIANA_NAL), size = rel(0.1)) + 
      facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6), label = "name", scales = "free_x",
                labeller = label_wrap_gen(20)) + 
      scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[c(6,3)],"white",
                                       paleta_tesis_fn$COLOR[5],"#0c1740"),
                           values = scales::rescale(cuantiles_votos[-c(5,8)]/cuantiles_votos[4]), 
                           breaks = c(0.1,1,1.5), limits = c(0.1,1.5), labels = c("0","1","1.5")) +
      # scale_fill_gradientn(colours =  c(paleta_tesis_fn$COLOR[3], "white", paleta_tesis_fn$COLOR[2]), 
      #                      values = scales::rescale(c(0.1,0.975,1,1.025,1.5)), 
      #                      breaks = c(0.1,1,1.5), limits = c(0.1,1.5), labels = c("0","1","1.5")) + 
      scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"), 
                         limits = c(0,lim_y), trans = "log1p") + 
      theme_minimal() + 
      labs(title = "% de votos por comuna por departamento",
           fill = "Cociente de medianas") + 
      theme(legend.direction = "horizontal",
            legend.position = c(0.5,0.05),
            legend.text = element_text(size = rel(1)),
            legend.title = element_text(size = rel(1)),
            legend.key.height = unit(10,"pt"),
            strip.text = element_text(size = rel(0.7), margin = margin(4,4,4,4)),
            panel.grid = element_blank(), 
            axis.title.y = element_blank(),
            axis.text.x = element_text(margin = margin(1,0,1,0), size = rel(0.5)),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_blank(),
            plot.title = element_text(size = rel(1), hjust = 0.5, margin = margin(b = 1)))}
  
{ggplot(tibble(x=0:1,y=0:1),aes(x,y)) +
    theme_void() +
    annotation_custom(grob = ggplotGrob(pct_comuna_dpto),
                      xmin = 0,xmax = 1,ymin = 0,ymax = 1) +
    annotation_custom(grob = ggplotGrob(pct_comuna_sub),
                      xmin = 0.8,xmax = 1,ymin = 0.825,ymax = 1) + 
    theme(plot.margin = margin(0,0,0,0))} %>% 
  ggsave(plot = ., width = 22.5/3, height = 17/2.85, device = cairo_pdf,
         filename = "AED/ELECTORALES/Geofacet_Dpto_P12_FN.pdf")


# geofacets_distr_por_reg <-list(elección = unique(datos_electorales_completos$ELECCION),
#                                familia = familias_politicas) %>% 
#   cross_df %>% 
#   mutate(Archivo = paste("AED/ELECTORALES/Geofacet_Reg/Geofacet_Distr_por_Reg_",
#                          str_remove_all(elección,"(residenciales 20)|(egislativas 20)"),
#                          "_",
#                          str_replace_all(familia, " ", "_"),
#                          ".pdf",
#                          sep = "")) %>% 
#   pmap(~genera_geofacet_distr_votos(..1,..2) %T>% 
#          ggsave(plot = ., width = 18, height = 15, device = cairo_pdf,
#                 filename =..3))
# 
# geofacets_distr_por_dpto <- list(elección = unique(datos_electorales_completos$ELECCION),
#                                  familia = familias_politicas) %>% 
#   cross_df %>% 
#   mutate(Archivo = paste("AED/ELECTORALES/Geofacet_Dpto/Geofacet_Distr_por_Dpto_",
#                          str_remove_all(elección,"(residenciales 20)|(egislativas 20)"),
#                          "_",
#                          str_replace_all(familia, " ", "_"),
#                          ".pdf",
#                          sep = "")) %>% 
#   pmap(~geofacet_pct_votos_dpto(..1,..2) %T>% 
#          ggsave(plot = ., width = 25, height = 25, device = cairo_pdf,
#                 filename =..3))
                                 

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

dorling_errores_muestra_P12 <- dorling_dptos_votos_P12 %>%
  mutate(Error_P12 = Pct_P12_Dpto_Real - Pct_P12_Dpto_Muestra) %>% 
  ggplot() + 
  geom_sf(data = fronteras_reg_dorling, fill = "transparent", color = paleta_tesis_fn$COLOR[1]) + 
  geom_sf(aes(fill = Error_P12)) + 
  geom_sf_text(aes(label = code_insee), color = "gray95", size = rel(5)) + 
  annotate("text", y = 5600000, x = 390000, size = rel(6),
           label = "Máximo error en \n Bas-Rhin (+4.2 pp)", color = paleta_tesis_fn$COLOR[1]) + 
  geom_curve(y = 5525000, x = 500000, yend = 5400000, xend = 440000, 
             curvature = -0.3, arrow = arrow(), 
             color = paleta_tesis_fn$COLOR[1]) + 
  scale_fill_gradientn(colours = paleta_tesis_fn$COLOR[c(6,3,2,3,6)],
                       values = scales::rescale(c(-.05,-.01,0,.01,.05)),
                       limits = c(-.05,.05),
                       labels = function(x){paste(100*x,"pp")}) +
  guides(fill = guide_legend(title = "Error de \n estimación")) + 
  theme_void() + 
  theme(legend.position = c(0.07,0.5),
        legend.text = element_text(size = rel(1.5)),
        legend.title = element_text(size = rel(1.5), hjust = 0.5))


ggsave(plot = dorling_errores_muestra_P12, width = 22.5/3, height = 17/3, device = cairo_pdf,
       filename = "AED/ELECTORALES/Dorling_Errores_P12_FN_MUESTRA.pdf")

#### Asociaciones ####

datos_asociaciones <- shape_votos_P12 %>% 
  as.data.frame %>% 
  as_tibble() %>% 
  filter(Muestra) %>% 
  select(COD_DPTO,Pct,Hom:Esc) %>% 
  select(-Hom,-Fra,-Loc,-starts_with("Ocu")) %>% 
  gather(Cats,Pct_Cats,-Pct,-COD_DPTO) %>%
  left_join(equivalencia_variables) %>%
  group_by(Etiqueta) %>% 
  mutate(Corr = cor(Pct,Pct_Cats)) %>% 
  ungroup %>% 
  mutate(Etiqueta = reorder(Etiqueta,Corr))

datos_asociaciones  %>% 
  {ggplot(.,aes(x=Pct_Cats,y=Pct)) + 
      geom_hex(bins = 75) + 
      facet_wrap(~Etiqueta,scales = "free_x", labeller = label_wrap_gen(20)) + 
      scale_y_continuous(trans="logit", breaks = c(0.05,0.175,0.62), 
                         labels = function(x) round(100*x,1) %>% paste("%",sep="")) + 
      scale_x_continuous(labels = function(x) round(100*x,1) %>% paste("%",sep="")) + 
      scale_fill_gradient(low = paleta_tesis_fn$COLOR[2], high = "#0c1740") + 
      labs(x = "Variable Explicativa", 
           y = "% bruto de votos en escala logística") + 
      theme(axis.title = element_text(size = rel(1.3)),
            axis.text = element_text(size = rel(0.8)),
            strip.text = element_text(size = rel(1)),
            panel.grid.major = element_blank(),
            legend.position = "none")} %>% 
  ggsave(plot = ., width = 19.5, height = 12.5, device = cairo_pdf,
         filename = "AED/Asociaciones_MUESTRA.pdf")

datos_asociaciones %>% 
  {ggplot(.,aes(x=Pct_Cats,y=Pct)) + 
      geom_smooth(aes(group = COD_DPTO), method = "lm", se = FALSE,
                  color=paleta_tesis_fn$COLOR[3], size = rel(0.1)) + 
      geom_smooth(method = "lm", se = FALSE, 
                  color=paleta_tesis_fn$COLOR[2], 
                  size = rel(0.8),) +
      facet_wrap(~Etiqueta,scales = "free", labeller = label_wrap_gen(20)) + 
      scale_y_continuous(trans="logit", breaks = c(0.01,0.05,0.175,0.3,0.62), 
                         labels = function(x) round(100*x,1) %>% paste("%",sep="")) + 
      scale_x_continuous(labels = function(x) round(100*x,1) %>% paste("%",sep="")) + 
      labs(title = "Tendencias ingenuas",
           x = "Variable Explicativa", 
           y = "% bruto de votos \n en escala logística") + 
      theme(plot.title = element_text(size = rel(1.2), margin = margin(b = 5)),
            axis.text.y = element_text(size = rel(0.75)),
            axis.text.x = element_text(size = rel(0.65)),
            axis.title = element_text(size = rel(1)),
            strip.text = element_text(size = rel(0.8)))} %>% 
  ggsave(plot = ., width = 22.5/1.5, height = 17/1.5, device = cairo_pdf,
         filename = "AED/Tend_Ingenuas_Todas_MUESTRA.pdf")

