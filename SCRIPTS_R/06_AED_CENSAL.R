############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
#################################################### AED ###################################################
############################################################################################################
############################################## DATOS CENSALES ##############################################
############################################################################################################


#### TEMA BASE GGPLOT2 ####
lucify_basics <- function(){
  theme(line = element_line(colour = rgb(red = 102, green = 102, blue = 102, maxColorValue = 255)),
        rect = element_rect(colour = rgb(red = 198, green = 198, blue = 198, maxColorValue = 255)),
        text = element_text(family = "AvantGarde Bk BT", 
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

#### GEOFACET DISTRIBUCIONES ####

print.noquote("###############################################################")
print.noquote("########### Generando geofacets distribuciones votos ##########")
print.noquote("###############################################################")

geofacet_distr_cat_dpto <- function(aaaa,cat,nombre,color){
  
  if(aaaa == 2007){
    datos <-  datos_censales %>% 
      filter(AÑO == aaaa) %>% 
      inner_join(COMUNAS_2007) %>% 
      filter(COD_REG %in% filter(fr_anc_reg,col<=6)$code) %>% 
      select(CODGEO,COD_DPTO:NOM_REG,cat) %>% 
      rename(Pct = UQ(cat)) %>% 
      filter(!is.na(Pct))
  } else{
    datos <-  datos_censales %>% 
      filter(AÑO == aaaa) %>% 
      inner_join(COMUNAS_2012) %>% 
      filter(COD_REG %in% filter(fr_anc_reg,col<=6)$code) %>% 
      select(CODGEO,COD_DPTO:NOM_REG,cat) %>% 
      rename(Pct = UQ(cat)) %>% 
      filter(!is.na(Pct))
  }
  
  pct_comuna_sub <- datos %>% 
    ggplot(aes(x="Metrópoli entera",y=Pct)) + 
    geom_hline(aes(yintercept = median(Pct)),color="gray20") + 
    geom_hline(aes(yintercept = quantile(Pct,0.25)),color = "gray50") + 
    geom_hline(aes(yintercept = quantile(Pct,0.75)),color = "gray50") + 
    geom_violin(fill = "transparent", color = color) + 
    scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"), trans = "log1p") + 
    scale_x_discrete(position = "top") +
    theme_minimal() + 
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(margin = margin(30,0,10,0), size = 10),
          axis.text.y = element_text(size = 15))
  
  pct_comuna_dpto <- datos %>% 
    mutate(MEDIANA_NAL = median(Pct)) %>% 
    group_by(COD_REG) %>% 
    mutate(MEDIANA_REG = median(Pct)) %>% 
    ungroup %>% 
    mutate(NOM_REG = reorder(NOM_REG,MEDIANA_REG)) %>% 
    group_by(COD_DPTO) %>% 
    mutate(MEDIANA_DPTO = median(Pct)) %>% 
    ungroup %>% 
    mutate(COD_DPTO = reorder(COD_DPTO,MEDIANA_DPTO)) %>% 
    ggplot(aes(x = COD_DPTO, y = Pct)) + 
    geom_hline(aes(yintercept = median(Pct)),color="gray20") + 
    geom_hline(aes(yintercept = quantile(Pct,0.25)),color = "gray50") + 
    geom_hline(aes(yintercept = quantile(Pct,0.75)),color = "gray50") + 
    geom_violin(aes(alpha = MEDIANA_DPTO/MEDIANA_NAL), fill = color) + 
    geofacet::facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6), label = "name", scales = "free_x") + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"),trans = "log1p") + 
    theme_minimal() + 
    labs(title = paste(nombre,"en",aaaa,sep = " "), 
         subtitle = "% de la población comunal por departamento") + 
    theme(legend.position = "none", 
          panel.grid = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.x = element_text(margin = margin(30,0,10,0), size = 10),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),#element_text(size = 15),
          strip.text.x = element_text(size = 15, margin = margin(10,0,0,0)),
          plot.title = element_text(size = 30, hjust = 0.5),
          plot.subtitle = element_text(margin = margin(5,0,25,0), size = 20, hjust = 0.5))
  
  ggplot(tibble(x=0:1,y=0:1),aes(x,y)) +
    theme_void() +
    annotation_custom(grob = ggplotGrob(pct_comuna_dpto),
                      xmin = 0,xmax = 1,ymin = 0,ymax = 1) +
    annotation_custom(grob = ggplotGrob(pct_comuna_sub),
                      xmin = 0.82,xmax = 0.92,ymin = 0.8,ymax = 0.95) %>%
    return(.)
  
}


datos_censales %>% 
  select(-c(CODGEO:Pob)) %>% colnames() %>% 
  tibble(Cat = ., 
         Nombre = c("Hombres","Mujeres",
                    "0 a 17 años", "18 a 24 años", "25 a 39 años", "40 a 54 años", "55 a 64 años", "65+ años",
                    "Franceses", "Extranjeros",
                    "Agricultores", "Artesanos, comerciantes y empresarios", "Cuadros y prof. intel. sup.",
                    "Profesiones intermediarias", "Empleados", "Obreros", "Retirados","Otras personas sin actividad",
                    "Inmigrantes", "Locales"),
         Color = c("Derecha","Izquierda",
                   familias_politicas[-1],
                   "Derecha","Izquierda",
                   familias_politicas,NA,
                   "Derecha","Izquierda")) %>% 
  mutate(Aux1 = 2007, Aux2 = 2012) %>% 
  gather(Aux,Año,Aux1,Aux2) %>% 
  select(-Aux) %>% 
  mutate(Archivo = paste("AED/CENSALES/Distr_Dptos/Geofacet_Distr_por_Dpto_",
                         Cat,
                         "_",
                         Año,
                         ".pdf",
                         sep = "")) %>% 
  sample_n(3) %>% 
  pmap(~ filter(paleta_tesis_fn, FAMILIA == ..3) %>% 
         extract2("COLOR") %>% 
         {if_else(length(.)==0,"gray98",.) %>% 
         {geofacet_distr_cat_dpto(aaaa = ..4,cat = ..1, nombre = ..2, color = .)} %T>% 
             ggsave(plot = ., width = 25, height = 25, device = cairo_pdf, filename =..5))

