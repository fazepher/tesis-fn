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
    geom_violin(fill = "transparent") + 
    scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"), trans = "log1p", limits = c(0,1)) + 
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
    scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"),trans = "log1p", limits = c(0,1)) + 
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

geofacet_disp_votos_cat_reg <- function(elec,familia,cat,nombre,color){
  
  datos_electorales <- filter(datos_electorales_completos, ELECCION == elec, FAMILIA == familia)
  aaaa <- str_extract_all(elec,"[0-9]{4}") %>% unlist
  
  if(aaaa == 2007){
    datos <-  datos_censales %>% 
      filter(AÑO == aaaa) %>% 
      inner_join(COMUNAS_2007) %>% 
      filter(COD_REG %in% filter(fr_anc_reg,col<=6)$code) %>% 
      select(CODGEO,COD_DPTO:NOM_REG,cat) %>% 
      rename(Pct = UQ(cat)) %>% 
      filter(!is.na(Pct)) %>% 
      inner_join(datos_electorales) %>% 
      group_by(COD_REG) %>% 
      mutate(Alpha = 1/n())
  } else{
    datos <-  datos_censales %>% 
      filter(AÑO == aaaa) %>% 
      inner_join(COMUNAS_2012) %>% 
      filter(COD_REG %in% filter(fr_anc_reg,col<=6)$code) %>% 
      select(CODGEO,COD_DPTO:NOM_REG,cat) %>% 
      rename(Pct = UQ(cat)) %>% 
      filter(!is.na(Pct)) %>% 
      inner_join(datos_electorales) %>% 
      group_by(COD_REG) %>% 
      mutate(Alpha = 1/n())
  }
  
  graf <- ggplot(datos, aes(x=Pct,y=PCT_VOTOS_BR,alpha = Alpha)) + 
        geom_point(color=color, size = rel(0.2)) + 
        geofacet::facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6), label = "name", scales = "free") + 
        scale_y_continuous(trans = "logit") + 
        scale_alpha_continuous(range = c(0.4,1)) + 
        theme_minimal() + 
        labs(title = paste(nombre, "vs", familia,"en las",elec,sep = " "), 
             x = "% población comunal",
             y = "% votos brutos en escala logit") + 
        theme(legend.position = "none", 
              axis.text.x = element_text(margin = margin(30,0,10,0), size = 10),
              axis.text.y = element_text(margin = margin(0,10,0,30), size = 10),
              axis.title.x = element_text(size = 25),
              axis.title.y = element_text(size = 25),
              strip.text.x = element_text(size = 15, margin = margin(10,0,10,0)),
              plot.title = element_text(size = 35, hjust = 0.5, margin = margin(10,0,30,0)))
  return(graf)
  
}




cambio_pct_cat <- datos_censales %>% 
  inner_join(COMUNAS_2007) %>% 
  select(CODGEO,AÑO,Hom:Loc) %>% 
  gather(Cat,Pct,-CODGEO,-AÑO) %>% 
  spread(AÑO,Pct) %>% 
  mutate(Dif = `2012` - `2007`) %>% 
  filter(!is.na(Dif)) %>% 
  group_by(Cat) %>% 
  summarise_at("Dif",funs(Q025=quantile(.,0.025),
                          Q10=quantile(.,0.1),
                          Q50=quantile(.,0.5),
                          Q90=quantile(.,0.9),
                          Q975=quantile(.,0.975))) %>% 
  mutate(Cat=reorder(Cat,Q975-Q025)) %>% 
  ggplot(aes(x=Cat,xend=Cat)) + 
  geom_segment(aes(y=Q025,yend=Q975),size=rel(2),alpha=0.4, color = paleta_tesis_fn$COLOR[3]) + 
  geom_segment(aes(y=Q10,yend=Q90),size=rel(4),alpha=0.8, color = paleta_tesis_fn$COLOR[2]) + 
  geom_point(aes(y=Q50),size=rel(6), color = paleta_tesis_fn$COLOR[1]) + 
  xlab("Categoría de variables") + 
  ylab("2012 - 2007") + 
  labs(title = "Dif en % de la población comunal",
       subtitle = "Intervalos empíricos al 95% y 80% junto con la mediana") + 
  scale_y_continuous(breaks = seq(-1,1,0.1), labels = seq(-100,100,10), limits = c(-1/3,1/3)) + 
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank())

ggsave(filename = "AED/CENSALES/Cambio_Pct_Comunal_Cat.pdf", plot = cambio_pct_cat, 
       device = cairo_pdf, width = 20, height = 10)

distribuciones_variables_censales <- datos_censales %>% 
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
  pmap(~ filter(paleta_tesis_fn, FAMILIA == ..3) %>% 
         extract2("COLOR") %>% 
         {ifelse(length(.)==0,"gray98",.)} %>% 
         {geofacet_distr_cat_dpto(aaaa = ..4,cat = ..1, nombre = ..2, color = .)} %T>% 
         ggsave(plot = ., width = 25, height = 25, device = cairo_pdf, filename =..5))

disperciones_variables_censales <- datos_censales %>% 
  select(-c(CODGEO:Pob)) %>% colnames() %>% 
  tibble(Cat = ., 
         Nombre = c("Hombres","Mujeres",
                    "0 a 17 años", "18 a 24 años", "25 a 39 años", "40 a 54 años", "55 a 64 años", "65+ años",
                    "Franceses", "Extranjeros",
                    "Agricultores", "Artesanos, comerciantes y empresarios", "Cuadros y profesiones intelectuales superiores",
                    "Profesiones intermediarias", "Empleados", "Obreros", "Retirados","Otras personas sin actividad",
                    "Inmigrantes", "Locales"),
         Color = c("Derecha","Izquierda",
                   familias_politicas[-1],
                   "Derecha","Izquierda",
                   familias_politicas,NA,
                   "Derecha","Izquierda")) %>% 
  mutate(Aux1 = "Legislativas 2007", Aux2 = "Legislativas 2012", 
         Aux3 = "Presidenciales 2007", Aux4 = "Presidenciales 2012") %>% 
  gather(Aux,Elección,starts_with("Aux")) %>% 
  select(-Aux) %>% 
  mutate(Archivo = paste("AED/CENSALES/Disper_FN_Reg/Geofacet_Disper_por_Dpto_FN_",
                         Cat,
                         "_",
                         Elección,
                         ".pdf",
                         sep = "")) %>% 
  pmap(~ filter(paleta_tesis_fn, FAMILIA == ..3) %>% 
         extract2("COLOR") %>% 
         {ifelse(length(.)==0,"gray98",.)} %>% 
         {geofacet_disp_votos_cat_reg(elec = ..4, familia = "FN", cat = ..1, nombre = ..2, color = .)} %T>% 
         ggsave(plot = ., width = 25, height = 25, device = cairo_pdf, filename =..5))

