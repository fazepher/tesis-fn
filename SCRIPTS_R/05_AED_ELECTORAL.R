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


#### GEOFACET DISTRIBUCIONES ####

print.noquote("###############################################################")
print.noquote("########### Generando geofacets distribuciones votos ##########")
print.noquote("###############################################################")


genera_geofacet_distr_votos <- function(elec, familia){
  
  datos_distr_por_anc_reg <- datos_electorales_completos %>% 
    filter(FAMILIA == familia, ELECCION == elec) %>% 
    inner_join(COMUNAS_2007) %>% 
    group_by(COD_REG) %>% 
    mutate(MEDIANA = median(PCT_VOTOS_BR)) %>% 
    ungroup() %>% 
    mutate(MEDIANA_NAL = median(PCT_VOTOS_BR)) %>% 
    mutate(NOM_REG = reorder(NOM_REG,MEDIANA),code=COD_REG)
  
  color <- filter(paleta_tesis_fn, FAMILIA == familia) %>% 
    extract2("COLOR")
  
  ggplot(datos_distr_por_anc_reg,
         aes(x = PCT_VOTOS_BR, fill = FAMILIA, #color = FAMILIA, 
             alpha = MEDIANA/MEDIANA_NAL, stat(density))) + 
    geom_histogram(binwidth = 0.01) +
    geom_density(data = select(datos_distr_por_anc_reg,-COD_REG),fill="transparent",color="black") +
    geofacet::facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6), label = "name") + 
    scale_fill_manual(values = color) + 
    #scale_color_manual(values = color) + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_x_continuous(breaks = seq(0,1,0.2),labels = paste(seq(0,100,20),"%",sep=""),trans = "log1p") + 
    scale_y_continuous(trans = "log1p") + 
    labs(title = paste(familia,"en las",elec,sep = " "), 
         subtitle = "% de votos por comuna para la metrópoli entera y por región") + 
    theme(legend.position = "none",
          strip.text.x = element_text(size = 15, margin = margin(10,0,0,0)),
          plot.title = element_text(size = 30, hjust = 0.5),
          plot.subtitle = element_text(margin = margin(5,0,25,0), size = 20, hjust = 0.5),
          panel.grid = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank())
  
}


geofacet_corr_leg_pres <- function(aaaa,familia_base){
  
  datos_familia_base <- datos_electorales_completos %>% 
    separate(ELECCION,c("Tipo","Año")) %>% 
    filter(Año == as.character(aaaa), Tipo == "Legislativas", FAMILIA == familia_base) %>% 
    transmute(Año,CODGEO,PCT_LEGIS_FAM_BASE=PCT_VOTOS_BR)
  
  datos_otras_familias <- datos_electorales_completos %>% 
    separate(ELECCION,c("Tipo","Año")) %>% 
    filter(Año == as.character(aaaa), Tipo == "Presidenciales") %>% 
    select(CODGEO,FAMILIA,PCT_VOTOS_BR) %>% 
    spread(FAMILIA,PCT_VOTOS_BR)
  
  datos_unidos <- left_join(datos_familia_base,datos_otras_familias)
  
  left_join(datos_familia_base,datos_otras_familias) %>% 
    inner_join(COMUNAS_2007) %>% 
    group_by(COD_DPTO) %>% 
    sample_frac(0.25) %>% 
    gather(FAMILIA,PCT_VOT_BR_PRES,one_of(familias_politicas)) %>% 
    filter(FAMILIA == "Izquierda") %>% 
    {ggplot(.,aes(x=PCT_VOT_BR_PRES,y=log(PCT_LEGIS_FAM_BASE/(1-PCT_LEGIS_FAM_BASE)),color=FAMILIA)) + 
        geom_point(alpha = 0.4) + 
        geofacet::facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6), label = "name",scales = "free") + 
        scale_color_manual(values = set_names(paleta_tesis_fn$COLOR,paleta_tesis_fn$FAMILIA))} %>% 
    ggsave(plot = ., filename = "Probando.pdf",device = cairo_pdf,width = 25,height = 25)
  
}


geofacet_pct_votos_dpto <- function(elec,familia){
  
  lim_y <- datos_electorales_completos %>% 
    filter(FAMILIA == familia) %>% 
    top_n(1,PCT_VOTOS_BR) %>% 
    extract2("PCT_VOTOS_BR") %>% 
    unique
  
  color <- filter(paleta_tesis_fn, FAMILIA == familia) %>% 
    extract2("COLOR")
  
  pct_comuna_sub <- datos_electorales_completos %>% 
    filter(FAMILIA == familia, ELECCION == elec) %>% 
    ggplot(aes(x="Metrópoli entera",y=PCT_VOTOS_BR,color=FAMILIA)) + 
    geom_hline(aes(yintercept = median(PCT_VOTOS_BR)),color="gray20") + 
    geom_hline(aes(yintercept = quantile(PCT_VOTOS_BR,0.25)),color = "gray50") + 
    geom_hline(aes(yintercept = quantile(PCT_VOTOS_BR,0.75)),color = "gray50") + 
    geom_violin(fill = "transparent") +
    scale_color_manual(values = color) + 
    scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"),  
                       limits = c(0,lim_y), trans = "log1p") + 
    scale_x_discrete(position = "top") +
    theme_minimal() + 
    theme(legend.position = "none",
          panel.grid = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(margin = margin(30,0,10,0), size = 10),
          axis.text.y = element_text(size = 15))
  
  pct_comuna_dpto <- datos_electorales_completos %>% 
    filter(FAMILIA == familia, ELECCION == elec) %>% 
    inner_join(COMUNAS_2007) %>% 
    mutate(MEDIANA_NAL = median(PCT_VOTOS_BR)) %>% 
    group_by(COD_REG) %>% 
    mutate(MEDIANA_REG = median(PCT_VOTOS_BR)) %>% 
    ungroup %>% 
    mutate(NOM_REG = reorder(NOM_REG,MEDIANA_REG)) %>% 
    group_by(COD_DPTO) %>% 
    mutate(MEDIANA_DPTO = median(PCT_VOTOS_BR)) %>% 
    ungroup %>% 
    mutate(COD_DPTO = reorder(COD_DPTO,MEDIANA_DPTO)) %>% 
    ggplot(aes(x = COD_DPTO, y = PCT_VOTOS_BR, fill = FAMILIA)) + 
    geom_hline(aes(yintercept = median(PCT_VOTOS_BR)),color="gray20") + 
    geom_hline(aes(yintercept = quantile(PCT_VOTOS_BR,0.25)),color = "gray50") + 
    geom_hline(aes(yintercept = quantile(PCT_VOTOS_BR,0.75)),color = "gray50") + 
    geom_violin(aes(alpha = MEDIANA_DPTO/MEDIANA_NAL)) + 
    geofacet::facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6), label = "name", scales = "free_x") + 
    scale_fill_manual(values = color) + 
    scale_alpha_continuous(range = c(0.3,1)) + 
    scale_y_continuous(breaks = seq(0,1,0.25),labels = paste(seq(0,100,25),"%"), 
                       limits = c(0,lim_y), trans = "log1p") + 
    theme_minimal() + 
    labs(title = paste(familia,"en las",elec,sep = " "), 
         subtitle = "% de votos por comuna por departamento") + 
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


geofacets_distr_por_reg <-list(elección = unique(datos_electorales_completos$ELECCION),
                               familia = familias_politicas) %>% 
  cross_df %>% 
  filter(familia == "FN") %>% 
  mutate(Archivo = paste("AED/ELECTORALES/Geofacet_Reg/Geofacet_Distr_por_Reg_",
                         str_remove_all(elección,"(residenciales 20)|(egislativas 20)"),
                         "_",
                         str_replace_all(familia, " ", "_"),
                         ".pdf",
                         sep = "")) %>% 
  pmap(~genera_geofacet_distr_votos(..1,..2) %T>% 
         ggsave(plot = ., width = 18, height = 15, device = cairo_pdf,
                filename =..3))

geofacets_distr_por_dpto <- list(elección = unique(datos_electorales_completos$ELECCION),
                                 familia = familias_politicas) %>% 
  cross_df %>% 
  mutate(Archivo = paste("AED/ELECTORALES/Geofacet_Dpto/Geofacet_Distr_por_Dpto_",
                         str_remove_all(elección,"(residenciales 20)|(egislativas 20)"),
                         "_",
                         str_replace_all(familia, " ", "_"),
                         ".pdf",
                         sep = "")) %>% 
  pmap(~geofacet_pct_votos_dpto(..1,..2) %T>% 
         ggsave(plot = ., width = 25, height = 25, device = cairo_pdf,
                filename =..3))
                                 

