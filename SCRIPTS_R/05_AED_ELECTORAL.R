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
    scale_alpha_continuous(range = c(0.4,1)) + 
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

geofacets_distr_por_reg <-list(elección = unique(datos_electorales_completos$ELECCION),
                               familia = familias_politicas) %>% 
  cross_df %>% 
  filter(familia == "FN") %>% 
  mutate(Archivo = paste("AED/ELECTORALES/Geofacet_Distr_por_Reg_",
                         str_remove_all(elección,"(residenciales 20)|(egislativas 20)"),
                         "_",
                         str_replace_all(familia, " ", "_"),
                         ".pdf",
                         sep = "")) %>% 
  pmap(~genera_geofacet_distr_votos(..1,..2) %T>% 
         ggsave(plot = ., width = 18, height = 15, device = cairo_pdf,
                filename =..3))
                                 

