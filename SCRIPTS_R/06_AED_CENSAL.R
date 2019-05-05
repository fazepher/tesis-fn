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

grafica_distr_por_comuna <- function(datos,variable,cats,nombres,colores){
  
  graf <- datos %>% 
    select("CODGEO","AÑO",cats) %>% 
    gather(VARIABLE,PCT,-CODGEO,-AÑO) %>% 
    ggplot(aes(x = VARIABLE, y = PCT, fill = VARIABLE)) + 
    geom_violin(adjust = 2) + 
    stat_summary(fun.data = function(PCT){return(tibble(y = quantile(PCT,0.5),
                                                        ymin = quantile(PCT,0.025), 
                                                        ymax = quantile(PCT,0.975)))},
                 size = 2) +
    facet_wrap(~AÑO,nrow = 2, ncol = 1) + 
    scale_fill_manual(values = colores) + 
    theme_minimal() + 
    labs(title = paste("Distribución por", variable, "de las poblaciones comunales",sep = " "), 
         subtitle = "Los puntos con línea representan la mediana y el 95% de las observaciones",
         y = "Porcentaje como proporción de la población de la comuna",
         x = variable) +
    scale_y_continuous(breaks = c(.25,.5,.75), labels = paste(c(25,50,75), "%", sep = " "),limits = c(0,1)) +
    scale_x_discrete(labels = nombres %>% set_names(cats)) + 
    theme(legend.position = "none", 
          strip.text.x = element_text(size = 15, margin = margin(10,0,0,0)),
          plot.title = element_text(size = 30, hjust = 0.5),
          plot.subtitle = element_text(margin = margin(5,0,25,0), size = 20, hjust = 0.5))
  
  return(graf)
}


grafica_distr_por_comuna(datos = datos_censales, 
                         variable = "Sexo", 
                         cats = c("Hom","Muj"),
                         nombres = c("Hombres","Mujeres"),
                         colores = filter(paleta_tesis_fn,FAMILIA %in% c("Izquierda","Otras derechas")) %>% 
                           extract2("COLOR") %>% rev) %>% 
  ggsave(plot = ., filename = "Prueba_Censales.pdf", device = cairo_pdf, width = 20, height = 15)
