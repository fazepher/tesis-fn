
library(sf)
library(cartogram)
library(cowplot)

equiv_var_modelo <- equivalencia_variables %>% 
  mutate(Param = case_when(Variable == "Escolaridad" ~ "beta",
                           Variable == "Cat. Socioprof." ~ "gamma",
                           Variable == "Edad" ~ "delta",
                           Variable == "Cond. Migratoria" ~ "lambda",
                           Variable == "Sexo" ~ "kappa",
                           Variable == "Ocupación General"~"zeta",
                           Variable == "Ocupación Juvenil"~"xi",
                           Variable == "Ocupación Mayores"~"upsilon")) %>% 
  filter(!is.na(Param)) %>% 
  group_by(Param) %>% 
  mutate(n_cat = 1:n()) %>% 
  ungroup

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
  quantile(c(0,.45,.5,.55,0.95,1))

etiquetas_votos <- round(100*cuantiles_votos[c(1,3,6)], 1) %>% 
{paste(c("Mínimo real: ", "Mediana real: ", "Máximo real: "),.,"%",sep="")}

### Leer datos de efectos ####

resumen_efecto <- read_csv("EFECTOS/Res_Efectos.csv",locale = locale(encoding = "latin1"))

#### GRÁFICOS ####

evol_efecto <- function(variable,cats_incluidas = unique(equivalencia_variables$Cats),tam_rel_etiqueta = 1.5){
  
  
  efectos_medianos <- resumen_efecto %>% 
    left_join(equivalencia_variables, by = "Cats") %>%
    filter(Variable %in% variable, 
           Cats %in% cats_incluidas) %>% 
    mutate(Cats = factor(Cats,levels = cats_incluidas,ordered = T)) %>% 
    arrange(Cats) %>% 
    mutate(Etiqueta=factor(Etiqueta,levels = unique(Etiqueta), ordered=T)) %>% 
    select(Variable,Cats,Etiqueta,Modelo,COD_DPTO,Mediana) %>% 
    group_by(Cats,COD_DPTO) %>% 
    arrange(COD_DPTO,Cats,Modelo) %>% 
    ggplot(aes(x=Modelo,y=Mediana)) + 
    geom_path(aes(group=COD_DPTO), color = paleta_tesis_fn$COLOR[1], size = rel(0.1)) + 
    geom_hline(yintercept = 0, color = paleta_tesis_fn$COLOR[4], size = rel(1)) + 
    facet_wrap(~Etiqueta, nrow = 1, labeller = labeller(Etiqueta = label_wrap_gen(25))) + 
    scale_y_continuous(labels = function(x){round(100*x,1) %>% paste("pp",sep="")}) + 
    labs(y="Efecto mediano estimado por departamento") + 
    theme_classic() + 
    theme(strip.text = element_text(size = rel(tam_rel_etiqueta)),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(1.75), margin = margin(r=10)),
          axis.text.x = element_text(size = rel(1)),
          axis.text.y = element_text(size = rel(1.5)))
  
  incertidumbre_efectos <- resumen_efecto %>% 
    left_join(equivalencia_variables, by = "Cats") %>%
    filter(Variable %in% variable, 
           Cats %in% cats_incluidas) %>% 
    mutate(Cats = factor(Cats,levels = cats_incluidas,ordered = T)) %>% 
    arrange(Cats) %>% 
    mutate(Etiqueta=factor(Etiqueta,levels = unique(Etiqueta), ordered=T)) %>% 
    transmute(Variable,Cats,Etiqueta,Modelo,COD_DPTO,Longitud = Q975 - Q025) %>% 
    arrange(Cats,Modelo) %>% 
    group_by(Etiqueta,Modelo) %>% 
    mutate(Long_Prom = mean(Longitud)) %>% 
    ungroup %>% 
    mutate(Modelo = factor(Modelo,ordered = T)) %>% 
    {ggplot(.,aes(x=as.numeric(Modelo))) + 
        geom_path(aes(y = Longitud, group=COD_DPTO), color = paleta_tesis_fn$COLOR[1], size = rel(0.1)) + 
        geom_path(data = distinct(.,Etiqueta,Modelo,Long_Prom), aes(y = Long_Prom, group = Etiqueta), 
                  color = paleta_tesis_fn$COLOR[7], size = rel(1)) + 
        facet_wrap(~Etiqueta, nrow = 1, labeller = labeller(Etiqueta = label_wrap_gen(25))) + 
        scale_x_continuous(breaks = 1:length(levels(.$Modelo)), labels = levels(.$Modelo), sec.axis = dup_axis()) + 
        scale_y_continuous(labels = function(x){round(100*x,1) %>% paste("pp",sep="")}) +
        labs(x = "Modelo",
             y="Longitud de intervalos al 95% por departamento") + 
        theme_classic() + 
        theme(strip.text = element_blank(),
              axis.title.x.bottom = element_text(size = rel(1.5), margin = margin(t = 20)),
              axis.title.x.top = element_text(size = rel(1.5), margin = margin(b = 20)),
              axis.title.y = element_text(size = rel(1.75), margin = margin(r=10)),
              axis.text.x = element_text(size = rel(1)),
              axis.text.y = element_text(size = rel(1.5)))}
  
  
  efectos_significativos <- resumen_efecto %>% 
    left_join(equivalencia_variables, by = "Cats") %>%
    filter(Variable %in% variable, 
           Cats %in% cats_incluidas) %>% 
    mutate(Cats = factor(Cats,levels = cats_incluidas,ordered = T)) %>% 
    arrange(Cats) %>% 
    mutate(Etiqueta=factor(Etiqueta,levels = unique(Etiqueta), ordered=T)) %>% 
    select(Variable,Cats,Etiqueta,Modelo,COD_DPTO,Mediana,Efecto_Mediana,Sign95) %>% 
    group_by(Variable,Etiqueta,Modelo,Efecto_Mediana) %>% 
    summarise(Significativos = sum(Sign95)) %>% 
    ggplot(aes(x=Modelo,y=Significativos,color=Efecto_Mediana,group=Efecto_Mediana)) + 
    geom_path(key_glyph = "point") + 
    geom_label(aes(label=Significativos), key_glyph = "point", size = rel(5)) + 
    facet_wrap(~Etiqueta, nrow = 1, strip.position = "bottom", labeller = labeller(Etiqueta = label_wrap_gen(25))) + 
    scale_color_manual(values = paleta_tesis_fn$COLOR[c(3,2)]) + 
    scale_x_discrete(position = "top") + 
    scale_y_continuous(limits = c(1,NA)) + 
    labs(y = "Departamentos con efecto significativo",
         color = "Efecto") + 
    theme_classic() + 
    theme(legend.position = "bottom",
          legend.text = element_text(size = rel(1.25)),
          legend.title = element_text(size = rel(1.5)),
          strip.text = element_text(size = rel(tam_rel_etiqueta)),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = rel(1.75), margin = margin(r=10)),
          axis.text.x = element_text(size = rel(1)),
          axis.text.y = element_text(size = rel(1.5)))
  
  plot_grid(efectos_medianos,incertidumbre_efectos,efectos_significativos,align = "v", nrow=3) %>% 
    return(.)
}

graf_efecto <- function(.data,colores=paleta_tesis_fn$COLOR[c(3,2)]){
  
  .data %<>% 
    mutate(Orden = rank(Media))
  
  ref <- .data$Media %>% mean
  color_ref <- if_else(ref>=0,2,3)
  
  ggplot(.data,aes(y=Orden,yend=Orden,color=Efecto_Media)) + 
    geom_vline(xintercept = 0, linetype = 2, color = paleta_tesis_fn$COLOR[1]) + 
    geom_vline(xintercept = ref,color = paleta_tesis_fn$COLOR[color_ref]) + 
    geom_segment(data = filter(.data,Sign95), aes(x=Q025,xend=Q975),
                 size=rel(0.8)) + 
    # geom_segment(data = filter(.data,Sign95), aes(x=Q10,xend=Q90),
    #              size=rel(0.6)) + 
    # geom_segment(data = filter(.data,Sign95), aes(x=Q25,xend=Q75),
    #              size=rel(0.8)) + 
    geom_point(aes(x=Mediana,alpha=Sign95), 
               size = rel(1.5)) + 
    geom_segment(data = filter(.data,!Sign95), aes(x=Q025,xend=Q975),
                 size=rel(0.8), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    # geom_segment(data = filter(.data,!Sign95), aes(x=Q10,xend=Q90),
    #              size=rel(0.6), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    # geom_segment(data = filter(.data,!Sign95), aes(x=Q25,xend=Q75),
    #              size=rel(0.8), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    geom_point(data = filter(.data,!Sign95), aes(x=Mediana), 
               size = rel(1.5), alpha = 0.6, color = paleta_tesis_fn$COLOR[1]) + 
    scale_color_manual(values = colores) + 
    scale_y_continuous(breaks = .data$Orden, labels = .data$NOM_DPTO) + 
    scale_x_continuous(limits = c(-.125,.125), 
                       breaks = seq(-.1,.1,by=.025), 
                       labels = c("-10pp","-7.5pp","-5pp","-2.5pp",
                                  "0",
                                  "+2.5pp","+5pp","+7.5pp","+10pp"),
                       sec.axis = dup_axis()) + 
    labs(title = unique(.data$Etiqueta)) + 
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5, size = rel(2)),
          axis.text.x = element_text(size = rel(1.25)),
          axis.text.y = element_text(size = rel(0.8)),
          axis.title = element_blank(), 
          legend.position = "none")
}

mapa_efecto_mediano <- function(.data,colores=paleta_tesis_fn$COLOR[c(6,2)]){
  
  .data %<>% 
    left_join(mapa_dptos, by = c("COD_DPTO"="code_insee")) %>% 
    st_as_sf() %>% 
    st_transform(crs = 25832)
  
  ggplot(.data) + 
    geom_sf(aes(fill=Mediana), size = rel(0.5)) + 
    scale_fill_gradient2(low=colores[1],mid="white",midpoint = 0, high = colores[2],
                         limits = c(-.125,.125), 
                         breaks = seq(-.1,.1,by=.05), 
                         labels = c("-10pp","-5pp",
                                    "0",
                                    "+5pp","+10pp")) + 
    labs(title = unique(.data$Etiqueta)) + 
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = rel(3.25)),
          legend.position = c(0.125,0.45),
          legend.title = element_text(size = rel(1.8)),
          legend.text = element_text(size = rel(1.5)))
}

dorling_efecto <- function(.data,colores=paleta_tesis_fn$COLOR[c(6,2)]){
  
  .data %<>% 
    left_join(dorling_dptos, by = c("COD_DPTO"="code_insee")) %>% 
    st_as_sf() %>% 
    st_transform(crs = 25832)
  
  ggplot(.data) + 
    geom_sf(data = fronteras_reg_dorling,fill="transparent", size = rel(0.05)) + 
    geom_sf(data = filter(.data,Sign95),aes(fill=Media), size = rel(0.5)) + 
    geom_sf_text(aes(label=round(100*Media,1), size = Sign95),color=paleta_tesis_fn$COLOR[1]) + 
    scale_fill_gradient2(low=colores[1],mid="white",midpoint = 0, high = colores[2],
                         limits = c(-.125,.125), 
                         breaks = seq(-.1,.1,by=.025), 
                         labels = c("-10pp","-7.5pp","-5pp","-2.5pp",
                                    "0",
                                    "+2.5pp","+5pp","+7.5pp","+10pp")) + 
    scale_size_manual(values = c(rel(4), rel(7))) + 
    labs(title = unique(.data$Etiqueta)) + 
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = rel(3.25)),
          legend.position = "none")
}

genera_fila_ef <- function(cats,tipo = "Graf",modelo="H",datos_resumidos=resumen_efecto,filas = 1, escala = 0.95, ...){
  
  datos <- datos_resumidos %>% 
    filter(Modelo == modelo) %>% 
    left_join(equiv_var_modelo) %>% 
    filter(Cats %in% cats) %>% 
    mutate(Cats = factor(Cats,levels = cats,ordered = T)) %>% 
    arrange(Cats) %>% 
    split(.$Cats) 
  
  if(tipo == "Graf"){
    graf <- map(datos, function(x) graf_efecto(x,...)) 
  }
  if(tipo == "Mapa"){
    graf <- map(datos, function(x) mapa_efecto_mediano(x)) 
  }
  if(tipo == "Dorling"){
    graf <- map(datos, function(x) dorling_efecto(x,...)) 
  }
  
  plot_grid(plotlist = graf, nrow = filas, scale = escala)
  
}

Aux_dptos <- DEPARTAMENTOS %>% 
  mutate(COD_REG = factor(COD_REG,orden_regiones,ordered = T), 
         COD_DPTO = factor(COD_DPTO,orden_departamentos,ordered = T)) %>% 
  arrange(COD_REG,COD_DPTO) %>% 
  group_by(COD_REG) %>% 
  mutate(Num_Dpto_Reg = 1:n())

#### Evoluciones ####
# ggsave(plot = evol_efecto("Escolaridad"), 
#        filename = "EFECTOS/Evol_Efectos_Escolaridad.pdf",width = 30,height = 20,device = cairo_pdf)
# ggsave(plot = evol_efecto("Cat. Socioprof."), 
#        filename = "EFECTOS/Evol_Efectos_Cat_Socioprof.pdf",width = 35,height = 20,device = cairo_pdf)
# ggsave(plot = evol_efecto("Edad"), 
#        filename = "EFECTOS/Evol_Efectos_Edad.pdf",width = 30,height = 20,device = cairo_pdf)
# ggsave(plot = evol_efecto(c("Cond. Migratoria","Sexo","Ocupación Juvenil","Ocupación General"),
#                           cats_incluidas = c("Inm","Muj","Des1","Des2")), 
#        filename = "EFECTOS/Evol_Efectos_Dicotom.pdf",width = 30,height = 20,device = cairo_pdf)

#### Efectos #####

equiv_var_modelo %>% 
  extract2("Cats") %>% 
  walk(~ genera_fila_ef(cats = .x, 
                        datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"), 
                        escala = 1) %>% 
         ggsave(plot = ., filename = paste("EFECTOS/Efectos",.x,"Modelo_H.pdf", sep = "_"),
                width = 22.5/2.5,height = 17/1.5, device = cairo_pdf))

#### Dorlings ####
equiv_var_modelo %>% 
  extract2("Cats") %>% 
  walk(~ genera_fila_ef(cats = .x, 
                        tipo = "Dorling", 
                        datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"), 
                        escala = 1) %>% 
         ggsave(plot = ., filename = paste("EFECTOS/Dorling_Efectos",.x,"Modelo_H.pdf", sep = "_"),
                width = 22.5/2.5,height = 17/1.5, device = cairo_pdf))

#### Mapas #### 
equiv_var_modelo %>% 
  extract2("Cats") %>% 
  walk(~ genera_fila_ef(cats = .x, 
                        tipo = "Mapa", 
                        datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"), 
                        escala = 1) %>% 
         ggsave(plot = ., filename = paste("EFECTOS/Mapa_Efectos",.x,"Modelo_H.pdf", sep = "_"),
                width = 22.5/2.5,height = 17/1.5, device = cairo_pdf))

equiv_var_modelo %>% 
  filter(Variable == "Escolaridad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"), filas = 2, escala = 1) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Escolaridad_Modelo_H.pdf", width = 23*1.5,height = 17*1.5, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Cat. Socioprof.") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=4, escala = 1) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Cat_Socioprof_Modelo_H.pdf", width = 17*1.5,height = 23*1.5, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Edad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2, escala = 1) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Edad_Modelo_H.pdf", width = 23*1.5,height = 17*1.5, device = cairo_pdf)

c("Des1","Des2","Des3","Inm","Muj") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2, escala = 1) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Dicotom_Modelo_H.pdf", width = 23*1.5,height = 17*1.5, device = cairo_pdf)

# #### Otros grafs ####
#
# resumen_efecto %>% 
#   filter(Modelo == "H") %>% 
#   left_join(equiv_var_modelo) %>% 
#   split(.$Cats) %>% 
#   walk(~ .x %>% 
#          mutate(Etiqueta = factor(Etiqueta,levels = unique(equivalencia_variables$Etiqueta)) %>% 
#                   forcats::fct_drop(.)) %>% 
#          left_join(Aux_dptos) %>% 
#          mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos[1:96],ordered = T)) %>% 
#          mutate(Color = case_when(!Sign95~"No Significativo",
#                                   Efecto_Mediana == "Positivo"~ "Efecto Positivo",
#                                   Efecto_Mediana == "Negativo"~ "Efecto Negativo")) %>% 
#          arrange(Etiqueta,COD_DPTO) %>% 
#          {ggplot(.,aes(y=COD_DPTO, xmin=Q025, x = Mediana, xmax=Q975, shape = Etiqueta)) + 
#              geom_vline(xintercept = 0, linetype = 2, color = paleta_tesis_fn$COLOR[1]) + 
#              geom_errorbarh(data = filter(.,Sign95), aes(color = Color, group = interaction(Etiqueta,COD_REG,COD_DPTO)),
#                             height = 0, position = ggstance::position_dodgev(height = 0.9), key_glyph = draw_key_rect) +
#              geom_errorbarh(data = filter(.,!Sign95), aes(color = Color, group = interaction(Etiqueta,COD_REG,COD_DPTO)),
#                             height = 0, position = ggstance::position_dodgev(height = 0.9), alpha = 0.2) +
#              geom_point(data = filter(.,Sign95), aes(color = Color, group = interaction(Etiqueta,COD_REG,COD_DPTO)), 
#                         position = ggstance::position_dodgev(height =  0.9)) + 
#              geom_point(data = filter(.,!Sign95), aes(color = Color, group = interaction(Etiqueta,COD_REG,COD_DPTO)), 
#                         position = ggstance::position_dodgev(height =  0.9), alpha = 0.2) + 
#              geofacet::facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6) %>% arrange(row,col), 
#                                  label = "name", scales = "free_y") + 
#              scale_y_discrete(breaks = orden_departamentos[1:96], 
#                               labels = distinct(.,COD_DPTO,NOM_DPTO) %>% arrange(COD_DPTO) %>% extract2("NOM_DPTO")) + 
#              scale_x_continuous(limits = c(-.125,.125), 
#                                 breaks = seq(-.1,.1,by=.05), 
#                                 labels = c("-10pp","-5pp","0","+5pp","+10pp")) + 
#              scale_color_manual(values = paleta_tesis_fn$COLOR[3:1] %>% 
#                                   set_names(c("Efecto Negativo", "Efecto Positivo","No Significativo"))) + 
#              theme_classic() + 
#              guides(shape = guide_legend(override.aes = list(size = rel(5)))) + 
#              theme(legend.position = "bottom",
#                    legend.box.margin = margin(t = 30),
#                    legend.text = element_text(size = rel(1.75)),
#                    legend.title = element_text(size = rel(1.75)),
#                    axis.title = element_blank(),
#                    axis.line = element_blank(),
#                    axis.text.y = element_text(size = rel(1.75)),
#                    strip.text = element_text(size = rel(1.75)),
#                    panel.grid.minor.x = element_blank(),
#                    panel.grid.major = element_blank())} %>% 
#          ggsave(plot = ., 
#                 filename = paste("EFECTOS/Geofacet",unique(.x$Cats),"Modelo_H.pdf",sep="0"),
#                 width = 23*1.5,height = 17*1.5, device = cairo_pdf))
# 
# resumen_efecto %>% 
#   filter(Modelo == "H") %>% 
#   left_join(equiv_var_modelo) %>% 
#   filter(Variable == "Escolaridad") %>% 
#   mutate(Etiqueta = factor(Etiqueta,levels = unique(equivalencia_variables$Etiqueta)) %>% 
#            forcats::fct_drop(.)) %>% 
#   left_join(Aux_dptos) %>% 
#   mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos[1:96],ordered = T)) %>% 
#   mutate(Color = case_when(!Sign95~"No Significativo",
#                            Efecto_Mediana == "Positivo"~ "Efecto Positivo",
#                            Efecto_Mediana == "Negativo"~ "Efecto Negativo")) %>% 
#   arrange(Etiqueta,COD_DPTO) %>% 
#   {ggplot(.,aes(y=COD_DPTO, x = Mediana)) + 
#       geom_vline(xintercept = 0, linetype = 2, color = paleta_tesis_fn$COLOR[1]) + 
#       geom_point(aes(color = Color, alpha = Sign95, shape = Etiqueta), size = rel(3), 
#                  position = ggstance::position_dodgev(height = 0.75)) +
#       geofacet::facet_geo(~COD_REG, grid = filter(fr_anc_reg,col<=6) %>% arrange(row,col), 
#                           label = "name", scales = "free_y") + 
#       scale_y_discrete(breaks = orden_departamentos[1:96], 
#                        labels = distinct(.,COD_DPTO,NOM_DPTO) %>% arrange(COD_DPTO) %>% extract2("NOM_DPTO")) + 
#       scale_x_continuous(limits = c(-.125,.125), 
#                          breaks = seq(-.1,.1,by=.05), 
#                          labels = c("-10pp","-5pp","0","+5pp","+10pp")) + 
#       scale_color_manual(values = paleta_tesis_fn$COLOR[3:1] %>% 
#                           set_names(c("Efecto Negativo", "Efecto Positivo","No Significativo"))) + 
#       scale_alpha_manual(values = c(0.2,1), guide = FALSE) + 
#       guides(shape = guide_legend(override.aes = list(size = rel(5))),
#              color = guide_legend(override.aes = list(size = rel(5)))) + 
#       theme_classic() + 
#       theme(legend.position = "bottom",
#             legend.box.margin = margin(t = 30),
#             legend.text = element_text(size = rel(1.75)),
#             legend.title = element_text(size = rel(1.75)),
#             axis.title = element_blank(),
#             axis.line = element_blank(),
#             axis.text.y = element_text(size = rel(1.75)),
#             strip.text = element_text(size = rel(1.75)),
#             panel.grid.minor.x = element_blank(),
#             panel.grid.major = element_blank())} %>% 
#   ggsave(plot = ., 
#          filename = "EFECTOS/Geofacet_Escolaridad_Modelo_H_Solo_Puntos.pdf",width = 23*1.5,height = 17*1.5, device = cairo_pdf)
#
# #### PCA ####
# 
# efectos <- equiv_var_modelo %>% 
#   extract2("Cats") %>% 
#   map_dfr(genera_resumen_efecto) %>% 
#   left_join(equiv_var_modelo) %>% 
#   left_join(DEPARTAMENTOS)
# 
# efectos_para_pca <- efectos %>% 
#   transmute(COD_DPTO,Cats,Mediana,Sign95) %>% 
#   gather(Nombre,X,Mediana,Sign95) %>%
#   transmute(COD_DPTO,Nombre = paste(Cats,Nombre,sep="_"),X) %>%
#   spread(Nombre,X)
#   # spread(Cats,Mediana)
# 
# efectos_para_pca <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_D_PRED.rds") %>% 
#   extract2("Predicciones") %>% 
#   extrae_simulaciones(n_simul = 1,tipo="Compuesto",pars="pct_votos_dptal_media") %>% 
#   transmute(COD_DPTO,Pct_Votos = Valor) %>% 
#   left_join(efectos_para_pca,by="COD_DPTO")
# 
# pca_ef <- efectos_para_pca %>% 
#   select(-COD_DPTO) %>% 
#   as.matrix %>% 
#   set_rownames(efectos_para_pca$COD_DPTO) %>% 
#   prcomp(scale=TRUE)
# 
# pca_ef$sdev %>% 
#   raise_to_power(2) %>% 
#   {divide_by(.,sum(.))} %>% 
#   tibble(Pct_Var_Explicada = .) %>% 
#   mutate(Componente = 1:n(),Cum = cumsum(Pct_Var_Explicada)) %>% 
#   gather(Tipo,Varianza,-Componente) %>% 
#   ggplot(aes(x=Componente,y=Varianza)) + 
#   geom_path(color = paleta_tesis_fn$COLOR[1]) + 
#   geom_point(aes(color = Componente <= 5),show.legend = FALSE) + 
#   facet_wrap(~Tipo,scales = "free") + 
#   scale_y_continuous(labels = scales::percent) + 
#   scale_color_manual(values = paleta_tesis_fn$COLOR[c(3,2)]) + 
#   theme_classic()
# 
# pca_ef %>% 
#   broom::tidy(matrix="samples") %>% 
#   filter(PC <= 10) %>% 
#   rename(COD_DPTO = row) %>% 
#   left_join(DEPARTAMENTOS) %>% 
#   left_join(dorling_dptos, by = c("COD_DPTO" = "code_insee")) %>% 
#   st_as_sf %>% 
#   st_transform(crs = 25832) %>% 
#   ggplot(aes(fill=value)) + 
#   geom_sf() + 
#   facet_wrap(~PC,ncol = 5) + 
#   scale_fill_gradient2(low=paleta_tesis_fn$COLOR[6],mid="white",high=paleta_tesis_fn$COLOR[2],midpoint = 0) + 
#   theme_bw()
# 
# 
# clustering_pca <- pca_ef %>% 
#   broom::tidy(matrix="samples") %>% 
#   filter(PC <= 5) %>% 
#   left_join(DEPARTAMENTOS,  by = c("row"="COD_DPTO")) %>% 
#   transmute(NOM_DPTO,PC = paste("CP",PC,sep=""),value) %>% 
#   spread(PC,value) %>% 
#   set_rownames(.,.$NOM_DPTO) %>% 
#   select(-NOM_DPTO) %>% 
#   as.matrix %>% 
#   scale %>% 
#   cluster::diana(.) %>%
#   as.hclust 
# 
# map_dfr(1:6,~cutree(clustering_pca,.x) %>% 
#           as.matrix %>% 
#           as.data.frame() %>% 
#           tibble::rownames_to_column("NOM_DPTO") %>% 
#           mutate(Tipo = as.factor(V1),
#                  N_clus = .x)) %>%
#   left_join(DEPARTAMENTOS) %>% 
#   left_join(mapa_dptos, by = c("COD_DPTO"="code_insee")) %>% 
#   st_as_sf %>% 
#   st_transform(crs=25832) %>% 
#   ggplot(aes(fill=Tipo)) + 
#   geom_sf() + 
#   facet_wrap(~N_clus,nrow = 2) + 
#   scale_fill_manual(values = paleta_tesis_fn$COLOR) + 
#   theme_map()
# 
# 
