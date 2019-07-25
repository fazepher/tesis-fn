############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################# MODELOS COMPUESTOS ###########################################
############################################################################################################
############################################ ANALIZA EFECTOS ##########################################
############################################################################################################

library(rstan)
library(sf)
library(cartogram)

inv_logit <- function(x){
  1/(1+exp(-x))
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

remove(datos_electorales_completos,datos_censales,otros_datos_comunales,prueba_datos_unificados)

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

#### ANALIZA ####

extrae_simulaciones <- function(.data,n_simul, tipo, ...){
  
  simulaciones <- .data %>% 
    as.data.frame(...) %>% 
    as_tibble() %>% 
    mutate(Simul = 1:n())
  
  if(n_simul > nrow(simulaciones)){
    n_simul <- nrow(simulaciones)
    print.noquote("Se pidieron más simulaciones de las existentes, se extraen todas las disponibles.")
  }
  
  simulaciones %<>% 
    filter(Simul <= n_simul) %>% 
    gather(Param,Valor,-Simul) 
  
  if(tipo == "Nacional"){
    simulaciones <- map_dfr(orden_departamentos[1:96], ~ mutate(simulaciones, COD_DPTO = .x))
  } else{
    simulaciones %<>% 
      mutate(COD_DPTO = str_extract(Param,"\\[\\d*(,|\\])") %>% 
               str_remove_all("[:punct:]") %>% 
               as.integer %>% 
               {orden_departamentos[.]}) 
  }
  
  return(simulaciones)
  
}

convierte_a_etiquetas <- function(.data){
  .data %>% 
    mutate(Param = str_remove_all(Param,"(\\d*,)|(\\[\\d*\\])")) %>% 
    separate(Param,c("Param","n_cat"),"_ajus\\[",fill="right") %>% 
    mutate(n_cat = str_remove_all(n_cat,"\\]") %>% as.numeric) %>% 
    left_join(equiv_var_modelo, by = c("Param","n_cat")) %>% 
    mutate(Etiqueta = if_else(is.na(Etiqueta),"Intercepto",Etiqueta))
}

equiv_var_modelo <- equivalencia_variables %>% 
  mutate(Param = case_when(Variable == "Escolaridad" ~ "beta",
                           Variable == "Cat. Socioprof." ~ "gamma",
                           Variable == "Edad" ~ "delta",
                           Variable == "Cond. Migratoria" ~ "lambda",
                           Variable == "Sexo" ~ "kappa")) %>% 
  filter(!is.na(Param)) %>% 
  group_by(Param) %>% 
  mutate(n_cat = 1:n()) %>% 
  ungroup
                           
grid_modelo <- equiv_var_modelo %>% 
  transmute(code = Cats,
            name = Etiqueta,
            row = case_when(Variable == "Escolaridad" ~ 1,
                            Variable == "Cat. Socioprof." ~ 2,
                            Variable == "Edad" ~ 3,
                            Variable == "Cond. Migratoria" ~ 4,
                            Variable == "Sexo" ~ 5),
            col = n_cat)

aux_dptos <- DEPARTAMENTOS %>% 
  mutate(COD_REG = factor(COD_REG,levels = orden_regiones,ordered = T)) %>% 
  arrange(COD_REG) %>% 
  group_by(COD_REG) %>% 
  mutate(n_dep = 1:n()) %>% 
  filter(!is.na(COD_REG))

rangos <- grid_modelo %>% 
  arrange(row,col) %>% 
  extract2("code") %>% 
  {select(datos_P12,COD_DPTO,.)} %>% 
  gather(Cats,X,-COD_DPTO) %>% 
  group_by(COD_DPTO,Cats) %>% 
  summarise(X_max = max(X),
            X_barra = mean(X),
            X_min = min(X)) %>% 
  ungroup %>% 
  mutate(Dif_Sup = X_max - X_barra, 
         Dif_Inf = X_barra - X_min, 
         Dif = if_else(Dif_Sup>=Dif_Inf,Dif_Inf,Dif_Sup)) %>% 
  select(COD_DPTO,Cats,X_barra,Dif) %>% 
  left_join(equiv_var_modelo) %>% 
  group_by(COD_DPTO,Variable) %>% 
  mutate(Dif = min(Dif),
         div = max(n_cat)-1) %>% 
  ungroup %>% 
  select(Variable,Param,Cats,COD_DPTO,X_barra,Dif,div)

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_D.rds")
datos_pred <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_D_PRED.rds") %>% 
  extract2("Predicciones")

interceptos <- extrae_simulaciones(.data = datos_modelo, 
                                   n_simul = 4000, 
                                   tipo = "Compuesto", 
                                   pars = "alfa") %>% 
  transmute(Simul,COD_DPTO,Intercepto = Valor)

simuls_var_int <- extrae_simulaciones(.data = datos_modelo, 
                    n_simul = 4000, 
                    tipo = "Compuesto", 
                    pars =  paste("lambda","ajus",sep="_")) %>% 
  convierte_a_etiquetas %>% 
  select(Simul,Cats,COD_DPTO,Valor) %>% 
  left_join(filter(rangos, Param == "lambda"), by = c("Cats","COD_DPTO")) %>% 
  mutate(Eta_Barra = Valor*X_barra, 
         Eta_i = Valor*(X_barra + Dif), 
         Eta_o = Valor*(X_barra - (Dif/div)))

etas <- extrae_simulaciones(.data = datos_modelo, 
                            n_simul = 4000, 
                            tipo = "Compuesto", 
                            pars =  c("alfa","lp__","log_lik"),
                            include = FALSE) %>% 
  convierte_a_etiquetas %>% 
  select(Simul,Cats,COD_DPTO,Valor) %>% 
  left_join(rangos, by = c("Cats","COD_DPTO")) %>% 
  mutate(Eta_Barra = Valor*X_barra, 
         Eta_i = Valor*(X_barra + Dif), 
         Eta_o = Valor*(X_barra - (Dif/div)))

v_barras <- etas %>% 
  group_by(Simul,COD_DPTO) %>% 
  summarise(Eta_Barra = sum(Eta_Barra)) %>% 
  ungroup %>% 
  left_join(interceptos, by = c("Simul","COD_DPTO")) %>% 
  transmute(Simul,
            COD_DPTO,
            v_Barra = Eta_Barra %>% add(Intercepto) %>% inv_logit)

genera_resumen_efecto <- function(cat){
  
  v_i <- etas %>% 
    filter(Cats == cat) %>% 
    select(Cats,Dif,Simul,COD_DPTO,Variable,Eta_i) %>% 
    left_join(interceptos) %>% 
    mutate(Eta_i = Eta_i + Intercepto) %>% 
    select(-Intercepto)
  
  v_i <- etas %>% 
    filter(Variable == unique(v_i$Variable), Cats != unique(v_i$Cats)) %>% 
    group_by(Simul,COD_DPTO) %>% 
    summarise(Eta_o = sum(Eta_o)) %>% 
    ungroup %>% 
    left_join(v_i) %>% 
    mutate(Eta_i = Eta_i + Eta_o) %>% 
    select(-Eta_o)
  
  v_i <- etas %>% 
    filter(Variable != unique(v_i$Variable)) %>% 
    group_by(Simul,COD_DPTO) %>% 
    summarise(Eta_Barra = sum(Eta_Barra)) %>% 
    ungroup %>% 
    left_join(v_i) %>% 
    mutate(v_i = Eta_i %>% add(Eta_Barra) %>% inv_logit) %>% 
    select(-Eta_i,-Eta_Barra)
  
  resumen_efectos <- v_i %>% 
    left_join(v_barras) %>% 
    mutate(Efecto = v_i %>% subtract(v_Barra)) %>% 
    group_by(Cats,COD_DPTO) %>% 
    summarise(Q025 = quantile(Efecto,0.025),
              Q10 = quantile(Efecto,0.10),
              Q25 = quantile(Efecto,0.25),
              Mediana = median(Efecto),
              Media = mean(Efecto),
              Q75 = quantile(Efecto,0.75),
              Q90 = quantile(Efecto,0.90),
              Q975 = quantile(Efecto,0.975),
              p_neg = sum(Efecto<0)/n()) %>% 
    mutate(p_pos = 1-p_neg,
           Sign50 = equals(sign(Q25),sign(Q75)),
           Sign80 = equals(sign(Q10),sign(Q90)),
           Sign95 = equals(sign(Q025),sign(Q975)),
           Efecto_Mediana = if_else(Mediana > 0, "Positivo", "Negativo"),
           Efecto_Media = if_else(Media > 0, "Positivo", "Negativo"),
           Efecto_PH = if_else(p_pos>p_neg,"Positivo","Negativo")) %>% 
    ungroup
  
  return(resumen_efectos)
}

coeficientes <- c("beta_ajus","gamma_ajus","delta_ajus")

# efectos_desequilibrados <- function(.data,n_simul,coef){
# 
#   coeficientes <- extrae_simulaciones(.data = datos_modelo, 
#                                      n_simul = 4000, 
#                                      tipo = "Compuesto", 
#                                      pars =  coef) %>% 
#     convierte_a_etiquetas
#   
#   resumen_efectos_desequilibrada <-  left_join(coeficientes,interceptos) %>% 
#     mutate(Efecto = inv_logit(Intercepto + Valor) - inv_logit(Intercepto)) %>% 
#     group_by(COD_DPTO,Cats) %>% 
#     summarise(Q025 = quantile(Efecto,0.025),
#               Q10 = quantile(Efecto,0.10),
#               Q25 = quantile(Efecto,0.25),
#               Mediana = median(Efecto),
#               Media = mean(Efecto),
#               Q75 = quantile(Efecto,0.75),
#               Q90 = quantile(Efecto,0.90),
#               Q975 = quantile(Efecto,0.975),
#               p_neg = sum(Efecto<0)/n()) %>% 
#     mutate(p_pos = 1-p_neg,
#            Sign50 = equals(sign(Q25),sign(Q75)),
#            Sign80 = equals(sign(Q10),sign(Q90)),
#            Sign95 = equals(sign(Q025),sign(Q975)),
#            Efecto_Mediana = if_else(Mediana > 0, "Positivo", "Negativo"),
#            Efecto_Media = if_else(Media > 0, "Positivo", "Negativo"),
#            Efecto_PH = if_else(p_pos>p_neg,"Positivo","Negativo")) %>% 
#     ungroup
#   
#   return(resumen_efectos_desequilibrada)
# }


# resumen_efectos_desequilibrada %>% 
#   ungroup %>% 
#   filter(!(Cats %in% c("Ed1","Inm","Hom"))) %>% 
#   left_join(grid_modelo, by = c("Cats"="code")) %>% 
#   rename(Fila = row, Columna = col)
  


graf_efecto <- function(.data, lims){
  
  .data %<>% 
    mutate(Orden = rank(Media))
  
  ggplot(.data,aes(y=Orden,yend=Orden,color=Efecto_Media)) + 
    geom_vline(xintercept = 0, linetype = 2, color = paleta_tesis_fn$COLOR[1]) + 
    geom_segment(data = filter(.data,Sign95), aes(x=Q025,xend=Q975),
                 size=rel(0.1)) + 
    geom_segment(data = filter(.data,Sign95), aes(x=Q10,xend=Q90),
                 size=rel(0.3)) + 
    geom_segment(data = filter(.data,Sign95), aes(x=Q25,xend=Q75),
                 size=rel(0.4)) + 
    geom_point(aes(x=Mediana,alpha=Sign95), 
               size = rel(0.5)) + 
    geom_segment(data = filter(.data,!Sign95), aes(x=Q025,xend=Q975),
                 size=rel(0.1), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    geom_segment(data = filter(.data,!Sign95), aes(x=Q10,xend=Q90),
                 size=rel(0.3), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    geom_segment(data = filter(.data,!Sign95), aes(x=Q25,xend=Q75),
                 size=rel(0.4), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    geom_point(data = filter(.data,!Sign95), aes(x=Mediana), 
               size = rel(0.5), alpha = 0.4, color = paleta_tesis_fn$COLOR[1]) + 
    scale_color_manual(values = paleta_tesis_fn$COLOR[c(6,2)]) + 
    scale_y_continuous(breaks = .data$Orden, labels = .data$NOM_DPTO) + 
    scale_x_continuous(limits = c(lims$Min,lims$Max), labels = scales::percent) + 
    labs(title = unique(.data$Etiqueta)) + 
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5, size = rel(0.9)),
          axis.text.y = element_text(size = rel(0.2)),
          axis.title = element_blank(), 
          legend.position = "none")
}

genera_fila_grafs_ef <- function(variable){
  
  datos_graf <- equiv_var_modelo %>% 
    filter(Variable == variable) %>% 
    extract2("Cats") %>% 
    map_dfr(genera_resumen_efecto) %>% 
    left_join(equiv_var_modelo) %>% 
    left_join(DEPARTAMENTOS)
  
  lims_x <- datos_graf %>%
    summarise(Min =  min(Q025) - .05,
              Max =  max(Q975) + .05)
  
  datos_graf %>% 
    split(.$Cats) %>% 
    map(graf_efecto, lims = lims_x) %>% 
    plot_grid(plotlist  = ., nrow = 1)
  
}

genera_fila_dorling_ef <- function(variable){
  
  datos_graf <- equiv_var_modelo %>% 
    filter(Variable == variable) %>% 
    extract2("Cats") %>% 
    map_dfr(genera_resumen_efecto) %>% 
    left_join(equiv_var_modelo) %>% 
    left_join(DEPARTAMENTOS)
  
  datos_graf %>% 
    split(.$Cats) %>% 
    map(graf_efecto, lims = lims_x) %>% 
    plot_grid(plotlist  = ., nrow = 1)
  
}

{ggdraw() + 
    draw_plot(genera_fila_grafs_ef("Escolaridad"), 
              y = 0.8, width = 5/8, height = 0.2) + 
    draw_plot(genera_fila_grafs_ef("Cat. Socioprof."), 
              y = 0.6, width = 8/8, height = 0.2) + 
    draw_plot(genera_fila_grafs_ef("Edad"), 
              y = 0.4, width = 6/8, height = 0.2) + 
    draw_plot(genera_fila_grafs_ef("Cond. Migratoria"), 
              y = 0.2, width = 2/8, height = 0.2) + 
    draw_plot(genera_fila_grafs_ef("Sexo"), 
              y = 0, width = 2/8, height = 0.2)} %>% 
  ggsave(plot = ., filename = "Efectos_Todos_Int2.pdf", width = 30, height = 15, device = cairo_pdf)

