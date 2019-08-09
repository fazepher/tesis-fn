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
library(cowplot)

inv_logit <- function(x){
  1/(1+exp(-x))
}

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

convierte_a_etiquetas_E <- function(.data){
  .data %>% 
    mutate(Param = str_remove_all(Param,"(\\d*,)|(\\[\\d*\\])")) %>% 
    separate(Param,c("Param","n_cat"),"_ajus\\[",fill="right") %>% 
    mutate(n_cat = str_remove_all(n_cat,"\\]") %>% as.numeric) %>% 
    left_join(equiv_var_modelo_E, by = c("Param","n_cat")) %>% 
    mutate(Etiqueta = if_else(is.na(Etiqueta),"Intercepto",Etiqueta))
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
remove(COMUNAS_2007)

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

equiv_var_modelo_E <- equivalencia_variables %>% 
  mutate(Param = case_when(Variable == "Escolaridad" ~ "beta",
                           Variable == "Cat. Socioprof." ~ "gamma",
                           Variable == "Edad" ~ "delta",
                           Variable == "Cond. Migratoria" ~ "lambda",
                           Variable == "Sexo" ~ "kappa",
                           Variable == "Ocupación Juvenil"~"zeta")) %>% 
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
                            Variable == "Sexo" ~ 5,
                            Variable == "Ocupación General" ~ 6,
                            Variable == "Ocupación Juvenil" ~ 7,
                            Variable == "Ocupación Mayores" ~ 8),
            col = n_cat)

aux_dptos <- DEPARTAMENTOS %>% 
  mutate(COD_REG = factor(COD_REG,levels = orden_regiones,ordered = T)) %>% 
  arrange(COD_REG) %>% 
  group_by(COD_REG) %>% 
  mutate(n_dep = 1:n()) %>% 
  filter(!is.na(COD_REG))

#### ANALIZA ####

rangos <- grid_modelo %>% 
  arrange(row,col) %>% 
  extract2("code") %>% 
  {select(datos_P12,COD_DPTO,.)} %>% 
  gather(Cats,X,-COD_DPTO) %>% 
  group_by(COD_DPTO,Cats) %>% 
  summarise(X_barra = mean(X),
            X_max = max(X),
            X_min = min(X),
            de_X = sd(X)) %>% 
  ungroup %>% 
  mutate(Dif_Sup = X_max - X_barra, 
         Dif_Inf = X_barra - X_min, 
         X_extrema = X_barra + de_X,
         Dif = X_extrema - X_barra) %>% 
  left_join(equiv_var_modelo) %>% 
  group_by(COD_DPTO,Variable) %>% 
  mutate(div = max(n_cat)-1) %>% 
  ungroup %>% 
  select(Variable,Param,Cats,COD_DPTO,X_barra,X_extrema,Dif,div)

#### EFECTOS ####

genera_resumen_efecto <- function(cat,modelo_comp = TRUE){
  
  print.noquote("Generando Efectos para")
  print.noquote(cat)
  
  v_extrema <- etas %>% 
    filter(Cats == cat) %>% 
    select(Cats,Simul,Dif,COD_DPTO,Variable,Eta_extrema) %>% 
    left_join(interceptos) %>% 
    mutate(Eta_extrema = Eta_extrema + Intercepto) %>% 
    select(-Intercepto)
  
  difs_cat <- v_extrema %>% 
    transmute(COD_DPTO,Dif_cat = Dif) %>% 
    distinct()
  
  v_extrema <- etas %>% 
    filter(Variable == unique(v_extrema$Variable), Cats != unique(v_extrema$Cats)) %>% 
    select(Simul:X_barra) %>% 
    group_by(Simul,COD_DPTO) %>% 
    mutate(prop = X_barra/sum(X_barra)) %>% 
    left_join(difs_cat) %>%
    mutate(X_control = X_barra*(1-Dif_cat*prop),
           Eta_control = Valor*X_control) %>% 
    summarise(Eta_control = sum(Eta_control)) %>% 
    left_join(v_extrema) %>% 
    mutate(Eta_extrema = Eta_extrema + Eta_control) %>% 
    select(-Eta_control)
  
  if(modelo_comp){
    v_extrema <- etas %>% 
      filter(Variable != unique(v_extrema$Variable)) %>% 
      group_by(Simul,COD_DPTO) %>% 
      summarise(Eta_barra = sum(Eta_barra)) %>% 
      ungroup %>% 
      left_join(v_extrema) %>% 
      mutate(v_extrema = inv_logit(Eta_barra + Eta_extrema)) %>% 
      select(-Eta_extrema,-Eta_barra)
  } else{
    v_extrema %<>% mutate(v_extrema = inv_logit(Eta_extrema)) %>% 
      select(-Eta_extrema)
  }
  
  print.noquote("Generando Resúmen")
  
  resumen_efectos <- v_extrema %>% 
    left_join(v_barras) %>% 
    mutate(Efecto = v_extrema - v_barra) %>% 
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

extrae_interceptos <- function(model_data){
  extrae_simulaciones(.data = model_data, 
                      n_simul = 4000, 
                      tipo = "Compuesto", 
                      pars = "alfa") %>% 
    transmute(Simul,COD_DPTO,Intercepto = Valor)
}

extrae_etas <- function(model_data){
  extrae_simulaciones(.data = model_data, 
                      n_simul = 4000, 
                      tipo = "Compuesto", 
                      pars =  c("alfa","lp__","log_lik"),
                      include = FALSE) %>% 
    convierte_a_etiquetas %>% 
    select(Simul,Cats,COD_DPTO,Valor) %>% 
    left_join(rangos, by = c("Cats","COD_DPTO")) %>% 
    mutate(Eta_barra = Valor*X_barra, 
           Eta_extrema = Valor*X_extrema) %>% 
    select(-div)
}

extrae_etas_E <- function(model_data){
  extrae_simulaciones(.data = model_data, 
                      n_simul = 4000, 
                      tipo = "Compuesto", 
                      pars =  c("alfa","lp__","log_lik"),
                      include = FALSE) %>% 
    convierte_a_etiquetas_E %>% 
    select(Simul,Cats,COD_DPTO,Valor) %>% 
    left_join(rangos, by = c("Cats","COD_DPTO")) %>% 
    mutate(Eta_barra = Valor*X_barra, 
           Eta_extrema = Valor*X_extrema) %>% 
    select(-div)
}

extrae_v_barras <- function(etas_modelo, interceptos_modelo){
  v_barras <- etas_modelo %>% 
    group_by(Simul,COD_DPTO) %>% 
    summarise(Eta_barra = sum(Eta_barra)) %>% 
    ungroup %>% 
    left_join(interceptos_modelo, 
              by = c("Simul","COD_DPTO")) %>% 
    transmute(Simul,
              COD_DPTO,
              v_barra = Eta_barra %>% add(Intercepto) %>% inv_logit)
}

#### MODELO Escolaridad ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Ind/Modelo_Jer_Escolaridad.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable == "Escolaridad") %>% 
  extract2("Cats") %>% 
  map_dfr(~genera_resumen_efecto(.x,modelo_comp = F)) %>% 
  mutate(Modelo = "0") %>% 
  select(Modelo,everything()) 

#### MODELO A ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_A.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable %in% c("Escolaridad","Cat. Socioprof.")) %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "A") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)

#### MODELO B ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_B.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable %in% c("Escolaridad","Cat. Socioprof.","Edad")) %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "B") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)

#### MODELO C ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_C.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable %in% c("Escolaridad","Cat. Socioprof.","Edad","Cond. Migratoria")) %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "C") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)

#### MODELO D ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_D.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable %in% c("Escolaridad","Cat. Socioprof.","Edad","Cond. Migratoria","Sexo")) %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "D") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)

#### MODELO E ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_E.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas_E(datos_modelo)
remove(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable %in% c("Escolaridad","Cat. Socioprof.","Edad","Cond. Migratoria","Sexo",
                         "Ocupación Juvenil")) %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "E") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)
remove(interceptos,etas,v_barras)

#### MODELO F ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_F.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)
remove(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable %in% c("Escolaridad","Cat. Socioprof.","Edad","Cond. Migratoria","Sexo",
                         "Ocupación General")) %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "F") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)
remove(interceptos,etas,v_barras)

#### MODELO G ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_G.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)
remove(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(!{Variable %in% c("Nacionalidad","Ocupación Mayores")}) %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "G") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)
remove(interceptos,etas,v_barras)

#### MODELO H ####

datos_modelo <- read_rds("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_H.rds")

interceptos <- extrae_interceptos(datos_modelo)

etas <- extrae_etas(datos_modelo)
remove(datos_modelo)

v_barras <- extrae_v_barras(etas,interceptos)

resumen_efecto <- equivalencia_variables %>% 
  filter(Variable != "Nacionalidad") %>% 
  extract2("Cats") %>% 
  map_dfr(genera_resumen_efecto) %>% 
  mutate(Modelo = "H") %>% 
  select(Modelo,everything()) %>% 
  bind_rows(resumen_efecto)
remove(interceptos,etas,v_barras)

#### GUARDAR EFECTOS ####

write.csv(x = resumen_efecto, file = "EFECTOS/Res_Efectos.csv", row.names = FALSE)

#### GRÁFICOS ####

evol_efecto <- function(variable,cats_incluidas = unique(equivalencia_variables$Cats),tam_rel_etiqueta = 1.5){
  
  
  efectos_medianos <- resumen_efecto %>% 
    left_join(mutate(equivalencia_variables,Etiqueta=factor(Etiqueta,ordered=T))) %>% 
    filter(Variable %in% variable, 
           Cats %in% cats_incluidas) %>% 
    select(Variable,Cats,Etiqueta,Modelo,COD_DPTO,Mediana,Sign95) %>% 
    group_by(Cats,COD_DPTO) %>% 
    arrange(COD_DPTO,Cats,Modelo) %>% 
    mutate(Modelo_Sig = lead(Modelo),
           Mediana_Sig = lead(Mediana), 
           Sign95_Sig = lead(Sign95)) %>% 
    arrange(Modelo) %>% 
    ggplot(aes(x=Modelo,xend=Modelo_Sig,
               y=Mediana,yend=Mediana_Sig)) + 
    geom_path(aes(group=COD_DPTO), color = paleta_tesis_fn$COLOR[1], size = rel(0.1)) + 
    geom_hline(yintercept = 0, color = paleta_tesis_fn$COLOR[4], size = rel(1)) + 
    facet_wrap(~Etiqueta, nrow = 1, labeller = labeller(Etiqueta = label_wrap_gen(25))) + 
    scale_y_continuous(labels = function(x){round(100*x,1) %>% paste("pp",sep="")}) + 
    labs(y="Efecto mediano estimado por departamento") + 
    theme_classic() + 
    theme(strip.text = element_text(size = rel(tam_rel_etiqueta)),
          axis.title.x = element_text(size = rel(1.5), margin = margin(t=15)),
          axis.title.y = element_text(size = rel(1.75), margin = margin(r=10)),
          axis.text.x = element_text(size = rel(1)),
          axis.text.y = element_text(size = rel(1.5)))
  
  efectos_significativos <- resumen_efecto %>% 
    left_join(mutate(equivalencia_variables,Etiqueta=factor(Etiqueta,ordered=T))) %>% 
    filter(Variable %in% variable, 
           Cats %in% cats_incluidas) %>% 
    select(Variable,Cats,Etiqueta,Modelo,COD_DPTO,Mediana,Efecto_Mediana,Sign95) %>% 
    group_by(Variable,Etiqueta,Modelo,Efecto_Mediana) %>% 
    summarise(Significativos = sum(Sign95)) %>% 
    ggplot(aes(x=Modelo,y=Significativos,color=Efecto_Mediana,group=Efecto_Mediana)) + 
    geom_path(key_glyph = "point") + 
    geom_label(aes(label=Significativos), key_glyph = "point") + 
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
  
  plot_grid(efectos_medianos,efectos_significativos,align = "v", nrow=2) %>% 
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
                 size=rel(0.4)) + 
    geom_segment(data = filter(.data,Sign95), aes(x=Q10,xend=Q90),
                 size=rel(0.6)) + 
    geom_segment(data = filter(.data,Sign95), aes(x=Q25,xend=Q75),
                 size=rel(0.8)) + 
    geom_point(aes(x=Mediana,alpha=Sign95), 
               size = rel(1.5)) + 
    geom_segment(data = filter(.data,!Sign95), aes(x=Q025,xend=Q975),
                 size=rel(0.4), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    geom_segment(data = filter(.data,!Sign95), aes(x=Q10,xend=Q90),
                 size=rel(0.6), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    geom_segment(data = filter(.data,!Sign95), aes(x=Q25,xend=Q75),
                 size=rel(0.8), alpha = 0.1, color = paleta_tesis_fn$COLOR[1]) +
    geom_point(data = filter(.data,!Sign95), aes(x=Mediana), 
               size = rel(1.5), alpha = 0.6, color = paleta_tesis_fn$COLOR[1]) + 
    scale_color_manual(values = colores) + 
    scale_y_continuous(breaks = .data$Orden, labels = .data$NOM_DPTO) + 
    scale_x_continuous(limits = c(-.06,.06), 
                       labels = c("-5pp","-2.5pp","0","+2.5pp","+5pp"), 
                       breaks = seq(-.05,.05, by = .025),
                       sec.axis = dup_axis()) + 
    labs(title = unique(.data$Etiqueta)) + 
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.25)),
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
                         limits = c(-.06,.06), 
                         breaks = seq(-.05,.05,by=.025), 
                         labels = c("-5pp","-2.5pp","0","+2.5pp","+5pp")) + 
    labs(title = unique(.data$Etiqueta)) + 
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = rel(1.25)),
          legend.position = "none")
}

dorling_efecto <- function(.data,colores=paleta_tesis_fn$COLOR[c(6,2)]){
  
  .data %<>% 
    left_join(dorling_dptos, by = c("COD_DPTO"="code_insee")) %>% 
    st_as_sf() %>% 
    st_transform(crs = 25832)
  
  ggplot(.data) + 
    geom_sf(data = fronteras_reg_dorling,fill="transparent", size = rel(0.05)) + 
    geom_sf(data = filter(.data,Sign95),aes(fill=Media), size = rel(0.5)) + 
    geom_sf_text(aes(label=round(100*Media,1)),color=paleta_tesis_fn$COLOR[1], size = rel(3)) + 
    scale_fill_gradient2(low=colores[1],mid="white",midpoint = 0, high = colores[2],
                         limits = c(-.06,.06), 
                         breaks = seq(-.05,.05,by=.025), 
                         labels = c("-5pp","-2.5pp","0","+2.5pp","+5pp")) + 
    labs(title = unique(.data$Etiqueta)) + 
    theme_bw() + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = rel(1.25)),
          legend.position = "none")
}

genera_fila_ef <- function(cats,tipo = "Graf",modelo="H",datos_resumidos=resumen_efecto,filas = 1,...){
  
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
  
  plot_grid(plotlist = graf, nrow = filas)
  
}

ggsave(plot = evol_efecto("Escolaridad"), 
       filename = "EFECTOS/Evol_Efectos_Escolaridad.pdf",width = 25,height = 12,device = cairo_pdf)
ggsave(plot = evol_efecto("Cat. Socioprof."), 
       filename = "EFECTOS/Evol_Efectos_Cat_Socioprof.pdf",width = 25,height = 12,device = cairo_pdf)
ggsave(plot = evol_efecto("Edad"), 
       filename = "EFECTOS/Evol_Efectos_Edad.pdf",width = 25,height = 12,device = cairo_pdf)
ggsave(plot = evol_efecto(c("Cond. Migratoria","Sexo","Ocupación Juvenil","Ocupación General"),
                          cats_incluidas = c("Inm","Muj","Des1","Des2")), 
       filename = "EFECTOS/Evol_Efectos_Dicotom.pdf",width = 25,height = 12,device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Escolaridad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"), filas = 2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Efectos_Escolaridad_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Cat. Socioprof.") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Efectos_Cat_Socioprof_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Edad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Efectos_Edad_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

c("Des1","Des2","Des3","Inm","Muj") %>% 
  genera_fila_ef(datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Efectos_Dicotom_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Escolaridad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Dorling", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"), filas = 2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Dorling_Efectos_Escolaridad_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Cat. Socioprof.") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Dorling", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Dorling_Efectos_Cat_Socioprof_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Edad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Dorling", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Dorling_Efectos_Edad_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

c("Des1","Des2","Des3","Inm","Muj") %>% 
  genera_fila_ef(tipo = "Dorling", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Dorling_Efectos_Dicotom_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)


equiv_var_modelo %>% 
  filter(Variable == "Escolaridad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"), filas = 2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Escolaridad_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Cat. Socioprof.") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Cat_Socioprof_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

equiv_var_modelo %>% 
  filter(Variable == "Edad") %>% 
  extract2("Cats") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Edad_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

c("Des1","Des2","Des3","Inm","Muj") %>% 
  genera_fila_ef(tipo = "Mapa", datos_resumidos = left_join(resumen_efecto,DEPARTAMENTOS,by="COD_DPTO"),filas=2) %>% 
  ggsave(plot = ., filename = "EFECTOS/Mapa_Efectos_Dicotom_Modelo_H.pdf", width = 30, height = 18, device = cairo_pdf)

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
