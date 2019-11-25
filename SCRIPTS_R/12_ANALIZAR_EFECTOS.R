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
            #de_X = sd(X),
            de_X = min(c(0.1,1-X_barra))) %>% 
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

