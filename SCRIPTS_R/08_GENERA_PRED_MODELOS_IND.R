############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ MODELOS INDIVIDUALES ##########################################
############################################################################################################
############################################ GENERA PREDICCIONES ###########################################
############################################################################################################


library(rstan) # Versión 2.18.2
rstan_options(auto_write = TRUE)

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

#### WAIC ####

aux_var_nombres_modelos <- function(variable){
  str_replace_all(variable," ","_") %>% 
    str_remove_all("\\.") %>% 
    str_remove_all("pación")
}

genera_nombre_archivo_modelo <- function(variable,tipo,tabla_equiv = equivalencia_variables){
  
  var_archivo <- tabla_equiv %>% 
    filter(Variable == variable) %>% 
    mutate(Variable = aux_var_nombres_modelos(Variable)) %>% 
    extract2(1) %>% 
    unique
  
  case_when(tipo == "Nacional" ~ "MODELOS_STAN/Modelos_Nal_Ind/Modelo_Nal_",
            tipo == "Departamental" ~ "MODELOS_STAN/Modelos_Dep_Ind/Modelo_Dep_",
            tipo == "Jerárquico" ~ "MODELOS_STAN/Modelos_Jer_Ind/Modelo_Jer_") %>% 
    paste(var_archivo, sep = "") %>% 
    paste(".rds", sep = "")
}

genera_nombres_archivos_modelos <- function(tabla){
  
  pmap_chr(tabla,~genera_nombre_archivo_modelo(..1,..2))
  
}

#### Pruebas ####

datos_P12 <- prueba_datos_unificados %>% 
  filter(ELECCION == "Presidenciales 2012") %>% 
  mutate(Int = 1) %>% 
  select(CODGEO,COD_DPTO,INSCRITOS,VOT_CANDIDATO,Int,Hom:Esc) %>% 
  distinct()

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

genera_predicciones_modelo <- function(variable, tipo, n_simul,..., 
                                       datos_prueba = datos_P12, semilla_stan = 51295, 
                                       modelo = modelo_pred_compilado){
  
  print.noquote("##### EXTRAYENDO SIMULACIONES #####")
  datos_modelo <- genera_nombre_archivo_modelo(variable = variable, tipo = tipo) %>% 
    readRDS
  cats <- equivalencia_variables %>% 
    filter(Variable == variable) %>% 
    extract2("Cats")
  simulaciones_mcmc <- extrae_simulaciones(.data = datos_modelo, 
                                           n_simul = n_simul, 
                                           tipo = tipo, 
                                           ...)
  
  print.noquote("##### PREPARANDO SIMULACIONES #####")
  data_test <- list(C = nrow(datos_prueba),
                    D = unique(datos_prueba$COD_DPTO) %>% 
                      length,
                    K = length(cats),
                    M = max(simulaciones_mcmc$Simul),
                    tol = 0.015,
                    votos = datos_prueba$VOT_CANDIDATO,
                    inscritos = datos_prueba$INSCRITOS,
                    dpto = datos_prueba$COD_DPTO %>% 
                      factor(levels = orden_departamentos, ordered = T) %>% 
                      as.integer,
                    variable = select(datos_prueba,cats) %>% 
                      as.matrix,
                    alfa = simulaciones_mcmc %>% 
                      filter(str_detect(Param,"alfa")) %>% 
                      split(.$Simul) %>% 
                      map(~.x$Valor) %>% 
                      as.array,
                    beta_ajus = simulaciones_mcmc %>% 
                      filter(str_detect(Param,"beta_ajus")) %>% 
                      mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos, ordered = T) %>% as.integer,
                             Param = str_remove_all(Param,"\\d*,")) %>% 
                      spread(Param,Valor) %>% 
                      split(.$Simul) %>% 
                      map(~ select(.x,-Simul,-COD_DPTO) %>% as.matrix))
  
  print.noquote("##### GENERANDO PREDICCIONES #####")
  pred_file_test <- sampling(object = modelo,
                             data = data_test,
                             chains = 1,iter = 1, algorithm = "Fixed_param", seed = semilla_stan)
  
  return(pred_file_test)
  
}

modelo_pred_compilado <- stan_model(file = "MODELOS_STAN/Modelos_Individuales_Indep_Solo_Pred.stan")

genera_pred_modelos <- equivalencia_variables %>% 
  distinct(Variable) %>% 
  extract2(1) %>% 
  map_dfr(~ tibble(Variable = .x, 
                   Tipo = c("Nacional","Departamental","Jerárquico")) %>% 
            {mutate(., 
                    Archivo_Guardar = genera_nombres_archivos_modelos(.) %>% 
                      str_replace_all(".rds","_PRED.rds"))}) %>% 
  pmap(~ genera_predicciones_modelo(variable = ..1, tipo = ..2, 
                                    n_simul = 4000, 
                                    pars = c("lp__","log_lik"), 
                                    include = FALSE) %>% 
         list(Variable = ..1, Tipo = ..2, Predicciones = .) %>% 
         saveRDS(file = ..3))
remove(genera_pred_modelos)

