############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################# MODELOS COMPUESTOS ###########################################
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

#### Pruebas ####

datos_P12 <- prueba_datos_unificados %>% 
  filter(ELECCION == "Presidenciales 2012") %>% 
  mutate(Int = 1) %>% 
  select(CODGEO,COD_DPTO,INSCRITOS,VOT_CANDIDATO,Int,Hom:Esc) %>% 
  distinct()

variables <- list(x_escol = datos_P12 %>% 
                    select(Esc,Dip1:Dip4) %>% 
                    as.matrix, 
                  x_csp = datos_P12 %>% 
                    select(CSP1:CSP8) %>% 
                    as.matrix, 
                  x_edad = datos_P12 %>%
                    select(Ed1:Ed6) %>%
                    as.matrix, 
                  x_migr = datos_P12 %>%
                    select(Inm,Loc) %>%
                    as.matrix, 
                  x_sexo = datos_P12 %>%
                    select(Muj,Hom) %>%
                    as.matrix,
                  x_ocu_juv = datos_P12 %>%
                    select(Ocu1,Des1) %>%
                    as.matrix,
                  x_ocu_gral = datos_P12 %>%
                    select(Ocu2,Des2) %>%
                    as.matrix,
                  x_ocu_may = datos_P12 %>%
                    select(Ocu3,Des3) %>%
                    as.matrix)

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

extrae_coeficientes_mcmc <- function(simulaciones,coef,n_cats=NULL,tam_simul = 4000){
  
  simul_filtradas <-  simulaciones %>% 
    filter(str_detect(Param,coef)) 
  
  if(nrow(simul_filtradas) > 0){
    coeficientes_simulados <- simul_filtradas %>% 
      mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos, ordered = T) %>% as.integer,
             Param = str_remove_all(Param,"\\d*,")) %>% 
      spread(Param,Valor) %>% 
      split(.$Simul) %>% 
      map(~ select(.x,-Simul,-COD_DPTO) %>% as.matrix)
  } else{
    stopifnot(!is.null(n_cats))
    coeficientes_simulados <- map(1:tam_simul,~matrix(0,nrow=96,ncol = n_cats) %>% 
          set_colnames(paste(coef,"[",1:n_cats,"]",sep=""))) %>% 
      set_names(1:tam_simul)
  }
    
  return(coeficientes_simulados)  
}

genera_predicciones_modelo <- function(modelo, n_simul,..., 
                                       datos_prueba = datos_P12, semilla_stan = 51295, 
                                       modelo_compilado = modelo_pred_compilado){
  
  print.noquote("##### EXTRAYENDO SIMULACIONES #####")
  datos_modelo <- paste("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_",modelo,".rds", sep = "") %>% 
    read_rds
  
  simulaciones_mcmc <- extrae_simulaciones(.data = datos_modelo, 
                                           n_simul = n_simul, 
                                           tipo = "Jerárquico", 
                                           ...)
  
  print.noquote("##### PREPARANDO SIMULACIONES #####")
  data_test <- list(C = nrow(datos_prueba),
                    D = unique(datos_prueba$COD_DPTO) %>% 
                      length,
                    M = max(simulaciones_mcmc$Simul),
                    tol = 0.015,
                    votos = datos_prueba$VOT_CANDIDATO,
                    inscritos = datos_prueba$INSCRITOS,
                    dpto = datos_prueba$COD_DPTO %>% 
                      factor(levels = orden_departamentos, ordered = T) %>% 
                      as.integer,
                    alfa = simulaciones_mcmc %>% 
                      filter(str_detect(Param,"alfa")) %>% 
                      split(.$Simul) %>% 
                      map(~.x$Valor) %>% 
                      as.array,
                    beta_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"beta_ajus",
                                                         n_cats = 5, tam_simul = n_simul),
                    gamma_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"gamma_ajus",
                                                          n_cats = 8, tam_simul = n_simul),
                    delta_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"delta_ajus",
                                                          n_cats = 6, tam_simul = n_simul),
                    lambda_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"lambda_ajus",
                                                           n_cats = 2, tam_simul = n_simul),
                    kappa_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"kappa_ajus",
                                                          n_cats = 2, tam_simul = n_simul),
                    zeta_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"zeta_ajus",
                                                         n_cats = 2, tam_simul = n_simul),
                    xi_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"xi_ajus",
                                                         n_cats = 2, tam_simul = n_simul),
                    upsilon_ajus = extrae_coeficientes_mcmc(simulaciones_mcmc,"upsilon_ajus",
                                                         n_cats = 2, tam_simul = n_simul)) %>% 
    c(variables)
  
  print.noquote("##### GENERANDO PREDICCIONES #####")
  pred_file_test <- sampling(object = modelo_compilado,
                             data = data_test,
                             chains = 1,iter = 1, algorithm = "Fixed_param", seed = semilla_stan)
  
  return(pred_file_test)
  
}



modelo_pred_compilado <- stan_model(file = "MODELOS_STAN/Modelos_Compuestos_Solo_Pred.stan")
modelo_ocu_juv <- stan_model(file = "MODELOS_STAN/Modelos_Compuestos_Ocu_Juv_Solo_Pred.stan")
modelo_ocu_gral <- stan_model(file = "MODELOS_STAN/Modelos_Compuestos_Ocu_Gral_Solo_Pred.stan")
modelo_ocu_todas <- stan_model(file = "MODELOS_STAN/Modelos_Compuestos_Ocu_Todas_Solo_Pred.stan")

genera_pred_modelos <- LETTERS[1:4] %>%
  map(~ genera_predicciones_modelo(modelo = .x,
                                   n_simul = 4000,
                                   pars = c("lp__","log_lik"),
                                   include = FALSE) %>%
         list(Modelo = .x, Predicciones = .) %>%
         saveRDS(file = paste("MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto",.x,"PRED.rds", sep = "_")))
remove(genera_pred_modelos)

genera_pred_modelos <- modelo_ocu_juv %>%
  genera_predicciones_modelo(modelo = "E",
                             n_simul = 4000,
                             pars = c("lp__","log_lik"),
                             include = FALSE,
                             modelo_compilado = .) %>%
  list(Modelo = "E", Predicciones = .) %>%
  saveRDS(file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_E_PRED.rds")
remove(genera_pred_modelos)

genera_pred_modelos <- modelo_ocu_gral %>%
  genera_predicciones_modelo(modelo = "F",
                             n_simul = 4000,
                             pars = c("lp__","log_lik"),
                             include = FALSE,
                             modelo_compilado = .) %>%
  list(Modelo = "F", Predicciones = .) %>%
  saveRDS(file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_F_PRED.rds")
remove(genera_pred_modelos)

genera_pred_modelos <- modelo_ocu_todas %>%
  genera_predicciones_modelo(modelo = "G",
                             n_simul = 4000,
                             pars = c("lp__","log_lik"),
                             include = FALSE,
                             modelo_compilado = .) %>%
  list(Modelo = "G", Predicciones = .) %>%
  saveRDS(file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_G_PRED.rds")
remove(genera_pred_modelos)

genera_pred_modelos <- modelo_ocu_todas %>% 
  genera_predicciones_modelo(modelo = "H", 
                             n_simul = 4000, 
                             pars = c("lp__","log_lik"), 
                             include = FALSE, 
                             modelo_compilado = .) %>% 
  list(Modelo = "H", Predicciones = .) %>% 
  saveRDS(file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_H_PRED.rds")
remove(genera_pred_modelos)

