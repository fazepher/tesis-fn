############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ MODELOS INDIVIDUALES ##########################################
############################################################################################################
############################################## DATOS CENSALES ##############################################
############################################################################################################


library(rstan) # Versión 2.18.2
#library(loo) # Versión 2.1.0
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

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

muestra_datos_unificados <- datos_electorales_completos %>% 
  filter(FAMILIA == "FN") %>% 
  mutate(AÑO = str_extract(ELECCION,"\\d{4}$") %>% as.integer) %>% 
  inner_join(datos_censales, by = c("CODGEO","AÑO")) %>% 
  inner_join(otros_datos_comunales, by = c("CODGEO","AÑO")) %>% 
  right_join(muestra) %>% 
  mutate(COD_REG = as.character(COD_REG)) %>% 
  select(CODGEO,NOM_COMUNA:NOM_REG,AÑO,ELECCION,everything())

prueba_datos_unificados <- datos_electorales_completos %>% 
  filter(FAMILIA == "FN") %>% 
  mutate(AÑO = str_extract(ELECCION,"\\d{4}$") %>% as.integer) %>% 
  inner_join(datos_censales, by = c("CODGEO","AÑO")) %>% 
  inner_join(otros_datos_comunales, by = c("CODGEO","AÑO")) %>% 
  left_join(full_join(COMUNAS_2007,COMUNAS_2012)) %>% 
  select(CODGEO,NOM_COMUNA:NOM_REG,AÑO,ELECCION,everything())


muestra_datos_base <- muestra_datos_unificados %>% 
  filter(ELECCION == "Presidenciales 2012") %>% 
  mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos, ordered = T)) %>% 
  {list(C = nrow(.), 
        votos = .$VOT_CANDIDATO,
        inscritos = .$INSCRITOS)}

aux_dptos_nal <- list(D = 1,
                      dpto = rep(1,muestra_datos_base$C))

aux_dptos_jer <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos, ordered = T)) %>%
  {list(D = .$COD_DPTO %>% unique %>% length,
        dpto = .$COD_DPTO %>% as.integer())}

### Compilamos solo una vez ####
modelo_compilado_ind <- stan_model(file = "MODELOS_STAN/Modelos_Individuales_Indep.stan")
modelo_compilado_jer <- stan_model(file = "MODELOS_STAN/Modelos_Individuales_Jer.stan")


modela_variable_individual <- function(nom_variable, tipo, archivo, 
                                       semilla = 51295, elec = "Presidenciales 2012", 
                                       pars_resumen = c("alfa","beta_ajus"), pars_resumen_incluye = TRUE,
                                       regresa_modelo = FALSE,
                                       ...){
  
  cat("Obteniendo datos\n")
  paste("Variable:", nom_variable,"\n") %>% 
    cat
  paste("Elección:", elec,"\n") %>% 
    cat
  
  tabla_equiv_var <- filter(equivalencia_variables, Variable == nom_variable)
  cats_ind <- nrow(tabla_equiv_var) - 1
  nom_vars <- tabla_equiv_var$Cats
  
  variable <-  muestra_datos_unificados %>% 
    filter(ELECCION == elec) %>% 
    select(nom_vars) %>% 
    {list(K = cats_ind + 1,
          variable = as.matrix(.))}
  
  cat("----------\n")
  if(tipo %in% c("Nacional","Departamental")){
    
    modelo_stan <- modelo_compilado_ind
    
    hiperparams <- list(mu_alfa = -1.7, sigma_alfa = 0.25,
                        mu_beta = array(0, dim = cats_ind), sigma_beta = 0.5)
    
    if(tipo == "Nacional"){
      
      datos_modelo <- c(muestra_datos_base, 
                        aux_dptos_nal,
                        hiperparams, 
                        variable)
      
      inicia_cadena <- function(dptos = 1, cat_indep = cats_ind){
        
        list(alfa = rep(rnorm(1,-1.7,2), dptos) %>% array(dim = dptos), 
             beta = rep(rnorm(cat_indep,0,4), dptos) %>% array(dim = c(dptos, cat_indep))) 
      }
      
      cat("Iniciando modelado nacional\n")
    } else{
      datos_modelo <- c(muestra_datos_base, 
                        aux_dptos_jer,
                        hiperparams, 
                        variable)
      
      inicia_cadena <- function(dptos = 96, cat_indep = cats_ind){
        
        list(alfa = rep(rnorm(1,-1.7,2), dptos) %>% array(dim = dptos), 
             beta = rep(rnorm(cat_indep,0,4), dptos) %>% array(dim = c(dptos, cat_indep))) 
      }
      
      cat("Iniciando modelado departamental\n")
    }
    
  } else{
    
    modelo_stan <- modelo_compilado_jer
    
    hiperparams <- list(m_alfa = -1.7, s_alfa = 0.25,
                        m_beta = array(0, dim = cats_ind), s_beta = array(0.5, dim = cats_ind),
                        sigma_alfa = 1,
                        sigma_beta = array(1, dim = cats_ind))
      
    datos_modelo <- c(muestra_datos_base, 
                      aux_dptos_jer,
                      hiperparams, 
                      variable)
    
    inicia_cadena <- function(dptos = 96, cat_indep = cats_ind){
      
      list(alfa = rep(rnorm(1,-1.7,2), dptos) %>% array(dim = dptos),
           beta = rep(rnorm(cat_indep,0,4), dptos) %>% array(dim = c(dptos, cat_indep)))
    }
    
    cat("Iniciando modelado jerárquico\n")
    
  }
  
  set.seed(semilla)
  cat("----------\n")
  cat("Modelando en STAN\n")
  cat("----------\n")
  
  if(length(list(...))){
    modelo <- sampling(object = modelo_stan, 
                       data = datos_modelo, 
                       init = inicia_cadena, 
                       ... = ...) 
  } else{
    modelo <- sampling(object = modelo_stan, 
                       data = datos_modelo, 
                       init = inicia_cadena, 
                       pars = c("alfa","beta_ajus","log_lik"), 
                       control = list(max_treedepth = 12),
                       seed = 51295) 
  }
  
  cat("Tiempos\n")
  get_elapsed_time(modelo) %>% 
    as_tibble %>% 
    set_colnames(c("Calentamiento","Muestreo")) %>% 
    mutate(Total = Calentamiento + Muestreo) %>% 
    print
  cat("----------\n")
  
  cat("Resumen de MCMC\n")
  rstan:::print.stanfit(modelo, pars = pars_resumen, include = pars_resumen_incluye)
  cat("----------\n")
  
  
  cat("Guardando simulaciones en archivo RDS\n")
  saveRDS(modelo, file = archivo)
  cat("###########\n")
  cat("###########\n")
  
  if(regresa_modelo){
    return(modelo)
  } 
  
}

#### Modelos Individuales ####

genera_modelos <- equivalencia_variables %>% 
  distinct(Variable) %>% 
  extract2(1) %>% 
  map_dfr(~ tibble(Variable = .x, 
                   Tipo = c("Nacional","Departamental","Jerárquico")) %>% 
            {mutate(., 
                    Archivo_Guardar = genera_nombres_archivos_modelos(.))}) %>% 
  pmap(~ modela_variable_individual(nom_variable = ..1,
                                    tipo = ..2, 
                                    archivo = ..3))
remove(genera_modelos)

