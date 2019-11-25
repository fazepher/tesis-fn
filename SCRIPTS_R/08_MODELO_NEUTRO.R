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

#### CARGANDO DATOS ####

print.noquote("###############################################################")
print.noquote("###################### Cargando datos #########################")
print.noquote("###############################################################")

datos_electorales_completos <- read_csv("DATOS/LIMPIOS/RESULTADOS_ELECTORALES.csv", 
                                        locale = locale(encoding = "latin1"))
muestra_previa <- read_csv("DATOS/Muestra_previa.csv", 
                           locale = locale(encoding = "latin1"))

muestra_previa_datos_unificados <- datos_electorales_completos %>% 
  filter(FAMILIA == "FN") %>% 
  mutate(AÑO = str_extract(ELECCION,"\\d{4}$") %>% as.integer) %>% 
  right_join(muestra_previa) %>% 
  mutate(COD_REG = as.character(COD_REG)) %>% 
  select(CODGEO,NOM_COMUNA:NOM_REG,AÑO,ELECCION,everything())

muestra_previa_datos_base <- muestra_previa_datos_unificados %>% 
  filter(ELECCION == "Presidenciales 2012") %>% 
  mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos, ordered = T)) %>% 
  {list(C = nrow(.), 
        votos = .$VOT_CANDIDATO,
        inscritos = .$INSCRITOS)}

### Compilamos solo una vez ####
modelo_compilado_neutro <- stan_model(file = "MODELOS_STAN/Modelo_Neutro.stan")

set.seed(51295)
cat("----------\n")
cat("Modelando en STAN\n")
cat("----------\n")
  
modelo <- sampling(object = modelo_compilado_neutro, 
                   data = c(muestra_previa_datos_base,list(m_a = 0, s_a = 1, sigma_alfa = 0.75)), 
                   init = "random", 
                   control = list(max_treedepth = 12),
                   seed = 51295) 
cat("Tiempos\n")
get_elapsed_time(modelo) %>% 
  as_tibble %>% 
  set_colnames(c("Calentamiento","Muestreo")) %>% 
  mutate(Total = Calentamiento + Muestreo) %>% 
  print
cat("----------\n")

cat("Resumen de MCMC\n")
rstan:::print.stanfit(modelo)
cat("----------\n")


cat("Guardando simulaciones en archivo RDS\n")
saveRDS(modelo, file = "MODELOS_STAN/Modelo_Neutro/Modelo_Neutro.rds")
cat("###########\n")
cat("###########\n")

