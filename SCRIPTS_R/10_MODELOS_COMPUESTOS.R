############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################ MODELOS COMPUESTOS ############################################
############################################################################################################
############################################# DATOS CENSALES ###############################################
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

aux_dptos_jer <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  mutate(COD_DPTO = factor(COD_DPTO,levels = orden_departamentos, ordered = T)) %>%
  {list(D = .$COD_DPTO %>% unique %>% length,
        dpto = .$COD_DPTO %>% as.integer())}

#### Modelos ####

x_escol <- muestra_datos_unificados %>% 
  filter(ELECCION == "Presidenciales 2012") %>% 
  select(Esc,Dip1:Dip4) %>% 
  as.matrix

x_csp <- muestra_datos_unificados %>% 
  filter(ELECCION == "Presidenciales 2012") %>% 
  select(CSP1:CSP8) %>% 
  as.matrix

x_edad <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  select(Ed1:Ed6) %>%
  as.matrix

x_migr <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  select(Inm,Loc) %>%
  as.matrix

x_sexo <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  select(Muj,Hom) %>%
  as.matrix

x_ocu_juv <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  select(Ocu1,Des1) %>%
  as.matrix

x_ocu_gral <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  select(Ocu2,Des2) %>%
  as.matrix

x_ocu_may <- muestra_datos_unificados %>%
  filter(ELECCION == "Presidenciales 2012") %>%
  select(Ocu3,Des3) %>%
  as.matrix

# #### Modelo A #####
# # Incluye: Escolaridad + CSP
# hiperparams <- list(m_alfa = -1.8, s_alfa = 0.5, sigma_alfa = 1,
#                     m_beta = array(0, dim = 4),
#                     s_beta = array(1, dim = 4),
#                     sigma_beta = array(1, dim = 4),
#                     m_gamma = array(0, dim = 7),
#                     s_gamma = array(1, dim = 7),
#                     sigma_gamma = array(1, dim = 7))
# 
# datos_modelo <- c(muestra_datos_base,
#                   aux_dptos_jer,
#                   hiperparams,
#                   list(x_escol = x_escol,
#                        x_csp = x_csp))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_A.stan",
#                data = datos_modelo,
#                init = inicia_cadena,
#                pars = c("alfa","beta_ajus","gamma_ajus","log_lik"),
#                control = list(max_treedepth = 12),
#                seed = 51295)
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_A.rds")
# remove(modelo)
# 
# #### Modelo B #####
# # Incluye: Escolaridad + CSP + Edad
# hiperparams <- list(m_alfa = -1.8, s_alfa = 0.5, sigma_alfa = 1,
#                     m_beta = array(0, dim = 4),
#                     s_beta = array(1, dim = 4),
#                     sigma_beta = array(1, dim = 4),
#                     m_gamma = array(0, dim = 7),
#                     s_gamma = array(1, dim = 7),
#                     sigma_gamma = array(1, dim = 7),
#                     m_delta = array(0, dim = 5),
#                     s_delta = array(1, dim = 5),
#                     sigma_delta = array(1, dim = 5))
# 
# datos_modelo <- c(muestra_datos_base,
#                   aux_dptos_jer,
#                   hiperparams,
#                   list(x_escol = x_escol,
#                        x_csp = x_csp,
#                        x_edad = x_edad))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)),
#        delta = rep(rnorm(5,0,4), dptos) %>% array(dim = c(dptos, 5)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_B.stan",
#                data = datos_modelo,
#                init = inicia_cadena,
#                pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus",
#                         "log_lik"),
#                control = list(max_treedepth = 12),
#                seed = 51295)
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_B.rds")
# remove(modelo)
# 
# #### Modelo C #####
# # Incluye: Escolaridad + CSP + Edad + Cond. Migr.
# hiperparams <- list(m_alfa = -1.8,
#                     s_alfa = 0.5,
#                     sigma_alfa = 1,
#                     m_beta = array(0, dim = 4),
#                     s_beta = array(1, dim = 4),
#                     sigma_beta = array(1, dim = 4),
#                     m_gamma = array(0, dim = 7),
#                     s_gamma = array(1, dim = 7),
#                     sigma_gamma = array(1, dim = 7),
#                     m_delta = array(0, dim = 5),
#                     s_delta = array(1, dim = 5),
#                     sigma_delta = array(1, dim = 5),
#                     m_lambda = array(0, dim = 1),
#                     s_lambda = array(1, dim = 1),
#                     sigma_lambda = array(1, dim = 1))
# 
# datos_modelo <- c(muestra_datos_base,
#                   aux_dptos_jer,
#                   hiperparams,
#                   list(x_escol = x_escol,
#                        x_csp = x_csp,
#                        x_edad = x_edad,
#                        x_migr = x_migr))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)),
#        delta = rep(rnorm(5,0,4), dptos) %>% array(dim = c(dptos, 5)),
#        lambda = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_C.stan",
#                data = datos_modelo,
#                init = inicia_cadena,
#                pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus","lambda_ajus",
#                         "log_lik"),
#                control = list(max_treedepth = 12),
#                seed = 51295)
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_C.rds")
# remove(modelo)
# 
# #### Modelo D #####
# # Incluye: Escolaridad + CSP + Edad + Cond. Migr. + Sexo
# hiperparams <- list(m_alfa = -1.8,
#                     s_alfa = 0.5,
#                     sigma_alfa = 1,
#                     m_beta = array(0, dim = 4),
#                     s_beta = array(1, dim = 4),
#                     sigma_beta = array(1, dim = 4),
#                     m_gamma = array(0, dim = 7),
#                     s_gamma = array(1, dim = 7),
#                     sigma_gamma = array(1, dim = 7),
#                     m_delta = array(0, dim = 5),
#                     s_delta = array(1, dim = 5),
#                     sigma_delta = array(1, dim = 5),
#                     m_lambda = array(0, dim = 1),
#                     s_lambda = array(1, dim = 1),
#                     sigma_lambda = array(1, dim = 1),
#                     m_kappa = array(0, dim = 1),
#                     s_kappa = array(1, dim = 1),
#                     sigma_kappa = array(1, dim = 1))
# 
# datos_modelo <- c(muestra_datos_base,
#                   aux_dptos_jer,
#                   hiperparams,
#                   list(x_escol = x_escol,
#                        x_csp = x_csp,
#                        x_edad = x_edad,
#                        x_migr = x_migr,
#                        x_sexo = x_sexo))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)),
#        delta = rep(rnorm(5,0,4), dptos) %>% array(dim = c(dptos, 5)),
#        lambda = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        kappa = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_D.stan",
#                data = datos_modelo,
#                init = inicia_cadena,
#                pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus","lambda_ajus","kappa_ajus",
#                         "log_lik"),
#                control = list(max_treedepth = 12),
#                seed = 51295)
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_D.rds")
# remove(modelo)
# 
# #### Modelo E #####
# # Incluye: Escolaridad + CSP + Edad + Cond. Migr. + Sexo + Ocu. Juvenil
# hiperparams <- list(m_alfa = -1.8,
#                     s_alfa = 0.5,
#                     sigma_alfa = 1,
#                     m_beta = array(0, dim = 4),
#                     s_beta = array(1, dim = 4),
#                     sigma_beta = array(1, dim = 4),
#                     m_gamma = array(0, dim = 7),
#                     s_gamma = array(1, dim = 7),
#                     sigma_gamma = array(1, dim = 7),
#                     m_delta = array(0, dim = 5),
#                     s_delta = array(1, dim = 5),
#                     sigma_delta = array(1, dim = 5),
#                     m_lambda = array(0, dim = 1),
#                     s_lambda = array(1, dim = 1),
#                     sigma_lambda = array(1, dim = 1),
#                     m_kappa = array(0, dim = 1),
#                     s_kappa = array(1, dim = 1),
#                     sigma_kappa = array(1, dim = 1),
#                     m_zeta = array(0, dim = 1),
#                     s_zeta = array(1, dim = 1),
#                     sigma_zeta = array(1, dim = 1))
# 
# datos_modelo <- c(muestra_datos_base,
#                   aux_dptos_jer,
#                   hiperparams,
#                   list(x_escol = x_escol,
#                        x_csp = x_csp,
#                        x_edad = x_edad,
#                        x_migr = x_migr,
#                        x_sexo = x_sexo,
#                        x_ocu_juv = x_ocu_juv))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)),
#        delta = rep(rnorm(5,0,4), dptos) %>% array(dim = c(dptos, 5)),
#        lambda = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        kappa = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        zeta = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_E.stan",
#                data = datos_modelo,
#                init = inicia_cadena,
#                pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus","lambda_ajus","kappa_ajus","zeta_ajus",
#                         "log_lik"),
#                control = list(max_treedepth = 12),
#                seed = 51295)
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_E.rds")
# remove(modelo)

# ### Modelo F #####
# # Incluye: Escolaridad + CSP + Edad + Cond. Migr. + Sexo + Ocu. General
# hiperparams <- list(m_alfa = -1.8,
#                     s_alfa = 0.5,
#                     sigma_alfa = 1,
#                     m_beta = array(0, dim = 4),
#                     s_beta = array(1, dim = 4),
#                     sigma_beta = array(1, dim = 4),
#                     m_gamma = array(0, dim = 7),
#                     s_gamma = array(1, dim = 7),
#                     sigma_gamma = array(1, dim = 7),
#                     m_delta = array(0, dim = 5),
#                     s_delta = array(1, dim = 5),
#                     sigma_delta = array(1, dim = 5),
#                     m_lambda = array(0, dim = 1),
#                     s_lambda = array(1, dim = 1),
#                     sigma_lambda = array(1, dim = 1),
#                     m_kappa = array(0, dim = 1),
#                     s_kappa = array(1, dim = 1),
#                     sigma_kappa = array(1, dim = 1),
#                     m_zeta = array(0, dim = 1),
#                     s_zeta = array(1, dim = 1),
#                     sigma_zeta = array(1, dim = 1))
# 
# datos_modelo <- c(muestra_datos_base,
#                   aux_dptos_jer,
#                   hiperparams,
#                   list(x_escol = x_escol,
#                        x_csp = x_csp,
#                        x_edad = x_edad,
#                        x_migr = x_migr,
#                        x_sexo = x_sexo,
#                        x_ocu_gral = x_ocu_gral))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)),
#        delta = rep(rnorm(5,0,4), dptos) %>% array(dim = c(dptos, 5)),
#        lambda = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        kappa = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        zeta = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_F.stan",
#                data = datos_modelo,
#                init = inicia_cadena,
#                pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus","lambda_ajus","kappa_ajus","zeta_ajus",
#                         "log_lik"),
#                control = list(max_treedepth = 12),
#                seed = 51295)
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_F.rds")
# remove(modelo)
# closeAllConnections()
# 
# #### Modelo G #####
# # Incluye: Escolaridad + CSP + Edad + Cond. Migr. + Sexo + Ocu. General + Ocu. Juvenil
# hiperparams <- list(m_alfa = -1.8,
#                     s_alfa = 0.5,
#                     sigma_alfa = 1,
#                     m_beta = array(0, dim = 4),
#                     s_beta = array(1, dim = 4),
#                     sigma_beta = array(1, dim = 4),
#                     m_gamma = array(0, dim = 7),
#                     s_gamma = array(1, dim = 7),
#                     sigma_gamma = array(1, dim = 7),
#                     m_delta = array(0, dim = 5),
#                     s_delta = array(1, dim = 5),
#                     sigma_delta = array(1, dim = 5),
#                     m_lambda = array(0, dim = 1),
#                     s_lambda = array(1, dim = 1),
#                     sigma_lambda = array(1, dim = 1),
#                     m_kappa = array(0, dim = 1),
#                     s_kappa = array(1, dim = 1),
#                     sigma_kappa = array(1, dim = 1),
#                     m_zeta = array(0, dim = 1),
#                     s_zeta = array(1, dim = 1),
#                     sigma_zeta = array(1, dim = 1),
#                     m_xi = array(0, dim = 1),
#                     s_xi = array(1, dim = 1),
#                     sigma_xi = array(1, dim = 1))
# 
# datos_modelo <- c(muestra_datos_base,
#                   aux_dptos_jer,
#                   hiperparams,
#                   list(x_escol = x_escol,
#                        x_csp = x_csp,
#                        x_edad = x_edad,
#                        x_migr = x_migr,
#                        x_sexo = x_sexo,
#                        x_ocu_gral = x_ocu_gral,
#                        x_ocu_juv = x_ocu_juv))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)),
#        delta = rep(rnorm(5,0,4), dptos) %>% array(dim = c(dptos, 5)),
#        lambda = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        kappa = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        zeta = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
#        xi = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_G.stan",
#                data = datos_modelo,
#                init = inicia_cadena,
#                pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus",
#                         "lambda_ajus","kappa_ajus","zeta_ajus","xi_ajus",
#                         "log_lik"),
#                control = list(max_treedepth = 12),
#                seed = 51295)
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_G.rds")
# remove(modelo)
# closeAllConnections()

#### Modelo H #####
# Incluye: Escolaridad + CSP + Edad + Cond. Migr. + Sexo + Ocu. General + Ocu. Juvenil + Ocu. Mayores
hiperparams <- list(m_alfa = -1.8,
                    s_alfa = 0.5,
                    sigma_alfa = 1,
                    m_beta = array(0, dim = 4),
                    s_beta = array(1, dim = 4),
                    sigma_beta = array(1, dim = 4),
                    m_gamma = array(0, dim = 7),
                    s_gamma = array(1, dim = 7),
                    sigma_gamma = array(1, dim = 7),
                    m_delta = array(0, dim = 5),
                    s_delta = array(1, dim = 5),
                    sigma_delta = array(1, dim = 5),
                    m_lambda = array(0, dim = 1),
                    s_lambda = array(1, dim = 1),
                    sigma_lambda = array(1, dim = 1),
                    m_kappa = array(0, dim = 1),
                    s_kappa = array(1, dim = 1),
                    sigma_kappa = array(1, dim = 1),
                    m_zeta = array(0, dim = 1),
                    s_zeta = array(1, dim = 1),
                    sigma_zeta = array(1, dim = 1),
                    m_xi = array(0, dim = 1),
                    s_xi = array(1, dim = 1),
                    sigma_xi = array(1, dim = 1),
                    m_upsilon = array(0, dim = 1),
                    s_upsilon = array(1, dim = 1),
                    sigma_upsilon = array(1, dim = 1))

datos_modelo <- c(muestra_datos_base,
                  aux_dptos_jer,
                  hiperparams,
                  list(x_escol = x_escol,
                       x_csp = x_csp,
                       x_edad = x_edad,
                       x_migr = x_migr,
                       x_sexo = x_sexo,
                       x_ocu_gral = x_ocu_gral,
                       x_ocu_juv = x_ocu_juv,
                       x_ocu_may = x_ocu_may))

inicia_cadena <- function(dptos = 96){
  list(alfa = rep(rnorm(1,-1.8,2), dptos) %>% array(dim = dptos),
       beta = rep(rnorm(4,0,4), dptos) %>% array(dim = c(dptos, 4)),
       gamma = rep(rnorm(7,0,4), dptos) %>% array(dim = c(dptos, 7)),
       delta = rep(rnorm(5,0,4), dptos) %>% array(dim = c(dptos, 5)),
       lambda = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
       kappa = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
       zeta = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
       xi = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)),
       upsilon = rep(rnorm(1,0,4), dptos) %>% array(dim = c(dptos, 1)))
}

set.seed(51295)
modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_H.stan",
               data = datos_modelo,
               init = inicia_cadena,
               pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus",
                        "lambda_ajus","kappa_ajus","zeta_ajus","xi_ajus","upsilon_ajus",
                        "log_lik"),
               control = list(max_treedepth = 15),
               seed = 51295)
saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_H.rds")
remove(modelo)
closeAllConnections()

#### Modelo S #####
# Incluye: Escolaridad + CSP + Edad + Cond. Migr. + Sexo + Ocu. General + Ocu. Juvenil + Ocu. Mayores
# hiperparams <- list(m_alfa = 0, 
#                     s_alfa = 10, 
#                     sigma_alfa = 10,
#                     m_beta = array(0, dim = 4), 
#                     s_beta = array(10, dim = 4),
#                     sigma_beta = array(10, dim = 4),
#                     m_gamma = array(0, dim = 7), 
#                     s_gamma = array(10, dim = 7),
#                     sigma_gamma = array(10, dim = 7),
#                     m_delta = array(0, dim = 5), 
#                     s_delta = array(10, dim = 5),
#                     sigma_delta = array(10, dim = 5),
#                     m_lambda = array(0, dim = 1), 
#                     s_lambda = array(10, dim = 1),
#                     sigma_lambda = array(10, dim = 1),
#                     m_kappa = array(0, dim = 1),
#                     s_kappa = array(10, dim = 1),
#                     sigma_kappa = array(10, dim = 1),
#                     m_zeta = array(0, dim = 1), 
#                     s_zeta = array(10, dim = 1),
#                     sigma_zeta = array(10, dim = 1),
#                     m_xi = array(0, dim = 1), 
#                     s_xi = array(10, dim = 1),
#                     sigma_xi = array(10, dim = 1),
#                     m_upsilon = array(0, dim = 1), 
#                     s_upsilon = array(10, dim = 1),
#                     sigma_upsilon = array(10, dim = 1))
# 
# datos_modelo <- c(muestra_datos_base, 
#                   aux_dptos_jer,
#                   hiperparams, 
#                   list(x_escol = x_escol,
#                        x_csp = x_csp,
#                        x_edad = x_edad,
#                        x_migr = x_migr,
#                        x_sexo = x_sexo,
#                        x_ocu_gral = x_ocu_gral,
#                        x_ocu_juv = x_ocu_juv,
#                        x_ocu_may = x_ocu_may))
# 
# inicia_cadena <- function(dptos = 96){
#   list(alfa = rep(rnorm(1,0,20), dptos) %>% array(dim = dptos),
#        beta = rep(rnorm(4,0,20), dptos) %>% array(dim = c(dptos, 4)),
#        gamma = rep(rnorm(7,0,20), dptos) %>% array(dim = c(dptos, 7)),
#        delta = rep(rnorm(5,0,20), dptos) %>% array(dim = c(dptos, 5)),
#        lambda = rep(rnorm(1,0,20), dptos) %>% array(dim = c(dptos, 1)),
#        kappa = rep(rnorm(1,0,20), dptos) %>% array(dim = c(dptos, 1)),
#        zeta = rep(rnorm(1,0,20), dptos) %>% array(dim = c(dptos, 1)),
#        xi = rep(rnorm(1,0,20), dptos) %>% array(dim = c(dptos, 1)),
#        upsilon = rep(rnorm(1,0,20), dptos) %>% array(dim = c(dptos, 1)))
# }
# 
# set.seed(51295)
# modelo <- stan(file = "MODELOS_STAN/Modelo_Jer_Compuesto_H.stan",
#                data = datos_modelo, 
#                init = inicia_cadena, 
#                pars = c("alfa","beta_ajus","gamma_ajus","delta_ajus",
#                         "lambda_ajus","kappa_ajus","zeta_ajus","xi_ajus","upsilon_ajus",
#                         "log_lik"), 
#                control = list(max_treedepth = 12),
#                seed = 51295) 
# saveRDS(modelo,file = "MODELOS_STAN/Modelos_Jer_Comp/Modelo_Jer_Compuesto_S.rds")
#remove(modelo)
