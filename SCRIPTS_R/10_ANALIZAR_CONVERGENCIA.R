############################################################################################################
################################################# TESIS FN #################################################
##################################### FERNANDO ANTONIO ZEPEDA HERRERA ######################################
################################################ ITAM 2019 #################################################
############################################################################################################

############################################################################################################
############################################# MODELOS COMPUESTOS ###########################################
############################################################################################################
########################################### ANALIZA CONVERGENCIAS ##########################################
############################################################################################################

library(rstan)
library(bayesplot)

#### ARCHIVO UTILITY DE BETANCOURT ####
# Check transitions that ended with a divergence
check_div <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  n = sum(divergent)
  N = length(divergent)
  
  print(sprintf('%s of %s iterations ended with a divergence (%s%%)',
                n, N, 100 * n / N))
  if (n > 0)
    print('  Try running with larger adapt_delta to remove the divergences')
}

# Check transitions that ended prematurely due to maximum tree depth limit
check_treedepth <- function(fit, max_depth = 10) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
  n = length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  N = length(treedepths)
  
  print(sprintf('%s of %s iterations saturated the maximum tree depth of %s (%s%%)',
                n, N, max_depth, 100 * n / N))
  if (n > 0)
    print('  Run again with max_depth set to a larger value to avoid saturation')
}

# Checks the energy Bayesian fraction of missing information (E-BFMI)
check_energy <- function(fit) {
  sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
  no_warning <- TRUE
  for (n in 1:length(sampler_params)) {
    energies = sampler_params[n][[1]][,'energy__']
    numer = sum(diff(energies)**2) / length(energies)
    denom = var(energies)
    if (numer / denom < 0.2) {
      print(sprintf('Chain %s: E-BFMI = %s', n, numer / denom))
      no_warning <- FALSE
    }
  }
  if (no_warning)
    print('E-BFMI indicated no pathological behavior')
  else
    print('  E-BFMI below 0.2 indicates you may need to reparameterize your model')
}

# Checks the effective sample size per iteration
check_n_eff <- function(fit) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]
  
  iter <- dim(extract(fit)[[1]])[[1]]
  
  no_warning <- TRUE
  for (n in 1:N) {
    ratio <- fit_summary[,5][n] / iter
    if (ratio < 0.001) {
      print(sprintf('n_eff / iter for parameter %s is %s!',
                    rownames(fit_summary)[n], ratio))
      no_warning <- FALSE
    }
  }
  if (no_warning)
    print('n_eff / iter looks reasonable for all parameters')
  else
    print('  n_eff / iter below 0.001 indicates that the effective sample size has likely been overestimated')
}

# Checks the potential scale reduction factors
check_rhat <- function(fit) {
  fit_summary <- summary(fit, probs = c(0.5))$summary
  N <- dim(fit_summary)[[1]]
  
  no_warning <- TRUE
  for (n in 1:N) {
    rhat <- fit_summary[,6][n]
    if (rhat > 1.1 || is.infinite(rhat) || is.nan(rhat)) {
      print(sprintf('Rhat for parameter %s is %s!',
                    rownames(fit_summary)[n], rhat))
      no_warning <- FALSE
    }
  }
  if (no_warning)
    print('Rhat looks reasonable for all parameters')
  else
    print('  Rhat above 1.1 indicates that the chains very likely have not mixed')
}

# Se modificó esta función para permitir agregar parámetros adicionales al treedepth
check_all_diagnostics <- function(fit,...) {
  check_n_eff(fit)
  check_rhat(fit)
  check_div(fit)
  check_treedepth(fit,...)
  check_energy(fit)
}

# #### ANALIZAMOS MODELOS NACIONALES INDIVIDUALES ####
# 
# MODELOS <- list.files("MODELOS_STAN/Modelos_Nal_Ind") %>% 
# {tibble(Archivo = paste("MODELOS_STAN/Modelos_Nal_Ind",.,sep="/"))} %>% 
#   filter(!str_detect(Archivo,"PRED"))
# 
# # DIAGNÓSTICOS DE BETANCOURT
# pmap(MODELOS,~read_rds(..1) %>% check_all_diagnostics(max_depth = 12))
# 
# #### ANALIZAMOS MODELOS JERÁRQUICOS INDIVIDUALES ####
# 
# MODELOS <- list.files("MODELOS_STAN/Modelos_Jer_Ind") %>% 
# {tibble(Archivo = paste("MODELOS_STAN/Modelos_Jer_Ind",.,sep="/"))} %>% 
#   filter(!str_detect(Archivo,"PRED"))
# 
# # DIAGNÓSTICOS DE BETANCOURT
# pmap(MODELOS,~read_rds(..1) %>% check_all_diagnostics(max_depth = 12))
# 
# #### ANALIZAMOS MODELOS COMPUESTOS ####
# 
# MODELOS <- list.files("MODELOS_STAN/Modelos_Jer_Comp") %>% 
# {tibble(Archivo = paste("MODELOS_STAN/Modelos_Jer_Comp",.,sep="/"))} %>% 
#   filter(!str_detect(Archivo,"PRED"),!str_detect(Archivo,"_H"))
# 
# # DIAGNÓSTICOS DE BETANCOURT
# pmap(MODELOS,~read_rds(..1) %>% check_all_diagnostics(max_depth = 12))

MODELOS <- list.files("MODELOS_STAN/Modelos_Jer_Comp") %>%
{tibble(Archivo = paste("MODELOS_STAN/Modelos_Jer_Comp",.,sep="/"))} %>%
  filter(!str_detect(Archivo,"PRED"))

# DIAGNÓSTICOS DE BETANCOURT
# pmap(MODELOS,~read_rds(..1) %>% check_all_diagnostics(max_depth = 15))

#### COMPARACIÓN SENSIBILIDAD ####

params_ejemplo <- c("alfa[5]",
                    "beta_ajus[36,5]",
                    "gamma_ajus[20,2]",
                    "delta_ajus[18,4]",
                    "lambda_ajus[95,1]",
                    "kappa_ajus[52,2]")

Resumen_H <- MODELOS$Archivo[8] %>%
  read_rds %>%
  summary(probs = 0.5) %>%
  .$summary %>%
  as.data.frame %>%
  tibble::rownames_to_column("Param") %>%
  as_tibble() %>%
  filter(!str_detect(Param,"lp|log_lik")) %>%
  mutate(Modelo = "H")
Resumen_S <- MODELOS$Archivo[9] %>%
  read_rds %>%
  summary(probs = 0.5) %>%
  .$summary %>%
  as.data.frame %>%
  tibble::rownames_to_column("Param") %>%
  as_tibble() %>%
  filter(!str_detect(Param,"lp|log_lik")) %>%
  mutate(Modelo = "S")

bind_rows(Resumen_H,Resumen_S) %>%
  select(Param,n_eff,Modelo) %>%
  spread(Modelo,n_eff) %>%
  {ggplot(.,aes(x=S,y=H)) +
      geom_point(color = paleta_tesis_fn$COLOR[2], size = rel(0.25)) +
      geom_abline(slope=1,intercept = 0, color = paleta_tesis_fn$COLOR[3], size = rel(1.5)) +
      annotate("text", label = "S=H", x = 4000, y = 3500, color = paleta_tesis_fn$COLOR[1], size = rel(8)) +
      cowplot::theme_half_open() +
      labs(title = "Tamaño efectivo de muestra por parámetro") +
      theme(plot.title = element_text(size = rel(1.5), hjust = 0.5),
            axis.title = element_text(size = rel(2.5)),
            axis.text = element_text(size = rel(1)))} %>%
  ggsave(plot = ., file = "Convergencia/Compara_n_eff.pdf",width = 22.5/3, height = 17/3, device = cairo_pdf)

bind_rows(Resumen_H,Resumen_S) %>%
  select(Param,Rhat,Modelo) %>%
  spread(Modelo,Rhat) %>%
  {ggplot(.,aes(x=S,y=H)) +
      geom_point(color = paleta_tesis_fn$COLOR[2], size = rel(0.1)) +
      geom_abline(slope=1,intercept = 0, color = paleta_tesis_fn$COLOR[3], size = rel(1.5)) +
      annotate("text", label = "S=H", x = 1.006, y = 1.003, color = paleta_tesis_fn$COLOR[1], size = rel(8)) +
      cowplot::theme_half_open() +
      labs(title = "Estadístico Rhat de reducción de escala por parámetro") +
      theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
            axis.title = element_text(size = rel(2)),
            axis.text = element_text(size = rel(1)))} %>%
  ggsave(plot = ., file = "Convergencia/Compara_Rhat.pdf",width = 22.5/3, height = 17/3, device = cairo_pdf)

MODELO_H <- MODELOS$Archivo[8] %>% read_rds %>% extract(inc_warmup = T, permuted = F,pars=params_ejemplo)
MODELO_S <- MODELOS$Archivo[9] %>% read_rds %>% extract(inc_warmup = T, permuted = F,pars=params_ejemplo)

{mcmc_trace(MODELO_H, size = rel(.2),
            facet_args = list(ncol=2), n_warmup = 1000) +
    scale_color_manual(values = paleta_tesis_fn$COLOR[c(2,3,4,7)]) +
    labs(color = "Cadena",
         title = "Traceplots modelo H") + 
    guides(color = guide_legend(override.aes = list(size = rel(1)))) + 
    theme(axis.text = element_text(size = rel(1.5)),
          strip.text = element_text(size = rel(1)),
          legend.position = "top",
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(1.5), hjust = 0.5))} %>%
  ggsave(plot = ., file = "Convergencia/Convergencia_Traceplots.pdf",width = 22.5/2, height = 17/2, device = cairo_pdf)

{mcmc_trace(MODELO_S, size = rel(.2),
            facet_args = list(ncol=2), n_warmup = 1000) +
    scale_color_manual(values = paleta_tesis_fn$COLOR[c(2,3,4,7)]) +
    labs(color = "Cadena",
         title = "Traceplots modelo S") + 
    guides(color = guide_legend(override.aes = list(size = rel(1)))) + 
    theme(axis.text = element_text(size = rel(1.5)),
          strip.text = element_text(size = rel(1)),
          legend.position = "top",
          legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5)),
          plot.title = element_text(size = rel(1.5), hjust = 0.5))} %>%
  ggsave(plot = ., file = "Convergencia/Convergencia_Traceplots_S.pdf",width = 22.5/2, height = 17/2, device = cairo_pdf)


color_scheme_set(paleta_tesis_fn$COLOR[c(7,7,5,2,3,1)])
{mcmc_acf_bar(MODELO_H[1001:2000,,]) +
    hline_at(0.25, linetype = 2, size = rel(0.5), color = paleta_tesis_fn$COLOR[4]) +
    hline_at(-0.25, linetype = 2, size = rel(0.5), color = paleta_tesis_fn$COLOR[4]) +
    scale_y_continuous(breaks = c(-0.25,0,0.25,1), limits = c(-.5,1)) +
    labs(x = "Espaciamiento",
         y = "Autocorrelación") + 
    theme(axis.text = element_text(size = rel(1.25)),
          axis.title = element_text(size = rel(1.5)),
          strip.text = element_text(size = rel(1.15)))} %>%
  ggsave(plot = ., file = "Convergencia/Convergencia_AutoCorr.pdf",width = 22.5/2, height = 17/2, device = cairo_pdf)

{mcmc_acf_bar(MODELO_S[1001:2000,,]) +
    hline_at(0.25, linetype = 2, size = rel(0.5), color = paleta_tesis_fn$COLOR[4]) +
    hline_at(-0.25, linetype = 2, size = rel(0.5), color = paleta_tesis_fn$COLOR[4]) +
    scale_y_continuous(breaks = c(-0.25,0,0.25,1), limits = c(-.5,1)) +
    labs(x = "Espaciamiento",
         y = "Autocorrelación") + 
    theme(axis.text = element_text(size = rel(1.25)),
          axis.title = element_text(size = rel(1.5)),
          strip.text = element_text(size = rel(1.15)))} %>%
  ggsave(plot = ., file = "Convergencia/Convergencia_AutoCorr_S.pdf",width = 22.5/2, height = 17/2, device = cairo_pdf)


# {mcmc_dens_overlay(MODELO_H[1001:2000,,]) +
#   scale_color_manual(values = paleta_tesis_fn$COLOR[c(2,3,4,7)]) +
#   labs(color = "Cadena")} %>%
#   ggsave(plot = ., file = "Convergencia/Convergencia_Densidades.pdf",width = 15, height = 10, device = cairo_pdf)
# 
# {mcmc_dens_overlay(MODELO_S[1001:2000,,]) +
#     scale_color_manual(values = paleta_tesis_fn$COLOR[c(2,3,4,7)]) +
#     labs(color = "Cadena")} %>%
#   ggsave(plot = ., file = "Convergencia/Convergencia_Densidades_S.pdf",width = 15, height = 10, device = cairo_pdf)

