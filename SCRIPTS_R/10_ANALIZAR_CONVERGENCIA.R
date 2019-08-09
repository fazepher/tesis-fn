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

#### ANALIZAMOS ####

MODELOS <- list.files("MODELOS_STAN/Modelos_Jer_Comp") %>% 
{tibble(Archivo = paste("MODELOS_STAN/Modelos_Jer_Comp",.,sep="/"))} %>% 
  filter(!str_detect(Archivo,"PRED"))

# DIAGNÓSTICOS DE BETANCOURT
#pmap(MODELOS,~read_rds(..1) %>% check_all_diagnostics(max_depth = 12))

MODELO_H <- MODELOS$Archivo[8] %>% read_rds

params_ejemplo <- c("alfa[5]",
                    "beta_ajus[36,5]",
                    "gamma_ajus[20,2]",
                    "delta_ajus[18,4]",
                    "lambda_ajus[95,1]",
                    "kappa_ajus[52,2]")

{mcmc_trace(MODELO_H,pars=params_ejemplo,facet_args = list(ncol=2)) + 
    scale_color_manual(values = paleta_tesis_fn$COLOR[c(2,3,4,7)]) + 
    labs(color = "Cadena")} %>% 
  ggsave(plot = ., file = "Convergencia/Convergencia_Traceplots.pdf",width = 15, height = 10, device = cairo_pdf)

color_scheme_set(paleta_tesis_fn$COLOR[c(7,7,5,2,3,1)])
{mcmc_acf_bar(MODELO_H,pars=params_ejemplo) + 
    hline_at(0.25, linetype = 2, size = 0.15, color = paleta_tesis_fn$COLOR[4]) + 
    hline_at(-0.25, linetype = 2, size = 0.15, color = paleta_tesis_fn$COLOR[4]) + 
    scale_y_continuous(breaks = c(-0.25,0,0.25,1), limits = c(-.3,1)) + 
    labs(x = "Espaciamiento",
         y = "Autocorrelación")} %>% 
  ggsave(plot = ., file = "Convergencia/Convergencia_AutoCorr.pdf",width = 15, height = 10, device = cairo_pdf)

{mcmc_dens_overlay(MODELO_H,pars=params_ejemplo) + 
  scale_color_manual(values = paleta_tesis_fn$COLOR[c(2,3,4,7)]) + 
  labs(color = "Cadena")} %>% 
  ggsave(plot = ., file = "Convergencia/Convergencia_Densidades.pdf",width = 15, height = 10, device = cairo_pdf)
