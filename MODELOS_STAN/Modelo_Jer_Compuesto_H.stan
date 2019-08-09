//////// Modelo Jerárquico H en Stan ////////

////////// Bloque de datos ////////
data{

  // Datos generales
  int<lower=1> C; // Número de comunas en muestra
  int<lower=1> D; // Número de departamentos en muestra

  // Variable de interés
  int<lower=0> votos[C]; // Número de votos recibidos por los candidatos en la comuna
  
  // Variables auxiliares
  int<lower=1> inscritos[C]; // Número de electores inscritos en la lista nominal de la comuna
  int<lower=1,upper=D> dpto[C]; // Departamento de la comuna
  
  // Variables explicativas
  matrix<lower=0,upper=1>[C,5] x_escol; // Composición comunal por escolaridad
  matrix<lower=0,upper=1>[C,8] x_csp; // Composición comunal por CSP
  matrix<lower=0,upper=1>[C,6] x_edad; // Composición comunal por Edad
  matrix<lower=0,upper=1>[C,2] x_migr; // Composición comunal por Cond. Migr.
  matrix<lower=0,upper=1>[C,2] x_sexo; // Composición comunal por Sexo
  matrix<lower=0,upper=1>[C,2] x_ocu_gral; // Composición comunal por Ocupación General
  matrix<lower=0,upper=1>[C,2] x_ocu_juv; // Composición comunal por Ocupación Juvenil
  matrix<lower=0,upper=1>[C,2] x_ocu_may; // Composición comunal por Ocupación Mayores
  
  // Hiperparámetros de locación, variabilidad departamental y escala
  real m_alfa; // Intercepto
  real s_alfa; // Intercepto
  real<lower = 0> sigma_alfa; // Intercepto
  
  vector[4] m_beta; // Escolaridad
  vector<lower = 0>[4] s_beta; // Escolaridad
  vector<lower = 0>[4] sigma_beta; // Escolaridad
  
  vector[7] m_gamma; // CSP
  vector<lower = 0>[7] s_gamma; // CSP
  vector<lower = 0>[7] sigma_gamma; // CSP
  
  vector[5] m_delta; // Edad
  vector<lower = 0>[5] s_delta; // Edad
  vector<lower = 0>[5] sigma_delta; // Edad
  
  vector[1] m_lambda; // Cond. Migr.
  vector<lower = 0>[1] s_lambda; // Cond. Migr.
  vector<lower = 0>[1] sigma_lambda; // Cond. Migr.
  
  vector[1] m_kappa; // Sexo
  vector<lower = 0>[1] s_kappa; // Sexo
  vector<lower = 0>[1] sigma_kappa; // Sexo
  
  vector[1] m_zeta; // Ocu. Gral.
  vector<lower = 0>[1] s_zeta; // Ocu. Gral.
  vector<lower = 0>[1] sigma_zeta; // Ocu. Gral.
  
  vector[1] m_xi; // Ocu. Juv.
  vector<lower = 0>[1] s_xi; // Ocu. Juv.
  vector<lower = 0>[1] sigma_xi; // Ocu. Juv.
  
  vector[1] m_upsilon; // Ocu. May.
  vector<lower = 0>[1] s_upsilon; // Ocu. May.
  vector<lower = 0>[1] sigma_upsilon; // Ocu. May.

}

////////// Bloque de parámetros ////////
parameters{

  // Coeficientes departamentales de la regresión
  vector[D] alfa; // Intercepto
  vector[4] beta[D]; // Escolaridad
  vector[7] gamma[D]; // CSP
  vector[5] delta[D]; // Edad
  vector[1] lambda[D]; // Cond. Migr. 
  vector[1] kappa[D]; // Sexo
  vector[1] zeta[D]; // Ocu. Gral.
  vector[1] xi[D]; // Ocu. Juv.
  vector[1] upsilon[D]; // Ocu. May.
  
  // Locación de los coeficientes departamentales
  real mu_alfa; // Intercepto
  vector[4] mu_beta; // Escolaridad
  vector[7] mu_gamma; // CSP
  vector[5] mu_delta; // Edad
  vector[1] mu_lambda; // Cond. Migr.
  vector[1] mu_kappa; // Sexo
  vector[1] mu_zeta; // Ocu. Gral.
  vector[1] mu_xi; // Ocu. Juv.
  vector[1] mu_upsilon; // Ocu. May.

}

//////// Bloque de parámetros transformados ////////
transformed parameters{
  
  // Declaración
  vector[C] logit_theta; // Predictor lineal
  vector[5] beta_ajus[D]; // Escolaridad
  vector[8] gamma_ajus[D]; // CSP
  vector[6] delta_ajus[D]; // Edad
  vector[2] lambda_ajus[D]; // Cond. Migr.
  vector[2] kappa_ajus[D]; // Sexo
  vector[2] zeta_ajus[D]; // Ocu. Gral.
  vector[2] xi_ajus[D]; // Ocu. Juv.
  vector[2] upsilon_ajus[D]; // Ocu. May.
  
  // Condiciones de identificabilidad
  for(d in 1:D){
  
   // Escolaridad
   beta_ajus[d,1:4] = beta[d];
   beta_ajus[d,5] = -sum(beta[d]);
   
   // CSP
   gamma_ajus[d,1:7] = gamma[d];
   gamma_ajus[d,8] = -sum(gamma[d]);
   
   // Edad
   delta_ajus[d,1:5] = delta[d];
   delta_ajus[d,6] = -sum(delta[d]);
   
   // Cond. Migr.
   lambda_ajus[d,1] = lambda[d,1];
   lambda_ajus[d,2] = -lambda[d,1];
   
   // Sexo
   kappa_ajus[d,1] = kappa[d,1];
   kappa_ajus[d,2] = -kappa[d,1];
   
   // Ocupación General
   zeta_ajus[d,1] = zeta[d,1];
   zeta_ajus[d,2] = -zeta[d,1];
   
   // Ocupación Juvenil
   xi_ajus[d,1] = xi[d,1];
   xi_ajus[d,2] = -xi[d,1];
   
   // Ocupación Mayores
   upsilon_ajus[d,1] = upsilon[d,1];
   upsilon_ajus[d,2] = -upsilon[d,1];
   
  }
  
  // Parámetro en escala logit
  for(c in 1:C){
    logit_theta[c] = alfa[dpto[c]] + 
                      x_escol[c]*beta_ajus[dpto[c]] + 
                      x_csp[c]*gamma_ajus[dpto[c]] + 
                      x_edad[c]*delta_ajus[dpto[c]] + 
                      x_migr[c]*lambda_ajus[dpto[c]] + 
                      x_sexo[c]*kappa_ajus[dpto[c]] + 
                      x_ocu_gral[c]*zeta_ajus[dpto[c]] + 
                      x_ocu_juv[c]*xi_ajus[dpto[c]] + 
                      x_ocu_may[c]*upsilon_ajus[dpto[c]];
  }
  
}

////////// Bloque del modelo ////////
model{

  // Hiperiniciales
  
  mu_alfa ~ normal(m_alfa, s_alfa); // Intercepto
  mu_beta ~ normal(m_beta, s_beta); // Escolaridad
  mu_gamma ~ normal(m_gamma, s_gamma); // CSP
  mu_delta ~ normal(m_delta, s_delta); // Edad
  mu_lambda ~ normal(m_lambda, s_lambda); // Cond. Migr.
  mu_kappa ~ normal(m_kappa, s_kappa); // Sexo
  mu_zeta ~ normal(m_zeta, s_zeta); // Ocu. Gral.
  mu_xi ~ normal(m_xi, s_xi); // Ocu. Juvenil
  mu_upsilon ~ normal(m_upsilon, s_upsilon); // Ocu. Mayores
  
  // Nivel departamento
  for(d in 1:D){
    
   alfa[d] ~ normal(mu_alfa,sigma_alfa); // Intercepto
   beta[d] ~ normal(mu_beta,sigma_beta); // Escolaridad
   gamma[d] ~ normal(mu_gamma,sigma_gamma); // CSP
   delta[d] ~ normal(mu_delta,sigma_delta); // Edad
   lambda[d] ~ normal(mu_lambda,sigma_lambda); // Cond. Migr.
   kappa[d] ~ normal(mu_kappa,sigma_kappa); // Sexo
   zeta[d] ~ normal(mu_zeta,sigma_zeta); // Ocu. Gral.
   xi[d] ~ normal(mu_xi,sigma_xi); // Ocu. Juvenil
   upsilon[d] ~ normal(mu_upsilon,sigma_upsilon); // Ocu. Mayores
  
  }

  // Modelo Lineal Generalizado
  
  for(c in 1:C){
   
   votos[c] ~ binomial_logit(inscritos[c], logit_theta[c]); 
  
  }

}

////////// Cantidades generadas ////////
generated quantities {
  
  vector[C] log_lik;
  
  for (c in 1:C){
    log_lik[c] = binomial_logit_lpmf(votos[c] | inscritos[c], logit_theta[c]);
  }
  
}

