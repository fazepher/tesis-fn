//////// Modelo Jerárquico A en Stan ////////

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

}

////////// Bloque de parámetros ////////
parameters{

    // Coeficientes departamentales de la regresión
  vector[D] alfa; // Intercepto
  vector[4] beta[D]; // Escolaridad
  vector[7] gamma[D]; // CSP
  
  // Locación de los coeficientes departamentales
  real mu_alfa; // Intercepto
  vector[4] mu_beta; // Escolaridad
  vector[7] mu_gamma; // CSP

}

//////// Bloque de parámetros transformados ////////
transformed parameters{
  
  // Declaración
  vector[C] logit_theta; // Predictor lineal
  vector[5] beta_ajus[D]; // Escolaridad
  vector[8] gamma_ajus[D]; // CSP
  
  
  // Condiciones de identificabilidad
  for(d in 1:D){
  
   // Escolaridad
   beta_ajus[d,1:4] = beta[d];
   beta_ajus[d,5] = -sum(beta[d]);
   
   // CSP
   gamma_ajus[d,1:7] = gamma[d];
   gamma_ajus[d,8] = -sum(gamma[d]);
   
  }
  
  // Parámetro en escala logit
  for(c in 1:C){
    logit_theta[c] = alfa[dpto[c]] + 
                      x_escol[c]*beta_ajus[dpto[c]] + 
                      x_csp[c]*gamma_ajus[dpto[c]];
  }
  
}

////////// Bloque del modelo ////////
model{

  // Hiperiniciales
  
  mu_alfa ~ normal(m_alfa, s_alfa); // Intercepto
  mu_beta ~ normal(m_beta, s_beta); // Escolaridad
  mu_gamma ~ normal(m_gamma, s_gamma); // CSP
  
  // Nivel departamento
  for(d in 1:D){
    
   alfa[d] ~ normal(mu_alfa,sigma_alfa); // Intercepto
   beta[d] ~ normal(mu_beta,sigma_beta); // Escolaridad
   gamma[d] ~ normal(mu_gamma,sigma_gamma); // CSP
  
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

