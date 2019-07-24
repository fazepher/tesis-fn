//////// Modelo Jer�rquico 1 en Stan ////////

////////// Bloque de datos ////////
data{

  // Datos generales
  int<lower=1> C; // N�mero de comunas en muestra
  int<lower=1> D; // N�mero de departamentos en muestra
  int<lower=1> K; // N�mero de categor�as de la variable explicativa
  
  // Variable de inter�s
  int<lower=0> votos[C]; // N�mero de votos recibidos por los candidatos en la comuna
  
  // Variables auxiliares
  int<lower=1> inscritos[C]; // N�mero de electores inscritos en la lista nominal de la comuna
  int<lower=1,upper=D> dpto[C]; // Departamento de la comuna
  
  // Variables explicativas
  matrix<lower=0,upper=1>[C,K] variable; // Composici�n comunal por la variable explicativa
  
  // Hiperpar�metros
  real m_alfa; // Hiperpar. de locaci�n intercepto
  real s_alfa; // Hiperpar. de variabilidad departamental para locaci�n intercepto
  vector[K - 1] m_beta; // Hiperpar. de locaci�n variable explicativa
  vector<lower = 0>[K - 1] s_beta; // Hiperpar. de variabilidad departamental para locaci�n variable explicativa
  real<lower = 0> sigma_alfa; // Escala de los interceptos
  vector<lower = 0>[K - 1] sigma_beta; // Escalas de los coeficientes de la variable explicativa

}

////////// Bloque de par�metros ////////
parameters{

    // Coeficientes departamentales de la regresi�n
  vector[D] alfa; // Intercepto
  vector[K - 1] beta[D]; // Variable explicativa
  
  // Locaci�n de los coeficientes departamentales
  real mu_alfa; // Intercepto
  vector[K - 1] mu_beta; // Variable explicativa

}

//////// Bloque de par�metros transformados ////////
transformed parameters{
  
  // Declaraci�n
  vector[K] beta_ajus[D]; 
  vector[C] logit_theta;
  
  
  // Condiciones de identificabilidad
  for(d in 1:D){
    
   beta_ajus[d,1:(K - 1)] = beta[d];
   beta_ajus[d,K] = -sum(beta[d]);
   
  }
  
  // Par�metro en escala logit
  for(c in 1:C){
    logit_theta[c] = alfa[dpto[c]] + variable[c]*beta_ajus[dpto[c]];
  }
  
}

////////// Bloque del modelo ////////
model{

  // Hiperiniciales
  
  mu_alfa ~ normal(m_alfa, s_alfa);
  mu_beta ~ normal(m_beta, s_beta);
  
  // Nivel departamento
  for(d in 1:D){
    
   alfa[d] ~ normal(mu_alfa,sigma_alfa);
   beta[d] ~ normal(mu_beta,sigma_beta);
  
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

