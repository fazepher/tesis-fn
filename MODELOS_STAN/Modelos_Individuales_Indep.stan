//////// Modelo Jerárquico 1 en Stan ////////

////////// Bloque de datos ////////
data{

  // Datos generales
  int<lower=1> C; // Número de comunas en muestra
  int<lower=1> D; // Número de departamentos en muestra
  int<lower=1> K; // Número de categorías de la variable explicativa
  
  // Variable de interés
  int<lower=0> votos[C]; // Número de votos recibidos por los candidatos en la comuna
  
  // Variables auxiliares
  int<lower=1> inscritos[C]; // Número de electores inscritos en la lista nominal de la comuna
  int<lower=1,upper=D> dpto[C]; // Departamento de la comuna
  
  // Variables explicativas
  matrix<lower=0,upper=1>[C,K] variable; // Composición comunal por la variable explicativa
  
  // Hiperparámetros
  real mu_alfa; // Locación de los interceptos
  real<lower = 0> sigma_alfa; // Escala de los interceptos
  vector[K - 1] mu_beta; // Locación de los coeficientes explicativos
  real<lower = 0> sigma_beta; // Escalas de los coeficientes de la variable explicativa

}

////////// Bloque de parámetros ////////
parameters{

    // Coeficientes de la regresión
  vector[D] alfa; // Intercepto
  vector[K - 1] beta[D]; // Variable

}

//////// Bloque de parámetros transformados ////////
transformed parameters{
  
  // Declaración
  vector[K] beta_ajus[D]; 
  vector[C] logit_theta;
  
  
  // Condiciones de identificabilidad
  for(d in 1:D){
    
   beta_ajus[d,1:(K - 1)] = beta[d];
   beta_ajus[d,K] = -sum(beta[d]);
   
  }
  
  // Parámetro en escala logit
  for(c in 1:C){
    logit_theta[c] = alfa[dpto[c]] + variable[c]*beta_ajus[dpto[c]];
  }
  
}

////////// Bloque del modelo ////////
model{

  
  // Iniciales
  for(d in 1:D){
    
   alfa[d] ~ normal(mu_alfa,sigma_alfa);
   beta[d] ~ normal(mu_beta,sigma_beta);
  
  }

  // Modelo Lineal Generalizado
  
  for(c in 1:C){
   
   votos[c] ~ binomial_logit(inscritos[c],logit_theta[c]); 
  
  }

}

////////// Cantidades generadas ////////
generated quantities {
  
  vector[C] log_lik;
  
  for (c in 1:C){
    log_lik[c] = binomial_logit_lpmf(votos[c] | inscritos[c], logit_theta[c]);
  }
  
}

