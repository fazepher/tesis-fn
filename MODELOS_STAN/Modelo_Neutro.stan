//////// Modelo Jerárquico 1 en Stan ////////

////////// Bloque de datos ////////
data{

  // Datos generales
  int<lower=1> C; // Número de comunas en muestra

  // Variable de interés
  int<lower=0> votos[C]; // Número de votos recibidos por los candidatos en la comuna
  
  // Variables auxiliares
  int<lower=1> inscritos[C]; // Número de electores inscritos en la lista nominal de la comuna
  
  real m_a; // Locación del intercepto
  real<lower = 0> s_a; // Escala del intercepto 
  real<lower = 0> sigma_alfa; // Escala del intercepto
  
}

////////// Bloque de parámetros ////////
parameters{

  real mu_alfa; // Locación del intercepto
  vector[C] alfa; // Intercepto

}

////////// Bloque del modelo ////////
model{

  
  // Iniciales
  mu_alfa ~ normal(m_a,s_a);
  alfa ~ normal(mu_alfa,sigma_alfa);


  // Modelo Lineal Generalizado
  
  for(c in 1:C){
   
   votos[c] ~ binomial_logit(inscritos[c],alfa[c]); 
  
  }

}
