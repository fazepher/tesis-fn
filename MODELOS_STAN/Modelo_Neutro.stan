//////// Modelo Jer�rquico 1 en Stan ////////

////////// Bloque de datos ////////
data{

  // Datos generales
  int<lower=1> C; // N�mero de comunas en muestra

  // Variable de inter�s
  int<lower=0> votos[C]; // N�mero de votos recibidos por los candidatos en la comuna
  
  // Variables auxiliares
  int<lower=1> inscritos[C]; // N�mero de electores inscritos en la lista nominal de la comuna
  
  real m_a; // Locaci�n del intercepto
  real<lower = 0> s_a; // Escala del intercepto 
  real<lower = 0> sigma_alfa; // Escala del intercepto
  
}

////////// Bloque de par�metros ////////
parameters{

  real mu_alfa; // Locaci�n del intercepto
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
