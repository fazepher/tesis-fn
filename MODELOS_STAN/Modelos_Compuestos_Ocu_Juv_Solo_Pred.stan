//////// Modelo Jerárquico 1 en Stan ////////

////////// Bloque de funciones ////////

functions{
  
  real como_real(int x){
    return(x + 0.0);
  }
  
  real int_div(int x, int y){
   return(como_real(x)/como_real(y));
  }
  
}

////////// Bloque de datos ////////
data{

  // Datos generales
  int<lower=1> C; // Número de comunas a predecir
  int<lower=1> D; // Número de departamentos
  int<lower=1> M; // Número de simulaciones de MCMC
  real<lower=0,upper=1> tol; // Tolerancia para pérdida
  
  // Número de votos recibidos por los candidatos
  int<lower=0> votos[C];
  
  // Número de electores inscritos en la lista nominal
  int<lower=1> inscritos[C];
  
  // Departamentos de las comunas
  int<lower=1,upper=D> dpto[C];
  
  // Variables explicativas
  matrix<lower=0,upper=1>[C,5] x_escol; // Composición comunal por escolaridad
  matrix<lower=0,upper=1>[C,8] x_csp; // Composición comunal por CSP
  matrix<lower=0,upper=1>[C,6] x_edad; // Composición comunal por Edad
  matrix<lower=0,upper=1>[C,2] x_migr; // Composición comunal por Cond. Migr.
  matrix<lower=0,upper=1>[C,2] x_sexo; // Composición comunal por Sexo
  matrix<lower=0,upper=1>[C,2] x_ocu_juv; // Composición comunal por Ocupación Juvenil
  
  // Simulaciones MCMC
  vector[D] alfa[M]; // Intercepto
  matrix[D,5] beta_ajus[M]; // Escolaridad
  matrix[D,8] gamma_ajus[M]; // CSP
  matrix[D,6] delta_ajus[M]; // Edad
  matrix[D,2] lambda_ajus[M]; // Cond. Migr.
  matrix[D,2] kappa_ajus[M]; // Sexo
  matrix[D,2] zeta_ajus[M]; // Ocupación Juvenil


}

////////// Bloque de datos transformados ////////
transformed data{
  
  // Nivel comuna
  vector<lower=0,upper=1>[C] pct_votos; // Porcentaje empírico de votos
  
  // Nivel departamento
  int votos_dptal[D]; // Votos
  int inscritos_dptal[D]; // Electores inscritos
  vector<lower=0,upper=1>[D] pct_votos_dptal; // Porcentaje empírico de votos
  
  // Nivel nacional 
  int votos_nal; // Votos
  int inscritos_nal; // Electores inscritos
  real<lower=0,upper=1> pct_votos_nal; // Porcentaje empírico de votos
  
  votos_dptal = rep_array(0,D);
  inscritos_dptal = rep_array(0,D);
  for(c in 1:C){
    pct_votos[c] = int_div(votos[c], inscritos[c]);
    votos_dptal[dpto[c]] += votos[c];
    inscritos_dptal[dpto[c]] += inscritos[c];
  }
  for(d in 1:D){
    pct_votos_dptal[d] = int_div(votos_dptal[d],inscritos_dptal[d]);
  }
  
  votos_nal = sum(votos_dptal);
  inscritos_nal = sum(inscritos_dptal);
  pct_votos_nal = int_div(votos_nal,inscritos_nal);
  
}

////////// Bloque de parámetros ////////
parameters{

}

////////// Bloque del modelo ////////
model{

}

////////// Cantidades generadas ////////
generated quantities {
  
  // Estimadores puntuales 
  vector<lower=0,upper=1>[C] pct_votos_media; // A nivel comuna
  vector<lower=0,upper=1>[D] pct_votos_dptal_media; // A nivel departamental
  real<lower=0,upper=1> pct_votos_nal_media; // A nivel nacional
  
  // Pérdidas a nivel comuna
  real PCMC; // Pérdida cuadrática media
  real PAMC; // Pérdida absoluta media
  real PTMC; // Pérdida tolerancia media
  
  // Pérdidas a nivel departamento
  real PCMD; // Pérdida cuadrática media
  real PAMD; // Pérdida absoluta media
  real PTMD; // Pérdida tolerancia media
  
  // Pérdidas a nivel nacional
  real PCN; // Pérdida cuadrática nacional
  real PAN; // Pérdida absoluta nacional
  real PTN; // Pérdida tolerancia nacional
  
  // Iniciamos local scope para poder declarar variables intermedias sin que se guarden
  {
    
    vector[C] theta_pred[M]; // Propensiones
    
    int votos_pred[M,C]; // Votos a nivel comuna
    int votos_dptal_pred[M,D]; // Votos a nivel departamento
    int votos_nal_pred[M]; // Votos a nivel nacional
    
    vector[C] pct_votos_pred[M]; // Porcentaje de votos a nivel comuna
    vector[D] pct_votos_dptal_pred[M]; // Porcentaje de votos a nivel departamento
    real pct_votos_nal_pred[M]; // Porcentaje de votos a nivel nacional
    
    vector[C] error_c[M]; // Error a nivel comuna
    vector[D] error_d[M]; // Error a nivel departamento
    real error_n[M]; // Error a nivel nacional
  
    
    // Inicializamos valores a acumular
    PCMC = 0;
    PAMC = 0;
    PTMC = 0;
    PCMD = 0;
    PAMD = 0;
    PTMD = 0;
    PCN = 0;
    PAN = 0;
    PTN = 0; 
    for(m in 1:M){
    
      votos_dptal_pred[m] = rep_array(0,D);
      pct_votos_dptal_pred[m] = rep_vector(0,D);
      
      for(c in 1:C){
      
      // Predicciones de theta
      theta_pred[m,c] = inv_logit(alfa[m,dpto[c]] + 
                                    x_escol[c]*(beta_ajus[m,dpto[c]]') + 
                                    x_csp[c]*(gamma_ajus[m,dpto[c]]') + 
                                    x_edad[c]*(delta_ajus[m,dpto[c]]') + 
                                    x_migr[c]*(lambda_ajus[m,dpto[c]]') + 
                                    x_sexo[c]*(kappa_ajus[m,dpto[c]]') + 
                                    x_ocu_juv[c]*(zeta_ajus[m,dpto[c]]'));
                                    
      // Predicciones de votos y errores*
      votos_pred[m,c] = binomial_rng(inscritos[c], theta_pred[m,c]);
      pct_votos_pred[m,c] = int_div(votos_pred[m,c],inscritos[c]);
      error_c[m,c] = pct_votos_pred[m,c] - pct_votos[c];
    
      // Acumulaciones a nivel comuna
      PCMC += square(error_c[m,c]);
      PAMC += fabs(error_c[m,c]);
      PTMC += fabs(error_c[m,c]) > tol;
      
      // Acumulación de votos a nivel departamental
      votos_dptal_pred[m,dpto[c]] += votos_pred[m,c];
      
      }
      
      // Predicciones de votos y acumulación de errores a nivel departamental
      for(d in 1:D){
        pct_votos_dptal_pred[m,d] = int_div(votos_dptal_pred[m,d],inscritos_dptal[d]);
        error_d[m,d] = pct_votos_dptal_pred[m,d] - pct_votos_dptal[d];
        PCMD += square(error_d[m,d]);
        PAMD += fabs(error_d[m,d]);
        PTMD += fabs(error_d[m,d]) > tol;
      }
      
      // Predicciones y errores a nivel nacional
      votos_nal_pred[m] = sum(votos_dptal_pred[m]);
      pct_votos_nal_pred[m] = int_div(votos_nal_pred[m],inscritos_nal);
      error_n[m] = pct_votos_nal_pred[m] - pct_votos_nal;
      PCN += square(error_n[m]);
      PAN += fabs(error_n[m]);
      PTN += fabs(error_n[m]) > tol;
        
    }
    
    // Errores medios a nivel comuna
    PCMC /= C*M;
    PAMC /= C*M;
    PTMC /= C*M;
     
    // Errores medios a nivel departamental
    PCMD /= D*M;
    PAMD /= D*M;
    PTMD /= D*M;
      
    // Errores medios a nivel comuna
    PCN /= M;
    PAN /= M;
    PTN /= M;
    
    // Estimadores puntuales
    for(c in 1:C){
      pct_votos_media[c] = sum(pct_votos_pred[1:M,c])/M;
    }
    for(d in 1:D){
      pct_votos_dptal_media[d] = sum(pct_votos_dptal_pred[1:M,d])/M;
    }
    pct_votos_nal_media = sum(pct_votos_nal_pred)/M;
    
  }
  
}
