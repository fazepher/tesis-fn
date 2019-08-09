

inv_logit <- function(x){
  1/(1+exp(-x))
}

datos_electorales_completos <- read_csv("DATOS/LIMPIOS/RESULTADOS_ELECTORALES.csv", 
                                        locale = locale(encoding = "latin1"))

datos_censales <- read_csv("DATOS/LIMPIOS/DATOS_CENSALES.csv", 
                           locale = locale(encoding = "latin1"))

otros_datos_comunales <- read_csv("DATOS/LIMPIOS/OTROS_DATOS_COMUNALES.csv", 
                                  locale = locale(encoding = "latin1"))

muestra <- read_csv("DATOS/Muestra.csv", 
                    locale = locale(encoding = "latin1"))

prueba_datos_unificados <- datos_electorales_completos %>% 
  filter(FAMILIA == "FN") %>% 
  mutate(AÑO = str_extract(ELECCION,"\\d{4}$") %>% as.integer) %>% 
  inner_join(datos_censales, by = c("CODGEO","AÑO")) %>% 
  inner_join(otros_datos_comunales, by = c("CODGEO","AÑO")) %>% 
  left_join(full_join(COMUNAS_2007,COMUNAS_2012)) %>% 
  select(CODGEO,NOM_COMUNA:NOM_REG,AÑO,ELECCION,everything())

datos_P12 <- filter(prueba_datos_unificados, ELECCION == "Presidenciales 2012")

set.seed(51295)
simulados <- tibble(DE10 = rnorm(500000,0,10),
                    DE05 = rnorm(500000,0,5),
                    DE025 = rnorm(500000,0,2.5),
                    DE015 = rnorm(500000,0,1.5),
                    DE01 = rnorm(500000,0,1),
                    Inicial = rnorm(500000,-1.7,0.25)) %>% 
  gather(DE,theta) %>% 
  mutate(Mu = 0,
         Tipo = case_when(DE == "Inicial"~"N(mu==-1.7,sigma==0.25)",
                          T ~ "Mala"),
         Sigma = case_when(DE == "DE10"~"sigma[1] == 10",
                           DE == "DE05"~"sigma[2] == 5",
                           DE == "DE025"~"sigma[3] == 2.5",
                           DE == "DE015"~"sigma[4] == 1.5",
                           DE == "DE01"~"sigma[5] == 1"),
         p = inv_logit(theta)) 

simulados %>% 
  filter(Tipo == "Mala") %>% 
{ggplot(.) + 
    geom_histogram(aes(x=p,stat(density)), color = paleta_tesis_fn$COLOR[2], 
                   fill = paleta_tesis_fn$COLOR[5], binwidth = 0.025) + 
    facet_wrap(~Sigma,nrow = 1, labeller = label_parsed) + 
    labs(y="Densidad") + 
    theme_classic()} %>% 
  ggsave(plot = ., width = 16, height = 8, filename = "INICIALES/Malas_Iniciales.pdf", device = cairo_pdf)


{ggplot(tibble(x=seq(-10,10,by=0.001)) %>% mutate(p=inv_logit(x))) + 
    geom_path(aes(x,p), color = paleta_tesis_fn$COLOR[1], size = rel(1)) + 
    annotate("rect",xmin=-5,xmax=5,ymin=0,ymax = 1, fill = paleta_tesis_fn$COLOR[2], alpha = 0.3) + 
    annotate("segment",x=0,xend=0,y=0,yend = 0.5, color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
    annotate("segment",x=-10,xend=0,y=0.5,yend = 0.5, color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
    annotate("text",x = 1.75, y = 0.5, color = paleta_tesis_fn$COLOR[1], 
             label = expression(atop(eta==0~"en la escala de los reales","equivale a"~p==0.5)), parse=TRUE) +
    annotate("text",x = 8, y = 0.9, color = paleta_tesis_fn$COLOR[1], 
             label = expression(atop(eta == 5~"lleva a"~p %~~% 0.9933,eta == 10~"a"~p %~~% 0.9999)), parse = TRUE) + 
    annotate("text",x = -7.75, y = 0.1, color = paleta_tesis_fn$COLOR[1], 
             label = expression(atop("Pasar de"~eta==-10~"a"~eta==-5~"solo aumenta p en .0066", 
                                     "pero otras 5 unidades aportan 0.4933 a p")), parse = TRUE) + 
    labs(title = "Rango de la escala logística",
         x = expression(eta)) + 
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))} %>% 
  ggsave(plot = ., width = 16, height = 8, filename = "INICIALES/Escala_Logis.pdf", device = cairo_pdf)

{ggplot(tibble(x=seq(-10,10,by=0.001)) %>% mutate(p=inv_logit(x),pa = inv_logit(x-1.7))) + 
    geom_path(aes(x,p), color = paleta_tesis_fn$COLOR[1], size = rel(1)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[1], linetype = 2,
             x=0,xend=0,
             y=0,yend = 0.5) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[1], linetype = 2,
             x=-10,xend=0,
             y=0.5,yend = 0.5) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[2], linetype = 2,
             x=0.5,xend=0.5,
             y=0,yend = inv_logit(0.5)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[2], linetype = 2,
             x=-10,xend=0.5,
             y=inv_logit(0.5),yend = inv_logit(0.5)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[2], size = rel(1), arrow = arrow(length = unit(0.1,"inches")),
             x=-0.2,xend=0.3,
             y=0.51,yend = inv_logit(0.5)-.01) + 
    annotate("text", color = paleta_tesis_fn$COLOR[1], 
             x = -2, y = 0.57,
             label = expression(atop("Aumentar una desv. est."~sigma,"resulta en"~Delta%~~%+12~"pp")), parse = TRUE) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[3], linetype = 2,
             x=-1,xend=-1,
             y=0,yend = inv_logit(-1)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[3], linetype = 2,
             x=-10,xend=-1,
             y=inv_logit(-1),yend = inv_logit(-1)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[3], size = rel(1), arrow = arrow(length = unit(0.1,"inches")),
             x=-0.3,xend=-1.3,
             y=0.49,yend = inv_logit(-1)+.01) +
    annotate("text", color = paleta_tesis_fn$COLOR[1], 
             x = -2.75, y = 0.38,
             label = expression(atop("Disminuir dos desv. est."~sigma,"resulta en"~Delta%~~%-23~"pp")), 
             parse = TRUE) + 
    labs(title = expression("Inicial"~N(mu==0,sigma==0.5)),
         x=expression(beta)) + 
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5))} %>% 
  ggsave(plot = ., width = 16, height = 8, filename = "INICIALES/Inicial_N0_un_medio.pdf", device = cairo_pdf)
  

simulados %>% 
  filter(Tipo != "Mala") %>% 
  mutate(Aux1 = median(p), LI = quantile(p,0.025),LS=quantile(p,0.975)) %>% 
  {ggplot(.) + 
      geom_histogram(aes(x=p,stat(density)), color = paleta_tesis_fn$COLOR[2], 
                     fill = paleta_tesis_fn$COLOR[5], binwidth = 0.005) + 
      geom_segment(data = distinct(.,LI,LS), aes(x = LI, xend = LS), y = 14, yend = 14,
                   color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
      geom_text(data = distinct(.,Aux1), 
                aes(x = Aux1, label = paste("Intervalo central al 95%",round(Aux1,3),sep = "\n")), 
                y = 14.5, color = paleta_tesis_fn$COLOR[1]) + 
      geom_text(data = distinct(.,LI), aes(x = LI-0.015, label = round(LI,3)), y = 14,
                color = paleta_tesis_fn$COLOR[6]) + 
      geom_text(data = distinct(.,LS), aes(x = LS+0.015, label = round(LS,3)), y = 14, 
                color = paleta_tesis_fn$COLOR[2]) + 
      scale_y_continuous(limits = c(0,15)) + 
      labs(y="Densidad") + 
      theme_classic()} %>% 
  ggsave(plot = ., width = 16, height = 8, filename = "INICIALES/Iniciales_Alpha.pdf", device = cairo_pdf)


medias <- datos_P12 %>% 
  summarise_if(is.double,mean) %>% 
  select(Hom:Esc) %>% 
  gather(Cats,Media) %>% 
  left_join(equivalencia_variables) %>% 
  select(-Etiqueta)

set.seed(51295)
simulados_edad <- map_dfr(1:10000,function(simul) medias %>%  
                            filter(Variable == "Edad") %>%  
                            arrange(Media) %>% 
                            mutate(Media_Aux = Media - min(Media)) %>% 
                            mutate(alpha = rnorm(1,-1.7,0.25), 
                                   beta = rnorm(n(),0,0.5), 
                                   eta = Media_Aux*beta) %>% 
                            summarise(Variable = unique(Variable), 
                                      p = inv_logit(sum(eta)+unique(alpha))))


simulados_edad %>% 
{ggplot(.) + 
    geom_histogram(aes(x=p,stat(density)), color = paleta_tesis_fn$COLOR[2], 
                   fill = paleta_tesis_fn$COLOR[5], binwidth = 0.005) + 
    geom_segment(data = summarise(.,LI=quantile(p,0.025),LS=quantile(p,0.975)), 
                 aes(x = LI, xend = LS), y = 14, yend = 14,
                 color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
    geom_text(data = summarise(.,Aux1=mean(p)), 
              aes(x = Aux1), label = "Intervalo central al 95%", 
              y = 14.5, color = paleta_tesis_fn$COLOR[1]) + 
    geom_text(data =  summarise(.,LS=quantile(p,0.975)), 
              aes(x = LS+0.015, label = round(LS,3)), y = 14,
              color = paleta_tesis_fn$COLOR[2]) + 
    geom_text(data =  summarise(.,LI=quantile(p,0.025)), 
              aes(x = LI-0.015, label = round(LI,3)), y = 14,
              color = paleta_tesis_fn$COLOR[6]) + 
    labs(title = "10,000 simulaciones de la predictiva inicial",
         y = "Densidad") + 
    ylim(c(0,15)) + 
    theme_minimal()} %>% 
  ggsave(plot = ., width = 16, height = 8, filename = "INICIALES/Pred_Inicial.pdf", device = cairo_pdf)
  
