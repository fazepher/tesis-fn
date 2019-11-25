

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
                    DE0125 = rnorm(500000,0,1.25),
                    DE01 = rnorm(500000,0,1),
                    Inicial = rnorm(500000,-1.8,1.25)) %>% 
  gather(DE,theta) %>% 
  mutate(Mu = 0,
         Tipo = case_when(DE == "Inicial"~"N(mu==-1.8,sigma==1.25)",
                          T ~ "Mala"),
         Sigma = case_when(DE == "DE10"~"sigma[1] == 10",
                           DE == "DE05"~"sigma[2] == 5",
                           DE == "DE025"~"sigma[3] == 2.5",
                           DE == "DE0125"~"sigma[4] == 1.25",
                           DE == "DE01"~"sigma[5] == 1"),
         p = inv_logit(theta)) 

{ggplot(tibble(eta=seq(-30,30,by=0.05)) %>% mutate(Densidad=dnorm(eta,sd=10))) + 
    geom_area(aes(x=eta,y=Densidad), color = paleta_tesis_fn$COLOR[2], 
              fill = paleta_tesis_fn$COLOR[5], size = rel(0.5), alpha = 0.4) + 
    ggforce::facet_zoom(xlim = c(-5,5)) + 
    labs(title = expression("Inical" ~ eta %~% N(mu==0,sigma^2==100) ~ "en el rango (-5,5)"), 
         x = expression(eta)) + 
    theme_half_open() + 
    theme(line = element_line(color = paleta_tesis_fn$COLOR[1]),
          plot.title = element_text(size = rel(1), hjust = 0.5, margin = margin(b = 20)), 
          axis.title.y = element_text(size = rel(0.8)),
          axis.title.x = element_text(size = rel(1.2)),
          strip.background = element_rect(fill = "#FFD1C9"))} %>% 
  ggsave(plot = ., width = 22.5/4, height = 17/4, filename = "INICIALES/Inicial_N_0_100_eta.pdf", device = cairo_pdf)

simulados %>% 
  filter(Tipo == "Mala") %>% 
{ggplot(.) + 
    geom_histogram(aes(x=p,stat(density)), color = paleta_tesis_fn$COLOR[2], 
                   fill = paleta_tesis_fn$COLOR[5], binwidth = 0.025, size = rel(0.5)) + 
    facet_wrap(~Sigma,nrow = 1, labeller = label_parsed) + 
    labs(title = expression("Diferentes distribuciones para" ~ p ~ "inducidas por" ~ eta %~% N(mu==0,sigma[i])), 
         y="Densidad") + 
    scale_x_continuous(breaks = c(0,0.5,1), labels = c("0","0.5","1")) + 
    theme_classic() + 
    theme(plot.title = element_text(size = rel(1.25), hjust = 0.5), 
          strip.text = element_text(size = rel(1.5)))} %>% 
  ggsave(plot = ., width = 22.5/3, height = 17/3, filename = "INICIALES/Malas_Iniciales.pdf", device = cairo_pdf)


{ggplot(tibble(x=seq(-10,10,by=0.001)) %>% mutate(p=inv_logit(x))) + 
    geom_path(aes(x,p), color = paleta_tesis_fn$COLOR[1], size = rel(1)) + 
    annotate("rect",xmin=-5,xmax=5,ymin=0,ymax = 1, fill = paleta_tesis_fn$COLOR[2], alpha = 0.1) + 
    annotate("segment",x=0,xend=0,y=0,yend = 0.5, color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
    annotate("segment",x=-10,xend=0,y=0.5,yend = 0.5, color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
    annotate("text",x = 2.5, y = 0.5, color = paleta_tesis_fn$COLOR[1], size = rel(3.5),
             label = expression(atop(eta==0~"en la escala real","equivale a"~p==0.5)), parse=TRUE) +
    annotate("text",x = 8, y = 0.9, color = paleta_tesis_fn$COLOR[1], size = rel(3.5),
             label = expression(atop(eta == 5~"lleva a"~p %~~% 0.9933,eta == 10~"a"~p %~~% 0.9999)), parse = TRUE) + 
    annotate("text",x = -6.3, y = 0.15, color = paleta_tesis_fn$COLOR[1], size = rel(3.2),
             label = expression(atop("Pasar de"~eta==-10~"a"~eta==-5~"aumenta p en .0066", 
                                     "pero otras 5 unidades aportan 0.4933 a p")), parse = TRUE) + 
    labs(title = "Rango de la escala logística",
         x = expression(eta)) + 
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, size = rel(1.5)),
          axis.text = element_text(size = rel(1.5)),
          axis.title = element_text(size = rel(2)))} %>% 
  ggsave(plot = ., width = 22.5/3, height = 17/3, filename = "INICIALES/Escala_Logis.pdf", device = cairo_pdf)

{ggplot(tibble(x=seq(-3.3,-0.3,by=0.001)) %>% mutate(p=inv_logit(x))) + 
    geom_path(aes(x,p), color = paleta_tesis_fn$COLOR[1], size = rel(1)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[1], linetype = 2,
             x=-1.8,xend=-1.8,
             y=0,yend = inv_logit(-1.8)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[1], linetype = 2,
             x=-3.3,xend=-1.8,
             y=inv_logit(-1.8),yend = inv_logit(-1.8)) +
    annotate("segment", color = paleta_tesis_fn$COLOR[2], linetype = 2,
             x=-1.8+0.5,xend=-1.8 + 0.5,
             y=0,yend = inv_logit(-1.8+0.5)) +
    annotate("segment", color = paleta_tesis_fn$COLOR[2], linetype = 2,
             x=-3.3,xend=-1.8+0.5,
             y=inv_logit(-1.8+.5),yend = inv_logit(-1.8+.5)) +
    annotate("segment", color = paleta_tesis_fn$COLOR[2], size = rel(1), arrow = arrow(length = unit(0.1,"inches")),
             x=-1.8-0.02,xend=-1.8+0.38,
             y=inv_logit(-1.8)+.01,yend = inv_logit(-1.8+.38) + .01) +
    annotate("text", color = paleta_tesis_fn$COLOR[1],
             x = -1.8-.3, y = inv_logit(-1.8) + 0.04 ,
             label = expression(atop("Aumentar una desv. est."~sigma,"resulta en"~Delta%~~%+7~"pp")), parse = TRUE) +
    annotate("segment", color = paleta_tesis_fn$COLOR[3], linetype = 2,
             x=-1.8-1,xend=-1.8-1,
             y=0,yend = inv_logit(-1.8-1)) +
    annotate("segment", color = paleta_tesis_fn$COLOR[3], linetype = 2,
             x=-3.3,xend=-1.8-1,
             y=inv_logit(-1.8-1),yend = inv_logit(-1.8-1)) +
    annotate("segment", color = paleta_tesis_fn$COLOR[3], size = rel(1), arrow = arrow(length = unit(0.1,"inches")),
             x=-1.8-0.1,xend=-1.8-0.95,
             y=inv_logit(-1.8-.1)+.005,yend = inv_logit(-1.8-.95) + .01) +
    annotate("text", color = paleta_tesis_fn$COLOR[1],
             x = -1.8-1, y = 0.11,
             label = expression(atop("Disminuir dos desv. est."~sigma,"resulta en"~Delta%~~%-8~"pp")),
             parse = TRUE) +
    labs(title = expression("Inicial"~N(mu==-1.8,sigma==0.5)),
         x=expression(alpha)) + 
    scale_x_continuous(breaks = c(-3.3,-1.8,-0.3)) + 
    scale_y_continuous(breaks = inv_logit(c(-2.8,-1.8,-1.3,-0.8,-0.3)) %>% round(2)) + 
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = rel(2)),
          axis.title = element_text(size = rel(2.5)),
          plot.title = element_text(hjust = 0.5, size = rel(2.5)))} %>% 
  ggsave(plot = ., width = 22.5/2.5, height = 17/2.5, filename = "INICIALES/Inicial_N0_un_medio.pdf", device = cairo_pdf)

{ggplot(tibble(x=seq(-10,10,by=0.001)) %>% mutate(p=inv_logit(x))) + 
    geom_path(aes(x,p), color = paleta_tesis_fn$COLOR[1], size = rel(1)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[1], linetype = 2,
             x=0,xend=0,
             y=0,yend = 0.5) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[1], linetype = 2,
             x=-10,xend=0,
             y=0.5,yend = 0.5) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[2], linetype = 2,
             x=1,xend=1,
             y=0,yend = inv_logit(1)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[2], linetype = 2,
             x=-10,xend=1,
             y=inv_logit(1),yend = inv_logit(1)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[2], size = rel(1), arrow = arrow(length = unit(0.1,"inches")),
             x=-0.2,xend=0.7,
             y=0.51,yend = inv_logit(1)-.01) + 
    annotate("text", color = paleta_tesis_fn$COLOR[1], size = rel(6),
             x = -4, y = 0.6,
             label = expression(atop("Aumentar una desv. est."~sigma,"resulta en"~Delta%~~%+23~"pp")), parse = TRUE) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[3], linetype = 2,
             x=-2,xend=-2,
             y=0,yend = inv_logit(-2)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[3], linetype = 2,
             x=-10,xend=-2,
             y=inv_logit(-2),yend = inv_logit(-2)) + 
    annotate("segment", color = paleta_tesis_fn$COLOR[3], size = rel(1), arrow = arrow(length = unit(0.1,"inches")),
             x=-0.3,xend=-2.01,
             y=0.49,yend = inv_logit(-2)+.02) +
    annotate("text", color = paleta_tesis_fn$COLOR[1], size = rel(6),
             x = -5, y = 0.32,
             label = expression(atop("Disminuir dos desv. est."~sigma,"resulta en"~Delta%~~%-38~"pp")), 
             parse = TRUE) + 
    labs(title = expression("Inicial"~N(mu==0,sigma==1)),
         x=expression(beta)) + 
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = rel(2)),
          axis.title = element_text(size = rel(2.5)),
          plot.title = element_text(hjust = 0.5, size = rel(2.5)))} %>% 
  ggsave(plot = ., width = 22.5/2.5, height = 17/2.5, filename = "INICIALES/Inicial_N0_uno.pdf", device = cairo_pdf)
  

# simulados %>% 
#   filter(Tipo != "Mala") %>% 
#   mutate(Aux1 = median(p), LI = quantile(p,0.025),LS=quantile(p,0.975)) %>% 
#   {ggplot(.) + 
#       geom_histogram(aes(x=p,stat(density)), color = paleta_tesis_fn$COLOR[2], 
#                      fill = paleta_tesis_fn$COLOR[5], binwidth = 0.005) + 
#       geom_segment(data = distinct(.,LI,LS), aes(x = LI, xend = LS), y = 5, yend = 5,
#                    color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
#       geom_text(data = distinct(.,Aux1), 
#                 aes(x = Aux1, label = paste("Intervalo central al 95%",round(Aux1,3),sep = "\n")), 
#                 y = 6, color = paleta_tesis_fn$COLOR[1]) + 
#       geom_text(data = distinct(.,LI), aes(x = LI-0.015, label = round(LI,3)), y = 5,
#                 color = paleta_tesis_fn$COLOR[6]) + 
#       geom_text(data = distinct(.,LS), aes(x = LS+0.015, label = round(LS,3)), y = 5, 
#                 color = paleta_tesis_fn$COLOR[2]) + 
#       scale_y_continuous(limits = c(0,15)) + 
#       labs(y="Densidad") + 
#       theme_classic()} %>% 
#   ggsave(plot = ., width = 22.5/3, height = 17/3, filename = "INICIALES/Iniciales_Alpha.pdf", device = cairo_pdf)


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
                            mutate(alpha = rnorm(1,-1.8,0.5), 
                                   beta = rnorm(n(),0,1), 
                                   eta = Media_Aux*beta) %>% 
                            summarise(Variable = unique(Variable), 
                                      p = inv_logit(sum(eta)+unique(alpha))))


simulados_edad %>% 
{ggplot(.) + 
    geom_histogram(aes(x=p,stat(density)), color = paleta_tesis_fn$COLOR[2], 
                   fill = paleta_tesis_fn$COLOR[5], binwidth = 0.005, size = rel(0.25)) + 
    geom_segment(data = summarise(.,LI=quantile(p,0.025),LS=quantile(p,0.975)), 
                 aes(x = LI, xend = LS), y = 6.5, yend = 6.5,
                 color = paleta_tesis_fn$COLOR[1], linetype = 2) + 
    geom_text(data = summarise(.,Aux1=mean(p)), 
              aes(x = Aux1), label = "Intervalo central al 95%", 
              y = 7, color = paleta_tesis_fn$COLOR[1]) + 
    geom_text(data =  summarise(.,LS=quantile(p,0.975)), 
              aes(x = LS+0.03, label = round(LS,3)), y = 6.5,
              color = paleta_tesis_fn$COLOR[2], size = rel(5)) + 
    geom_text(data =  summarise(.,LI=quantile(p,0.025)), 
              aes(x = LI-0.03, label = round(LI,3)), y = 6.5,
              color = paleta_tesis_fn$COLOR[6], size = rel(5)) + 
    labs(title = "10,000 simulaciones de la predictiva inicial",
         y = "Densidad") + 
    ylim(c(0,8)) + 
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = rel(1.25)),
          axis.title = element_text(size = rel(1.5)))} %>% 
  ggsave(plot = ., width = 22.5/3, height = 17/3, filename = "INICIALES/Pred_Inicial.pdf", device = cairo_pdf)
  
