library(loo)

nombres_nal <- discard(list.files("MODELOS_STAN/Modelos_Nal_Ind"),~str_detect(.,"PRED."))
waics_nal <- nombres_nal  %>% 
{map(.,~paste("MODELOS_STAN/Modelos_Nal_Ind",.x,sep="/") %>% 
       read_rds %>% extract_log_lik %>% waic) %>% 
    set_names(nombres_nal)}

nombres_jer <- discard(list.files("MODELOS_STAN/Modelos_Jer_Ind"),~str_detect(.,"PRED."))
waics_jer <- nombres_jer  %>% 
{map(.,~paste("MODELOS_STAN/Modelos_Jer_Ind",.x,sep="/") %>% 
       read_rds %>% extract_log_lik %>% waic) %>% 
    set_names(nombres_jer)}

nombres_comp <-  discard(list.files("MODELOS_STAN/Modelos_Jer_Comp"),~str_detect(.,"PRED.|_S"))
waics_comp <- nombres_comp  %>%
{map(.,~paste("MODELOS_STAN/Modelos_Jer_Comp",.x,sep="/") %>%
       read_rds %>% extract_log_lik %>% waic) %>%
    set_names(nombres_comp)}

waics_todos <- c(waics_nal,waics_jer,waics_comp)
nombres_todos <- waics_todos %>% names
compara_todos <- map(names(waics_todos),function(m1) 
  map(names(waics_todos),function(m2) 
    compare(x=waics_todos[c(m1,m2)])) %>% 
    set_names(names(waics_todos)) %>% 
    unlist) %>% 
  set_names(names(waics_todos)) %>% 
  unlist() %>% 
  data.frame(Valor=.)%>% 
  tibble::rownames_to_column("Aux") %>% 
  mutate(Aux = str_remove_all(Aux,".rds")) %>% 
  separate(Aux,c("M1","M2","Est"),"\\.") %>% 
  spread(Est,Valor) %>% mutate(Aux = 1:n())

set.seed(51295)
compara_todos <- compara_todos$Aux %>% 
  map_dfr(~ compara_todos %>% 
            slice(.x) %>% 
            mutate(p = pnorm(0,elpd_diff,se)))

compara_waics <- names(waics_todos) %>% 
  map_dfr(~tibble(M1 = str_remove(.x,".rds"), 
                  WAIC =waics_todos[[.x]]$estimates["waic","Estimate"],
                  EE = waics_todos[[.x]]$estimates["waic","SE"])) %>% 
  filter(str_detect(M1,"Jer")) %>% 
  mutate(Variables = case_when(str_detect(M1,"Compuesto_A")~2, 
                               str_detect(M1,"Compuesto_B")~3,
                               str_detect(M1,"Compuesto_C")~4, 
                               str_detect(M1,"Compuesto_D")~5,
                               str_detect(M1,"Compuesto_E")~6,
                               str_detect(M1,"Compuesto_F")~6,
                               str_detect(M1,"Compuesto_G")~7,
                               str_detect(M1,"Compuesto_H")~8,
                               T ~ 1) %>% as.character,
         Etiqueta = if_else(Variables == 1,"0",str_sub(M1,-1)),
         Modelo = case_when(Variables == "1" ~ str_replace_all(str_remove_all(M1,"Modelo_Jer_"),"_","\\. "),
                            Variables == "2" ~ "Escol. + CSP", 
                            Variables == "3" ~ "Escol. + CSP + Edad",
                            Variables == "4" ~ "Escol. + CSP + Edad + Cond. Migr.",
                            Variables == "5" ~ "Escol. + CSP + Edad + Cond. Migr. + Sexo",
                            str_detect(M1,"_E") ~ "Escol. + CSP + Edad + Cond. Migr. + Sexo + Ocu. Juv.",
                            str_detect(M1,"_F") ~ "Escol. + CSP + Edad + Cond. Migr. + Sexo + Ocu. Gral.",
                            Variables == "7" ~ "Escol. + CSP + Edad + Cond. Migr. + Sexo + Ocu. Gral. + Ocu. Juv.",
                            Variables == "8" ~ "Escol. + CSP + Edad + Cond. Migr. + Sexo + Ocu. Gral. + Ocu. Juv. + Ocu. May.") %>% 
           reorder(WAIC)) 

compara_waics %>%
  filter(WAIC <= 50000) %>% 
  {ggplot(.,aes(x=WAIC,y=Modelo,color=Variables,label=Etiqueta)) + 
      geom_segment(aes(x=WAIC-EE,xend=WAIC+EE,yend=Modelo),key_glyph = draw_key_rect) + 
      geom_label(size=rel(4.5),show.legend = FALSE) + 
      scale_color_manual(values = c("black",paleta_tesis_fn$COLOR[c(1,6,4,3,2,5,7)])) +
      guides(col = guide_legend(nrow=1)) + 
      labs(title = "El WAIC mejora al ir agregando las variables explicativas") +
      theme_linedraw() +
      theme(legend.position = "bottom", 
            panel.grid = element_blank(),
            axis.text.y = element_text(size = rel(0.75)),
            plot.margin = margin(r = 10, l = 10),
            plot.title = element_text(margin = margin(t = 10, b = 10), size = rel(1.25)),
            axis.title.x = element_text(margin = margin(t = 15, b = 7), size = rel(1.2)),
            axis.title.y = element_blank())} %>%
  ggsave(filename = "MODELOS_STAN/Graf_WAIC_Modelos_Compuestos.pdf",plot = .,
         device = cairo_pdf, width = 22.5/2, height = 17/3)

# names(waics_todos) %>%
#   map_dfr(~tibble(M1 = str_remove(.x,".rds"),
#                   WAIC =waics_todos[[.x]]$estimates["waic","Estimate"])) %>%
#   right_join(compara_todos) %>%
#   filter(M1!=M2,!str_detect(M1,"Compuesto"),!str_detect(M2,"Compuesto")) %>%
#   mutate(M1 = str_remove_all(M1,"Modelo_") %>% str_replace_all("_"," ") %>% reorder(WAIC),
#          M2 = str_remove_all(M2,"Modelo_") %>% str_replace_all("_"," ") %>% factor(levels=rev(levels(M1)),ordered = T),
#          p = round(100*p)) %>%
#   {ggplot(data = ., aes(x=M1,y=M2,label=p,fill=p)) +
#       geom_label() +
#       scale_fill_gradientn(colours = c(paleta_tesis_fn$COLOR[6],"white",paleta_tesis_fn$COLOR[2]),
#                            values = c(0,0.01,0.5,0.99,1)) +
#       labs(title = expression("Probabilidad estimada de que"~WAIC[M1]<=WAIC[M2])) +
#       theme_minimal() +
#       theme(axis.text.x = element_text(angle = 90))} %>%
#   ggsave(filename = "MODELOS_STAN/Graf_WAIC_Probas_Modelos_Individuales.pdf",plot = .,
#          device = cairo_pdf, width = 20, height = 10)


