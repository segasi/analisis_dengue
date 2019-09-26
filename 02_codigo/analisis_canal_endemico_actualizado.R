### Cargar paquetes, definir setup general y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
bd <- 
  read_excel("01_datos/bd_casos_dengue_semana_38.xlsx")

### Calcular el número de casos confirmados nuevos semanales ---
bd_canal <- 
  bd %>% 
  group_by(año) %>% 
  mutate(numero_semanal = casos_confirmados - lag(casos_confirmados),
         numero_semanal = ifelse(is.na(numero_semanal), 0, numero_semanal)) %>% 
  ungroup()  

### Gráfica del CANAL ENDÉMICO ----
bd_canal %>% 
  select(año, semana, numero_semanal) %>% 
  arrange(año, semana) %>% 
  mutate(ln_incidencia = log(numero_semanal + 1)) %>% 
  filter(año > 2013) %>% 
  group_by(semana) %>% 
  summarise(media_geom = geometric.mean(ln_incidencia),
            des_est = sd(ln_incidencia), 
            limite_sup = media_geom + ((des_est*2.78)/sqrt(5)),
            limite_inf = media_geom - ((des_est*2.78)/sqrt(5))) %>% 
  ungroup() %>% 
  mutate(casos_esperados = exp(media_geom) - 1,
         superior = exp(limite_sup) - 1,
         inferior = exp(limite_inf) - 1) %>% 
  ggplot() +
  annotate(geom = "rect", xmin = 1, xmax = 38, ymin = 0, ymax = 2500, fill = "salmon") +
  geom_area(aes(semana, superior), fill = "#fed976") +
  geom_area(aes(semana, casos_esperados), fill = "#74c476") +
  geom_area(aes(semana, inferior), fill = "#6baed6") +
  geom_vline(xintercept = c(1, seq(5, 30, 5), 38), color = "white", linetype = 3, size = 0.4) +
  geom_hline(yintercept =  seq(0, 2500, 500), color = "white", linetype = 3, size = 0.4) +
  geom_line(data = bd_canal %>% filter(año == 2019), aes(semana, numero_semanal), 
            color = "grey40",
            size = 1) +
  geom_point(data = bd_canal %>% filter(año == 2019), aes(semana, numero_semanal), color = "grey40") +
  annotate(geom = "text", x = 35.5, y = 150, label = "Zona de", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 80, label = "éxito", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 550, label = "Zona de", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 470, label = "seguridad", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 1050, label = "Zona de", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 970, label = "alerta", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 33, y = 1850, label = "Zona", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 33, y = 1770, label = "epidémica", family = "Didact Gothic Regular", size = 5) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(1, seq(5, 30, 5), 38),
                     limits = c(-0.02, 38.5)) +
  scale_y_continuous(breaks = seq(0, 2500, 500), labels = comma) +
  labs(title = str_wrap("CANAL ENDÉMICO (2014-2018) E INCIDENCIA SEMANAL DE DENGUE (2019) EN MÉXICO", width = 55), 
       subtitle = "Calculados con el número de casos confirmados semanalmente hasta la semana epidemiológica 38 de cada año",
       x = "\nSemana  ",
       y = "Casos confirmados\n",
       caption = "@segasi y @jorgeacast / Fuente: SS, Boletín Epidemiológico y Panorama Epidemiológico de Dengue.") +
  tema +
  theme(panel.background = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_line(color = "grey20", linetype = 3, size = 0.4),
        axis.ticks = element_line(color = "grey20", linetype = 3, size = 0.4)) +
  ggsave("03_graficas/corredor_endemico_casos_confirmados_dengue_semana_38.png", width = 13.2, height = 10, dpi = 200)

### Cálculo del número de semanas en cada zona ----
bd_canal %>% 
  select(año, semana, numero_semanal) %>% 
  arrange(año, semana) %>% 
  mutate(ln_incidencia = log(numero_semanal + 1)) %>% 
  filter(año > 2013) %>% 
  group_by(semana) %>% 
  summarise(media_geom = geometric.mean(ln_incidencia),
            des_est = sd(ln_incidencia), 
            limite_sup = media_geom + ((des_est*2.78)/sqrt(5)),
            limite_inf = media_geom - ((des_est*2.78)/sqrt(5))) %>% 
  ungroup() %>% 
  mutate(casos_esperados = exp(media_geom) - 1,
         superior = exp(limite_sup) - 1,
         inferior = exp(limite_inf) - 1) %>%
  left_join(bd_canal %>% filter(año == 2019) %>% select(semana, numero_semanal), by = "semana") %>% 
  summarise(zona_exito = sum(numero_semanal <= inferior),
            zona_seguridad = sum(numero_semanal > inferior & numero_semanal <= casos_esperados),
            zona_alerta = sum(numero_semanal > casos_esperados & numero_semanal <= superior),
            zona_epidemica = sum(numero_semanal > superior)) %>% 
  gather()


### Gráfica del CANAL ENDÉMICO acumulado ----

bd_canal %>% 
  mutate(ln_casos_confirmados = log(casos_confirmados + 1)) %>% 
  filter(año > 2013) %>% 
  group_by(semana) %>% 
  summarise(media_geom = geometric.mean(ln_casos_confirmados),
            des_est = sd(ln_casos_confirmados), 
            limite_sup = media_geom + (des_est*2.78/sqrt(5)),
            limite_inf = media_geom - (des_est*2.78/sqrt(5))) %>% 
  ungroup() %>% 
  mutate(casos_esperados = exp(media_geom) - 1,
         superior = exp(limite_sup) - 1,
         inferior = exp(limite_inf) - 1) %>% 
  ggplot() +
  annotate(geom = "rect", xmin = 1, xmax = 38, ymin = 0, ymax = 20000, fill = "salmon") +
  geom_area(aes(semana, superior), fill = "#fed976") +
  geom_area(aes(semana, casos_esperados), fill = "#74c476") +
  geom_area(aes(semana, inferior), fill = "#6baed6") +
  geom_vline(xintercept = c(1, seq(5, 30, 5), 38), color = "white", linetype = 3, size = 0.4) +
  geom_hline(yintercept =  seq(0, 20000, 2500), color = "white", linetype = 3, size = 0.4) +
  geom_line(data = bd_canal %>% filter(año == 2019), aes(semana, casos_confirmados), 
            color = "grey40",
            size = 1) +
  geom_point(data = bd_canal %>% filter(año == 2019), aes(semana, casos_confirmados), color = "grey40") +
  annotate(geom = "text", x = 35.5, y = 1500, label = "Zona de", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 800, label = "éxito", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 6400, label = "Zona de", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.5, y = 5600, label = "seguridad", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.6, y = 10500, label = "Zona de", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 35.6, y = 9700, label = "alerta", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 33, y = 19000, label = "Zona", family = "Didact Gothic Regular", size = 5) +
  annotate(geom = "text", x = 33, y = 18200, label = "epidémica", family = "Didact Gothic Regular", size = 5) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = c(1, seq(5, 30, 5), 38),
                     limits = c(0, 38.5)) +
  scale_y_continuous(breaks = seq(0, 20000, 2500), labels = comma) +
  labs(title = str_wrap("CANAL ENDÉMICO ACUMULADO (2014-2018) E INCIDENCIA ACUMULADA (2019) EN MÉXICO", width = 55), 
       subtitle = "Calculados con el número acumulado de casos confirmados hasta la semana epidemiológica 38 de cada año",
       x = "\nSemana  ",
       y = "Número acumulado\n",
       caption = "@segasi y @jorgeacast / Fuente: SS, Boletín Epidemiológico y Panorama Epidemiológico de Dengue.") +
  tema +
  theme(panel.background = element_rect(fill = "transparent", color = "transparent"),
        panel.grid = element_line(color = "grey20", linetype = 3, size = 0.4),
        axis.ticks = element_line(color = "grey20", linetype = 3, size = 0.4)) +
  ggsave("03_graficas/corredor_endemico_acumulado_casos_confirmados_dengue_semana_38.png", width = 13.2, height = 10, dpi = 200)


### Identificar desde qué semana la incidencia de dengue se encuentra en la zona de alerta ----
bd_canal %>% 
  mutate(ln_casos_confirmados = log(casos_confirmados + 1)) %>% 
  filter(año > 2013) %>% 
  group_by(semana) %>% 
  summarise(media_geom = geometric.mean(ln_casos_confirmados),
            des_est = sd(ln_casos_confirmados), 
            limite_sup = media_geom + (des_est*2.78/sqrt(5)),
            limite_inf = media_geom - (des_est*2.78/sqrt(5))) %>% 
  ungroup() %>% 
  mutate(casos_esperados = exp(media_geom) - 1,
         superior = exp(limite_sup) - 1,
         inferior = exp(limite_inf) - 1) %>%
  left_join(bd_canal %>% filter(año == 2019) %>% select(semana, casos_confirmados), by = "semana") %>%  
  mutate(zona_exito = casos_confirmados <= inferior,
         zona_seguridad = casos_confirmados > inferior & casos_confirmados <= casos_esperados,
         zona_alerta = casos_confirmados > casos_esperados & casos_confirmados <= superior,
         zona_epidemica = casos_confirmados > superior) %>% 
  select(-c(media_geom:limite_inf)) %>% 
  select(semana, inferior, casos_esperados, superior, casos_confirmados, everything()) %>% 
  print(n = Inf)
