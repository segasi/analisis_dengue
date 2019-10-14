### Cargar paquetes, definir setup general y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
bd <- 
  read_excel("01_datos/bd_casos_dengue_semana_40.xlsx")

### Gráfica de LÍNEAS del número semanal de casos de dengue, 2013-2019 hasta la semana epidemiológica 40 ----

bd %>% 
  # Calcular el número de casos confirmados nuevos semanales
  group_by(año) %>% 
  mutate(numero_semanal = casos_confirmados - lag(casos_confirmados),
         numero_semanal = ifelse(is.na(numero_semanal), 0, numero_semanal), 
         color_lineas = ifelse(año == 2019, "sí", "no")) %>% 
  ungroup() %>% 
  # Generar etiquetas direrenciadas para 2014 y el resto de los años
  mutate(etiqueta_años = ifelse(año != 2014 & semana == max(semana), año, ""),
         etiqueta_2014 = ifelse(año == 2014 & semana == max(semana), año, "")) %>% 
  ggplot(aes(semana, numero_semanal, group = año, color = color_lineas)) +
  geom_line(size = 1.5) +
  geom_text(aes(label = etiqueta_años), color = "grey30", hjust = -0.1, fontface = "bold", size = 6) +
  geom_text(aes(label = etiqueta_2014), color = "grey30", hjust = -0.1, vjust = -0.5, fontface = "bold", size = 6) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 40, 5), 40)) +
  scale_y_continuous(label = comma, breaks = seq(0, 3000, 250)) +
  scale_color_manual(values = c("grey60", "salmon")) +
  labs(title = str_wrap(str_to_upper("número semanal de casos de dengue, 2013-2019"), width = 55),
       subtitle = "Casos confirmados hasta la semana epidemiológica 40",
       x = "\nSemana epidemiológica         \n",
       y = "Núm. de casos confirmados\n",
       caption = "@segasi / Fuente: SS, Boletín Epidemiológico y Panorama Epidemiológico de Dengue.") +
  tema +
  theme(panel.grid = element_line(size = 0.3),
        legend.position = "none") +
  ggsave("03_graficas/numero_semanal_casos_confirmados_dengue_semana_40.png", width = 13.2, height = 10, dpi = 200)



### Gráfica de BARRAS del número semanal de casos confirmados de dengue en 2019 vs. cada año entre 2013 y 2018 ----

## Preparar datos ----

# Calcular el número semanal de casos para cada año ----

bd_num_semanal <- 
  bd %>% 
  group_by(año) %>% 
  mutate(numero_semanal = casos_confirmados - lag(casos_confirmados),
         numero_semanal = ifelse(is.na(numero_semanal), 0, numero_semanal), 
         etiqueta_año = ifelse(semana == max(semana), año, ""),
         color_lineas = ifelse(año == 2019, "sí", "no")) %>% 
  ungroup() 

# Generar un vector con los datos de 2019 ----

num_semanal_2019 <- 
  bd_num_semanal %>% 
  filter(año == 2019) %>% 
  select(numero_semanal) %>% 
  pull()


## Graficar ----

# Definir tema para esta gráfica ----
tema_bis <- 
  tema +
  theme(plot.title = element_markdown(size = 20, 
                                      hjust = 0.5,
                                      color = "grey40"),
        panel.grid = element_line(linetype = 3))


# 2013 vs. 2019 ----
g_2013 <- 
  bd_num_semanal %>%
  filter(año == 2013) %>%
  mutate(num_semanal_2019 = num_semanal_2019) %>% 
  ggplot() +
  geom_col(aes(semana, numero_semanal), fill = "grey70") +
  geom_col(aes(semana, num_semanal_2019), fill = "salmon", alpha = 0.5) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 35, 5), 40)) +
  scale_y_continuous(label = comma, 
                     breaks = seq(0, 3000, 500), 
                     limits = c(0, 3000)) +
  labs(title = "<span style='color:#8c8c8c; padding-right: 20px'>2013</span> <span style='color:#4d4d4d'>vs.</span> <span style='color:#FA8072'>2019</span>",
       x = NULL,
       y = "Núm. semanal\n") +
  tema_bis +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# 2014 vs. 2019 ----
g_2014 <- 
  bd_num_semanal %>% 
  filter(año == 2014) %>% 
  mutate(num_semanal_2019 = num_semanal_2019) %>% 
  ggplot() +
  geom_col(aes(semana, numero_semanal), fill = "grey70") +
  geom_col(aes(semana, num_semanal_2019), fill = "salmon", alpha = 0.5) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 35, 5), 40)) +
  scale_y_continuous(label = comma, 
                     breaks = seq(0, 3000, 500), 
                     limits = c(0, 3000)) +
  labs(title = "<span style='color:#8c8c8c; padding-right: 20px'>2014</span> <span style='color:#4d4d4d'>vs.</span> <span style='color:#FA8072'>2019</span>",
       x = NULL,
       y = NULL) +
  tema_bis +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# 2015 vs. 2019 ----
g_2015 <- 
  bd_num_semanal %>% 
  filter(año == 2015) %>% 
  mutate(num_semanal_2019 = num_semanal_2019) %>% 
  ggplot() +
  geom_col(aes(semana, numero_semanal), fill = "grey70") +
  geom_col(aes(semana, num_semanal_2019), fill = "salmon", alpha = 0.5) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 35, 5), 40)) +
  scale_y_continuous(label = comma, 
                     breaks = seq(0, 3000, 500), 
                     limits = c(0, 3000)) +
  labs(title = "<span style='color:#8c8c8c; padding-right: 20px'>2015</span> <span style='color:#4d4d4d'>vs.</span> <span style='color:#FA8072'>2019</span>",
       x = NULL,
       y = NULL) +
  tema_bis +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
 
# 2016 vs. 2019 ----
g_2016 <- 
  bd_num_semanal %>% 
  filter(año == 2016) %>% 
  mutate(num_semanal_2019 = num_semanal_2019) %>% 
  ggplot() +
  geom_col(aes(semana, numero_semanal), fill = "grey70") +
  geom_col(aes(semana, num_semanal_2019), fill = "salmon", alpha = 0.5) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 35, 5), 40)) +
  scale_y_continuous(label = comma, 
                     breaks = seq(0, 3000, 500), 
                     limits = c(0, 3000)) +
  labs(title = "<span style='color:#8c8c8c; padding-right: 20px'>2016</span> <span style='color:#4d4d4d'>vs.</span> <span style='color:#FA8072'>2019</span>",
       x = "\n",
       y = "Núm. semanal\n") +
  tema_bis


# 2017 vs. 2019 ----
g_2017 <- 
  bd_num_semanal %>% 
  filter(año == 2017) %>% 
  mutate(num_semanal_2019 = num_semanal_2019) %>% 
  ggplot() +
  geom_col(aes(semana, numero_semanal), fill = "grey70") +
  geom_col(aes(semana, num_semanal_2019), fill = "salmon", alpha = 0.5) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 35, 5), 40)) +
  scale_y_continuous(label = comma, 
                     breaks = seq(0, 3000, 500), 
                     limits = c(0, 3000)) +
  labs(title = "<span style='color:#8c8c8c; padding-right: 20px'>2017</span> <span style='color:#4d4d4d'>vs.</span> <span style='color:#FA8072'>2019</span>",
       x = "\n",
       y = NULL) +
  tema_bis +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# 2018 vs. 2019 ----
g_2018 <- 
  bd_num_semanal %>% 
  filter(año == 2018) %>% 
  mutate(num_semanal_2019 = num_semanal_2019) %>% 
  ggplot() +
  geom_col(aes(semana, numero_semanal), fill = "grey70") +
  geom_col(aes(semana, num_semanal_2019), fill = "salmon", alpha = 0.5) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 35, 5), 40)) +
  scale_y_continuous(label = comma, 
                     breaks = seq(0, 3000, 500), 
                     limits = c(0, 3000)) +
  labs(title = "<span style='color:#8c8c8c; padding-right: 20px'>2018</span> <span style='color:#4d4d4d'>vs.</span> <span style='color:#FA8072'>2019</span>",
       subtitle = NULL,
       x = "\nSemana epidemiológica   ",
       y = NULL) +
  tema_bis +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


# Generar gráficas ----

# Panel de arriba
g_arriba <- 
  plot_grid(g_2013, g_2014, g_2015, rel_widths = c(1.26, 1, 1), ncol = 3)

# Panel de abajo
g_abajo <- 
  plot_grid(g_2016, g_2017, g_2018, rel_widths = c(1.26, 1, 1), ncol = 3)

# Juntar páneles
grafica <- 
  plot_grid(g_arriba, g_abajo, nrow = 2, rel_heights = c(0.46, 0.54)) 

# Definir título
titulo <- 
  ggdraw() + draw_label(str_to_upper("número semanal de casos confirmados de dengue\nen 2019 vs. cada año entre 2013 y 2018                   "), fontface = 'bold', size = 40, hjust = 0.5, fontfamily = "Trebuchet MS Bold", colour = "grey20")

# Generar espacio blanco entre título y gráfica
espacio_vacio <- 
  ggdraw() + draw_label(str_to_upper(""), fontface = 'bold', size = 10)

# Definir caption
caption <- 
  ggdraw() + draw_label("\n@segasi / Fuente: SS, Boletín Epidemiológico y Panorama Epidemiológico de Dengue.                                                                                               ", fontface = 'bold', fontfamily = "Didact Gothic Regular", size = 15, hjust = 0.5, colour = "grey20")

# Unir todo y graficar ----
plot_grid(titulo, espacio_vacio, grafica, caption, ncol = 1, rel_heights = c(0.13, 0.02, 1, 0.08)) +
  ggsave("03_graficas/barras_numero_semanal_casos_confirmados_dengue_semana_40.png", width = 14.5, height = 10, dpi = 200)





### Gráfica de LÍNEAS del número acumulado de casos confirmados de dengue hasta la semana epidemiológica 40 ----

bd %>% 
  mutate(color_lineas = ifelse(año == 2019, "sí", "no"),
         etiqueta_años = ifelse(!año %in% c(2014, 2015, 2019) & semana == max(semana), año, ""),
         etiqueta_2014 = ifelse(año == 2014 & semana == max(semana), año, ""),
         etiqueta_2015 = ifelse(año == 2015 & semana == max(semana), año, ""),
         etiqueta_2019 = ifelse(año == 2019 & semana == max(semana), año, "")) %>% 
  ggplot(aes(semana, casos_confirmados, group = año, color = color_lineas)) +
  geom_line(size = 1.5) +
  geom_text(aes(label = etiqueta_años), color = "grey30", hjust = -0.1, fontface = "bold", size = 6) +
  geom_text(aes(label = etiqueta_2015), color = "grey30", hjust = -0.1, 
            vjust = 0.7,
            fontface = "bold", size = 6) +
  geom_text(aes(label = etiqueta_2014), color = "grey30", hjust = -0.1, 
            vjust = -0.3,
            fontface = "bold", size = 6) +
  geom_text(aes(label = etiqueta_2019), color = "grey30", hjust = -0.1, 
            vjust = -0.2,
            fontface = "bold", size = 6) +
  scale_x_continuous(limits = c(1, 40.5), breaks = c(1, seq(5, 35, 5), 40)) +
  scale_y_continuous(label = comma, breaks = seq(0, 50000, 2500)) +
  scale_color_manual(values = c("grey60", "salmon")) +
  labs(title = str_wrap(str_to_upper("número acumulado de casos de dengue, 2013-2019"), width = 50),
       subtitle = "Casos confirmados hasta la semana epidemiológica 40",
       x = "\nSemana epidemiológica         \n",
       y = "Núm. de casos confirmados\n",
       caption = "@segasi / Fuente: SS, Boletín Epidemiológico y Panorama Epidemiológico de Dengue.") +
  tema +
  theme(panel.grid = element_line(size = 0.3),
        legend.position = "none") +
  ggsave("03_graficas/numero_acumulado_casos_confirmados_dengue_semana_40.png", width = 13, height = 10, dpi = 200)




### Gráfica de BARRAS del número de casos confirmados de dengue hasta la semana epidemiológica 40 de cada año, 2013-2019 ----

bd %>% 
  filter(semana == 40) %>% 
  mutate(color_barras = ifelse(año == 2019, "sí", "no"),
         etiqueta_años = ifelse(!año %in% c(2014, 2015, 2019) & semana == max(semana), año, ""),
         etiqueta_2014 = ifelse(año == 2014 & semana == max(semana), año, ""),
         etiqueta_2015 = ifelse(año == 2015 & semana == max(semana), año, ""),
         etiqueta_2019 = ifelse(año == 2019 & semana == max(semana), año, "")) %>% 
  ggplot(aes(año, casos_confirmados, fill = color_barras)) +
  geom_col() +
  geom_text(aes(label = comma(casos_confirmados)), color = "white", vjust = 1.6, fontface = "bold", size = 6, family = "Trebuchet MS Bold") +
  scale_x_continuous(breaks = 2013:2019) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("grey60", "salmon")) +
  labs(title = str_wrap(str_to_upper("número de casos confirmados de dengue hasta la semana epidemiológica 40, 2013-2019"), width = 55),
       x = "\n",
       y = NULL,
       caption = "@segasi / Fuente: SS, Boletín Epidemiológico y Panorama Epidemiológico de Dengue.") +
  tema +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  ggsave("03_graficas/numero_casos_confirmados_dengue_semana_40.png", width = 13, height = 10, dpi = 200)
