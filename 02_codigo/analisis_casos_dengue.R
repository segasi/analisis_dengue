### Cargar paquetes, definir setup general y tema de gráficas ----
source("02_codigo/paquetes_setup_tema.R")

### Importar datos ----
bd <- 
  read_excel("01_datos/bd_casos_dengue.xlsx")

### Gráfica de LÍNEAS del número semanal de casos de dengue, 2013-2019 hasta la semana epidemiológica 35 ----

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
  scale_x_continuous(limits = c(1, 35.5), breaks = c(1, seq(5, 35, 5))) +
  scale_y_continuous(label = comma, breaks = seq(0, 2500, 250)) +
  scale_color_manual(values = c("grey60", "salmon")) +
  labs(title = str_wrap(str_to_upper("número semanal de casos de dengue, 2013-2019"), width = 55),
       subtitle = "Casos confirmados hasta la semana epidemiológica 35",
       x = "\nSemana epidemiológica         \n",
       y = "Casos confirmados\n",
       caption = "@segasi / Fuente: SS, Boletín epidemiológico y Panorama Epidemiológico de Dengue.") +
  tema +
  theme(panel.grid = element_line(size = 0.3),
        legend.position = "none") +
  ggsave("03_graficas/numero_semanal_casos_confirmados_dengue_semana_35.png", width = 13.2, height = 10, dpi = 200)
