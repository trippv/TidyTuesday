## Cargar paquetes

library(tidyverse)
library(patchwork)
library(skimr)
library(summarytools)
library(paletteer)  # para paleta de colores

# ───────────────────────────────────────────────────────────────
# Carga de datos desde el repo de TidyTuesday
nsf_terminations <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv'
)

# ───────────────────────────────────────────────────────────────
# Exploracioon de datos
glimpse(nsf_terminations)
skim(nsf_terminations)

# ───────────────────────────────────────────────────────────────
# Tidy data

nsf_terminations_tidy <- nsf_terminations %>% 
  select(
    award_type, 
    directorate, 
    usaspending_obligated, 
    termination_letter_date,
    nsf_startdate,
    nsf_expected_end_date
  ) %>% 
  drop_na() %>%  # eliminar observaciones con valores faltantes
  
  # Calcular duración del proyecto hasta la fecha de cancelacion
  mutate(
    days_active = as.numeric(termination_letter_date - nsf_startdate),
    months_active = days_active / 30.44,
    years_active = months_active / 12
  ) %>% 
  
  # Calcular porcentaje completado respecto a duración original
  mutate(
    grant_duration_days = as.numeric(nsf_expected_end_date - nsf_startdate),
    pct_completed = days_active / grant_duration_days * 100
  ) %>% 
  
  # Corregir y traducir etiquetas del directorado
  mutate(
    directorate = str_remove_all(directorate, "\\\""),
    directorate = recode(directorate,
                         "Biological Sciences" = "Ciencias Biológicas",
                         "Computer and Information Science and Engineering" = "Ciencias de la Computación e Ingeniería",
                         "STEM Education" = "Educación STEM",
                         "Engineering" = "Ingeniería",
                         "Geosciences" = "Geociencias",
                         "Mathematical and Physical Sciences" = "Ciencias Matemáticas y Físicas",
                         "Social, Behavioral and Economic Sciences" = "Ciencias Sociales, del Comportamiento y Económicas",
                         "Office of the Director" = "Oficina del Director",
                         "Technology, Innovation and Partnerships" = "Tecnología, Inovación y Alianza"
    ),
    directorate = str_wrap(directorate, 30)  # envolver para etiquetas legibles
  ) %>% 
  
  # Convertir fondos en millones de USD
  mutate(spending_mlln = usaspending_obligated / 1e6) %>% 
  
  # Reordenar factor de directorado por suma total de fondos
  mutate(directorate = fct_reorder(directorate, spending_mlln, .fun = sum, .desc = FALSE))

# ───────────────────────────────────────────────────────────────
# Gráficos

bts <- 12                     # tamaño de fuente base
text_col <- "grey75"         # color de texto principal
mypal <- paletteer::paletteer_d("ltc::hat")  # paleta de colores

# ───────────────────────────────────────────────────────────────
# main plot

main_plot <- nsf_terminations_tidy %>%  
  ggplot() +
  geom_point(
    aes(
      x = pct_completed,
      y = directorate,
      size = spending_mlln,
      colour = directorate
    ),
    alpha = 0.6
  ) +
  scale_colour_manual(values = mypal, guide = "none") +
  labs(
    x = "% de avance al momento de la notificación",
    y = NULL,
    size = "Fondos comprometidos por proyecto (Millones USD)"
  ) +
  theme_minimal(base_size = bts) +
  coord_cartesian(clip = "off") +
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1)) +
  theme(
    plot.margin = margin(5, 5, 5, 5, "mm"),
    legend.position = "top",
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = text_col, lineheight = 1, hjust = 0.5),
    axis.text.x.top = element_text(margin = margin(0, 0, -20, 0, "mm")),
    axis.text.y = element_text(margin = margin(0, -5, 0, 0, "mm"))
  )

# ───────────────────────────────────────────────────────────────
# Cantidad de proyectos terminados

barplot <- nsf_terminations_tidy %>% 
  group_by(directorate) %>% 
  summarise(
    spending_sum = round(sum(spending_mlln), 0),
    count = n()
  ) %>% 
  ungroup() %>% 
  ggplot(aes(y = directorate, x = count, fill = directorate)) +
  geom_col() +
  geom_text(
    aes(label = count),
    hjust = -0.1,
    colour = text_col,
    size = bts / 3
  ) +
  labs(x = "Número de proyectos cancelados") +
  scale_colour_manual(values = mypal, guide = "none") +
  scale_fill_manual(values = mypal, guide = "none") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    plot.margin = margin(5, 5, 5, 5, "mm"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    text = element_text(color = text_col, lineheight = 1, hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )




# Gráfico de densidad 
density_plot <- nsf_terminations_tidy %>% 
  ggplot(aes(x = pct_completed)) +
  geom_density(aes(y = after_stat(density) * -1),
               fill = "grey75", alpha = 0.8, color = "gray55") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = bts) +
  scale_x_continuous(limits = c(0, 120)) +
  scale_y_continuous(labels = NULL) +  # oculta el eje invertido
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(0, 5, 5, 5, "mm"),
    text = element_text(color = text_col)
  )


density_plot

# ───────────────────────────────────────────────────────────────
# Grafico combinado


# Parte izquierda: main plot arriba y densidad invertida abajo
left_column <- main_plot / density_plot +
  plot_layout(heights = c(4, 1))

# Parte derecha: barplot arriba y un "espaciador" del mismo tamaño que la densidad
right_column <- barplot / plot_spacer() +
  plot_layout(heights = c(4, 1))  # debe coincidir con left_column

# Combinar ambas columnas
combined_plot <- left_column | right_column +
  plot_layout(widths = c(12, 1), guides = "collect")

# Añadir título y pie
combined_plot <- combined_plot +
  plot_annotation(
    title = "Cancelación Masiva de Proyectos NSF en 2025",
    subtitle ="En abril de 2025, una orden ejecutiva detuvo la entrega de fondos a cientos de proyectos científicos en curso, \nafectando el avance del conocimiento en múltiples disciplinas",
    caption = "Fuente: grant-watch.us / Visualización: @miketrippv"
  )


combined_plot

### guardar 
ggsave(filename = "plots/Trump_NSF.jpg", 
       width = 320, 
       height = 210, 
       units = "mm", 
       dpi = 300, 
       bg = "white")
