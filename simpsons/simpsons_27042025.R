# ANÁLISIS DE RATINGS DE LOS SIMPSON - INFOGRAFÍA
# ----------------------------------------------------------
# Este script genera una infografía comparando los ratings de IMDb
# por temporada y episodio de Los Simpson, mostrando:
#  Un gráfico de líneas con tendencia
# Un heatmap por temporada/episodio
# ----------------------------------------------------------




## 1.1 Carga de librerías ----
## --------------------------
library(here)           # Para manejo de rutas
library(tidyverse)      # Manipulación y visualización de datos (ggplot2, dplyr)
library(viridis)        # Escalas de color
library(ggimage)        # Integración de imágenes en gráficos
library(ggtext)         # Renderizado de texto mejorado
library(showtext)       # Uso de fuentes personalizadas
library(patchwork)      # Combinación de gráficos
library(magick)         # Manejo de imágenes (para logo)



## 1.2 Configuración de fuentes ---------------------------------
font_add_google("Rock Salt", family = "title_font")          # Fuente para títulos
font_add_google("Saira Extra Condensed", family = "caption_font") # Fuente para pies
font_add_google("Titillium Web", family = "body_font")       # Fuente principal
showtext_auto()  # Activar fuentes

## 1.3 Parámetros de diseño -------------------------------
# Colores
bg_col <- "#f0ee90"               # Color de fondo amarillo
azul_cielo <- "grey95"            # Color azul para áreas de gráfico
azul_transparente <- azul_cielo   # Versión transparente

# Tamaños
bts <- 50                         # Tamaño base de texto
lineheight <- 0.4                 # Espaciado entre líneas

# Textos
titulo <- "Mi Vieja Mula \nYa No Es lo Que Era"
subtitulo <- paste(
  "Las calificaciones de IMDb y el número de espectadores muestran una clara caída",
  "en la calidad percibida especialmente después de la temporada 10", sep = "\n"
)
caption <- "Data: Tod Schneider & Prashant Banerjee | Viz: @MiguelTripp"


## 2.1 Carga de datos -------------------------
episodes <- read_csv(here("simpsons/data/simpsons_episodes.csv"))

## 2.2 Limpieza y transformación -----------------------------------
episodes_tidy <- episodes %>%
  select(
    id = 1,               # ID del episodio
    rating = 3,           # Rating en IMDb
    number_series = 6,    # Número de episodio global
    number_season = 5,    # Número de episodio por temporada
    season = 10,          # Número de temporada
    views = 12,           # Número de espectadores (millones)
    title = 11,           # Título del episodio
    date = 7              # Fecha de emisión
  ) 

#  Preparar columnas como factores para ordenarlos en los gráficos
episodes_tidy_plot <- episodes_tidy %>%
  mutate(
    number_season = fct_rev(factor(number_season)),
    season = factor(season)
  )



## 2.3 Identificación de episodios destacados ------------------------------------------------
# Episodio mejor calificado
max_rate <- episodes_tidy %>%
  filter(rating == max(rating, na.rm = TRUE)) %>%
  slice(which.min(views)) %>%  # En caso de empate, el menos visto
  mutate(image = "simpsons/data/grimes_edit.png")

# Episodio peor calificado
min_rate <- episodes_tidy %>%
  filter(rating == min(rating, na.rm = TRUE)) %>%
  mutate(image = "simpsons/data/gaga_edit.png")





## 3.1 Gráfico de líneas (ratings por temporada) ---------------------------------------------------
line_plot <- ggplot(episodes_tidy, aes(x = season, y = rating)) +
  # Capas de datos
  geom_point(aes(size = views, fill = rating), shape = 21, color = "grey65") +
  geom_smooth(se = FALSE, color = "#966d15") +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey85") +
  
  # Anotaciones para episodios destacados
  geom_curve(
    data = max_rate, 
    aes(x = 5, y = 10, xend = season, yend = rating),
    curvature = 0.2, arrow = arrow(length = unit(0.03, "npc")), size = 0.5
  ) +
  geom_curve(
    data = min_rate, 
    aes(x = 24, y = 4, xend = season, yend = rating),
    curvature = 0.2, arrow = arrow(length = unit(0.03, "npc")), size = 0.5
  ) +
  geom_image(
    data = max_rate, 
    aes(x = 5, y = 10, image = image), size = 0.20, asp = 1
  ) +
  geom_image(
    data = min_rate, 
    aes(x = 25, y = 3.9, image = image), size = 0.20, asp = 1
  ) +
  geom_text(
    data = max_rate, 
    aes(x = 5, y = 10.5, label = title),
    size = 12, fontface = "italic", hjust = 0.5, vjust = 0
  ) +
  geom_text(
    data = min_rate, 
    aes(x = 25, y = 3, label = title),
    size = 12, fontface = "italic", hjust = 0.5, vjust = 1
  ) +
  
  # Escalas y ejes
  scale_fill_viridis(option = "cividis", direction = 1, guide = "none") +
  scale_x_continuous(limits = c(0.5, 28.5), expand = c(0, 0)) +
  
  # Etiquetas y tema
  labs(
    y = "Calificación de IMDb",
    size = "Espectadores en EEUU (mlls)"
  ) +
  theme_light(base_family = "body_font", base_size = bts) +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.title.y = element_text(color = "grey45"),
    axis.text.y = element_text(vjust = 0.5),
    plot.background = element_rect(fill = azul_transparente, color = NA),
    panel.background = element_rect(fill = azul_transparente),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5),
    legend.spacing.x = unit(0.5, 'cm'),
    legend.background = element_blank(),
    axis.title = element_text(margin = margin(t = 2, r = 2, b = -10, l = -10, unit = "pt")),
    axis.text = element_text(margin = margin(t = -5, r = -5, unit = "pt"))
  ) +
  guides(
    size = guide_legend(
      keywidth = unit(0.4, "cm"),
      title.position = "top",
      title.hjust = 0.5,
      label.position = "bottom",
      direction = "horizontal",
      nrow = 1
    )
  )

## 3.2 Heatmap (ratings por episodio/temporada) ------------------------------------------------
heatmap <- ggplot(episodes_tidy_plot, aes(x = season, y = number_season, fill = rating)) +
  # Capa de datos
  geom_tile(color = "white", size = 0.2) +
  
  # Escalas
  scale_fill_viridis(option = "cividis", direction = 1, na.value = "grey90") +
  scale_x_discrete(position = "top") +
  
  # Etiquetas y tema
  labs(
    x = "Temporada", 
    y = "Episodio", 
    fill = "Calificación de IMDb"
  ) +
  theme_light(base_family = "body_font", base_size = bts) +
  theme(
    axis.title.x = element_text(colour = "grey45", hjust = 0),
    axis.title.y = element_text(color = "grey45", hjust = 1),
    axis.text.x = element_text(face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(margin = margin(t = 2, r = 10, b = -10, l = -10, unit = "pt")),
    axis.text = element_text(margin = margin(t = -5, r = -10, unit = "pt")),
    plot.background = element_rect(fill = azul_transparente, color = NA),
    panel.background = element_rect(fill = azul_transparente),
    legend.position = "top",
    legend.title.align = 0.5,
    legend.background = element_blank()
  ) +
  guides(
    fill = guide_colorbar(
      keywidth = unit(5, "cm"),
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      barwidth = unit(10, "cm"),
      barheight = unit(0.5, "cm")
    )
  )


## 4.1 Combinación con patchwork -----------------------------------
combined_plot <- line_plot / plot_spacer() / heatmap + 
  plot_layout(
    heights = c(1, 0.01, 1.5),  # Ajuste de proporciones
    guides = "collect"           # Unificar leyendas
  ) & 
  theme(
    legend.position = "top",
    legend.text = element_text(
      color = "grey45",
      lineheight = lineheight,
      margin = margin(t = 0, b = 0, r = 2, l = 2)
    ),
    legend.title = element_text(
      size = bts * 0.5,
      lineheight = lineheight,
      margin = margin(b = 0)
    ),
    plot.margin = margin(t = 0, r = 20, b = 0, l = 20),
    plot.background = element_rect(fill = azul_transparente, color = NA),
    panel.background = element_rect(fill = azul_transparente),
    legend.box.background = element_blank()
  )

## 4.2 Añadir metadatos y estilo final ----------------------------------------
final_infographic <- combined_plot +
  plot_annotation(
    title = titulo,
    subtitle = subtitulo,
    caption = caption,
    theme = theme(
      plot.title = element_text(
        color = "grey20",
        family = "title_font", 
        size = bts * 2.5,
        hjust = 0.5,
        face = "bold",
        margin = margin(t = 10, b = 1),
        lineheight = lineheight
      ),
      plot.subtitle = element_text(
        color = "grey20",
        family = "body_font",
        size = bts * 0.95,
        hjust = 0.5,
        margin = margin(t = 15, b = 15, r = 30, l = 30),
        lineheight = lineheight
      ),
      plot.caption = element_text(
        family = "caption_font",
        size = bts * 0.6,
        hjust = 0.5,
        margin = margin(t = 0, b = 0, l = 5, r = 5)
      ),
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA)
    )
  )

## 4.3 Exportar infografía ----------------------------
ggsave(
  here("simpsons/simpsons_ratings_infographic.png"),
  plot = final_infographic,
  device = "png",
  width = 8,       # Ancho en pulgadas
  height = 14,      # Alto en pulgadas
  units = "in",
  dpi = 300,        # Resolución
  bg = bg_col       # Color de fondo
)
