# Script for the analysis of the Simpsons dataset

# Cargar librerías necesarias
library(here)
library(tidyverse)        # Incluye ggplot2, dplyr, readr, etc.
library(viridis)          # Paletas de colores
library(ggimage)          # Para agregar imágenes en ggplot
library(ggtext)           # Soporte para markdown en texto
library(showtext)         # Uso de fuentes personalizadas
library(patchwork)        # Combinar gráficos


# 🖋️ Cargar fuentes desde Google Fonts
font_add_google("Rock Salt", family = "title_font")         # Título
font_add_google("Saira Extra Condensed", family = "caption_font") # Pie de gráfico
font_add_google("Titillium Web", family = "body_font")      # Texto general
showtext_auto()


# parametros de color

bg_col <- "#f0ee90"
azul_cielo <- "grey95"
azul_transparente <- azul_cielo

# tamaño de texto base
bts <- 50 




#  Leer los datos de episodios
episodes <- read_csv(here("simpsons/data/simpsons_episodes.csv"))

# Seleccionar columnas informativas
episodes_tidy <- episodes %>%
  select(
    id = 1,
    rating = 3,
    number_series = 6,
    number_season = 5,
    season = 10,
    views = 12,
    title = 11,
    date = 7
  )

#  Preparar columnas como factores para ordenarlos en los gráficos
episodes_tidy_plot <- episodes_tidy %>%
  mutate(
    number_season = fct_rev(factor(number_season)),
    season = factor(season)
  )

#  identificar el episodio con mayor y menor rating
max_rate <- episodes_tidy %>%
  filter(rating == max(rating, na.rm = TRUE)) %>%
  slice(which.min(views)) %>%
  mutate(image = "simpsons/data/grimes_edit.png")

min_rate <- episodes_tidy %>%
  filter(rating == min(rating, na.rm = TRUE)) %>%
  mutate(image = "simpsons/data/gaga_edit.png")

#  Gráfico 1: Puntos con suavizado (lineplot)
line_plot <- ggplot(episodes_tidy, aes(x = season, y = rating)) +
  geom_point(aes(size = views, fill = rating), shape = 21, color = "grey65") +
  geom_smooth(se = FALSE, color = "#966d15") +

  #agregar flechas y anotaciones al peor y mejor episodio
  geom_curve(data = max_rate, 
             aes(x = 5, y = 10, xend = season, yend = rating),
             curvature = 0.2, arrow = arrow(length = unit(0.03, "npc")), size = 0.5) +
  geom_curve(data = min_rate, 
             aes(x = 24, y = 4, xend = season, yend = rating),
             curvature = 0.2, arrow = arrow(length = unit(0.03, "npc")), size = 0.5) +
  geom_image(data = max_rate, 
             aes(x = 5, y = 10, image = image), size = 0.20, asp = 1) +
  geom_image(data = min_rate, 
             aes(x = 25, y = 3.9, image = image), size = 0.20, asp = 1) +
  geom_text(data = max_rate, 
            aes(x = 5, y = 10.5, label = title),
            size = 12, fontface = "italic", hjust = 0.5, vjust = 0) +
  geom_text(data = min_rate, 
            aes(x = 25, y = 3, label = title),
            size = 12, fontface = "italic", hjust = 0.5, vjust = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "grey85")+
  
  # paleta de colores
  scale_fill_viridis(option = "cividis", direction = 1, guide = "none") +
  
  #modificar la escala
  scale_x_continuous(limits = c(0.5, 28.5), expand = c(0, 0)) +
  
  #añadir etiqueda
  labs(y = "Calificación de IMDb",
       size = "Espectadores en EEUU (mlls)",
       fill = "Calificación de IMBd")+
  theme_light(base_family = "body_font",
              base_size = bts) +
  theme(legend.position = "right",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.title.y = element_text(color = "grey45"),
        axis.text.y = element_text(vjust = 0.5),
        plot.background = element_rect(fill = azul_transparente, color = NA),
        panel.background = element_rect(fill = azul_transparente)
        )


#line_plot


line_plot <- line_plot +
  guides(
    size = guide_legend(
      keywidth = unit(0.4, "cm"),
      title.position = "top",    # Título arriba
      title.hjust = 0.5,         # Centrar título
      label.position = "bottom", # Etiquetas abajo
      direction = "horizontal",  # Disposición horizontal
      nrow = 1                   # Una sola fila
    )
  ) +
  theme(
    legend.box = "horizontal",   # Caja de leyenda horizontal
    legend.position = "bottom",  # Posición general abajo
    legend.title = element_text(hjust = 0.5),    # Alineación central del título
    legend.spacing.x = unit(0.5, 'cm'),  # Espaciado entre items
    legend.background = element_blank(),
    axis.title = element_text(margin = margin(t = 2, r = 2, b = -10, l = -10, unit = "pt")),
    axis.text = element_text(margin = margin(t = -5, r = -5, unit = "pt"))
  )




# ️ Gráfico 2: Heatmap de rating por episodio
heatmap <- ggplot(episodes_tidy_plot, aes(x = season, y = number_season, fill = rating)) +
  geom_tile(color = "white", size = 0.2) +
  scale_fill_viridis(option = "cividis", direction = 1, na.value = "grey90") +
  scale_x_discrete(position = "top") +
  labs(x = "Temporada", y = "Episodio", fill = "Calificación de IMDb") +
  theme_light(base_family = "body_font",
              base_size = bts) +
  theme(
    axis.title.x = element_text(colour = "grey45", hjust = 0),
    axis.title.y = element_text(color = "grey45", hjust = 1),
    axis.text.x = element_text(face = "bold"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "right",
    axis.title = element_text(margin = margin(t = 2, r = 10, b = -10, l = -10, unit = "pt")),
    axis.text = element_text(margin = margin(t = -5, r = -10, unit = "pt")),
    plot.background = element_rect(fill = azul_transparente, color = NA),
    panel.background = element_rect(fill = azul_transparente)
  )


#heatmap


heatmap <- heatmap +
  guides(
    fill = guide_colorbar(
      keywidth = unit(5, "cm"),
      title.position = "top",    # Título arriba
      title.hjust = 0.5,         # Centrar título
      direction = "horizontal",  # Barra horizontal
      barwidth = unit(10, "cm"), # Ancho de la barra
      barheight = unit(0.5, "cm") # Alto de la barra
    )
  ) +
  theme(
    legend.position = "bottom",  # Posición general abajo
    legend.title.align = 0.5,      # Alineación central del título
    legend.background = element_blank()
  )



titulo = "Mi Vieja Mula \nYa No Es lo Que Era"
subtitulo = "Las calificaciones de IMDb y el número de espectadores muestran una clara caída \nen la calidad percibida especialmente después de la temporada 10"
caption = "Data: Tod Schneider & Prashant Banerjee | Viz: @MiguelTripp"
lineheight = 0.4


# Combinar los gráficos con patchwork
combined_plot <- line_plot / plot_spacer() / heatmap + 
  plot_layout(heights = c(1, 0.01, 1.5),
              guides = "collect") &  # Ajustar proporciones
  theme(legend.position = "top",
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
        plot.background = element_rect(fill = azul_transparente, color =NA),
        panel.background = element_rect(fill = azul_transparente),
        legend.box.background = element_blank()
        )


# Añadir título y detalles generales
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




# Exportar como PNG
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

# Mostrar el resultado
#final_infographic






