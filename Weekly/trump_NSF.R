


pacman::p_load(
  tidyverse,            # All things tidy
  
  scales,               # Nice Scales for ggplot2
  fontawesome,          # Icons display in ggplot2
  ggtext,               # Markdown text support for ggplot2
  showtext,             # Display fonts in ggplot2
  colorspace,           # Lighten and Darken colours
  
  magick,               # Download images and edit them
  ggimage,              # Display images in ggplot2
  patchwork,            # Composing Plots
  vayr,                 # visualize as you randomize
  packcircles           # Circles Packed layout
)

library(skimr)
library(summarytools)

# Cargar datos de repo TidyTuesday
nsf_terminations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-06/nsf_terminations.csv')

# Vistaso a los datos
glimpse(nsf_terminations)
skim(nsf_terminations)



########################################################
 
## tidydata
# niveles de directorate para ordenar los factore

nsf_terminations_tidy <- nsf_terminations %>% 
  select(award_type, 
         directorate, 
         usaspending_obligated, 
         termination_letter_date,
         nsf_startdate,
         nsf_expected_end_date
         ) %>% 
  #eliminar na
  drop_na() %>% 
  #Calcular el tiempo activo hasta carta de termino
  mutate(
    days_active = as.numeric(termination_letter_date - nsf_startdate),
    months_active = days_active / 30.44,
    years_active = months_active / 12,
  ) %>% 
  # calcular el porcentaje completado 
  mutate(
  grant_duration_days = as.numeric(nsf_expected_end_date - nsf_startdate),
  pct_completed = days_active / grant_duration_days * 100) %>% 
  #corregir etiquetas de directorate
  mutate(directorate = str_remove_all(directorate, "\\\"")) %>% 
  mutate(directorate = recode(directorate,
                              "Biological Sciences" = "Ciencias Biológicas",
                              "Computer and Information Science and Engineering" = "Ciencias de la Computación e Ingeniería",
                              "STEM Education" = "Educación STEM",
                              "Engineering" = "Ingeniería",
                              "Geosciences" = "Geociencias",
                              "Mathematical and Physical Sciences" = "Ciencias Matemáticas y Físicas",
                              "Social, Behavioral and Economic Sciences" = "Ciencias Sociales, del Comportamiento y Económicas",
                              "Office of the Director" = "Oficina del Director",
                              "Technology, Innovation and Partnerships" = "Tecnología, Inovación y Alianza"
  )) %>% 
  mutate(directorate = str_wrap(directorate, 30)) %>% 
  #convertir usa spending en millones
  
  mutate(spending_mlln = usaspending_obligated / 1000000) %>% 
  mutate(directorate = fct_reorder(directorate, 
                                   spending_mlln, 
                                   .fun = sum, 
                                   .desc = FALSE))




#nsf_terminations_tidy <- nsf_terminations_tidy %>% 
 # mutate(directorate = fct(directorate, levels = levels_directorate))



ggplot(nsf_terminations_tidy, aes(x = pct_completed, 
                                  y = directorate, 
                                  size = spending_mlln, 
                                  color = directorate))+
  geom_point(alpha = 0.4)


ggplot(nsf_terminations_tidy, aes(y = directorate ))+
         geom_bar()



#########################

bts <- 12
text_col <- "grey75"
mypal <- paletteer::paletteer_d("ltc::hat")

main_plot <- nsf_terminations_tidy %>%  
  ggplot() +
  geom_point(
    mapping = aes(
      x = pct_completed,
      y = directorate,
      size = spending_mlln,
      colour = directorate
    ),
    alpha = 0.6
  )+
  scale_colour_manual(values = mypal, guide = "none") +
  theme_minimal(
    base_size = bts
  ) +
  guides(size = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1))+
  coord_cartesian(clip = "off") +
  labs(
    x = "% de avance al momento de la notificación", y = NULL,
    size = "Fondos comprometidos por proyecto (Millon USD) "
  ) +
  theme(
    
    # Overall
    plot.margin = margin(5,5,5,5, "mm"),
    legend.position = "top",
    plot.title.position = "plot",
    panel.background = element_rect(
      fill = NA, colour = NA
    ),
    panel.grid = element_blank(),
    text = element_text(
      colour = text_col,
      lineheight = 1,
      hjust = 0.5
    ),
    axis.text.x.top = element_text(
      margin = margin(0,0,-20,0, "mm")
    ),
    axis.text.y = element_text(
      margin = margin(0,-5,0,0, "mm")
    )
  )

main_plot


#### columns
barplot <- nsf_terminations_tidy %>% 
  group_by(directorate) %>% 
  summarise(spending_sum = round(sum(spending_mlln),0),
            count = n()) %>% 
  ungroup() %>% 
  ggplot(aes(y = directorate, 
             x = count, 
             fill = directorate))+
  geom_col() +
  geom_text(
    mapping = aes(
      label = number(
        count, 
        scale_cut = cut_short_scale(),
        accuracy = 1
      )
    ),
    hjust = -0.1,
    colour = text_col,
    size = bts / 3
  ) +
  labs(x = "Número de proyectos cancelados")+
  scale_colour_manual(values = mypal, guide = "none") +
  scale_fill_manual(values = mypal, guide = "none") +
  coord_cartesian(clip = "off")+
  theme_minimal()+
  theme(
    
    # Overall
    plot.margin = margin(5,5,5,5, "mm"),
    panel.background = element_rect(
      fill = NA, colour = NA
    ),
    panel.grid = element_blank(),
    text = element_text(
      colour = text_col,
      lineheight = 1,
      hjust = 0.5
    ),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )


############# combinar grafico

combined_plot <- main_plot | barplot +
  plot_layout(widths = c(2, 1), guides = "collect")  

combined_plot <- combined_plot + plot_annotation(
  title = "Cancelación Masiva de Proyectos NSF en 2025",
  caption = "Data: grant-watch.us"
  )

combined_plot


### guardar 
ggsave(filename = "plots/Trump_NSF.jpg", 
       width = 300, 
       height = 160, 
       units = "mm", 
       dpi = 300, 
       bg = "white")
