# week 8. DuBois challenge 

library(tidyverse)
library(here)
library(showtext)



# 1. Get data -------------------------------------------------------------

freed_slaves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-16/freed_slaves.csv')

free_long <- freed_slaves %>% 
  pivot_longer(-Year, names_to = "Slave", values_to = "Prop") %>% 
  # Slave + Free should add up to 100 for all years
  rows_update(tibble(Year = 1800, Slave = "Slave", Prop = 89), by = c("Year", "Slave"))



title <- "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES . \n \nPROPORTION DES NÈGRES LIBRES ET DES ESCLAVES EN AMÉRIQUE ."
subtitle <- "DONE BY ATLANTA UNIVERSITY"
caption <- "Source: #DuBoisChallenge | Visualization: @MiguelTripp"

#colors
colors <- c("#00aa00", "#000000")

labels <- freed_slaves %>% 
  mutate(perc = paste0(Free, "%")) %>% 
  #add offset
  mutate(Slave_off = Slave + 3.75) %>% 
  mutate(Slave_off = ifelse(Free == 100, 91, Slave_off)) %>% 
  mutate(Segment_i = 100) %>% 
  mutate(Segment_e = ifelse(Year %in% c(1790, 1870), NA, Segment_i - Free))



ggplot(free_long, aes( x = Year, y = Prop, fill = Slave))+
  #Add area. Add show.lengend to remove legend
  geom_area(show.legend = FALSE)+
  #Add segments
  geom_segment(data = labels, 
               aes(x = Year, xend = Year,
                   y = 100, yend = Segment_e),
               inherit.aes = FALSE,
               color = "grey35",
               size = 0.3)+
  scale_fill_manual(values = colors)+
  #set NULL to remove the title 
  scale_x_continuous(position = "top", name = NULL,  breaks = seq(1790, 1870, 10))+
  #Set names and scale to NULL to remove it completely
  scale_y_continuous(expand = c(0, 0), labels = NULL, name = NULL) +
  #add Slaves label
  geom_text(aes(x = 1830, y = 55, label = "SLAVES\nESCLAVES"), size = 5, color = "white", fontface = "bold") +
  
  #add Free label
  geom_text(aes(x = 1830, y = 95, label = "FREE - LIBRE"), size = 5, col = colors[2], fontface = "bold")+
  #add perc labels
  geom_text(data = labels, 
            aes(x = Year, y = Slave_off, label =perc), inherit.aes = FALSE, fontface = "bold")+
  labs(title = title,
       subtitle = subtitle,
       caption = caption)+
  theme(
    plot.margin = margin(20, 80, 20, 80),
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0.5, margin = margin(t = 10, b = 30)),
    plot.subtitle = element_text(hjust = 0.5,size = 7, margin = margin(t = 10, b = 30)),
    plot.caption = element_text(hjust = 1, margin = margin(t = 10)),
    plot.background = element_rect(fill = "#fcf5eb", color = NULL),
    axis.text.x = element_text(size = 10, face = "bold"),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 
  
ggsave(here("plots", "Week8_DuBois.png"), width = 120, height = 90,units = "mm", dpi = 150, scale = 2.15)
