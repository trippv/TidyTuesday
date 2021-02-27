# Week 9: Employed status

library(here)
library(tidytuesdayR)
library(tidyverse)
library(showtext)


font_add_google("Indie Flower", "Indie")

# open showtext
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

fontfam <- "Indie"


# 1. Get data -------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 9)


earn <- tuesdata$earn



# 2. Data transform ----------------------------------------------------------------

earn_df <- earn %>% 
  #filter only the 25 to 54 year age group
  filter(age == "25 to 54 years") %>% 
  
  #remove the All origins group
  filter(race != "All Races") %>% 
  
  #remove the both sexes
  filter(sex != "Both Sexes") %>% 
  
  select(race, sex, year, median_weekly_earn) %>% 

  #get year average of weekly earn
  group_by(year, race, sex) %>% 
  summarise(year_avg = mean(median_weekly_earn, na.rm = TRUE )) %>% 
  
  ungroup() %>% 
  
  group_by(year, race) %>% 
  mutate(ribbon_max = (year_avg - lag(year_avg)) * -1) %>% 
  mutate(race = factor(race, level = c("Black or African American",
                                       "White",
                                       "Asian")) )




fill_color <- viridis::cividis(2)

text_labels <- data.frame(
  race = factor(unique(earn_df$race), level = c("Black or African American",
                               "White",
                               "Asian")),
  position = c(1510, 1010, 1210)
)


# Labels
title <- "GENDER GAP BY RACE IN THE U.S."

subtitle <- "Mean weekly earnings in the US for people ages 25 and over"

caption <- "Source: U.S. BEREAU OF LABOR STATISTICS| Visualization: @MiguelTripp"



e_plot <- ggplot(data = earn_df, aes( x = year, y = year_avg, col = sex, group = sex))+
  geom_line(size = 3)+
  
  geom_ribbon(data = earn_df %>% 
              filter(sex == "Women"), aes( x = year, ymin = year_avg, ymax = year_avg + ribbon_max),
              inherit.aes = FALSE, fill = "grey85", alpha = 0.4, show.legend = FALSE)+
  
  scale_color_manual(values = fill_color)+
  
  scale_x_continuous(breaks = seq(2010, 2020, 2), 
                     labels = c("2010", "'12","'14", "'16", "'18", "'20"))+
  
  scale_y_continuous(breaks = seq(0, 1600, 200), 
                     labels = scales::dollar_format(scale = 1), name = NULL)+
                     
  #Add unnecesary text
  geom_text(data = text_labels, aes(x = 2014, y = position, label = race),
            inherit.aes = FALSE,
            fontface = "bold", family = fontfam, size = 6 )+
  
  facet_wrap(~race) +
  
  labs(x = "Year", 
       title = title, 
       subtitle = subtitle,
       caption = caption)+
  
  theme(text = element_text(),
        panel.background = element_rect(fill = "white",
                                        colour = NULL),
        panel.grid.major.y  = element_line(size = 1, colour = "grey95", linetype = "dashed"),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(family = fontfam, size = 12),
        plot.caption = element_text(hjust = 1, face = "italic", colour= "darkgrey"),
        plot.title=element_text(hjust = .5, face="bold", size = 22, family = fontfam),
        plot.subtitle = element_text(hjust=.5, size = 18),
        axis.text.x = element_text(color="darkgrey", size = 12),
        axis.text.y = element_text(hjust = 1, vjust = -1),
        strip.text = element_blank()
        )

#save plot
ggsave(here("plots", "Week9_Earnings.png"),plot = e_plot, width=12, height=8)


