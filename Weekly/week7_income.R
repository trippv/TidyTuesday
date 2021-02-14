library(tidyverse)
library(here)
library(skimr) # view data summary
library(ggrepel)


# Read data ---------------------------------------------------------------

income_distribution <- readr::read_csv(
  'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/income_distribution.csv')

#categories



skim(income_distribution)



### version 2

set.seed(2)

income_df  <- income_distribution %>% 
  filter(race != "All Races") %>%
  #filter(!str_detect(race, "Combination")) %>% 
  filter(str_detect(income_bracket, "over")) %>% 
  filter(year == 2019) %>% 
  mutate(income_th = income_mean / 1000) %>% 
  mutate(income_th_moe = income_mean_moe / 1000) %>% 
  mutate(number_mll = number / 1000000) %>% 
  mutate(loc = rnorm(7, 5, 2))
#add dummy year



ssystem <- data.frame(x = rep(4,5), y = seq(0, 160, 40), label = c(" ", "40 K", "80 K", "120 K", "160 K"))


colors <- RColorBrewer::brewer.pal(n = 7, "Dark2")


ggplot(income_df,
       aes(x = loc, y = income_th, size = number_mll, color = race))+
  
  geom_point() +
  
  geom_segment(aes(x= loc,
                   xend = loc, 
                   y = income_th - income_th_moe,
                   yend = income_th + income_th_moe), size = 1) +
  
    scale_y_continuous(breaks = c(0,40, 80, 120, 160),limits = c(0, 165))+
  
  geom_label_repel(data = income_df, aes(x = loc, y = income_th, label = race, color = race), 
                   box.padding = 1,
                   inherit.aes = FALSE)+
  
  geom_text(data = ssystem, aes(x = x, y = y+5 , label = label),
            angle= 275, 
            size = 4,
            color = "grey95",
            inherit.aes = FALSE)+
  
  coord_polar()+
  
  scale_color_manual(values = colors)+
  
  labs(size = "Number of \nhouseholds \nin millons", y = "", x = "",
       title = "A UNIVERSE APART!",
       subtitle = "The mean income (in thousands USD) and number of \nhousehold for year 2019 at the highest income \nbracket (>$200,000) demonstrates the wealth \ninequality by race",
       caption = "Data: Urban Institute & U.S. Census | Viz: @MiguelTripp")+
  
  guides(label = FALSE, color = FALSE)+
  
  theme_minimal()+
  
  theme(
        text = element_text(size = 14, colour = "grey95"),
        plot.title = element_text(size = 26, hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0, margin  = margin(10,0,0,0, unit = "mm")),
        plot.caption = element_text(hjust = 0.5),
        
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        legend.position = "right",
        
        legend.background = element_blank(),
        plot.background = element_rect(fill = "#30475e"),
        panel.background = element_rect(fill = "#30475e", color = NA))

ggsave(here("plots", "Week7_IncomeInequal.png"), width = 180, height = 240, units = "mm", dpi = 150)




