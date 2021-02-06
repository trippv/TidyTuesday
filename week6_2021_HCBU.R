
#Tidytuesday
# week 4, 2021
#Miguel Tripp 
#HCBU college enrollment


library(tidyverse)
library(patchwork)
library(scales)



# 1. Read data ------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 6)

hbcu_all <- tuesdata$hbcu_all


Public_dataset <- hbcu_all %>% 
  select(Year, contains("public")) %>% 
  rename(total_pub = "Total - Public",
         year4_pub = "4-year - Public",
         year2_pub = "2-year - Public") %>% 
  pivot_longer(-Year, names_to = "Category", values_to = "Value") %>% 
  filter(!str_detect(Category, "total"))




Private_dataset <- hbcu_all %>% 
  select(Year, contains("private")) %>% 
  rename(total_pub = "Total - Private",
         year4_pub = "4-year - Private",
         year2_pub = "2-year - Private") %>% 
  pivot_longer(-Year, names_to = "Category", values_to = "Value") %>% 
  filter(!str_detect(Category, "total"))






#define background color
bck_clr <- "#ADD8E6"

colors <- viridis::viridis(2)

#uptade plots panel
theme_update(
  legend.backgroun = element_blank(),
  plot.background = element_rect(fill = bck_clr, color = NA),
  panel.background = element_blank(),
  panel.grid = element_blank())




# Use function to change direction of axis.
opposite <- scales::trans_new(
  "opposite",
  transform = function(x) -x,
  inverse = function(x) -x
)

#function to change scale lables to thousands
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }


#plot for public df
Public_plot <- ggplot(Public_dataset, aes(x = Year, y = Value, fill = Category)) +
  geom_col()+
  coord_flip()+
  geom_text(aes(x = 1972, y = 200000, label = "Public"))+
  scale_x_continuous(trans = opposite,
                     labels = seq(from = 1976, to = 2016, by = 4), 
                     breaks = seq(from = 1976, to = 2016, by = 4))+
  scale_y_continuous(limits = c(0,250000), labels = ks)+
  scale_fill_manual(labels  = c("2-year", "4-year"), values = colors)+
  guides(fill = guide_legend(title.position = "top"))+
  labs(x = "", fill = "Enrollment program", y = "Number of stundents")+
  theme(axis.text.y = element_text(hjust = 0.35, size = 14, face = "bold"),
        axis.title.y = element_blank(),
        legend.position = "none")




#plot for private df
Private_plot <- ggplot(Private_dataset, aes(x = Year, y = Value, fill = Category)) +
  geom_col()+
  coord_flip()+
  scale_y_continuous(limits = c(250000,0), trans = opposite, labels = ks)+
  geom_text(aes(x = 1972, y = 200000, label = "Private"))+
  scale_fill_manual(labels  = c("2-year", "4-year"), values = colors)+
  labs(x = "", fill = "Enrollment program", y = "Number of students")+
  guides(fill = guide_legend(title.position = "top"))+
  scale_x_continuous(trans = opposite,
                     labels = seq(from = 1976, to = 2016, by = 4), 
                     breaks = seq(from = 1976, to = 2016, by = 4),
                     sec.axis = dup_axis())+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y.left   = element_blank(),
        legend.position = c(0.25, 0.75))


Private_plot + Public_plot  +
  plot_annotation(
    title = "Number of enrolled students at HBCUs over time",
    subtitle = "Enrollment in two year or four year programs in public versus private schools",
    caption = "Source: HBCU | Visualization: @MiguelTripp",
    theme = theme(
      plot.title = element_text(size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, lineheight = 1.1),
      plot.caption = element_text(size = 10, color = "grey30", margin = margin(15,0,0,0))      
    )
  )

ggsave(filename = "Week6_HBCU.png", width = 90, height = 120, units = "mm", dpi = 300, scale = 1.75)
