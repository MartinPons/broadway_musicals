
# Script for tidytuesday. Dataset from 2020-04-28. Broadway Musicals
# Data comes from PlayBill, extracted and cleaned by Alex Cookson
# Link to the tidytuesday dataset: https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-28/


# Author: Martín Pons



# INITTIAL SETTTING -------------------------------------------------------


# libraries
library(tidyverse)
library(lubridate)
library(forcats)
library(scales)
library(extrafont)
library(viridis)
library(here)


# helpler functions

# adjust_variable is used to deflate revenue variables, taking inflation into account
adjust_revenue_variable <- function(var, deflator) {
  
  # numeric, numeric -> numeric
  
  var / deflator * 100
  
}



# LOAD DATA ---------------------------------------------------------------

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')



# WRANGLING ----------------------------------------------------------------


## Deflated variables ##

# Adjustement of revenue related variables to take inflation into account

# creation of month and year variables in grosses
grosses <- grosses %>% 
  mutate(month = month(week_ending), 
         year = year(week_ending))

# creation of month and year variables in cpi
cpi <- cpi %>% 
  mutate(month = month(year_month), 
         year = year(year_month))

# table join
grosses <- grosses %>% 
  left_join(cpi %>% 
              select(-year_month), by = c("year", "month"))


# compute adusted varibles by cpi
grosses <- grosses %>% 
  mutate_at(.vars = vars(weekly_gross_overall, weekly_gross:top_ticket_price),
            .funs = list(adjusted = ~adjust_revenue_variable(., cpi)))



# ADDITIONAL FEATURE CREATION ---------------------------------------------

# obtaining top 10 shows by adjusted revenue
top_10_shows <- grosses %>% 
  group_by(show) %>% 
  summarise(weekly_gross = sum(weekly_gross)) %>%
  top_n(10, weekly_gross) %>% 
  .$show


# PREPARING DATA FOR VISUALIZATION: TOP 10 SHOWS AND QUANTILE GENERATION --



grosses_top_10 <- grosses %>% 
  
  # filtering top 10 selling shows
  filter(show %in% top_10_shows) %>% 
  
  # obtaining revenue quantile per show
  group_by(show) %>% 
  mutate(
    show_gross_quantile = ntile(weekly_gross_adjusted, 100))


# VISUALIZATION: REVENUE DISTRIBUTION THROUGH TIME ------------------------

# Note: you'll have to uncomment the two lines in the next paragraph if you use extrafont library 
# for the first time, in order for the code to detect the Georgia Font. please not that fonts are 
# loaded from Windows operating system

# font_import
# loadfonts(device = "win")

grosses_top_10 %>% 
  
  # plot base. Shows are ordered by revenue
  ggplot(aes(x = week_ending, y = fct_reorder(show, weekly_gross_adjusted, .fun = "sum"))) + 
  
  # point layer with "|" shape
  geom_point(aes(color = show_gross_quantile), shape = "|", size = 4) + 
  
  # color viridis scale
  scale_color_viridis(option = "E", begin = 0.2, end = 0.8) + 
  
  # formating x date axis
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  
  # adding show titles
  geom_text(
    
    # the geoms gets aggregated data: summarisation by mean date
    # so every show label is at the middle of the distribution
    data = grosses_top_10 %>% 
              group_by(show) %>% 
              summarise(week_ending = median(week_ending), 
                        weekly_gross_adjusted = sum(weekly_gross_adjusted)), 
    
    # Label is show title plus revenue earned in millions of dollars
            aes(label = paste(show, "(", dollar(weekly_gross_adjusted,
                                                scale = 1/1e6, 
                                                accuracy = 1, 
                                                suffix = " M"), ")")),
            nudge_y = 0.3, family = "Georgia", color = "grey45", size = 4.5) + 
  
  # title, color and captin labs
  labs(title = "Revenue distribution through time for the top 10 selling Broadway musicals (1985 - 2020)", 
       color = "Weekly revenue \npercentile", 
       caption = "Data comes from PlayBill, extracted and cleaned by Alex Cookson \nRevenue adjusted by monthly consumer price index. Base = 1985") +
  
  # plot theme
  theme_bw() +
  theme(text = element_text(family = "Georgia", color = "grey45")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.x = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color = "grey45"), 
        axis.ticks.x = element_line(color = "grey45"), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(size = 10),
        axis.text.x = element_text(size = 14), 
        plot.title = element_text(size = 16)) 

# uncomment and change path accordingly
# ggsave(here::here("final_figures", "revenue_distribution.png"), width = 16, height = 8)

