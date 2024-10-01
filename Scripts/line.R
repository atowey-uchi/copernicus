# PROJECT:  C:/Users/atowey/Documents/Github/copernicus
# PURPOSE:  
# AUTHOR:   A. Towey | USAID
# REF ID:   58b8e979 
# LICENSE:  MIT
# DATE:     2024-10-01
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
library(glamr)
library(glitr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(ggtext)  # Load the ggtext package
library(scales)  # If you are using the scales package for other formatting



# IMPORT ------------------------------------------------------------------

remotes::install_github("USAID-OHA-SI/mindthegap", "dev_edms")
library(mindthegap)
path <- "Data/DataList_10_1_2024-6_37_13-PM.csv"
df <- munge_edms(path, epi_95s_flag = FALSE)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "58b8e979"  #a reference to be places in viz captions 


# VIZ ---------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(ggtext)

  curve_viz <-df %>% 
  filter(age == "All", sex == "All",
         country == "Benin",
         indicator %in% c("Number PLHIV")) %>% 
  #select(-c(estimate_flag)) %>% 
  select(year, country, indicator, estimate, lower_bound, upper_bound) %>% 
  spread(indicator, estimate) %>% 
  janitor::clean_names() %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = number_plhiv), color = denim, size = 1) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),linetype = 2, alpha = 0.1) + 
  geom_point(data = . %>% filter(year == max(year)), 
             aes(y = number_plhiv, fill = denim), shape = 21, color = "white", size = 3)+
  geom_text(data = . %>% filter(year == max(year)), 
            aes(y = number_plhiv, color = denim, 
                label = paste0(round(number_plhiv/1000, digits = 3), "K")),
            hjust = -0.3, size = 12/.pt,
            family = "Source Sans Pro SemiBold") +
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(labels = ~((scales::label_number(scale_cut = scales::cut_short_scale()))(abs(.))), 
                     expand = c(0, 0)) + 
  scale_x_continuous(limits = c(1990, 2030), breaks = seq(1990, 2030, 5)) +
  geom_hline(yintercept = 0, color = grey80k) +
  si_style_ygrid(text_scale = 1.15) +
  labs(x = NULL, y = NULL,
       title = "dynamic") + #{paste(authors, collapse = '/')}
  coord_cartesian(expand = T, clip = "off") +
  theme(plot.title = element_markdown())
curve_viz


