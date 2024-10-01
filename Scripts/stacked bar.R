# PROJECT:  C:/Users/atowey/Documents/Github/copernicus
# PURPOSE:  
# AUTHOR:   A. Towey | USAID
# REF ID:   26606de7 
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
# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "26606de7"  #a reference to be places in viz captions 
  
  
  
# IMPORT ------------------------------------------------------------------
  
  remotes::install_github("USAID-OHA-SI/mindthegap", "dev_edms")
  library(mindthegap)
  path <- "Data/DataList_10_1_2024-6_37_13-PM.csv"
  df <- munge_edms(path, epi_95s_flag = FALSE)
  

# MUNGE / VIZ -------------------------------------------------------------------

  create_hiv_plot <- function(df, country_name, indicator_name, year_name) {
    print('starting!')
    plot <- df %>% 
      filter(
        country == country_name,
        sex != "All",  # Using != for clarity
        age != "All",
        indicator %in% c(indicator_name),
        year == year_name
      ) %>% 
      select(year, country, sex, age, indicator, estimate)%>% 
      pivot_wider(names_from = indicator, values_from = estimate) %>%
      rename(n = `Number PLHIV`) %>%
      mutate(fill_color = ifelse(sex == "Male", genoa, moody_blue)) %>% 
      ggplot(aes(x = age, y = ifelse(sex == "Male", -n, n), fill = fill_color), na.rm = TRUE) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = ifelse(n >= 1000, paste0(round(n / 1000, 1), "K"), n), hjust = ifelse(sex == "Male", 1.2, -0.4)),  # Format labels in K
                family = "Source Sans Pro", size = 4, color = "#505050") +
      coord_flip() + 
      scale_fill_identity() +
      si_style_nolines() +
      labs(x = NULL, y = NULL,
           title = "The participant cohort (n = 6,376) comprises of more <span style = 'color:#8980cb'>women</span> than  <span style = 'color:#287c6f'>men</span> and mostly patients over the age of 50",
           subtitle = "Uganda COP22 Patient Preference Analysis | USAID",
           caption = "Source: UNAIDS") + 
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            axis.text.x = element_blank(),
            plot.title = element_markdown()
      )
    print(plot)
  }
  
  create_hiv_plot(df, "Zambia", "Number PLHIV", 2024)

# VIZ ---------------------------------------------------------------------


