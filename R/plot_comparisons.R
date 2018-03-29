##   Project: PROJECT WARM HEART
##   Author:  A.Chafetz, USAID
##   Purpose: visualize
##   Date:    2018.03.29
##   Updated: 

# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)
library(scales)


# Load data ---------------------------------------------------------------

df_mwi <- read_csv(here("Output", "mwi-keyind-mer_ea.csv"), 
                   col_types = cols(mechanismid = "c"))
  
  
# Define Focus Mechanism --------------------------------------------------

mech <- 



# Achievement Definitions -------------------------------------------------

  #quarterile cutoffs
    qtl_1  <- .75
    qtl_2  <- .9
    qtl_4  <- 1.1
    
  #quartile colors
    orange <- "#ff6600"
    yellow <- "#ffcc00"
    green  <- "#70ad47"
    blue   <- "#6699ff"
    
    ach_fill <- c(orange, yellow, green, blue)
    
  df_mwi <- df_mwi %>% 
    mutate(ach_quartile = case_when(
      achievement > qtl_4 ~ 4,
      achievement > qtl_2 ~ 3,
      achievement < qtl_1 ~ 1,
      TRUE                ~ 2
    ))
    
    
    
# Partner Achievement -----------------------------------------------------

  df_mwi %>% 
    filter(mechanismid %in% mech) %>% 
    ggplot(aes(reorder(indicator, desc(indicator)), achievement)) +
      geom_col(aes(fill = ach_quartile), show.legend = FALSE) + 
      geom_text(aes(label = paste0(round(achievement*100,0), "%")), color = "white", hjust = 2, fontface = "bold") +
      coord_flip() +
      scale_y_continuous(name="Achievement (APR/Target)", labels = percent) +
      labs(x = "") +
      theme(axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank(),
            axis.text.x=element_blank())

# Partner Results ---------------------------------------------------------
  
  df_mwi %>% 
    filter(mechanismid %in% mech) %>% 
    ggplot(aes(reorder(indicator, desc(indicator)), fy2017apr)) +
    geom_col() + 
    coord_flip() +
    scale_y_continuous(name="FY17 APR", labels = comma) +
    labs(x = "") +
    theme(axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank())
  


# MWI Achievenment Comparison ---------------------------------------------
  
  
  df_mwi %>% 
    filter(indicator == "HTS_TST") %>% 
    ggplot(aes(reorder(mechanismid, achievement), achievement)) +
      geom_col() +
      coord_flip() +  
      scale_y_continuous(name="Achievement (APR/Target)", labels = percent) +
      labs(x = "") +
      theme(axis.ticks.y=element_blank(),
            axis.ticks.x=element_blank())
    
    
  df_mwi %>% 
    ggplot(aes(achievement)) +
      geom_histogram() +
      scale_x_continuous(name="Achievement (APR/Target)", labels = percent) +
      facet_grid(. ~ indicator)
  
  df_mer %>% 
    ggplot(aes(achievement)) +
    geom_histogram() +
      scale_x_continuous(name="Achievement (APR/Target)", labels = percent) +
      facet_grid(. ~ indicator)
  
  df_mer  %>% 
    ggplot(aes( achievement, reorder(indicator, desc(indicator)))) +
      geom_boxplot() +
      scale_x_continuous(name="Achievement (APR/Target)", labels = percent)
  

# MWI Results Comparison --------------------------------------------------

  
  df_mwi %>% 
    ggplot(aes(fy2017apr)) +
    geom_histogram() +
      scale_x_continuous(name="Achievement (APR/Target)", labels = comma) +
      facet_grid(. ~ indicator)
  