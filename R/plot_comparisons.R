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


# Define Focus Mechanism --------------------------------------------------

mech <- 

# Partner Achievement -----------------------------------------------------

df_mwi %>% 
  filter(mechanismid %in% mech) %>% 
  ggplot(aes(reorder(indicator, desc(indicator)), achievement)) +
    geom_col() + 
    geom_text(aes(label = paste0(round(achievement*100,0), "%")), color = "white", hjust = 2, fontface = "bold") +
    coord_flip() +
    scale_y_continuous(name="Achievement (APR/Target)", labels = percent) +
    labs(x = "")
  
    