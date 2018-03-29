##   Project: PROJECT WARM HEART
##   Author:  A.Chafetz, USAID
##   Purpose: mechanism results/achievement comparison - compile data
##   Date:    2018.03.29
##   Updated: 

# Dependencies ------------------------------------------------------------

library(here)
library(tidyverse)

# Import ------------------------------------------------------------------

  #location of the Fact View Dataset
    fvdata <-   "~/ICPI/Data"
  #import MER data
    df_mer <- read_rds(Sys.glob(file.path(fvdata, "ICPI_FactView_OU_IM_*.Rds")))
  
  #save just raw data from EA Data Nav (MWI)
    # readxl::read_excel("~/Expenditure Analysis (EA)/Malawi 2017/2017 EA Data Nav Tool 1.12.2018-Malawi.xlsx", sheet = "Totals_MCCNav") %>% 
    #  write_csv( here("RawData", "MWI_EA_2017.csv"), na = "")
  #import EA data
    df_ea_mwi <- read_csv(here("RawData", "MWI_EA_2017.csv"))
  

# Munge -------------------------------------------------------------------

  #indicators to compare
    indlist <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR")
  #limit to just indicators of interest
    df_mer <- df_mer %>% 
      filter(indicator %in% indlist, standardizeddisaggregate == "Total Numerator", !mechanismid %in% c("00000", "00001"))
  
  #NET NEW
    source(here("R", "netnew.R"))
    df_mer <- netnew(df_mer)
    
  #subset and aggregate to variable of interst
    df_mer <- df_mer %>% 
      filter(indicator != "TX_CURR") %>% 
      group_by(operatingunit, mechanismid, primepartner, implementingmechanismname, fundingagency,indicator) %>% 
      summarise_at(vars(fy2017apr, fy2017_targets), ~ sum(., na.rm = TRUE)) %>% 
      ungroup()
    
  #add offical names onto dataset
    source(here("R", "officialnames.R"))
    df_mer <- officialnames(df_mer, here("RawData"), 2016)

  #add achievement variable
    df_mer <- df_mer %>% 
      filter(fy2017_targets != 0) %>% #remove any rows where no targets were set for that mechanism
      mutate(achievement = round(fy2017apr / fy2017_targets , 3))
    
# Munge EA Data -----------------------------------------------------------
    
  #subset EA data to HTS/TX expenditures, grouping by above site vs site
    df_ea_mwi_clean <- df_ea_mwi %>%
      filter(type == "Expenditure", data_type == "DIRECT", program_area %in% c("HTC", "FBCTS", "CBCTS")) %>% 
      select(mechanismid, sub_type, program_area, fy17) %>% 
      mutate(major_cc = ifelse(sub_type == "Above Site Total", "exp_above_site", "exp_site"),
             indicator = ifelse(program_area == "HTC", "HTS_TST", "TX_NEW"),
             fy17 = as.double(fy17),
             fy17 = round(fy17, 2),
             mechanismid = as.character(mechanismid)) %>% 
   #for each mech x indicator, aggregate up to site/above site expenses 
      group_by(mechanismid, indicator, major_cc) %>% 
      summarise(fy17 = sum(fy17, na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(fy17 != 0, mechanismid!="0")
    
  #create a total expense category
    df_ea_mwi_tot <- df_ea_mwi_clean %>% 
      group_by(mechanismid, indicator) %>% 
      summarise(exp_total = sum(fy17)) %>% 
      ungroup()
    
 #reshape wide so site/above site are their own colu,ns
    df_ea_mwi_clean <- df_ea_mwi_clean %>% 
      spread(major_cc, fy17)
    
  #merge on total      
    df_ea_mwi <- left_join(df_ea_mwi_clean, df_ea_mwi_tot, by = c("mechanismid", "indicator"))
      rm(df_ea_mwi_tot)


# MWI data frame ----------------------------------------------------------

    
    df_mwi <- df_mer %>% 
        filter(operatingunit == "Malawi")
  
    df_mwi <- left_join(df_mwi, df_ea_mwi, by = c("mechanismid", "indicator"))
      