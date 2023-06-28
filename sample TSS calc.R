library(tidyverse)
library(timetk)

# https://www.trainingpeaks.com/coach-blog/normalized-power-how-coaches-use/

gxt_summary <- readRDS("gxt_summary.rds")
joined_df <- readRDS("joined_df.rds")


# view data ---------------------------------------------------------------
joined_df %>% 
  plot_time_series(time, load, .smooth = F)

# calculate TSS -----------------------------------------------------------


time_calc <- joined_df %>% select(time, load) %>% 
    filter(load >0) 
  
load_df <- joined_df %>% select(time, load) %>% 
    filter(load >0) %>% 
    mutate(load_roll_30 = timetk::slidify_vec(load, .f = ~ mean(.x, na.rm = T), .period = 30, align = "right",
                                         .partial = T)) %>% 
    mutate(load_4 = load_roll_30^4)
  
NP <- mean(load_df$load_4, na.rm = T)^(1/4)
  
FTP <- gxt_summary$vt2_power
  
IF <- NP/ FTP
  
seconds <- max(time_calc$time) - min(time_calc$time)
  
tss_power <- ((seconds * NP * IF)/(FTP * 3600) * 100) %>% round(.,1) 
  
tss_power


# calculate TSS hr --------------------------------------------------------

time_calc_hr <- joined_df %>% select(time, load) %>% 
  filter(load >0) 

hr_df <- joined_df %>% select(time, hr, load) %>% 
  filter(load >0) %>% 
  mutate(
    hr_roll_30 = timetk::slidify_vec(hr, .f = ~ mean(.x, na.rm = T), .period = 30, align = "right",
                                     .partial = T)) %>% 
  mutate(hr_4 = hr_roll_30^4)

Nhr <- mean(hr_df$hr_4, na.rm = T)^(1/4)

FThr <- gxt_summary$vt2_hr

IF_hr <- Nhr/ FThr

seconds_hr <- max(time_calc_hr$time) - min(time_calc_hr$time)

tss_hr <- ((seconds_hr * Nhr * IF_hr)/(FThr * 3600) * 100) %>% round(.,1) 

tss_hr
