############################################################
# Name of Programmer:- Keyur Patel
# Date              :- FEB2025
# Purpose           :- To Create Figure 
############################################################



# load the packages

library(dplyr) # data manipulation tasks 
library(r2rtf) # reporting rtf file 
library(haven)  # export the sas7bdat file
library(tibble)
library(tidyverse)
library(ggplot2)


advs <- read_sas("advs.sas7bdat")
adsl <- read_sas("adsl.sas7bdat")

vs <- advs |> select(SITEID,AVAL,PARAM,PARAMCD) |> filter(!is.na(AVAL)) |> 
              mutate(PARAM=if_else(PARAMCD=="TOTBSA","Body Surface Area",
                           if_else(PARAMCD=="DIABP","Diastolic Blood Pressure (mmHg)",
                           if_else(PARAMCD=="HEIGHT","Height (cm)",
                           if_else(PARAMCD=="HR","Pulse Rate (beats/min)",
                           if_else(PARAMCD=="RESP","Respiratory Rate (breaths/min)",
                          if_else(PARAMCD=="SYSBP","Systolic Blood Pressure (mmHg)",
                          if_else(PARAMCD=="TEMP","Temperature (C)",
                          if_else(PARAMCD=="WEIGHT","Weight (kg)","")))))))))

ggplot(vs, aes(x = SITEID, y = AVAL, fill = SITEID)) + 
  geom_boxplot() +  facet_wrap(~ PARAM,scales = "free") +
  #theme_minimal() +  # Clean theme
  labs(title = "Figure 1: Distribution of Analysis Value over different sites for each parameter",
       x = "Site Number",
       y = "Analysis Value",
       fill="Site Number") +
  scale_color_brewer(palette = "Set1") 

##Generating RTF file
filename <- "Rplot02.png"
filename %>%
  rtf_read_figure() %>%
  rtf_figure() %>% 
  rtf_encode(doc_type = "figure",page_source = "all") %>%
  write_rtf(file = "Rplot.rtf")


