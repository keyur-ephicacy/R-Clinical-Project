############################################################
# Name of Programmer:- Keyur Patel
# Date              :- DEC2024
# Purpose           :- To Create Demogarphic table 
############################################################
# load the packages

library(dplyr) # data manipulation tasks 
library(r2rtf) # reporting rtf file 
library(haven)  # export the sas7bdat file
library(tibble)
library(tidyverse)


adsl <- read_sas("adsl.sas7bdat")

# Sample demographic data
demog1 <- adsl |> select(USUBJID,SUBJID,SEX,RACE,ETHNIC,AGEGR1,AGE,COUNTRY,HEIGHTBL,WEIGHTBL,COHORT,SUBGROUP,BSABL,TOTBSABL,RANDFL) |> 
                 filter(RANDFL == "Y")
demog2 <- demog1 |> mutate(SUBGROUP=(if_else(SUBGROUP !="","3","")))

demog <- rbind(demog1,demog2)

#Getting Number Subject
num <- demog |> group_by(COHORT,SUBGROUP) |> summarise(n=n(),.groups = "drop") 
num1 <- num |> pivot_wider(names_from = COHORT, values_from = n, names_prefix = "col")
num2 <- num1 |> pivot_wider(names_from = SUBGROUP, values_from = c(col1, col2)) |> mutate(col="Number Subject",
                                                                                          ord=1,ord1=1)
col1_1 <- num$n[1]
col1_2 <- num$n[2]
col1_3 <- num$n[3]
col2_1 <- num$n[4]
col2_2 <- num$n[5]
col2_3 <- num$n[6]


# Calculate frequency and percentage for Gender
summary_gen <- demog %>%
  group_by(COHORT,SUBGROUP,SEX) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

summary_gen1 <- merge(summary_gen,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
                mutate(percent=(round((Count/n*100),digits = 1)),
                       pn=paste(Count,paste0("(",round(percent,digits=1),")"))) |> 
                select(COHORT,SUBGROUP,SEX,pn)  |> 
                pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
                pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> rename(col=SEX) |> 
                mutate(ord=2,
                       col=(if_else(col=="F","Female","Male")),
                       ord1=(if_else(col=="Female",2,1))) |> 
                add_row(col="Sex",ord=2,ord1=0.5, .before = 1)

                
# Calculate frequency and percentage for RACE
summary_race <- demog %>%
  group_by(COHORT,SUBGROUP,RACE) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

summary_race1 <- merge(summary_race,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
  mutate(percent=(round((Count/n*100),digits = 1)),
         pn=paste(Count,paste0("(",round(percent,digits=1),")"))) |> 
  select(COHORT,SUBGROUP,RACE,pn) |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> rename(col=RACE)|> 
  mutate(ord=3,
         col=if_else(col=="WHITE","White","American Indian/Alaskan Native"),
         ord1=if_else(col=="White",1,2)) |> 
  add_row(col="Race",ord=3,ord1=0.5, .before = 1) 

# Calculate frequency and percentage for ETHNIC
summary_ethn <- demog %>%
  group_by(COHORT,SUBGROUP,ETHNIC) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

summary_ethn1 <- merge(summary_ethn,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
  mutate(percent=(round((Count/n*100),digits = 1)),
         pn=paste(Count,paste0("(",round(percent,digits=1),")"))) |> 
  select(COHORT,SUBGROUP,ETHNIC,pn) |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> rename(col=ETHNIC) |> 
  mutate(ord=4,
         col=if_else(col=="HISPANIC OR LATINO","Hispanic or Latino","Non Hispanic or Latino"),
         ord1=if_else(col=="Hispanic or Latino",1,2)) |> 
  add_row(col="Ethnicity",ord=4,ord1=0.5, .before = 1)

# Calculate frequency and percentage for AGE
summary_age <- demog %>%
  group_by(COHORT,SUBGROUP,AGEGR1) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

summary_age1 <- merge(summary_age,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
  mutate(percent=(round((Count/n*100),digits = 1)),
         pn=paste(Count,paste0("(",round(percent,digits=1),")"))) |> 
  select(COHORT,SUBGROUP,AGEGR1,pn) |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> rename(col=AGEGR1) |> 
  mutate(ord=5,
         ord1=if_else(col=="<25 years",1,2)) |> 
  add_row(col="Age",ord=5,ord1=0.5, .before = 1) 


# Calculate frequency and percentage for COUNTRY
summary_country <- demog %>%
  group_by(COHORT,SUBGROUP,COUNTRY) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )

summary_country1 <- merge(summary_country,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
  mutate(percent=(round((Count/n*100),digits = 1)),
         pn=paste(Count,paste0("(",round(percent,digits=1),")"))) |> 
  select(COHORT,SUBGROUP,COUNTRY,pn) |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> rename(col=COUNTRY)|> 
  mutate(ord=7,
         col=if_else(col=="GERMANY","Germany",""),
         ord1=1) |> 
  add_row(col="Country",ord=7,ord1=0.5, .before = 1)


# Calculate Descriptive stat for AGE

summary_ag <- demog %>%
  group_by(COHORT,SUBGROUP) %>%
  summarise(
    mean_ag=round(mean(AGE),digits = 1),
    sd_ag=round(sd(AGE),digits =2),
    med=as.character(round(median(AGE),digits=1)),
    min_ag=min(AGE),
    max_ag=max(AGE),
    .groups = "drop"
  ) |> mutate(mean_sd=paste(mean_ag,paste0("(",sd_ag,")")),
              min_max=paste0("(",min_ag,", ",max_ag,")")) |> 
  select(COHORT,SUBGROUP,mean_sd,med,min_max) |> 
  pivot_longer(cols = c(mean_sd,med,min_max), names_to = "col", values_to = "pn") |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2))|> mutate(ord=6,ord1=6) |> 
  add_row(col="",ord=6, .before = 1)


# Calculate Descriptive stat for Height

summary_ht <- demog %>%
  group_by(COHORT,SUBGROUP) %>%
  summarise(
    Count = as.character(n()),
    mean_ht=round(mean(HEIGHTBL),digits = 1),
    sd_ht=round(sd(HEIGHTBL),digits =2),
    med=as.character(round(median(HEIGHTBL),digits=1)),
    min_ht=min(HEIGHTBL),
    max_ht=max(HEIGHTBL),
    .groups = "drop"
  ) |> mutate(mean_sd=paste(mean_ht,paste0("(",sd_ht,")")),
              min_max=paste0("(",min_ht,", ",max_ht,")")) |> 
       select(COHORT,SUBGROUP,Count,mean_sd,med,min_max) |> 
       pivot_longer(cols = c(Count,mean_sd,med,min_max), names_to = "col", values_to = "pn") |> 
       pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
       pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> mutate(ord=8,ord1=8) |> 
  add_row(col="Height^cm",ord=8,ord1=0.5, .before = 1) 

# Calculate Descriptive stat for Weight

summary_wt <- demog %>%
  group_by(COHORT,SUBGROUP) %>%
  summarise(
    Count = as.character(n()),
    mean_wt=round(mean(WEIGHTBL),digits = 1),
    sd_wt=round(sd(WEIGHTBL),digits =2),
    med=as.character(round(median(WEIGHTBL),digits=1)),
    min_wt=min(WEIGHTBL),
    max_wt=max(WEIGHTBL),
    .groups = "drop"
  ) |> mutate(mean_sd=paste(mean_wt,paste0("(",sd_wt,")")),
              min_max=paste0("(",min_wt,", ",max_wt,")")) |> 
  select(COHORT,SUBGROUP,Count,mean_sd,med,min_max) |> 
  pivot_longer(cols = c(Count,mean_sd,med,min_max), names_to = "col", values_to = "pn") |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> mutate(ord=9,ord1=9) |> 
  add_row(col="Weigth^kg",ord=9,ord1=0.5, .before = 1)

# Calculate Descriptive stat for TOTBSABL

summary_tot <- demog %>%
  group_by(COHORT,SUBGROUP) %>%
  summarise(
    Count = as.character(n()),
    mean_tot=round(mean(TOTBSABL),digits = 1),
    sd_tot=round(sd(TOTBSABL),digits =2),
    med=as.character(round(median(TOTBSABL),digits=1)),
    min_tot=min(TOTBSABL),
    max_tot=max(TOTBSABL),
    .groups = "drop"
  ) |> mutate(mean_sd=paste(mean_tot,paste0("(",sd_tot,")")),
              min_max=paste0("(",min_tot,", ",max_tot,")")) |> 
  select(COHORT,SUBGROUP,Count,mean_sd,med,min_max) |> 
  pivot_longer(cols = c(Count,mean_sd,med,min_max), names_to = "col", values_to = "pn") |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> mutate(ord=10,ord1=10) |> 
  add_row(col="Total Body Surface Area Affected (%)",ord=10,ord1=0.5, .before = 1) 


# Calculate Descriptive stat for BSABL

summary_bsa <- demog %>%
  group_by(COHORT,SUBGROUP) %>%
  summarise(
    Count = as.character(n()),
    mean_bs=round(mean(BSABL),digits = 2),
    sd_bs=round(sd(BSABL),digits =3),
    med=as.character(round(median(BSABL),digits=2)),
    min_bs=round(min(BSABL),digits=1),
    max_bs=round(max(BSABL),digits = 1),
    .groups = "drop"
  ) |> mutate(mean_sd=paste(mean_bs,paste0("(",sd_bs,")")),
              min_max=paste0("(",min_bs,", ",max_bs,")")) |> 
  select(COHORT,SUBGROUP,Count,mean_sd,med,min_max) |> 
  pivot_longer(cols = c(Count,mean_sd,med,min_max), names_to = "col", values_to = "pn") |> 
  pivot_wider(names_from = COHORT, values_from = pn, names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), values_from = c(col1, col2)) |> mutate(ord=11,ord1=11) |> 
  add_row(col="Body Surface Area^m2",ord=11,ord1=0.5, .before = 1) 

#combining all datasets

fin <- rbind(num2,summary_gen1,summary_race1,summary_ethn1,summary_age1,summary_ag,summary_country1,summary_ht,
             summary_wt,summary_tot,summary_bsa) |> 
                                                           
       mutate(col=if_else(col=="Count","n",
                  if_else(col=="mean_sd","Mean (SD)",
                  if_else(col=="med","Median",
                  if_else(col=="min_max","(Min, Max)",col)))),
              
              ord1=if_else(col=="n",1,
                   if_else(col=="Mean (SD)",2,
                   if_else(col=="Median",3,
                   if_else(col=="(Min, Max)",4,ord1)))),
              
              var_label=case_when(ord==1 ~ "Number Subject",
                                  ord==2 ~ "Sex",
                                  ord==3 ~ "Race",
                                  ord==4 ~ "Ethnicity",
                                  ord==5 ~ "Age",
                                  ord==7 ~ "Country",
                                  ord==8 ~ "Height^cm",
                                  ord==9 ~ "Weigth^kg",
                                  ord==10 ~ "Total Body Surface Area Affected (%)",
                                  ord==11 ~ "Body Surface Area^m2")) |> 
   
               arrange(var_label,ord,ord1) |> 
      
      
              mutate(col=if_else(ord1 !=0.5 & ord !=1,paste(" ",col),col)) |> 
              filter(!is.na(ord1)) |> 
              group_split(ord) |> 
              map_dfr(~add_row(.x, .after = Inf)) |> 
              select(col,col1_1,col1_2,col2_1,col2_2,col1_3,col2_3)
              
####Creating rtf file using r2rtf library

  rtf_page(fin,orientation = "landscape",
               border_first = "single",
               border_last =""
               ) |> 
  
  rtf_title("Table 1: Demographics and Baseline Characteristics - Randomized Population",
            text_justification = "l",
            text_font_size = 8) |> 
    
  rtf_footnote(footnote = c("Note: a. N = Number of participants in the specified group. This value is the denominator for the percentage calculation.", 
                                  "      b. n = Number of participants with the specified characteristic. Each participant was counted once."),
                            border_left = "",
                            border_right= "",
                            border_top= "",
                            border_bottom = "") |> 
  
  rtf_colheader(colheader = " | Leo 2.5mg/g Cream | Leo Cream Vehicle | Total",
                col_rel_width = c(2,2,2,2),
                text_format="u",
                border_top=rep("single",4),
                border_left=rep("",4),border_right=rep("",4)
                ) |> 
  
  rtf_colheader(colheader = " | Cohort I | Cohort II | Cohort I | Cohort II |Cohort I | Cohort II",
                text_format="u",
                col_rel_width = c(3,1.5,1.5,1.5,1.5,1.5,1.5),
                border_top=rep("",7),
                border_left=rep("",7),border_right=rep("",7)
                
                ) |> 
    
    rtf_colheader(colheader = paste0(" | (N{\\super a}=" ,col1_1,") | (N{\\super a}=",col1_2,") | (N{\\super a}=",col2_1,") | (N{\\super a}=",col2_2,") | (N{\\super a}=",col1_3,") | (N{\\super a}=",col2_3,")"),
                  col_rel_width = c(3,1.5,1.5,1.5,1.5,1.5,1.5),
                  border_top=rep("",7),
                  border_left=rep("",7),border_right=rep("",7)
                  
    ) |> 
    
    rtf_colheader(colheader = " | n{\\super b} (%) | n{\\super b} (%) | n{\\super b} (%) | n{\\super b} (%) | n{\\super b} (%) | n{\\super b} (%)",
                  col_rel_width = c(3,1.5,1.5,1.5,1.5,1.5,1.5),
                  border_top=rep("",7),
                  border_left=rep("",7),border_right=rep("",7)
                  
    ) |> 
  
  rtf_body(as_colheader = F,
           col_rel_width = c(4,2,2,2,2,2,2),
           text_justification = c("l",rep("c",6)),border_left=rep("",7),border_right=rep("",7))|>
    
  rtf_encode(doc_type = "table",
             page_footnote = "all") |> 
  
  write_rtf("demog_table.rtf")

  

  