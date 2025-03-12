############################################################
# Name of Programmer:- Keyur Patel
# Date              :- FEB2025
# Purpose           :- To Create AE table 
############################################################
# load the packages

library(dplyr) # data manipulation tasks 
library(r2rtf) # reporting rtf file 
library(haven)  # export the sas7bdat file
library(tibble)
library(tidyverse)

adae <- read_sas("adae.sas7bdat")
adsl <- read_sas("adsl.sas7bdat")


demog1 <- adsl |> select(USUBJID,SUBJID,COHORT,SUBGROUP,SAFFL) |> 
  filter(SAFFL == "Y")

###merging adae and adsl dataset
adae1 <- merge(adae,demog1,by=c("USUBJID"),all=FALSE)
adae2 <- adae1 |> mutate(SUBGROUP=(if_else(SUBGROUP !="","3","")))
adae3 <- rbind(adae1,adae2)

###Getting Big N
num <- adae3 |> group_by(COHORT,SUBGROUP) |> summarise(n=n(),.groups = "drop") 
num1 <- num |> pivot_wider(names_from = COHORT, values_from = n, names_prefix = "col")

col1_1 <- num$n[1]
col1_2 <- num$n[2]
col1_3 <- num$n[3]
col2_1 <- num$n[4]
col2_2 <- num$n[5]
col2_3 <- num$n[6]

#calculate any AE
any_ae <- adae3 |> 
  group_by(USUBJID, COHORT,SUBGROUP) |> 
  arrange(USUBJID, COHORT,SUBGROUP) |> 
  slice_head(n=1)

any_ae1 <- any_ae |> 
  group_by(COHORT,SUBGROUP) |> 
  summarise(Count=n(), .groups = "keep") 

any_ae2 <- merge(any_ae1,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
  mutate(percent=(round((Count/n*100),digits = 1)),
         pn=paste(Count,paste0("(",round(percent,digits=1),")")))

any_ae3 <- any_ae2 |> select(COHORT,SUBGROUP,pn) |> 
  pivot_wider(names_from = c(COHORT),
              values_from = pn,
              names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), 
              values_from = c(col1),
              names_prefix = "col1_") |> 
  mutate (
    aebodsys="Any AE",
    order1 = 0,
    aedecod=NA
  )

##calculate AE SOC
any_soc <- adae3 |> 
  group_by(USUBJID, COHORT,SUBGROUP,AEBODSYS) |> 
  arrange(USUBJID, COHORT,SUBGROUP,AEBODSYS) |> 
  slice_head(n=1)

any_soc1 <- any_soc |> 
  group_by(COHORT,SUBGROUP,AEBODSYS) |> 
  summarise(Count=n(), .groups = "keep") 

any_soc2 <- merge(any_soc1,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
  mutate(percent=(round((Count/n*100),digits = 1)),
         pn=paste(Count,paste0("(",round(percent,digits=1),")"))) |> filter(!is.na(AEBODSYS))

any_soc3 <- any_soc2 |> select(COHORT,SUBGROUP,pn,AEBODSYS) |> 
  pivot_wider(names_from = c(COHORT),
              values_from = pn,
              names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), 
              values_from = c(col1),
              names_prefix = "col1_") |> 
  mutate (
    aedecod=NA,
    order1 = 1
  ) |> arrange(AEBODSYS) |> ungroup()

# calculate AE SOC and PT

any_pt <- adae3 |> 
  group_by(USUBJID, COHORT,SUBGROUP,AEBODSYS,AEDECOD) |> 
  arrange(USUBJID, COHORT,SUBGROUP,AEBODSYS,AEDECOD) |> 
  slice_head(n=1) |> mutate(
    socpt = paste(AEBODSYS, AEDECOD, sep='/')
  )


any_pt1 <- any_pt |> 
  group_by(COHORT,SUBGROUP,AEBODSYS,AEDECOD,socpt) |> 
  summarise(Count=n(), .groups = "keep") 

any_pt2 <- merge(any_pt1,num,by=c("COHORT","SUBGROUP"),all=TRUE) |> 
  mutate(percent=(round((Count/n*100),digits = 1)),
         pn=paste(Count,paste0("(",round(percent,digits=1),")"))) |> filter(!is.na(AEBODSYS))

any_pt3 <- any_pt2 |> select(COHORT,SUBGROUP,pn,AEBODSYS,AEDECOD,socpt) |> 
  pivot_wider(names_from = c(COHORT),
              values_from = pn,
              names_prefix = "col") |> 
  pivot_wider(names_from = c(SUBGROUP), 
              values_from = c(col1),
              names_prefix = "col1_") |> 
  mutate (
    aedecod=AEDECOD,
    order1 = 2
  ) |> arrange(AEBODSYS,AEDECOD) |> ungroup()

#create final data frame
final <- bind_rows(any_soc3, any_pt3) %>%
  arrange(AEBODSYS,order1, AEDECOD)
final <- bind_rows(any_ae3, final) |> mutate(aebodsys=if_else(order1==1,AEBODSYS,
                                                      if_else(order1==2,paste("  ",AEDECOD),aebodsys)),
                                             col1_1=if_else(is.na(col1_1),"0",col1_1),
                                             col1_2=if_else(is.na(col1_2),"0",col1_2),
                                             col1_3=if_else(is.na(col1_3),"0",col1_3),
                                             col2_1=if_else(order1==0,"0 (0.0)","0"),
                                             col2_2=if_else(order1==0,"0 (0.0)","0"),
                                             col2_3=if_else(order1==0,"0 (0.0)","0"),
                                             AEBODSYS=if_else(is.na(AEBODSYS),aebodsys,AEBODSYS)) |> 
  
  group_split(AEBODSYS) |> 
  map_dfr(~add_row(.x, .after = Inf)) |> 
        select(aebodsys,col1_1,col1_2,col2_1,col2_2,col1_3,col2_3)


####Creating rtf file using r2rtf library

rtf_page(final,orientation = "landscape",
         border_first = "single",
         border_last =""
) |> 
  
  rtf_title("Table 2: Summary of Adverse Events by System Organ Class and Preferred Term - Safety Population",
            text_justification = "l",
            text_font_size = 8) |> 
  
  rtf_footnote(footnote = c("Note: a. N = Number of participants in the specified group. This value is the denominator for the percentage calculation.", 
                            "      b. n = Number of participants reporting at least 1 occurrence of the specified adverse events."),
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
  
  write_rtf("ae_table.rtf")
