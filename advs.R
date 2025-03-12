############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 11NOV2024
# Purpose           :- To Create ADaM ADVS dataset 
############################################################
# load the packages
library(readxl) # read the raw datasets 
library(dplyr) # data manipulation tasks 
library(lubridate)   # date format
library(haven)  # export the sas7bdat file
library(xportr) # to assign label for each variable as per spec
library(tidyverse)
library(purrr)
library(admiral) #to generate adam dataset by in built function

#setting working directory 
getwd()
adsl <- read_sas("adsl.sas7bdat")
vs <- read_sas("VS.sas7bdat")
vs <- convert_blanks_to_na(vs)

#getting required variabled from adsl
adsl <- adsl |> select(STUDYID,USUBJID,SUBJID,SITEID,AGE,AGEU,SEX,SEXN,RACE,RACEN,ARM,ACTARM,
                       ARMCD,ACTARMCD,TRTSDT,TRTSDTM,TRTEDT,TRTEDTM,RANDDT,TRT01P,TRT01PN,TRT01A,TRT01AN,
                       RANDFL,COMPLFL,SAFFL)

advs <- vs |> derive_vars_dt(new_vars_prefix = "AST",
                             dtc = VSDTC)  |>  
  
                            mutate(ADT=toupper(format(ASTDT,"%d%b%Y")),
                                   ADY=VSDY)|> 
  
                            derive_vars_dtm(dtc = VSDTC,
                            new_vars_prefix = "A"
                            ) |> 
                          
                            mutate(ATM=format(ADTM,"%H:%M"),
                                   PARAM=if_else(is.na(VSSTRESU),VSTESTCD,paste(VSTESTCD,"(",VSSTRESU,")","")),
                                   PARAMCD=VSTESTCD,
                                   PARAMN=case_when(PARAMCD == "TOTBSA" ~ 1,
                                                    PARAMCD == "DIABP" ~ 2,
                                                    PARAMCD == "HEIGHT" ~ 3,
                                                    PARAMCD == "HR" ~ 4,
                                                    PARAMCD == "RESP" ~ 5,
                                                    PARAMCD == "SYSBP" ~ 6,
                                                    PARAMCD == "TEMP" ~ 7,
                                                    PARAMCD == "WEIGHT" ~ 8,),
                                   AVAL=VSSTRESN,
                                   AVALC=VSSTRESC,
                                   BASE=if_else(VSBLFL == "Y",AVAL,NA),
                                   )
                                          
#calculating chg and pchg

advs <- advs |> group_by(USUBJID, PARAMCD) |> 
  fill(BASE, .direction = "down")  |> 
  ungroup() |>  mutate(BASE_=case_when(VSBLFL=="Y" ~ NA,.default = BASE),
                       BASE=BASE_,
                       CHG=AVAL-BASE,
                       PCHG=((AVAL-BASE)/BASE)*100,
                       BASEC=as.character(BASE),
                       ABLFL=VSBLFL,
                       AVISIT=VISIT,
                       AVISITN=case_when(VISIT=="DAY 1" ~ 1,
                                         VISIT=="SCREENING" ~ 0,
                                         VISIT=="DAY 2" ~ 2,
                                         VISIT=="DAY 3" ~ 3,
                                         VISIT=="DAY 4" ~ 4,
                                         VISIT=="DAY 5" ~ 5,
                                         VISIT=="DAY 6" ~ 6,
                                         VISIT=="DAY 7" ~ 7,
                                         VISIT=="DAY 8" ~ 8,
                                         VISIT=="DAY 9" ~ 9,
                                         VISIT=="FOLLOW-UP 1" ~ 10,
                                         VISIT=="UNSCHEDULED 1" ~ 101)
                                         ) |> 
  
                      derive_vars_merged(dataset_add = adsl,
                                         by_vars = exprs(STUDYID,USUBJID)) |> 
                      mutate(TRTA=TRT01A,
                             TRTAN=TRT01AN,
                             TRTP=TRT01P,
                             TRTPN=TRT01PN)

#keeping only required variables as per spec
advs <- advs |> select(STUDYID,USUBJID,SUBJID,SITEID,ARM,ARMCD,ACTARM,ACTARMCD,SEX,AGE,AGEU,RACE,
                       COMPLFL,SAFFL,TRTSDT,TRTSDTM,TRTEDT,TRTEDTM,TRTA,TRTAN,TRTP,TRTPN,ADT,ADTM,ADY,
                       ATM,AVISIT,AVISITN,PARAM,PARAMCD,PARAMN,AVAL,AVALC,BASE,BASEC,CHG,PCHG,ABLFL,RANDFL)

#Importing adsl spec for attributes assignment

imort_xlsx_meta <- function (name){
  file <- read_xlsx(paste(getwd(),"02_ADaM_programming_specification.xlsx",sep="/"),col_names = TRUE,sheet=name)
  return(file)
}


advs_metadata <- data.frame(imort_xlsx_meta("ADVS")) |>  mutate(variable=Variable,
                                                                label=Label,
                                                                dataset=Dataset) |> 
  
  select(dataset,variable,label) 

advs_final <- xportr_label(advs,advs_metadata,domain = "ADVS") #getting labels 

#exporting final dataset into xpt and sas7bdat format

write_xpt(advs_final,"advs.xpt")
write_sas(advs_final,"advs.sas7bdat")

