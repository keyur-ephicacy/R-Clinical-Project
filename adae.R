############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 27SEP2024
# Purpose           :- To Create ADaM ADSL dataset 
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
ae <- read_sas("AE.sas7bdat")
suppae <- data.frame(read_xpt("SUPPAE.XPT"))

#Removing dupicates from suppae
suppae_ <- convert_blanks_to_na(suppae) |> mutate( USUBJID=paste("TEST00000_C99",SITEID,SUBJID,sep="-"),
                                                  AESEQ=IDVARVAL) |> group_by(USUBJID,QNAM,AESEQ) |> 
                                           slice_tail(n=1) |> ungroup()

#getting required variabled from adsl
adsl <- adsl |> select(STUDYID,USUBJID,SUBJID,SITEID,AGE,AGEU,SEX,SEXN,RACE,RACEN,ARM,ACTARM,
                       ARMCD,ACTARMCD,TRTSDT,TRTSDTM,TRTEDT,TRTEDTM,RANDDT,TRT01P,TRT01PN,TRT01A,TRT01AN)


adae <- ae |> derive_vars_merged(dataset_add = adsl,
                                   by_vars = exprs(STUDYID,USUBJID)) |> 
  
              derive_vars_dtm(dtc = AESTDTC,
                             new_vars_prefix = "AST",
                             date_imputation = "first",
                             time_imputation = "first",
                             highest_imputation = "M"
                             ) |>                       #deriving astdtm
  
             derive_vars_dtm(dtc = AEENDTC,
                             new_vars_prefix = "AEN",
                             date_imputation = "last",
                             time_imputation = "last",
                             highest_imputation = "M") |> #deriving aendtm
  
  
            derive_vars_dtm_to_dt(source_vars = exprs(ASTDTM, AENDTM)) |> #getting astdt and aendt
  
            derive_var_trtemfl(new_var = TRTEMFL,              # deriving trtemfl
                               start_date = ASTDTM,
                               end_date = AENDTM,
                               trt_start_date = TRTSDTM,
                               trt_end_date = NULL,
                               ignore_time_for_trt_end = FALSE)|> 
           
            derive_vars_merged(dataset_add = select(suppae_,USUBJID,QNAM,QVAL,AESEQ),
                               by_vars = exprs(USUBJID,AESEQ),
                               filter_add = QNAM == "AESKIN",
                               new_vars = exprs(AESKIN=QVAL)) |> 
  
            derive_vars_merged(dataset_add = select(suppae_,USUBJID,QNAM,QVAL,AESEQ),
                               by_vars = exprs(USUBJID,AESEQ),
                               filter_add = QNAM == "AEWITH",
                               new_vars = exprs(AEWITH=QVAL)) |> 
  
              mutate(AEOUTN=case_when(AEOUT=="RECOVERED/RESOLVED" ~ 1,
                                      AEOUT=="NOT RECOVERED/NOT RESOLVED*" ~ 2,
                                      AEOUT=="RECOVERING/RESOLVING*" ~ 3,
                                      AEOUT=="RECOVERED/RESOLVED WITH SEQUELAE" ~ 4),
                     ASEV=AESEV,
                     ASEVN=(case_when(AESEV=="MILD" ~ 1,
                                      AESEV=="MODERATE" ~ 2,
                                      AESEV=="SEVERE" ~ 3)),
                     AREL=AEREL,
                     ARELN=(case_when(AEREL %in% c("RELATED","POSSIBLE","PROBABLE") ~ 1,
                                      AEREL %in% c("NOT RELATED","NOT ASSESSABLE") ~ 2)),
                     AEACNN=case_when(AEACN=="DOSE CHANGED" ~ 1,
                                      AEACN %in% c("DOSE NOT CHANGED") ~2),
                     TRTP=TRT01P,
                     TRTPN=TRT01PN,
                     TRTA=TRT01A,
                     TRTAN=TRT01AN)


#keeping only required variables as per spec
adae <- adae |> select(STUDYID,USUBJID,SUBJID,SITEID,AGE,AGEU,SEX,SEXN,RACE,RACEN,ARM,ACTARM,
                        ARMCD,ACTARMCD,TRTSDT,TRTSDTM,TRTEDT,TRTEDTM,RANDDT,TRTP,TRTPN,TRTA,TRTAN,
                        AETERM,AEDECOD,AEBODSYS,AESER,AEOUT,AEOUTN,AELOC,AESEV,ASEV,ASEVN,
                        AEREL,AREL,ARELN,AEACN,AEACNN,AEACNOTH,AESCONG,AESDISAB,AESDTH,AESHOSP,
                        AESLIFE,AESMIE,AESTDTC,AEENDTC,ASTDTM,AENDTM,ASTDT,AENDT,ASTDTF,AENDTF,
                        TRTEMFL,AESKIN,AEWITH)

#Importing adsl spec for attributes assignment

imort_xlsx_meta <- function (name){
  file <- read_xlsx(paste(getwd(),"02_ADaM_programming_specification.xlsx",sep="/"),col_names = TRUE,sheet=name)
  return(file)
}

adae_metadata <- data.frame(imort_xlsx_meta("ADAE")) |>  mutate(variable=Variable,
                                                                label=Label,
                                                                dataset=Dataset) |> 
  
  select(dataset,variable,label) 

adae_final <- xportr_label(adae,adae_metadata,domain = "ADAE") #getting labels 

#exporting final dataset into xpt and sas7bdat format

write_xpt(adae_final,"adae.xpt")
write_sas(adae_final,"adae.sas7bdat")

