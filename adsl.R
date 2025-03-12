#############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 12SEP2024
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

dm <- read_sas("DM.sas7bdat")
vs <- read_sas("vs.sas7bdat")
ex <- read_sas("ex.sas7bdat")
ds <- read_xpt("DS.XPT")
suppdm <- data.frame(read_xpt("SUPPDM.XPT"))


dm <- convert_blanks_to_na(dm)
ex <- convert_blanks_to_na(ex)
vs <- convert_blanks_to_na(vs)
suppdm <- convert_blanks_to_na(suppdm) |> mutate( USUBJID=paste("TEST00000_C99",SITEID,SUBJID,sep="-"))
ds <- as.data.frame(convert_blanks_to_na(ds)) |> mutate( USUBJID=paste("TEST00000_C99",SITEID,SUBJID,sep="-"),
                                                         DSSTDTC_=as.character(DSSTDTC),
                                                         EOSSTT=case_when(DSCAT=="DISPOSITION EVENT" & DSDECOD=="SUBJECT COMPLETED THE STUDY ACCORDING TO THE PROTOCOL." ~ "COMPLETED",
                                                                          DSCAT=="DISPOSITION EVENT" & DSDECOD == "SCREENING FAILURE" ~ "DISCONTINUED",
                                                                          DSCAT=="DISPOSITION EVENT" & !(DSDECOD) %in% c("SUBJECT COMPLETED THE STUDY ACCORDING TO THE PROTOCOL.", "DEATH") ~ "ONGOING"
                                                                          ))

adsl <- dm %>% select(-DOMAIN,-INVID,-INVNAM) |> 
               mutate(BIRTHDT_=format(ymd(BRTHDTC),format="%d%b%Y"),
                      BIRTHDT=dmy(BIRTHDT_)) |> 
  
                      derive_vars_merged(dataset_add = select(ds,USUBJID,DSDECOD,DSSTDTC_),
                                         by_vars=exprs(USUBJID),
                                         filter_add = DSDECOD == "INFORMED CONSENT OBTAINED",
                                         new_vars = exprs(RFICDTC = DSSTDTC_)) |> 
  
                      derive_vars_dt(new_vars_prefix = "RFIC", #deriving RFCIDT
                                   dtc= RFICDTC) |> 
  
                      derive_vars_aage(start_date = BIRTHDT, #deriving AAGE and AAGEU
                                       end_date = RFICDT) |> 
  
                      derive_vars_merged(dataset_add = select(ds,USUBJID,DSDECOD,DSCAT,DSSTDTC_),
                                        by_vars=exprs(USUBJID),
                                        filter_add = DSDECOD == "RANDOMIZED" & DSCAT == "PROTOCOL MILESTONE",
                                        new_vars = exprs(RANDTC = DSSTDTC_)) |> 
  
                      derive_vars_dt(new_vars_prefix = "RAND", #deriving RANDDT
                                     dtc= RANDTC) |> 
  
                      derive_vars_merged(dataset_add = select(ds,USUBJID,EOSSTT,DSDTC,DSCAT,DSDECOD), # deriving EOSDT,EOSSTT
                                         by_vars=exprs(USUBJID),
                                         filter_add = DSCAT == "DISPOSITION EVENT",
                                         new_vars = exprs(EOSDT = DSDTC, EOSSTT=EOSSTT, DCSREAS=DSDECOD, DTHCAUS=DSDECOD)) |> 
                      
                      derive_vars_merged(dataset_add = select(suppdm,USUBJID,QNAM,QVAL), #deriving COHORT
                                         by_vars=exprs(USUBJID),
                                         filter_add = QNAM == "COHORT",
                                         new_vars = exprs(COHORT = QVAL)) |> 
  
                      derive_vars_merged(dataset_add = select(suppdm,USUBJID,QNAM,QVAL),#deriving SUBGROUP
                                         by_vars=exprs(USUBJID),
                                         filter_add = QNAM == "SUBGROUP",
                                         new_vars = exprs(SUBGROUP = QVAL)) |> 
  
                      mutate(AGEGR1=case_when(!is.na(AAGE) & AAGE < 25 ~ "<25 years",
                                              AAGE >= 25 & AAGE <=55 ~ "25-55 years",
                                              AAGE > 55 ~ ">55 years"),
                                              AGEGR1N=case_when(AGEGR1=="<25 years" ~ 1,
                                              AGEGR1=="25-55 years" ~ 2,
                                              AGEGR1==">55 years" ~ 3),
                                              DCSREAS=if_else(EOSSTT != "COMPLETED",DCSREAS,NA),
                                              DTHCAUS=if_else(EOSSTT == "DEATH",DTHCAUS,NA),
                                              RANDFL=if_else(!is.na(RANDDT),"Y","N"),
                                              COMPLFL=if_else(EOSSTT=="COMPLETED","Y","N"),
                                              SEXN=if_else(SEX=="M",1,2),
                                              RACEN=case_when(RACE=="Caucasian" ~ 1,
                                              RACE=="Asian" ~ 2,
                                              RACE=="American Indian/Native Alaskan" ~ 3,
                                              RACE=="Black" ~ 4,
                                              RACE=="Native Hawaiian or other Pacific Islander" ~ 5,
                                              RACE=="WHITE" ~ 6,
                                              RACE=="AMERICAN INDIAN/ALASKAN NATIVE" ~ 7),
                                              ETHNICN=case_when(ETHNIC=="HISPANIC OR LATINO" ~ 1,
                                              ETHNIC=="NOT HISPANIC OR LATINO" ~ 2),
                                              TRT01P=ARM,
                                              TRT01A=NA,
                                              TRT01PN=NA,
                                              TRT01AN=NA,
                                              ACTARM=NA,
                                              ACTARMCD=NA,
                                              SCRNFFL=if_else(DCSREAS=="SCREENING FAILURE","Y","N"),
                                              BRTHDTF=NA,
                                              PACKNUM=NA,
                                              DTHDT=if_else(EOSSTT=="DEATH",EOSDT,NA))
                                              
                             
                      
                      

               
#deriving trtsdtm and trtedtm              
ex_st <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_st,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "LEO29102"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_st,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "LEO29102"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )

#deriving trtsdt and trtedt using admiral lib function derive_vars_dtm_to_dt
adsl <- adsl |> 
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM))

#deriving SAFFL
adsl <- adsl |> mutate(SAFFL=if_else(RANDFL=="Y" & !is.na(TRTSDT),"Y","N")) |> 
                
                derive_vars_merged(dataset_add = select(vs,USUBJID,VSTESTCD,VSSTRESN), #deriving HEIGHTBL
                                   by_vars=exprs(USUBJID),
                                   filter_add = VSTESTCD == "HEIGHT",
                                   new_vars = exprs(HEIGHTBL = VSSTRESN)) |> 
  
                derive_vars_merged(dataset_add = select(vs,USUBJID,VSTESTCD,VSSTRESN), #deriving WEIGHTBL
                                   by_vars=exprs(USUBJID),
                                   filter_add = VSTESTCD == "WEIGHT",
                                   new_vars = exprs(WEIGHTBL = VSSTRESN)) |> 
  
                derive_vars_merged(dataset_add = select(vs,USUBJID,VSTESTCD,VSSTRESN,VISIT), #deriving TOTBSA
                                   by_vars=exprs(USUBJID),
                                   filter_add = VSTESTCD == "TOTBSA" & VISIT=="DAY 1",
                                   new_vars = exprs(TOTBSABL = VSSTRESN)) |> 
  
                mutate(BSABL=0.007184 * (HEIGHTBL^0.725) * (WEIGHTBL^0.425))  #deriving BSABL based on height and weight






#keeping only required variables as per spec
adsl <- adsl %>% select(STUDYID,USUBJID,SUBJID,SITEID,COUNTRY,AGE,AGEU,AAGE,AAGEU,AGEGR1,
                        AGEGR1N,SEX,SEXN,RACE,RACEN,ETHNIC,ETHNICN,SAFFL,COMPLFL,RANDFL,
                        HEIGHTBL,WEIGHTBL,BSABL,TOTBSABL,SCRNFFL,ARM,ARMCD,ACTARM,
                        ACTARMCD,TRT01P,TRT01PN,TRT01A,TRT01AN,TRTSDT,TRTEDT,TRTSDTM,TRTEDTM,
                        BIRTHDT,BRTHDTF,COHORT,SUBGROUP,PACKNUM,RFICDT,RANDDT,DTHDT,EOSSTT,EOSDT,
                        DCSREAS,DTHCAUS)

#Importing adsl spec for attributes assignment

imort_xlsx_meta <- function (name){
  file <- read_xlsx(paste(getwd(),"02_ADaM_programming_specification.xlsx",sep="/"),col_names = TRUE,sheet=name)
  return(file)
}

adsl_metadata <- data.frame(imort_xlsx_meta("ADSL")) |>  mutate(variable=Variable,
                                                                label=Label,
                                                                dataset=Dataset) |> 
                                                                
                                                         select(dataset,variable,label) 
  
adsl_final <- xportr_label(adsl,adsl_metadata,domain = "ADSL") #getting labels 

#exporting final dataset into xpt and sas7bdat format

write_xpt(adsl_final,"adsl.xpt")
write_sas(adsl_final,"adsl.sas7bdat")
