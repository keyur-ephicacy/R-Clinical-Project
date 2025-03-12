#############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 06AUG2024
# Purpose           :- To Create SDTM AE domain 
############################################################

# load the packages
library(readxl) # read the raw datasets 
library(dplyr) # data manipulation tasks 
library(lubridate)   # date format
library(haven)  # export the sas7bdat file
library(xportr) # to assign label for each variable as per spec
library(tidyverse)

#setting working directory 
getwd()

#Importing raw datasets for AE

ae_raw <- read_xpt(paste(getwd(),"raw_datasets","AE.xpt",sep="/"))
aerep_raw <- read_xpt(paste(getwd(),"raw_datasets","AEREP.xpt",sep="/"))

#AE programming

ae_data <- aerep_raw %>% select(-AEREL,-AEOUT,-AESCONG,AESDISAB,-AESDTH,-AESHOSP,-AESLIFE,-AESMIE,AETERM) %>% 
                            mutate(STUDYID="TEST00000_C99",
                            DOMAIN="AE",
                            USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                            AETERM=TERM_TEXT,
                            AEDECOD=LEVEL_4_TEXT,
                            AEBODSYS=LEVEL_1_TEXT,
                            AELOC=str_to_upper(AELOCL),
                            AESEV=str_to_upper(AEINTL),
                            AESER=case_when(AESERL== "No" ~ "N",TRUE ~ "Y"),
                            AEACN=str_to_upper(AEACNL),
                            AEACNOTH=if_else((!is.na(AEACNO1N) | !is.na(AEACNO2N)) & (!is.na(AEACNO3N) | !is.na(AEACNO4N)),"MULTIPLE",
                                     if_else((!is.na(AEACNO1N) | !is.na(AEACNO3N)) & (!is.na(AEACNO2N) | !is.na(AEACNO4N)),"MULTIPLE",
                                     if_else((!is.na(AEACNO1N) | !is.na(AEACNO4N)) & (!is.na(AEACNO2N) | !is.na(AEACNO3N)),"MULTIPLE",""))),
                            AEREL=str_to_upper(AERELL),
                            AEOUT=str_to_upper(AEOUTL),
                            AESCONG=AESCONGL,
                            AESDISAB=AESDISAL,
                            AESDTH=AESDTHL,
                            AESHOSP=AESHOSPL,
                            AESLIFE=AESLIFEL,
                            AESMIE=AESMIEL,
                            AESTDTC_=parse_date_time(AESTDAT,order="dmy"),
                            AESTDTC=as.character(AESTDTC_),
                            AEENDTC_=parse_date_time(AEENDAT,order="dmy"),
                            AEENDTC=as.character(AEENDTC_))
                          
                            
#loading dm dataset to get rfstdtc

dm_rf <- read_sas("DM.sas7bdat") %>% select(USUBJID,RFSTDTC) %>% mutate(rfstdtc_=as.Date(substring(RFSTDTC,1,10)))

#merging dm_rf with ae_data to get AESTDY and AEENDY

arrange(dm_rf,USUBJID)
arrange(ae_data,USUBJID)

ae_data2 <- merge(ae_data,dm_rf,by="USUBJID",all.x=TRUE)

ae_data3 <- ae_data2 %>% mutate(AESTDY_=case_when(as.Date(AESTDTC) < rfstdtc_ ~ as.Date(AESTDTC) - rfstdtc_, 
                                                 as.Date(AESTDTC) >= rfstdtc_ ~ as.Date(AESTDTC) - rfstdtc_  + 1),
                                AEENDY_=case_when(as.Date(AEENDTC) < rfstdtc_ ~ as.Date(AEENDTC) - rfstdtc_, 
                                                  as.Date(AEENDTC) >= rfstdtc_ ~ as.Date(AEENDTC) - rfstdtc_  + 1),
                                AESTDY=as.numeric(as.character(AESTDY_)),
                                AEENDY=as.numeric(as.character(AEENDY_)))

#Derivation for AESEQ as per key variables in metadata
ae_data4 <- ae_data3 %>% group_by(USUBJID) %>% 
                         arrange(STUDYID, USUBJID, AEDECOD, AESTDTC, .by_group = TRUE) %>% 
                         mutate(AESEQ = as.numeric(seq(1:n())))

#selecting only req variables as per spec

ae_fin <- ae_data4 %>% select(STUDYID,DOMAIN,USUBJID,AESEQ,AETERM,AEDECOD,AEBODSYS,AELOC,
                              AESEV,AESER,AEACN,AEACNOTH,AEREL,AEOUT,AESCONG,AESDISAB,
                              AESDTH,AESHOSP,AESLIFE,AESMIE,AESTDTC,AEENDTC,AESTDY,AEENDY)
                              
#Importing AE spec for attributes assignment

imort_xlsx_meta <- function (name){
  file <- read_xlsx(paste(getwd(),"02_SDTM_programming_specification.xlsx",sep="/"),col_names = TRUE,sheet=name)
  return(file)
}

ae_metadata <- data.frame(imort_xlsx_meta("AE")) %>% mutate(variable=Variable.Name) %>% 
  mutate(label=Variable.Label) %>% mutate(dataset=Domain) %>% mutate(length=Length) %>% 
  select(dataset,variable,label,length)                              
                              
ae_final <- xportr_label(ae_fin,ae_metadata,domain = "AE") #getting labels 
ae_final_1 <- xportr_length(ae_final,ae_metadata,domain = "AE",length_source="metadata") #getting lengths


#exporting final dataset into xpt and sas7bdat format

write_xpt(ae_final_1,"AE.xpt")
write_sas(ae_final_1,"AE.sas7bdat")

warnings()

#End of Programming                          
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              

