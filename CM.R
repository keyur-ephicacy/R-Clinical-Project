#############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 13AUG2024
# Purpose           :- To Create SDTM CM domain 
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

cm_raw <- read_xpt(paste(getwd(),"raw_datasets","CM.xpt",sep="/"))
cmrepm_raw <- read_xpt(paste(getwd(),"raw_datasets","CMREPM.xpt",sep="/"))

#AE programming
browser()
cm_data <- cmrepm_raw %>% mutate(STUDYID="TEST00000_C99",
                          DOMAIN="CM",
                          USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                          CMDECOD=LEVEL_3_TEXT,
                          CMCAT="Prior/Concomitant Medication",
                          CMCLAS = LEVEL_3_TEXT,
                          CMCLASCD = LEVEL_3_CODE,
                          CMDOSE = as.numeric(CMDSTXT),
                          CMSTDTC_ = parse_date_time(CMSTDATE, order = "dmy"),
                          CMSTDTC = as.character(CMSTDTC_),
                          CMENDTC_ = parse_date_time(CMENDAT, order = "dmy"),
                          CMENDTC = as.character(CMENDTC_),
                          CMENRTPT = if_else(CMONGOL == "Yes", 'ONGOING',
                                             if_else(!is.na(CMENDTC), "BEFORE", 'U')),
                          CMENTPT = "TIME MEDICATION RECORDED")
  

#loading dm dataset to get rfstdtc

dm_rf <- read_sas("DM.sas7bdat") %>% select(USUBJID,RFSTDTC) %>% mutate(rfstdtc_=as.Date(substring(RFSTDTC,1,10)))

#merging dm_rf with ae_data to get AESTDY and AEENDY

arrange(dm_rf,USUBJID)
arrange(cm_data,USUBJID)

cm_data2 <- merge(cm_data,dm_rf,by="USUBJID",all.x=TRUE)

cm_data3 <- cm_data2 %>% mutate(CMSTDY_=case_when(as.Date(CMSTDTC) < rfstdtc_ ~ as.Date(CMSTDTC) - rfstdtc_, 
                                                  as.Date(CMSTDTC) >= rfstdtc_ ~ as.Date(CMSTDTC) - rfstdtc_  + 1),
                                CMENDY_=case_when(as.Date(CMENDTC) < rfstdtc_ ~ as.Date(CMENDTC) - rfstdtc_, 
                                                  as.Date(CMENDTC) >= rfstdtc_ ~ as.Date(CMENDTC) - rfstdtc_  + 1),
                                CMSTDY=as.numeric(as.character(CMSTDY_)),
                                CMENDY=as.numeric(as.character(CMENDY_)))

#Derivation for AESEQ as per key variables in metadata
cm_data4 <- cm_data3 %>% group_by(USUBJID) %>% 
  arrange(STUDYID, USUBJID, CMTRT, CMSTDTC, .by_group = TRUE) %>% 
  mutate(CMSEQ = as.numeric(seq(1:n())))

#selecting only req variables as per spec

cm_fin <- cm_data4 %>% select(STUDYID,DOMAIN,USUBJID,CMSEQ,CMTRT,CMDECOD,CMCAT,CMINDC,CMCLAS,
                              CMCLASCD,CMDOSE,CMDOSU,CMDOSFRQ,CMROUTE,CMSTDTC,CMENDTC,CMSTDY,
                              CMENDY,CMENRTPT,CMENTPT)

#Importing AE spec for attributes assignment

imort_xlsx_meta <- function (name){
  file <- read_xlsx(paste(getwd(),"02_SDTM_programming_specification.xlsm",sep="/"),col_names = TRUE,sheet=name)
  return(file)
}

cm_metadata <- data.frame(imort_xlsx_meta("CM")) %>% mutate(variable=Variable.Name) %>% 
  mutate(label=Variable.Label) %>% mutate(dataset=Domain) %>% mutate(length=as.character(Length)) %>% 
  select(dataset,variable,label,length)                             

cm_final <- xportr_label(cm_fin,cm_metadata,domain = "CM") #getting labels 
cm_final_1 <- suppressWarnings(xportr_length(cm_final,cm_metadata,verbose="warn",domain = "CM",length_source="metadata")) #getting lengths


#exporting final dataset into xpt and sas7bdat format

write_xpt(cm_final_1,"CM.xpt")
write_sas(cm_final_1,"CM.sas7bdat")

str(cm_metadata)

#End of Programming       