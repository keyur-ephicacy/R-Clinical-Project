#############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 20AUG2024
# Purpose           :- To Create SDTM EX domain 
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

sd_raw <- read_xpt(paste(getwd(),"raw_datasets","SD.xpt",sep="/"))
et_raw <- read_xpt(paste(getwd(),"raw_datasets","ET.xpt",sep="/"))

#AE programming

ex_data <- sd_raw %>% mutate(STUDYID="TEST00000_C99",
                                 DOMAIN="EX",
                                 USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                                 EXTRT="LEO29102",
                                 EXDOSE=as.numeric(SDDOSE),
                                 EXDOSTXT = "CREAM VEHICLE",
                                 EXDOSU = "g",
                                 EXDOSFRM = "CREAM",
                                 EXDOSFRQ = "BID",
                                 EXROUTE = "TOPICAL",
                                 EXLOC = "SKIN",
                                 EXTPT = VSTIM,
                                 EXTPTNUM=if_else(str_detect(VSTIM,"AM"),2,7),
                                 EXTPTREF=case_when(EXTPTNUM == 2 ~ "AM DOSE",
                                                    EXTPTNUM == 7 ~ "PM DOSE"),
                                 EXSTDT_T=paste(substr(SDATIM, 1, 2), substr(SDATIM, 3, 4),sep=":"),
                                 EXSTDT_DT=paste(SDDAT,EXSTDT_T,sep="-"),
                                 EXSTDT_DT1=as.POSIXct(EXSTDT_DT, format = "%Y-%m-%d-%H:%M"),
                                 EXSTDTC=format(EXSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                                 EXSTDTC_=as.Date(SDDAT))
                            
et_data <- et_raw %>% mutate(EXENDT_T=if_else(!is.na(et_raw$ETLDDAT),paste('00','00',sep=":"),NA),
                             EXENDT_DT=paste(ETLDDAT,EXENDT_T,sep="-"),
                             EXENDT_DT1=as.POSIXct(EXENDT_DT, format = "%Y-%m-%d-%H:%M"),
                             EXENDTC=format(EXENDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                             EXENDTC_=as.Date(ETLDDAT)) %>% 
                            select(PT,EXENDTC,EXENDTC_)

#merging et_data with main dataframe ex_data to get exendtc

arrange(ex_data,PT)
arrange(et_data,PT)

ex_all <- merge(ex_data,et_data,by="PT",all.x=TRUE)
#loading dm dataset to get rfstdtc

dm_rf <- read_sas("DM.sas7bdat") %>% select(USUBJID,RFSTDTC) %>% mutate(rfstdtc_=as.Date(substring(RFSTDTC,1,10)))

#merging dm_rf with ae_data to get AESTDY and AEENDY

arrange(dm_rf,USUBJID)
arrange(ex_all,USUBJID)

ex_all1 <- merge(ex_all,dm_rf,by="USUBJID",all.x=TRUE)

ex_all2 <- ex_all1 %>% mutate(EXSTDY_=case_when(EXSTDTC_ < rfstdtc_ ~ EXSTDTC_ - rfstdtc_, 
                                                EXSTDTC_ >= rfstdtc_ ~ EXSTDTC_ - rfstdtc_  + 1),
                                EXENDY_=case_when(EXENDTC_ < rfstdtc_ ~ EXENDTC_ - rfstdtc_, 
                                                  EXENDTC_ >= rfstdtc_ ~ EXENDTC_ - rfstdtc_  + 1),
                                EXSTDY=as.numeric(as.character(EXSTDY_)),
                                EXENDY=as.numeric(as.character(EXENDY_)))

#Derivation for AESEQ as per key variables in metadata
ex_all3 <- ex_all2 %>% group_by(USUBJID) %>% 
  arrange(STUDYID, USUBJID, EXTRT, EXSTDTC, EXTPTREF, .by_group = TRUE) %>% 
  mutate(EXSEQ = as.numeric(seq(1:n())))

#selecting only req variables as per spec

ex_fin <- ex_all3 %>% select(STUDYID,DOMAIN,USUBJID,EXSEQ,EXTRT,EXDOSE,EXDOSTXT,EXDOSU,EXDOSFRM,EXDOSFRQ,
                              EXROUTE,EXLOC,EXSTDTC,EXENDTC,EXSTDY,EXENDY,EXTPT,EXTPTNUM,EXTPTREF)
                              
                              
#Importing EX spec for attributes assignment

imort_xlsx_meta <- function (name){
  file <- read_xlsx(paste(getwd(),"02_SDTM_programming_specification.xlsm",sep="/"),col_names = TRUE,sheet=name)
  return(file)
}

ex_metadata <- data.frame(imort_xlsx_meta("EX")) %>% mutate(variable=Variable.Name) %>% 
  mutate(label=Variable.Label) %>% mutate(dataset=Domain) %>% mutate(length=as.character(Length)) %>% 
  select(dataset,variable,label,length)                             

ex_final <- xportr_label(ex_fin,ex_metadata,domain = "EX") #getting labels 
ex_final_1 <- suppressWarnings(xportr_length(ex_final,ex_metadata,domain = "EX",length_source="metadata")) #getting lengths


#exporting final dataset into xpt and sas7bdat format

write_xpt(ex_final_1,"ex.xpt")
write_sas(ex_final_1,"ex.sas7bdat")



#End of Programming       