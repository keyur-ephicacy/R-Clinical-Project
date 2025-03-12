#############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 20AUG2024
# Purpose           :- To Create SDTM VS domain 
############################################################

# load the packages
library(readxl) # read the raw datasets 
library(dplyr) # data manipulation tasks 
library(lubridate)   # date format
library(haven)  # export the sas7bdat file
library(xportr) # to assign label for each variable as per spec
library(tidyverse)
library(purrr)

#setting working directory 
getwd()


#Importing raw datasets for VS

vs_raw <- read_xpt(paste(getwd(),"raw_datasets","VS.xpt",sep="/"))
vsr_raw <- read_xpt(paste(getwd(),"raw_datasets","VSR.xpt",sep="/"))
bs_raw <- read_xpt(paste(getwd(),"raw_datasets","BS.xpt",sep="/"))

#################Deriving VSTESTCD="SYSBP" from vs_raw and vsr_raw datasets#################

vs_sbp <- vs_raw %>% mutate(STUDYID="TEST00000_C99",
                            DOMAIN="VS",
                            USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                            VSTESTCD="SYSBP",
                            VSTEST="Systolic Blood Pressure",
                            VSCAT="Vital Signs",
                            VSORRES=as.character(VSSBP),
                            VSORRESU=VSBPU,
                            VSSTRESC=VSORRES,
                            VSSTRESN=VSSBP,
                            VSSTRESU=if_else(VSORRESU !="","mmHg",""),
                            VSSTAT=case_when(VSYN=="NO" ~ "NOT DONE",
                                             VSYN=="YES" ~ ""),
                            VISITNUM=VISIT,
                            VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                  if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                            VISITDY=case_when(VISITNUM==-1 ~ -28,
                                              VISITNUM==1 ~ 1,
                                              VISITNUM==2 ~ 2,
                                              VISITNUM==3 ~ 3,
                                              VISITNUM==4 ~ 4,
                                              VISITNUM==5 ~ 5,
                                              VISITNUM==6 ~ 6,
                                              VISITNUM==7 ~ 7,
                                              VISITNUM==8 ~ 8,
                                              VISITNUM==9 ~ 9,
                                              VISITNUM==14 ~ 14),
                             VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                             VISTIM=if_else(VISTIM_=="",VSTM3,VISTIM_),
                             VISTIM_1=if_else(VISTIM !="",paste0(substr(VISTIM, 1, 2),":", substr(VISTIM, 3, 4),":","00"),""),
                             VSDTC_=if_else(VSDAT !="",paste0(substr(VSDAT, 1, 4),"-", substr(VSDAT, 5, 6),"-",substr(VSDAT, 7, 8)),""),
                             VSDT_DT=if_else(VSDTC_ !='' & VISTIM_1 !='',paste(VSDTC_,VISTIM_1,sep="-"),""),
                             VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                             VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                             VSTPT=NA ,
                             VSTPTNUM=NaN,
                             VSLOC=NA) %>% 
                             select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
                                    VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)
                             
                            

  #data from vsr_raw

vsr_sbp <- vsr_raw %>% mutate(STUDYID="TEST00000_C99",
                            DOMAIN="VS",
                            USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                            VSTESTCD="SYSBP",
                            VSTEST="Systolic Blood Pressure",
                            VSCAT="Vital Signs",
                            VSORRES=as.character(VSSBP),
                            VSORRESU=VSBPU,
                            VSSTRESC=VSORRES,
                            VSSTRESN=VSSBP,
                            VSSTRESU=if_else(VSORRESU !="","mmHg",""),
                            VSSTAT="",
                            VISITNUM=VISIT,
                            VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                           if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                            VISITDY=case_when(VISITNUM==-1 ~ -28,
                                              VISITNUM==1 ~ 1,
                                              VISITNUM==2 ~ 2,
                                              VISITNUM==3 ~ 3,
                                              VISITNUM==4 ~ 4,
                                              VISITNUM==5 ~ 5,
                                              VISITNUM==6 ~ 6,
                                              VISITNUM==7 ~ 7,
                                              VISITNUM==8 ~ 8,
                                              VISITNUM==9 ~ 9,
                                              VISITNUM==14 ~ 14),
                            VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                            
                            VISTIM_1=if_else(VISTIM_ !="",paste0(substr(VISTIM_, 1, 2),":", substr(VISTIM_, 3, 4),":","00"),""),
                            VSDTC_=dmy(VSDAT),
                            VSDTC_1=format(VSDTC_, format="%Y-%m-%d"),
                            VSDT_DT=if_else(VSDTC_1 !='' & VISTIM_1 !='',paste(VSDTC_1,VISTIM_1,sep="-"),""),
                            VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                            VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                            VSTPT=if_else(str_detect(VSTIM,"Pre AM"),"Pre-AM",
                                  if_else(str_detect(VSTIM,"2 hours"),"2H",
                                  if_else(str_detect(VSTIM,"4 hours"),"4H",
                                  if_else(str_detect(VSTIM,"6 hours"),"6H",
                                  if_else(str_detect(VSTIM,"24 hours"),"24H",
                                  if_else(str_detect(VSTIM,"48 hours"),"48H",NA)))))),
                            VSTPTNUM=case_when(VSTPT=="Pre-AM" ~ 1,
                                               VSTPT=="2H" ~ 4,
                                               VSTPT=="4H" ~ 5,
                                               VSTPT=="6H" ~ 6,
                                               VSTPT=="24H" ~ 12,
                                               VSTPT=="48H" ~ 14),
                            VSLOC=NA) %>% 
                            select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
                                   VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)

   #setting vs_sbp abd vsr_sbp to get all data

vs_sbp_all <- rbind(vs_sbp,vsr_sbp) %>% arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM) %>% 
                                        mutate(VSBLFL=case_when(VISITNUM <= 2 & VSTPT=="Pre-AM" ~ "Y"))


##################Deriving VSTESTCD="DIABP" from vs_raw and vsr_raw datasets###################

vs_dbp <- vs_raw %>% mutate(STUDYID="TEST00000_C99",
                            DOMAIN="VS",
                            USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                            VSTESTCD="DIABP",
                            VSTEST="Diastolic Blood Pressure",
                            VSCAT="Vital Signs",
                            VSORRES=as.character(VSDBP),
                            VSORRESU=VSBPU,
                            VSSTRESC=VSORRES,
                            VSSTRESN=VSDBP,
                            VSSTRESU=if_else(VSORRESU !="","mmHg",""),
                            VSSTAT=case_when(VSYN=="NO" ~ "NOT DONE",
                                             VSYN=="YES" ~ ""),
                            VISITNUM=VISIT,
                            VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                           if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                            VISITDY=case_when(VISITNUM==-1 ~ -28,
                                              VISITNUM==1 ~ 1,
                                              VISITNUM==2 ~ 2,
                                              VISITNUM==3 ~ 3,
                                              VISITNUM==4 ~ 4,
                                              VISITNUM==5 ~ 5,
                                              VISITNUM==6 ~ 6,
                                              VISITNUM==7 ~ 7,
                                              VISITNUM==8 ~ 8,
                                              VISITNUM==9 ~ 9,
                                              VISITNUM==14 ~ 14),
                            VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                            VISTIM=if_else(VISTIM_=="",VSTM3,VISTIM_),
                            VISTIM_1=if_else(VISTIM !="",paste0(substr(VISTIM, 1, 2),":", substr(VISTIM, 3, 4),":","00"),""),
                            VSDTC_=if_else(VSDAT !="",paste0(substr(VSDAT, 1, 4),"-", substr(VSDAT, 5, 6),"-",substr(VSDAT, 7, 8)),""),
                            VSDT_DT=if_else(VSDTC_ !='' & VISTIM_1 !='',paste(VSDTC_,VISTIM_1,sep="-"),""),
                            VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                            VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                            VSTPT=NA ,
                            VSTPTNUM=NaN,
                            VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)



#data from vsr_raw

vsr_dbp <- vsr_raw %>% mutate(STUDYID="TEST00000_C99",
                              DOMAIN="VS",
                              USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                              VSTESTCD="DIABP",
                              VSTEST="Diastolic Blood Pressure",
                              VSCAT="Vital Signs",
                              VSORRES=as.character(VSDBP),
                              VSORRESU=VSBPU,
                              VSSTRESC=VSORRES,
                              VSSTRESN=VSDBP,
                              VSSTRESU=if_else(VSORRESU !="","mmHg",""),
                              VSSTAT="",
                              VISITNUM=VISIT,
                              VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                             if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                              VISITDY=case_when(VISITNUM==-1 ~ -28,
                                                VISITNUM==1 ~ 1,
                                                VISITNUM==2 ~ 2,
                                                VISITNUM==3 ~ 3,
                                                VISITNUM==4 ~ 4,
                                                VISITNUM==5 ~ 5,
                                                VISITNUM==6 ~ 6,
                                                VISITNUM==7 ~ 7,
                                                VISITNUM==8 ~ 8,
                                                VISITNUM==9 ~ 9,
                                                VISITNUM==14 ~ 14),
                              VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                              
                              VISTIM_1=if_else(VISTIM_ !="",paste0(substr(VISTIM_, 1, 2),":", substr(VISTIM_, 3, 4),":","00"),""),
                              VSDTC_=dmy(VSDAT),
                              VSDTC_1=format(VSDTC_, format="%Y-%m-%d"),
                              VSDT_DT=if_else(VSDTC_1 !='' & VISTIM_1 !='',paste(VSDTC_1,VISTIM_1,sep="-"),""),
                              VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                              VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                              VSTPT=if_else(str_detect(VSTIM,"Pre AM"),"Pre-AM",
                                            if_else(str_detect(VSTIM,"2 hours"),"2H",
                                                    if_else(str_detect(VSTIM,"4 hours"),"4H",
                                                            if_else(str_detect(VSTIM,"6 hours"),"6H",
                                                                    if_else(str_detect(VSTIM,"24 hours"),"24H",
                                                                            if_else(str_detect(VSTIM,"48 hours"),"48H",NA)))))),
                              VSTPTNUM=case_when(VSTPT=="Pre-AM" ~ 1,
                                                 VSTPT=="2H" ~ 4,
                                                 VSTPT=="4H" ~ 5,
                                                 VSTPT=="6H" ~ 6,
                                                 VSTPT=="24H" ~ 12,
                                                 VSTPT=="48H" ~ 14),
                              VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)

#setting vs_dbp abd vsr_dbp to get all data

vs_dbp_all <- rbind(vs_dbp,vsr_dbp) %>% arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM) %>% 
  mutate(VSBLFL=case_when(VISITNUM <= 2 & VSTPT=="Pre-AM" ~ "Y"))

#####################Deriving VSTESTCD="RESP"#############################

vs_resp <- vs_raw %>% mutate(STUDYID="TEST00000_C99",
                            DOMAIN="VS",
                            USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                            VSTESTCD="RESP",
                            VSTEST="Respiratory Rate",
                            VSCAT="Vital Signs",
                            VSORRES=as.character(VSRESP),
                            VSORRESU=VSRESPU,
                            VSSTRESC=VSORRES,
                            VSSTRESN=VSRESP,
                            VSSTRESU=if_else(VSORRESU !="","BREATHS/MIN",""),
                            VSSTAT=case_when(VSYN=="NO" ~ "NOT DONE",
                                             VSYN=="YES" ~ ""),
                            VISITNUM=VISIT,
                            VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                           if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                            VISITDY=case_when(VISITNUM==-1 ~ -28,
                                              VISITNUM==1 ~ 1,
                                              VISITNUM==2 ~ 2,
                                              VISITNUM==3 ~ 3,
                                              VISITNUM==4 ~ 4,
                                              VISITNUM==5 ~ 5,
                                              VISITNUM==6 ~ 6,
                                              VISITNUM==7 ~ 7,
                                              VISITNUM==8 ~ 8,
                                              VISITNUM==9 ~ 9,
                                              VISITNUM==14 ~ 14),
                            VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                            VISTIM=if_else(VISTIM_=="",VSTM3,VISTIM_),
                            VISTIM_1=if_else(VISTIM !="",paste0(substr(VISTIM, 1, 2),":", substr(VISTIM, 3, 4),":","00"),""),
                            VSDTC_=if_else(VSDAT !="",paste0(substr(VSDAT, 1, 4),"-", substr(VSDAT, 5, 6),"-",substr(VSDAT, 7, 8)),""),
                            VSDT_DT=if_else(VSDTC_ !='' & VISTIM_1 !='',paste(VSDTC_,VISTIM_1,sep="-"),""),
                            VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                            VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                            VSTPT=NA ,
                            VSTPTNUM=NaN,
                            VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)



#data from vsr_raw

vsr_resp <- vsr_raw %>% mutate(STUDYID="TEST00000_C99",
                              DOMAIN="VS",
                              USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                              VSTESTCD="RESP",
                              VSTEST="Respiratory Rate",
                              VSCAT="Vital Signs",
                              VSORRES=as.character(VSRESP),
                              VSORRESU=VSRESPU,
                              VSSTRESC=VSORRES,
                              VSSTRESN=VSRESP,
                              VSSTRESU=if_else(VSORRESU !="","BREATHS/MIN",""),
                              VSSTAT="",
                              VISITNUM=VISIT,
                              VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                             if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                              VISITDY=case_when(VISITNUM==-1 ~ -28,
                                                VISITNUM==1 ~ 1,
                                                VISITNUM==2 ~ 2,
                                                VISITNUM==3 ~ 3,
                                                VISITNUM==4 ~ 4,
                                                VISITNUM==5 ~ 5,
                                                VISITNUM==6 ~ 6,
                                                VISITNUM==7 ~ 7,
                                                VISITNUM==8 ~ 8,
                                                VISITNUM==9 ~ 9,
                                                VISITNUM==14 ~ 14),
                              VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                              
                              VISTIM_1=if_else(VISTIM_ !="",paste0(substr(VISTIM_, 1, 2),":", substr(VISTIM_, 3, 4),":","00"),""),
                              VSDTC_=dmy(VSDAT),
                              VSDTC_1=format(VSDTC_, format="%Y-%m-%d"),
                              VSDT_DT=if_else(VSDTC_1 !='' & VISTIM_1 !='',paste(VSDTC_1,VISTIM_1,sep="-"),""),
                              VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                              VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                              VSTPT=if_else(str_detect(VSTIM,"Pre AM"),"Pre-AM",
                                            if_else(str_detect(VSTIM,"2 hours"),"2H",
                                                    if_else(str_detect(VSTIM,"4 hours"),"4H",
                                                            if_else(str_detect(VSTIM,"6 hours"),"6H",
                                                                    if_else(str_detect(VSTIM,"24 hours"),"24H",
                                                                            if_else(str_detect(VSTIM,"48 hours"),"48H",NA)))))),
                              VSTPTNUM=case_when(VSTPT=="Pre-AM" ~ 1,
                                                 VSTPT=="2H" ~ 4,
                                                 VSTPT=="4H" ~ 5,
                                                 VSTPT=="6H" ~ 6,
                                                 VSTPT=="24H" ~ 12,
                                                 VSTPT=="48H" ~ 14),
                              VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)

#setting vs_resp abd vsr_resp to get all data

vs_resp_all <- rbind(vs_resp,vsr_resp) %>% arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM) %>% 
  mutate(VSBLFL=case_when(VISITNUM <= 2 & VSTPT=="Pre-AM" ~ "Y"))

#####################Deriving VSTESTCD="TEMP"#############################

vs_temp <- vs_raw %>% mutate(STUDYID="TEST00000_C99",
                             DOMAIN="VS",
                             USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                             VSTESTCD="TEMP",
                             VSTEST="Temperature",
                             VSCAT="Vital Signs",
                             VSORRES=as.character(VSTEMP),
                             VSORRESU=VSTEMPU,
                             VSSTRESC=VSORRES,
                             VSSTRESN=VSTEMP,
                             VSSTRESU=if_else(VSORRESU !="","C",""),
                             VSSTAT=case_when(VSYN=="NO" ~ "NOT DONE",
                                              VSYN=="YES" ~ ""),
                             VISITNUM=VISIT,
                             VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                            if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                             VISITDY=case_when(VISITNUM==-1 ~ -28,
                                               VISITNUM==1 ~ 1,
                                               VISITNUM==2 ~ 2,
                                               VISITNUM==3 ~ 3,
                                               VISITNUM==4 ~ 4,
                                               VISITNUM==5 ~ 5,
                                               VISITNUM==6 ~ 6,
                                               VISITNUM==7 ~ 7,
                                               VISITNUM==8 ~ 8,
                                               VISITNUM==9 ~ 9,
                                               VISITNUM==14 ~ 14),
                             VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                             VISTIM=if_else(VISTIM_=="",VSTM3,VISTIM_),
                             VISTIM_1=if_else(VISTIM !="",paste0(substr(VISTIM, 1, 2),":", substr(VISTIM, 3, 4),":","00"),""),
                             VSDTC_=if_else(VSDAT !="",paste0(substr(VSDAT, 1, 4),"-", substr(VSDAT, 5, 6),"-",substr(VSDAT, 7, 8)),""),
                             VSDT_DT=if_else(VSDTC_ !='' & VISTIM_1 !='',paste(VSDTC_,VISTIM_1,sep="-"),""),
                             VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                             VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                             VSTPT=NA ,
                             VSTPTNUM=NaN,
                             VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)



#data from vsr_raw

vsr_temp <- vsr_raw %>% mutate(STUDYID="TEST00000_C99",
                               DOMAIN="VS",
                               USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                               VSTESTCD="TEMP",
                               VSTEST="Temperature",
                               VSCAT="Vital Signs",
                               VSORRES=as.character(VSTEMP),
                               VSORRESU=VSTEMPU,
                               VSSTRESC=VSORRES,
                               VSSTRESN=VSTEMP,
                               VSSTRESU=if_else(VSORRESU !="","C",""),
                               VSSTAT="",
                               VISITNUM=VISIT,
                               VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                              if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                               VISITDY=case_when(VISITNUM==-1 ~ -28,
                                                 VISITNUM==1 ~ 1,
                                                 VISITNUM==2 ~ 2,
                                                 VISITNUM==3 ~ 3,
                                                 VISITNUM==4 ~ 4,
                                                 VISITNUM==5 ~ 5,
                                                 VISITNUM==6 ~ 6,
                                                 VISITNUM==7 ~ 7,
                                                 VISITNUM==8 ~ 8,
                                                 VISITNUM==9 ~ 9,
                                                 VISITNUM==14 ~ 14),
                               VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                               
                               VISTIM_1=if_else(VISTIM_ !="",paste0(substr(VISTIM_, 1, 2),":", substr(VISTIM_, 3, 4),":","00"),""),
                               VSDTC_=dmy(VSDAT),
                               VSDTC_1=format(VSDTC_, format="%Y-%m-%d"),
                               VSDT_DT=if_else(VSDTC_1 !='' & VISTIM_1 !='',paste(VSDTC_1,VISTIM_1,sep="-"),""),
                               VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                               VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                               VSTPT=if_else(str_detect(VSTIM,"Pre AM"),"Pre-AM",
                                             if_else(str_detect(VSTIM,"2 hours"),"2H",
                                                     if_else(str_detect(VSTIM,"4 hours"),"4H",
                                                             if_else(str_detect(VSTIM,"6 hours"),"6H",
                                                                     if_else(str_detect(VSTIM,"24 hours"),"24H",
                                                                             if_else(str_detect(VSTIM,"48 hours"),"48H",NA)))))),
                               VSTPTNUM=case_when(VSTPT=="Pre-AM" ~ 1,
                                                  VSTPT=="2H" ~ 4,
                                                  VSTPT=="4H" ~ 5,
                                                  VSTPT=="6H" ~ 6,
                                                  VSTPT=="24H" ~ 12,
                                                  VSTPT=="48H" ~ 14),
                               VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)

#setting vs_temp abd vsr_temp to get all data

vs_temp_all <- rbind(vs_temp,vsr_temp) %>% arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM) %>% 
  mutate(VSBLFL=case_when(VISITNUM <= 2 & VSTPT=="Pre-AM" ~ "Y"))


#####################Deriving VSTESTCD="HR"#############################

vs_hr <- vs_raw %>% mutate(STUDYID="TEST00000_C99",
                             DOMAIN="VS",
                             USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                             VSTESTCD="HR",
                             VSTEST="Heart Rate",
                             VSCAT="Vital Signs",
                             VSORRES=as.character(VSPULS),
                             VSORRESU=VSPULSU,
                             VSSTRESC=VSORRES,
                             VSSTRESN=VSPULS,
                             VSSTRESU=if_else(VSORRESU !="","BEATS/MIN",""),
                             VSSTAT=case_when(VSYN=="NO" ~ "NOT DONE",
                                              VSYN=="YES" ~ ""),
                             VISITNUM=VISIT,
                             VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                            if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                             VISITDY=case_when(VISITNUM==-1 ~ -28,
                                               VISITNUM==1 ~ 1,
                                               VISITNUM==2 ~ 2,
                                               VISITNUM==3 ~ 3,
                                               VISITNUM==4 ~ 4,
                                               VISITNUM==5 ~ 5,
                                               VISITNUM==6 ~ 6,
                                               VISITNUM==7 ~ 7,
                                               VISITNUM==8 ~ 8,
                                               VISITNUM==9 ~ 9,
                                               VISITNUM==14 ~ 14),
                             VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                             VISTIM=if_else(VISTIM_=="",VSTM3,VISTIM_),
                             VISTIM_1=if_else(VISTIM !="",paste0(substr(VISTIM, 1, 2),":", substr(VISTIM, 3, 4),":","00"),""),
                             VSDTC_=if_else(VSDAT !="",paste0(substr(VSDAT, 1, 4),"-", substr(VSDAT, 5, 6),"-",substr(VSDAT, 7, 8)),""),
                             VSDT_DT=if_else(VSDTC_ !='' & VISTIM_1 !='',paste(VSDTC_,VISTIM_1,sep="-"),""),
                             VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                             VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                             VSTPT=NA ,
                             VSTPTNUM=NaN,
                             VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)



#data from vsr_raw

vsr_hr <- vsr_raw %>% mutate(STUDYID="TEST00000_C99",
                               DOMAIN="VS",
                               USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                               VSTESTCD="HR",
                               VSTEST="Heart Rate",
                               VSCAT="Vital Signs",
                               VSORRES=as.character(VSPULS),
                               VSORRESU=VSPULSU,
                               VSSTRESC=VSORRES,
                               VSSTRESN=VSPULS,
                               VSSTRESU=if_else(VSORRESU !="","BEATS/MIN",""),
                               VSSTAT="",
                               VISITNUM=VISIT,
                               VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                              if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                               VISITDY=case_when(VISITNUM==-1 ~ -28,
                                                 VISITNUM==1 ~ 1,
                                                 VISITNUM==2 ~ 2,
                                                 VISITNUM==3 ~ 3,
                                                 VISITNUM==4 ~ 4,
                                                 VISITNUM==5 ~ 5,
                                                 VISITNUM==6 ~ 6,
                                                 VISITNUM==7 ~ 7,
                                                 VISITNUM==8 ~ 8,
                                                 VISITNUM==9 ~ 9,
                                                 VISITNUM==14 ~ 14),
                               VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                               
                               VISTIM_1=if_else(VISTIM_ !="",paste0(substr(VISTIM_, 1, 2),":", substr(VISTIM_, 3, 4),":","00"),""),
                               VSDTC_=dmy(VSDAT),
                               VSDTC_1=format(VSDTC_, format="%Y-%m-%d"),
                               VSDT_DT=if_else(VSDTC_1 !='' & VISTIM_1 !='',paste(VSDTC_1,VISTIM_1,sep="-"),""),
                               VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                               VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                               VSTPT=if_else(str_detect(VSTIM,"Pre AM"),"Pre-AM",
                                             if_else(str_detect(VSTIM,"2 hours"),"2H",
                                                     if_else(str_detect(VSTIM,"4 hours"),"4H",
                                                             if_else(str_detect(VSTIM,"6 hours"),"6H",
                                                                     if_else(str_detect(VSTIM,"24 hours"),"24H",
                                                                             if_else(str_detect(VSTIM,"48 hours"),"48H",NA)))))),
                               VSTPTNUM=case_when(VSTPT=="Pre-AM" ~ 1,
                                                  VSTPT=="2H" ~ 4,
                                                  VSTPT=="4H" ~ 5,
                                                  VSTPT=="6H" ~ 6,
                                                  VSTPT=="24H" ~ 12,
                                                  VSTPT=="48H" ~ 14),
                               VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)

#setting vs_hr abd vsr_hr to get all data

vs_hr_all <- rbind(vs_hr,vsr_hr) %>% arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM) %>% 
  mutate(VSBLFL=case_when(VISITNUM <= 2 & VSTPT=="Pre-AM" ~ "Y"))


#####################Deriving VSTESTCD="HEIGHT"#############################

vs_ht <- vs_raw %>% mutate(STUDYID="TEST00000_C99",
                           DOMAIN="VS",
                           USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                           VSTESTCD="HEIGHT",
                           VSTEST="Height",
                           VSCAT="Vital Signs",
                           VSORRES=as.character(VSHT),
                           VSORRESU=VSHTU,
                           VSSTRESC=VSORRES,
                           VSSTRESN=VSHT,
                           VSSTRESU=if_else(VSORRESU !="","cm",""),
                           VSSTAT=case_when(VSYN=="NO" ~ "NOT DONE",
                                            VSYN=="YES" ~ ""),
                           VISITNUM=VISIT,
                           VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                          if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                           VISITDY=case_when(VISITNUM==-1 ~ -28,
                                             VISITNUM==1 ~ 1,
                                             VISITNUM==2 ~ 2,
                                             VISITNUM==3 ~ 3,
                                             VISITNUM==4 ~ 4,
                                             VISITNUM==5 ~ 5,
                                             VISITNUM==6 ~ 6,
                                             VISITNUM==7 ~ 7,
                                             VISITNUM==8 ~ 8,
                                             VISITNUM==9 ~ 9,
                                             VISITNUM==14 ~ 14),
                           VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                           VISTIM=if_else(VISTIM_=="",VSTM3,VISTIM_),
                           VISTIM_1=if_else(VISTIM !="",paste0(substr(VISTIM, 1, 2),":", substr(VISTIM, 3, 4),":","00"),""),
                           VSDTC_=if_else(VSDAT !="",paste0(substr(VSDAT, 1, 4),"-", substr(VSDAT, 5, 6),"-",substr(VSDAT, 7, 8)),""),
                           VSDT_DT=if_else(VSDTC_ !='' & VISTIM_1 !='',paste(VSDTC_,VISTIM_1,sep="-"),""),
                           VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                           VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                           VSTPT=NA ,
                           VSTPTNUM=NaN,
                           VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)

#setting vs_hr abd vsr_hr to get all data

vs_ht_all <- vs_ht %>% arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM) %>% 
  mutate(VSBLFL=case_when(VISITNUM <= 2 & VSTPT=="Pre-AM" ~ "Y")) %>% subset(VISIT_ =="SCREENING")



#####################Deriving VSTESTCD="WEIGHT"#############################

vs_wt <- vs_raw %>% mutate(STUDYID="TEST00000_C99",
                           DOMAIN="VS",
                           USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                           VSTESTCD="WEIGHT",
                           VSTEST="Weight",
                           VSCAT="Vital Signs",
                           VSORRES=as.character(VSWT),
                           VSORRESU=VSWTU,
                           VSSTRESC=VSORRES,
                           VSSTRESN=VSWT,
                           VSSTRESU=if_else(VSORRESU !="","kg",""),
                           VSSTAT=case_when(VSYN=="NO" ~ "NOT DONE",
                                            VSYN=="YES" ~ ""),
                           VISITNUM=VISIT,
                           VISIT_=if_else(CPEVENT=="FOLWUP1 +/-2DAYS","FOLLOW-UP 1",
                                          if_else(CPEVENT=="UNSCH1","UNSCHEDULED 1",CPEVENT)),
                           VISITDY=case_when(VISITNUM==-1 ~ -28,
                                             VISITNUM==1 ~ 1,
                                             VISITNUM==2 ~ 2,
                                             VISITNUM==3 ~ 3,
                                             VISITNUM==4 ~ 4,
                                             VISITNUM==5 ~ 5,
                                             VISITNUM==6 ~ 6,
                                             VISITNUM==7 ~ 7,
                                             VISITNUM==8 ~ 8,
                                             VISITNUM==9 ~ 9,
                                             VISITNUM==14 ~ 14),
                           VISTIM_=if_else(VSTM1=="",VSTM2,VSTM1),
                           VISTIM=if_else(VISTIM_=="",VSTM3,VISTIM_),
                           VISTIM_1=if_else(VISTIM !="",paste0(substr(VISTIM, 1, 2),":", substr(VISTIM, 3, 4),":","00"),""),
                           VSDTC_=if_else(VSDAT !="",paste0(substr(VSDAT, 1, 4),"-", substr(VSDAT, 5, 6),"-",substr(VSDAT, 7, 8)),""),
                           VSDT_DT=if_else(VSDTC_ !='' & VISTIM_1 !='',paste(VSDTC_,VISTIM_1,sep="-"),""),
                           VSTDT_DT1=as.POSIXct(VSDT_DT, format = "%Y-%m-%d-%H:%M"),
                           VSDTC=format(VSTDT_DT1, format = "%Y-%m-%dT%H:%M:%S"),
                           VSTPT=NA ,
                           VSTPTNUM=NaN,
                           VSLOC=NA) %>% 
  select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,
         VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM)

#setting vs_hr abd vsr_hr to get all data

vs_wt_all <- vs_wt %>% arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM) %>% 
  mutate(VSBLFL=case_when(VISITNUM <= 2 & VSTPT=="Pre-AM" ~ "Y")) %>% subset(VISIT_ =="SCREENING")


#####################Deriving VSTESTCD="TOTBSA"#############################

vs_bsa <- bs_raw %>% mutate(STUDYID="TEST00000_C99",
                           DOMAIN="VS",
                           USUBJID=paste(STUDYID,INVSITE,PT,sep="-"),
                           VSTESTCD="TOTBSA",
                           VSTEST="Total Body Surface Area Affected",
                           VSCAT="Evaluation of BSA",
                           VSORRES=as.character(BSATOT),
                           VSORRESU="",
                           VSSTRESC=VSORRES,
                           VSSTRESN=BSATOT,
                           VSSTRESU="",
                           VSSTAT="",
                           VISITNUM=if_else(CPEVENT=="SCREENING",-1,1),
                           VISIT_=if_else(CPEVENT=="DAY - 1","DAY 1","SCREENING"),
                           VISITDY=case_when(VISITNUM==-1 ~ -28,
                                             VISITNUM==1 ~ 1),
                           VSDTC=NA,
                           VSTPT=NA ,
                           VSTPTNUM=NaN)%>% 
                           select(STUDYID,DOMAIN,USUBJID,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,
                           VISITNUM,VISIT_,VISITDY,VSDTC,VSTPT,VSTPTNUM,PT,CPEVENT)

#Deriving VSLOC

vsloc <-bs_raw %>%  select(PT,BSARMN,BSFACN,BSLEGN,BSSCPN,BSTBKN,BSTFRN,CPEVENT) %>% 
        rename(col1=BSARMN, col2=BSFACN, col3=BSLEGN, col4=BSSCPN, col5=BSTBKN,col6=BSTFRN) %>% 
  rowwise() %>%
  mutate(VSLOC_ = sum(c_across(col1:col6),na.rm=TRUE),
         col1_=if_else(col1 == 1,"ARM",NA),
         col2_=if_else(col2 == 1,"FACE",NA),
         col3_=if_else(col3 == 1,"LEG",NA),
         col4_=if_else(col4 == 1,"SCALP",NA),
         col5_=if_else(col5 == 1,"TRUNK BACK",NA),
         col6_=if_else(col6 == 1,"TRUNK FRONT",NA),
         VSLOC=if_else(VSLOC_ > 1,"MULTIPLE",coalesce(col1_,col2_,col3_,col4_,col5_,col6_))) %>% 
         select(PT,CPEVENT,VSLOC)
         
#merging VSLOC with main dataset vs_bsa

vs_bsa_all <- merge(vs_bsa,vsloc,by=c("PT","CPEVENT"),all.x=TRUE) %>% mutate(VSBLFL=NA) %>% 
              select(-PT,-CPEVENT)

#############setting all VSTESTCD together##############

vs_1 <- rbind(vs_sbp_all,vs_dbp_all,vs_resp_all,vs_hr_all,vs_temp_all,vs_ht_all,vs_wt_all,vs_bsa_all)


#loading dm dataset to get rfstdtc

dm_rf <- read_sas("DM.sas7bdat") %>% select(USUBJID,RFSTDTC) %>% mutate(rfstdtc_=as.Date(substring(RFSTDTC,1,10)))

#merging dm_rf with ae_data to get VSDY 

arrange(dm_rf,USUBJID)
arrange(vs_1,USUBJID)

vs_all1 <- merge(vs_1,dm_rf,by="USUBJID",all.x=TRUE) %>% mutate(vsdtc_=as.Date(substring(VSDTC,1,10)))

vs_all2 <- vs_all1 %>% mutate(VSDY_=case_when(vsdtc_ < rfstdtc_ ~ vsdtc_ - rfstdtc_, 
                                              vsdtc_ >= rfstdtc_ ~ vsdtc_ - rfstdtc_  + 1),
                              VSDY=as.numeric(as.character(VSDY_)))

#Derivation for AESEQ as per key variables in metadata
vs_all3 <- vs_all2 %>% group_by(USUBJID) %>% 
  arrange(STUDYID, USUBJID, VSCAT, VSTESTCD, VISITNUM, VSTPTNUM, .by_group = TRUE) %>% 
  mutate(VSSEQ = as.numeric(seq(1:n()))) %>% rename(VISIT=VISIT_) %>% select(STUDYID,DOMAIN,USUBJID,VSSEQ,VSTESTCD,VSTEST,VSCAT,VSORRES,VSORRESU,VSSTRESC,VSSTRESN,VSSTRESU,VSSTAT,VSLOC,VSBLFL,
                                                                             VISITNUM,VISIT,VISITDY,VSDTC,VSDY,VSTPT,VSTPTNUM)

#Importing VS spec for attributes assignment

imort_xlsx_meta <- function (name){
  file <- read_xlsx(paste(getwd(),"02_SDTM_programming_specification.xlsm",sep="/"),col_names = TRUE,sheet=name)
  return(file)
}

vs_metadata <- data.frame(imort_xlsx_meta("VS")) %>% mutate(variable=Variable.Name) %>% 
  mutate(label=Variable.Label) %>% mutate(dataset=Domain) %>% mutate(length=as.character(Length)) %>% 
  select(dataset,variable,label,length)                             

vs_final <- xportr_label(vs_all3,vs_metadata,domain = "VS") #getting labels 
vs_final_1 <- suppressWarnings(xportr_length(vs_final,vs_metadata,domain = "VS",length_source="metadata")) #getting lengths


#exporting final dataset into xpt and sas7bdat format

write_xpt(vs_final_1,"vs.xpt")
write_sas(vs_final_1,"vs.sas7bdat")


#End of Programming       