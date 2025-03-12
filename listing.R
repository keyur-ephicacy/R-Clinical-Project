############################################################
# Name of Programmer:- Keyur Patel
# Date              :- 04DEC2024
# Purpose           :- To Create ADAE Listig 
############################################################
# load the packages

library(dplyr) # data manipulation tasks 
library(r2rtf) # reporting rtf file 

###reading required data from adae dataset
adae <- read_sas("adae.sas7bdat")

ae1 <- adae |> select(USUBJID,AEBODSYS) |> 
  group_by(USUBJID,AEBODSYS) |> slice_head(n=1) |> mutate(ord=1)


ae2 <- adae |> select(USUBJID,AEBODSYS,AEDECOD,AESEV,ASTDT,AENDT,TRTSDT) |> 
              group_by(USUBJID,AEBODSYS,AEDECOD,AESEV,ASTDT,AENDT) |> slice_head(n=1) 

ae3 <- rbind(ae1,ae2) |>  arrange(USUBJID,AEBODSYS,ord,AEDECOD) |> ungroup() |> 
                          mutate(AEBODSYS=(if_else(!is.na(AEDECOD),paste0("  ",AEDECOD),AEBODSYS)))

ae4 <- ae3 |>group_by(USUBJID)  |>  mutate(USUBJID = ifelse(row_number() == 1, USUBJID, ""),
                                          ASTDY= ASTDT - TRTSDT,
                                          AENDY= AENDT - TRTSDT,
                                          ASTDT=format(ASTDT,"%d-%b-%Y"),
                                          AENDT=format(AENDT,"%d-%b-%Y"),
                                          ) |> 
             select(USUBJID,AEBODSYS,AESEV,ASTDT,AENDT,ASTDY,AENDY) |> ungroup()

####Creating rtf file using r2rtf library

rtf_page(ae4,orientation = "landscape",
         border_first = "single",
         border_last = "single") |> 
  
       rtf_title(title="Listing 1: Adverse Events by System organ class and Preferred Term - Safety population",
                 text_justification = "l",
                 text_font_size = 8) |> 

       rtf_colheader(colheader = "Unique Subject Identifier | System Organ Class/Preffered Term | Severity/Intensity | Start Date/Time of Adverse Event | End Date/Time of Adverse Event | Study Day of Start of Adverse Event | Study Day of End of Adverse Event",
                     border_top=rep("single",7),
                     border_right=rep("single",7),
                     border_left=rep("single",7),
                     col_rel_width = c(0.5,1,0.6,0.5,0.5,0.5,0.5),
                     text_justification = c("c",rep("l",6)),
                     text_format="b"
                     ) |> 
                       
        rtf_body(as_colheader = F,
                col_rel_width = c(0.5,1,0.6,0.5,0.5,0.5,0.5),
                text_justification = c("c",rep("l",6)),
                border_right=rep("single",7),
                border_left=rep("single",7),
                border_top=rep("single",7)
                     )|> 
        rtf_encode() |> 
  
  write_rtf("ae_listing.rtf")
  

#####End of programming#######
     