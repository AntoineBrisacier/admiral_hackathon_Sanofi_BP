
library(diffdf)

#ADSL----
adsl_cdisc <- read_xpt("/cloud/project/adam/cdi/adsl.xpt") %>% rename(TRTDURD=TRTDUR, DCSREAS=DCREASCD)
adsl_xpt   <- read_xpt("/cloud/project/adsl.xpt")
diffdf( adsl_cdisc,adsl_xpt,c("USUBJID"))

#ADBLC----
adlbc_cdisc <- read_xpt("/cloud/project/adam/cdi/adlbc.xpt")%>%
  arrange (USUBJID, PARAMCD, AVISITN)%>%
  filter (USUBJID=="01-701-1015" & PARAMCD %in% c ('ALT', '_ALT'))



adlbc_xpt <- read_xpt("/cloud/project/adam/adlbc.xpt") %>% arrange (USUBJID, PARAMCD, AVISITN)

#ADBLHY----
adlbhy_cdisc <- read_xpt("/cloud/project/adam/cdi/adlbhy.xpt") %>% filter( USUBJID == "01-701-1033")
adlbhy_xpt <- read_xpt("/cloud/project/adam/adlbhy.xpt")

adlbhy_cdisc <- read_xpt("/cloud/project/adam/cdi/adlbhy.xpt")
table (adlbhy_cdisc$CRIT1)
table (adlbc_cdisc$PARAM)


#ADLBC----
adlbc_cdisc <- read_xpt("./adam/cdi/adlbc.xpt") %>% arrange(USUBJID,PARAMCD,ADT)

adlb_check1 <- adlbc %>%  filter (USUBJID == '01-709-1312' & PARAMCD=='ALB')
adlb_check2 <- adlbc %>%  filter (USUBJID == '01-701-1047' & PARAMCD=='ALB')


table (adlbc_cdisc$PARAMCD)
table (adlbc_cdisc$PARAM)
