# ADSL Prog ----

##Label: Subject Level Analysis Dataset

## Libraries loading----

library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)
library(lubridate)
library(stringr)

## Input datasets reading----

dm <- read_xpt("sdtm/dm.xpt")
vs <- read_xpt("sdtm/vs.xpt")
mh <- read_xpt("sdtm/mh.xpt")
sv <- read_xpt("sdtm/sv.xpt")
sc <- read_xpt("sdtm/sc.xpt")
qs <- read_xpt("sdtm/qs.xpt")
ds <- read_xpt("sdtm/ds.xpt")
ex <- read_xpt("sdtm/ex.xpt")

## Convert blanks to NA----
dm <- convert_blanks_to_na(dm)
vs <- convert_blanks_to_na(vs)
mh <- convert_blanks_to_na(mh)
sv <- convert_blanks_to_na(sv)
sc <- convert_blanks_to_na(sc)
qs <- convert_blanks_to_na(qs)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)


## Functions----

format_agegr1n <- function(x) {
  case_when(
    x < 65 ~ 1,
    between(x, 65, 80) ~ 2,
    x > 80 ~ 3
  )
}

format_agegr1 <- function(x) {
  case_when(
    x == 1 ~ "<65",
    x == 2 ~ "65-80",
    x == 3 ~ ">80",
    TRUE ~ "Missing"
  )
}


format_racen <- function(x) {
  case_when(
    x== "AMERICAN INDIAN OR ALASKA NATIVE" ~ 1,
    x== "ASIAN" ~ 2,
    x== "BLACK OR AFRICAN AMERICAN" ~ 3,
    x== "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x== "WHITE" ~ 6,
        TRUE ~ 999999
  )
}


format_durdis <- function(x) {
  case_when(
    x < 12 ~ "<12",
    x>=12 ~ ">=12",
    TRUE ~ "Missing"
  )
}

format_bmiblgr1 <- function(x) {
  case_when(
    x < 25 ~ "<25",
    x >= 25 & x < 30 ~ "25-<30",
    x >= 30 ~ ">=30",
    TRUE ~ ""
  )
}

format_armn <- function(x) {
  case_when(
    x=="Placebo" ~ 0,
    x=="Xanomeline Low Dose" ~ 54,
    x=="Xanomeline High Dose" ~ 81
  )
}

format_dctreas <- function(x) {
  case_when(
  x=="ADVERSE EVENT" ~ "Adverse Event",
  x=="COMPLETED" ~ "",
  x=="DEATH" ~ "Death",
  x=="I/E NOT MET" ~ "I/E Not Met",
  x=="LACK OF EFFICACY" ~ "Lack of Efficacy",
  x=="LOST TO FOLLOW-UP" ~ "Lost to Follow-up",
  x=="PHYSICIAN DECISION" ~ "Physician Decision",
  x=="PROTOCOL VIOLATION" ~ "Protocol Violation",
  x== "STUDY TERMINATED BY SPONSOR" ~ "Sponsor Decision",
  x=="WITHDRAWAL BY SUBJECT" ~ "Withdrew Consent",
  TRUE ~ "TERM NOT IMPLEMENTED"
  )
}


#####PIGE PAS CETTE FONCTION
format_dcsreas <- function(x, y = NULL) {
  if (is.null(y)) {
    if_else(!x %in% c("SCREEN FAILURE") & !is.na(x), x, NA_character_)
  } else {
    if_else(x == "OTHER", y, NA_character_)
  }
}

## Remove screen failure ----
adsl <- dm %>% filter(ARM !='Screen Failure') %>%

## DM Predecessors----
  select(AGE, AGEU, ARM, ARMCD, DTHFL, ETHNIC, RACE, RFSTDTC, SEX, SITEID,
         STUDYID, SUBJID, USUBJID, RFENDTC_=RFENDTC)%>%

## Derivations----
### TRT01P(N) / TRT01A(N) AGEGR1(N) /RACEN ----
          mutate(TRT01P  = ARM,
                 TRT01PN = format_armn(ARM),
                 TRT01A  = ARM,
                 ARMN    = format_armn(ARM),
                 TRT01AN = format_armn(ARM),
                 AGEGR1N = format_agegr1n(AGE),
                 AGEGR1  = format_agegr1(AGEGR1N),
                 RACEN   = format_racen(RACE)
                 )

###EOSSTT / DCDECOD----
adsl <- adsl %>%
  derive_var_disposition_status(
    dataset_ds = ds,
    new_var = EOSSTT,
    status_var = DSDECOD,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  ) %>%
  derive_vars_merged(
    dataset_add = ds,
    filter_add =  DSCAT=='DISPOSITION EVENT',
    new_vars = vars(DCDECOD = DSDECOD ),
    order = vars(USUBJID),
    mode = "last",
    by_vars = vars(USUBJID)
  )

###TRTSDT / TRTEDT / RFENDT----

  #last treatment
    # The date of final dose (from the CRF) is EX.EXENDTC on the subject's last EX record.
    # If the date of final dose is missing for the subject and the subject discontinued after visit 3,
    # use the date of discontinuation as the date of last dose. Convert the date to a SAS date.

    #retrieve disposition and format disposition event date (convert to SAS date)
ds_ext <- ds %>%
  derive_vars_dtm(
    dtc = DSDTC,
    new_vars_prefix = "DST"
    ) %>%
  derive_vars_dtm_to_dt(source_vars = vars(DSTDTM)) %>%
  filter(DSCAT=='DISPOSITION EVENT' &DSTERM != "SCREEN FAILURE") %>%
  select(USUBJID,DSTDT,DSDECOD, VISITNUM)

    #retrieve last exendtc, conversion to SAS date and replacement by date of discontinuation when exendt is missing
ex_filt <- ex %>% derive_vars_dtm(
                    dtc = EXENDTC,
                    new_vars_prefix = "EXEN"
                  )%>%
                  derive_vars_dtm_to_dt(source_vars = vars(EXENDTM))%>%
                  arrange(USUBJID,desc(VISITNUM))%>%
                  select(USUBJID,VISITNUM,EXENDT )%>%
                  group_by(USUBJID)%>%
                  slice_head()%>% ungroup () %>%
                  derive_vars_merged(
                    dataset_add = ds_ext,
                    new_vars = vars(DSTDT = DSTDT, DCDECOD=DSDECOD),
                    order = vars(USUBJID),
                    mode = "last",
                    by_vars = vars(USUBJID)
                  )%>%
                  mutate(EXENDT_= case_when(
                    is.na(EXENDT) & VISITNUM>=3 & DCDECOD!='COMPLETED'~ DSTDT,
                    TRUE ~ EXENDT)
                  ) %>%
                  rename(LSTVISTN=VISITNUM)

    #add TRTEDT to ADSL
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_filt,
    filter_add = !is.na(EXENDT_),
    new_vars = vars(TRTEDT = EXENDT_, LSTVISTN),
    order = vars(USUBJID),
    mode = "last",
    by_vars = vars(USUBJID)
    )


  # first treatment,
    # converted SV.SVSTDTC to SAS date
    # sv_conv also used for COMPxxFL flag derivation
sv_conv <- sv %>% derive_vars_dtm(
                    dtc = SVSTDTC,
                    new_vars_prefix = "SVST"
                    )%>% derive_vars_dtm_to_dt(source_vars = vars(SVSTDTM))%>%
                    select(STUDYID, USUBJID, SVSTDT, VISITNUM)

adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = sv_conv,
    filter_add = VISITNUM==3,
    new_vars = vars(TRTSDT = SVSTDT),
    order = vars(USUBJID),
    mode = "last",
    by_vars = vars(USUBJID)
  ) %>%
  #add RFENDT to ADSL
  derive_vars_dtm(
    dtc = RFENDTC_,
    new_vars_prefix = "RFEN"
  )%>%
  derive_vars_dtm_to_dt(source_vars = vars(RFENDTM)) %>%
  select(-c(RFENDTM, RFENTMF), RFENDTC=RFENDTC_)

###TRTDURD----
adsl <- adsl %>%
  derive_var_trtdurd()

###DCSREAS / DISCONFL / DSRAEFL----

#case TERM is not equal to 'PROTOCOL ENTRY CRITERIA NOT MET'
adsl <- adsl %>% rename (DCDECOD_=DCDECOD)%>%
  derive_vars_disposition_reason(
    dataset_ds = ds,
    new_var = DCSREAS_1_,
    reason_var = DSDECOD,
    # reason_var_spe = DSTERM,
    format_new_vars = format_dcsreas,
    filter_ds = DSCAT == "DISPOSITION EVENT" & DSTERM != 'PROTOCOL ENTRY CRITERIA NOT MET'
  )%>%
  mutate(DCSREAS_1 = format_dctreas(DCSREAS_1_)) %>%

#case TERM is  equal to 'PROTOCOL ENTRY CRITERIA NOT MET'

  derive_vars_disposition_reason(
    dataset_ds = ds,
    new_var = DCSREAS_2_,
    reason_var = DSDECOD,
    # reason_var_spe = DSTERM,
    format_new_vars = format_dcsreas,
    filter_ds = DSCAT == "DISPOSITION EVENT" & DSTERM == 'PROTOCOL ENTRY CRITERIA NOT MET'
  ) %>%
  mutate(DCSREAS_2 = if_else(!is.na(DCSREAS_2_), "I/E Not Met", NA_character_ ) ) %>%
  mutate(DCSREAS   = if_else(!is.na(DCSREAS_2),DCSREAS_2,DCSREAS_1))%>%


  select(!c(DCSREAS_1,DCSREAS_2,DCSREAS_1_,DCSREAS_2_)) %>% rename (DCDECOD=DCDECOD_) %>%
  mutate(DISCONFL = if_else(!(DCSREAS %in% c('Completed','')),'Y','')) %>%
  mutate(DSRAEFL  = if_else(DCSREAS=='Adverse Event','Y',''))

###ITTFL / SAFFL----
adsl <- adsl %>% mutate(ITTFL=if_else(ARMCD != "","Y","N"))%>%
                 mutate(SAFFL=if_else((ITTFL == "Y" & !is.na(TRTSDT)),"Y","N"))
###EFFFL----
#retrieve QS information for efficacy pop
qs_cibic <- qs %>% filter(QSTESTCD == "CIBIC" & VISITNUM > 3) %>% select(USUBJID, CIBIC=QSTESTCD)
qs_cibic <- unique(qs_cibic)
qs_actot <- qs %>% filter(QSTESTCD == "ACTOT" & VISITNUM > 3) %>% select(USUBJID, ACTOT=QSTESTCD)
qs_eff <- unique(qs_actot) %>% inner_join(qs_cibic,qs_actot, by = 'USUBJID')

adsl <- adsl %>%  derive_var_merged_exist_flag(
    dataset_add = qs_eff,
    by_vars = vars(USUBJID),
    new_var = EFFFL,
    false_value = 'N',
    missing_value = 'N',
    condition = (!is.na(USUBJID))
)

###COMP8FL / COMP16FL / COMP24FL----
adsl <- adsl %>% derive_vars_merged(
                  dataset_add = sv_conv,
                  filter_add = VISITNUM==8,
                  new_vars = vars(VIS_08_DATE = SVSTDT),
                  order = vars(USUBJID),
                  mode = "last",
                  by_vars = vars(USUBJID)
                 )%>%
                 derive_vars_merged(
                  dataset_add = sv_conv,
                  filter_add = VISITNUM==10,
                  new_vars = vars(VIS_10_DATE = SVSTDT),
                  order = vars(USUBJID),
                  mode = "last",
                  by_vars = vars(USUBJID)
                 )%>%
                 derive_vars_merged(
                  dataset_add = sv_conv,
                  filter_add = VISITNUM==12,
                  new_vars = vars(VIS_12_DATE = SVSTDT),
                  order = vars(USUBJID),
                  mode = "last",
                  by_vars = vars(USUBJID)
                  )%>%
                 mutate(COMP8FL  = if_else((!is.na(VIS_08_DATE) & RFENDT >= VIS_08_DATE),"Y","N"),
                        COMP16FL = if_else((!is.na(VIS_10_DATE) & RFENDT >= VIS_10_DATE),"Y","N"),
                        COMP24FL = if_else((!is.na(VIS_12_DATE) & RFENDT >= VIS_12_DATE),"Y","N")
                 )%>%
                 select(!starts_with("VIS_"))

###VISNUMEN----
adsl<- adsl %>% derive_vars_merged(
                  dataset_add = ds,
                  filter_add = DSCAT == "DISPOSITION EVENT",
                  new_vars = vars(VISNUMEN_ = VISITNUM),
                  order = vars(USUBJID),
                  mode = "last",
                  by_vars = vars(USUBJID)
                ) %>%
                mutate(VISNUMEN = if_else(VISNUMEN_==13,12,VISNUMEN_))%>%
                select(-VISNUMEN_)

###HEIGHTBL / WEIGHTBL / BMIBL / BMIBLGR1----
adsl<- adsl %>% derive_vars_merged(
                  dataset_add = vs,
                  filter_add = VSTESTCD=='HEIGHT' & VISITNUM==1,
                  new_vars = vars(HEIGHTBL_=VSSTRESN),
                  order = vars(USUBJID),
                  mode = "last",
                  by_vars = vars(USUBJID)
                # ) %>% mutate(HEIGHTBL=HEIGHTBL_) %>%
                ) %>% mutate(HEIGHTBL=round(HEIGHTBL_,1)) %>%

                derive_vars_merged(
                  dataset_add = vs,
                  filter_add = VSTESTCD=='WEIGHT' & VISITNUM==3,
                  new_vars = vars(WEIGHTBL_=VSSTRESN),
                  order = vars(USUBJID),
                  mode = "last",
                  by_vars = vars(USUBJID)
                # ) %>% mutate(WEIGHTBL=WEIGHTBL_) %>%
                ) %>% mutate(WEIGHTBL=round(WEIGHTBL_,1)) %>%

                select(-c(WEIGHTBL_, HEIGHTBL_)) %>%
                # mutate (BMIBL= WEIGHTBL / ((HEIGHTBL/100)**2) )%>%
                mutate (BMIBL= round (WEIGHTBL / ((HEIGHTBL/100)**2),1)) %>%
                mutate (BMIBLGR1 = format_bmiblgr1 (BMIBL))




###EDUCLVL----
adsl<- adsl %>% derive_vars_merged(
  dataset_add = sc,
  filter_add = SCTESTCD=='EDLEVEL',
  new_vars = vars(EDUCLVL=SCSTRESN),
  order = vars(USUBJID),
  mode = "last",
  by_vars = vars(USUBJID)
)

###DISONSDT / VISIT1DT ----
adsl<- adsl %>% derive_vars_merged(
  dataset_add = mh,
  filter_add =  MHCAT=='PRIMARY DIAGNOSIS',
  new_vars = vars(DISONSDTM=MHSTDTC),
  order = vars(USUBJID),
  mode = "last",
  by_vars = vars(USUBJID)
  ) %>%
  mutate( DISONSDTM = as_datetime(DISONSDTM)) %>%
  derive_vars_dtm_to_dt(vars(DISONSDTM)) %>%
  select(-DISONSDTM)%>%
  #VISIT1DT
  derive_vars_merged(
  dataset_add = sv,
  filter_add =  VISITNUM==1,
  new_vars = vars(VISIT1DTM=SVSTDTC),
  order = vars(USUBJID),
  mode = "last",
  by_vars = vars(USUBJID)
  )%>%
  mutate( VISIT1DTM = as_datetime(VISIT1DTM)) %>%
  derive_vars_dtm_to_dt(vars(VISIT1DTM)) %>%
  select(-VISIT1DTM)
#### DURDIS / DURDSGR1 ----
adsl$DURDIS_ <- compute_duration(
  adsl$DISONSDT,
  adsl$VISIT1DT,
    in_unit = "days",
  out_unit = "months",
  floor_in = TRUE,
  add_one = TRUE,
  trunc_out = FALSE
)

adsl <- adsl%>%
  mutate (DURDIS = round (DURDIS_,1)) %>%
  mutate (DURDSGR1 = format_durdis (DURDIS))


###MMSETOT----
adsl <- adsl %>% derive_var_merged_summary(
                  dataset_add = qs,
                  by_vars = vars(USUBJID),
                  filter_add = QSCAT == "MINI-MENTAL STATE",
                  new_var = MMSETOT,
                  analysis_var = QSSTRESN,
                  summary_fun = function(x) sum(x, na.rm = TRUE)
                )


###SITE1GR----
#Attribute grouping value to each site
sitegr <- adsl %>% group_by(SITEID,TRT01P) %>% count() %>% ungroup()
sitegr <-pivot_wider(sitegr, names_from = TRT01P, values_from = n)
sitegr[is.na(sitegr)] <- 0
sitegr <- sitegr %>%mutate (SITEGR1 =if_else(
(Placebo >=3 & `Xanomeline Low Dose`>=3 & `Xanomeline High Dose`>=3),SITEID, '900'))

#Assign grouping to each SITE in ADSL
adsl <- adsl %>% derive_vars_merged(
            dataset_add = sitegr,
            new_vars = vars(SITEGR1),
            order = vars(SITEID),
            mode = "last",
            by_vars = vars(SITEID)
          )


###CUMDOSE / AVGDD----

#prep data
Intervals <- adsl %>%
                          #1st interval
                          derive_vars_merged(
                            dataset_add = sv_conv,
                            filter_add = VISITNUM == 4,
                            new_vars = vars(VIS_4 = SVSTDT),
                            order = vars(USUBJID),
                            mode = "last",
                            by_vars = vars(USUBJID)
                            )%>%
                          derive_vars_merged(
                            dataset_add = ds_ext,
                            filter_add = VISITNUM == 4,
                            new_vars = vars(DISC_4 = DSDECOD),
                            order = vars(USUBJID),
                            mode = "last",
                            by_vars = vars(USUBJID)
                          ) %>%
                          #2nd interval
                          derive_vars_merged(
                            dataset_add = sv_conv,
                            filter_add = VISITNUM == 12,
                            new_vars = vars(VIS_12 = SVSTDT),
                            order = vars(USUBJID),
                            mode = "last",
                            by_vars = vars(USUBJID)
                          )%>%
                          derive_vars_merged(
                            dataset_add = ds_ext,
                            filter_add = VISITNUM == 12,
                            new_vars = vars(DISC_12 = DSDECOD),
                            order = vars(USUBJID),
                            mode = "last",
                            by_vars = vars(USUBJID)
                          )%>%
                          mutate (Int1 = (if_else(is.na(DISC_4)  & !is.na(VIS_4) , as.integer(VIS_4-TRTSDT+1), as.integer(TRTDURD))))%>%
                          mutate (Int2_= (if_else(is.na(DISC_12) & !is.na(VIS_12), as.integer(VIS_12-VIS_4),   as.integer(TRTEDT-VIS_4)))) %>%
                          mutate (Int2 = (if_else(Int2_<=0, NA_integer_, Int2_))) %>%
                          mutate (Int3 = (if_else(is.na(DISC_12), as.integer(TRTEDT-VIS_12),NA_integer_)))%>%
                          mutate (dose_int1=Int1*54, dose_int2= Int2*81, dose_int3= Int3*54) %>%
                          group_by(USUBJID) %>%
                          mutate (CUMDOSE_ = round(sum(dose_int1,dose_int2,dose_int3, na.rm=TRUE),1) ) %>%
                          ungroup()



#adsl integration
adsl <- adsl %>% derive_vars_merged(
                                    dataset_add = Intervals,
                                    new_vars = vars(CUMDOSE_),
                                    order = vars(USUBJID),
                                    mode = "last",
                                    by_vars = vars(USUBJID)
                                   ) %>%
                 mutate(CUMDOSE = case_when(
                               ARMN %in% c(0,54) ~ TRT01PN*TRTDURD,
                               ARMN == 81 ~ CUMDOSE_)
                        ) %>%
                 mutate(AVGDD= round(CUMDOSE/TRTDURD,1))




## Final dataset, applying metadata----

var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADSL")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adsl <-  adsl %>% select(var_spec$variable)

adsl <- adsl %>%
  xportr_length (var_spec, "ADSL") %>%
  xportr_order  (var_spec, "ADSL") %>%
  xportr_label  (var_spec, "ADSL")

## Save as XPT----
adsl %>%
  xportr_format (var_spec, "ADSL") %>%
  xportr_type   (var_spec, "ADSL") %>%
  xportr_label  (var_spec, "ADSL") %>%
  xportr_write  ("./adam/adsl.xpt", label = "Subject-Level Analysis Dataset")

