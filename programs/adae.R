## ADAE derivations

## Launch library
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(stringr)
library(metacore)
library(metatools)
library(xportr)

## Read data

dm <- read_xpt("sdtm/dm.xpt")
ae <- read_xpt("sdtm/ae.xpt")
adsl <- read_xpt("adam/adsl.xpt")

# RACEN New description - Different from ADSL

format_racen <- function(x) {
  case_when(
    x== "AMERICAN INDIAN OR ALASKA NATIVE" ~ 6,
    x== "ASIAN" ~ 3,
    x== "BLACK OR AFRICAN AMERICAN" ~ 2,
    x== "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ 5,
    x== "WHITE" ~ 1,
    TRUE ~ 999999
  )
}


## ADSL : Get a set of values useful for the derivations and to be
##         kept in the final dataset

adsl <- adsl %>%
  dplyr::select (AGE, RACE, SAFFL, SEX, SITEID, STUDYID, TRTEDT, TRTSDT, 
                USUBJID, AGEGR1, AGEGR1N, RACEN, TRT01A, TRT01AN)

## AE : Get a set of values useful for the derivations and to be
##         kept in the final dataset

ae <- ae %>%
  dplyr::select(AEACN, AEBODSYS, AEDECOD, AEHLGT, AEHLGTCD, AEHLT, AEHLTCD,
                AELLT, AELLTCD, AEOUT, AEPTCD, AEREL, AESCAN, AESCONG, AESDISAB,
                AESDTH, AESEQ,AESER, AESEV,AESHOSP, AESLIFE, AESOC, AESOCCD,
                AESOD, AETERM, STUDYID, USUBJID, AESTDTC, AEENDTC)

## Merge all data as one database to start derivations.
## and create TRTA and TRTAN

adae0 <-
  derive_vars_merged(
    ae,
    adsl,
    by_vars=vars(STUDYID, USUBJID),
    order = NULL,
    new_vars = NULL,
    mode = NULL,
    filter_add = NULL,
    match_flag = NULL,
    check_type = "warning",
    duplicate_msg = NULL
  ) %>%
  mutate (TRTA=TRT01A, TRTAN=TRT01AN)

## Create Imputed start date of event and store resulting date in ASTDT
## ASTDTF is also created; only the day is imputed when missing

adae1 <-
  admiral::derive_vars_dt(
    adae0,
    "AST",
    AESTDTC,
    highest_imputation = "D",
    date_imputation = "first",
    flag_imputation = "auto",
    min_dates = NULL,
    max_dates = NULL,
    preserve = FALSE
  )

# Derive a temporary event start date without imputation to derive the duration
adae1b <-
  derive_vars_dt(
    adae1,
    new_vars_prefix = "RAWDAT",
    dtc= AESTDTC
  )

## Create end date of event from --DTC variable
## No imputation for end dates

adae2 <-
  derive_vars_dt(
    adae1b,
    new_vars_prefix = "AEN",
    dtc= AEENDTC
  )

## Derive AE duration and assign a value to the unit
## Derivation is done using the non-imputed start date

adae3 <-
  derive_vars_duration(
    adae2,
    ADURN,
    new_var_unit = ADURURRAW,
    RAWDATDT,
    AENDT,
    in_unit = "days",
    out_unit = "days",
    floor_in = TRUE,
    add_one = TRUE,
    trunc_out = FALSE
  ) %>%
  dplyr::mutate (
    ADURU = dplyr::case_when(
      !is.na(ADURURRAW)  ~ "DAY")
  )


## Derive Study Days (AENDY and ASTDY)

adae4 <-
  derive_vars_dy(
    adae3,
    reference_date = TRTSDT,
    source_vars = vars(AENDT,ASTDT)
  )


## CQ01NAM
##If AEDECOD contains any of the character strings of ('APPLICATION', 
##   'DERMATITIS', 'ERYTHEMA', 'BLISTER')
## OR if AEBODSYS='SKIN AND SUBC UTANEOUS TISSUE DISORDERS'
##but AEDECOD is not in ('COLD SWEAT', 'HYPERHIDROSIS', 'ALOPECIA')
## then CQ01NAM='DERMATOLOGIC EVENTS' Otherwise CQ01NAM=NULL

adae5 <- adae4 %>%
  dplyr::mutate (
    CQ01NAM = dplyr::case_when(
      str_detect (AEDECOD,'APPLICATION')  ~ "DERMATOLOGIC EVENTS",
      str_detect (AEDECOD,'DERMATITIS')  ~ "DERMATOLOGIC EVENTS",
      str_detect (AEDECOD,'ERYTHEMA')  ~ "DERMATOLOGIC EVENTS",
      str_detect (AEDECOD,'BLISTER')  ~ "DERMATOLOGIC EVENTS",
      AEBODSYS %in% c('SKIN AND SUBCUTANEOUS TISSUE DISORDERS') &
        !(AEDECOD %in% c('COLD SWEAT', 'HYPERHIDROSIS', 'ALOPECIA')) ~ "DERMATOLOGIC EVENTS"
    )
  )


## TRTEMFL : Emergence flag


adae6 <-
  derive_var_trtemfl(
    adae5,
    new_var = TRTEMFL,
    start_date = ASTDT,
    end_date = AENDT,
    trt_start_date = TRTSDT,
    trt_end_date = NULL,
    end_window = NULL,
    ignore_time_for_trt_end = TRUE,
    initial_intensity = NULL,
    intensity = NULL
  )

## If the date is missing, no value is assigned to TRTEMFL
adae6 <- mutate (adae6, TRTEMFL=ifelse(is.na(ASTDT), "", TRTEMFL))


## AOCC01FL
#  Subset to CQ01NAM='' and TRTEMFL='Y' and sort by Subject (USUBJID),
# Start Date (ASTDT), and Sequence Number (AESEQ) and flag the first record
# (set AOCC01FL=?Y?) within each Subject (Flag First Treatment Emergent
# Dermatological Event for Time to Event Analysis)

adae7 <- adae6 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID, ASTDT, AESEQ),
      new_var = AOCC01FL,
      mode = "first"
    ),
    filter = (CQ01NAM !="" & TRTEMFL=="Y")
  )


## AOCC02FL
## Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject (USUBJID),
## Start Date (ASTDT), and Sequence Number (AESEQ) and flag the first record
## (set AOCC02FL=?Y?) within each Subject

adae8 <- adae7 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID, ASTDT, AESEQ),
      new_var = AOCC02FL,
      mode = "first"
    ),
    filter = (AESER=="Y" & TRTEMFL=="Y")
  )

## AOCC03FL
## Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject (USUBJID),
## System Organ Class (AEBODSYS), Start Date (ASTDT), and Sequence Number
##(AESEQ) and flag the first record (set AOCC03FL='Y') within each Subject & SOC


adae9 <- adae8 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS),
      order = vars(USUBJID, AEBODSYS, ASTDT, AESEQ),
      new_var = AOCC03FL,
      mode = "first"
    ),
    filter = (AESER=="Y" & TRTEMFL=="Y")
  )

## AOCC04FL
## Subset to TRTEMFL='Y' and AESER='Y' and sort by Subject (USUBJID),
## System Organ Class (AEBODSYS), Preferred Term (AEDECOD), Start Date (ASTDT),
## and Sequence Number (AESEQ) and flag the first record (set AOCC04FL='Y')
## within each Subject, SOC, and PT

adae10 <- adae9 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS, AEDECOD),
      order = vars(USUBJID, AEBODSYS, AEDECOD, ASTDT, AESEQ),
      new_var = AOCC04FL,
      mode = "first"
    ),
    filter = (AESER=="Y" & TRTEMFL=="Y")
  )


## AOCCFL
##  Subset to TRTEMFL='Y' and sort by Subject (USUBJID), Start Date (ASTDT),
## and Sequence Number (AESEQ) and flag the first record (set AOCCFL='Y')
## within each Subject

adae11 <- adae10 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID, ASTDT, AESEQ),
      new_var = AOCCFL,
      mode = "first"
    ),
    filter = (TRTEMFL=="Y")
  )


## AOCCPFL
## Subset to TRTEMFL='Y' and sort by Subject (USUBJID), System Organ Class
##  (AEBODSYS), Preferred Term (AEDECOD), Start Date (ASTDT), and Sequence
##  Number (AESEQ) and flag the first record (set AOCCPFL='Y') within each
##  Subject, SOC, and PT

adae12 <- adae11 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS, AEDECOD),
      order = vars(USUBJID, AEBODSYS, AEDECOD, ASTDT, AESEQ),
      new_var = AOCCPFL,
      mode = "first"
    ),
    filter = (TRTEMFL=="Y")
  )

## AOCCSFL
##  Subset to TRTEMFL='Y' and sort by Subject (USUBJID), System Organ Class
## (AEBODSYS), Start Date (ASTDT), and Sequence Number (AESEQ) and flag the
##  first record (set AOCCSFL='Y') within each Subject and SOC

adae13 <- adae12 %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID, AEBODSYS),
      order = vars(USUBJID, AEBODSYS, ASTDT, AESEQ),
      new_var = AOCCSFL,
      mode = "first"
    ),
    filter = (TRTEMFL=="Y")
  )


adae13 <- adae13 %>% mutate(RACEN = format_racen(RACE))


# Checks
## Final dataset, applying metadata----

var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADAE")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
#var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adae <-  adae13 %>% select(var_spec$variable)

adae <- adae %>%
  xportr_length (var_spec, "ADAE") %>%
  xportr_order  (var_spec, "ADAE") %>%
  xportr_label  (var_spec, "ADAE")

## Save as XPT----
adae %>%
  xportr_format (var_spec, "ADAE") %>%
  xportr_type   (var_spec, "ADAE") %>%
  xportr_label  (var_spec, "ADAE") %>%
  xportr_write  ("./adam/adae.xpt", label = "Adverse Events")
