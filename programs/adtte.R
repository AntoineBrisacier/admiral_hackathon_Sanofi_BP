# ADTTE Prog ----
#ADMIRAL HACKATHON----
#Team SANOFI_BP----
#Date 28FEB2023----
#Label: AE Time To 1st Derm. Event Analysis Dataset

## ADTTE derivations

## Launch library ----
library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(stringr)
library(metacore)
library(metatools)
library(xportr)

## Read data ----

dm <- read_xpt("sdtm/dm.xpt")
ae <- read_xpt("sdtm/ae.xpt")
adsl <- read_xpt("adam/adsl.xpt")
adae <- read_xpt("adam/adae.xpt")

## RACEN New description - Different from ADSL ----

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

## ADSL : Get a set of values useful for the derivations and to be ----
###         kept in the final dataset----

adsl <- adsl %>%
  dplyr::select (AGE, RACE, SAFFL, SEX, SITEID, STUDYID, TRTEDT, TRTSDT, USUBJID,
                 AGEGR1, AGEGR1N, RACEN, TRT01A, TRT01AN, TRT01P, TRTDURD, 
                 RFENDT)

## Define Event ----

ae_source <- event_source(
  dataset_name = "adae",
  filter = CQ01NAM=="DERMATOLOGIC EVENTS" & AOCC01FL =="Y" & TRTEMFL == "Y",
  date = ASTDT,
  set_values_to = vars(
    EVNTDESC = "Dematologic Event Occured",
    SRCDOM = "ADAE",
    SRCVAR = "ASTDT",
    SRCSEQ = AESEQ
  )
)

## Define censor ----

last_trt <- censor_source(
  dataset_name = "adsl",
  date = RFENDT,
  set_values_to = vars(
    EVNTDESC = "Study Completion Date",
    SRCDOM = "ADSL",
    SRCVAR = "RFENDT"
  )
)


## Time to event derivation  ----
adtte <- derive_param_tte(
  dataset_adsl = adsl,
  event_conditions = list(ae_source),
  censor_conditions = list(last_trt),
  source_datasets = list(adsl = adsl, adae=adae),
  set_values_to = vars(
    PARAMCD = "TTDE",
    PARAM = "Time to First Dermatologic Event"
  )
)

##  AVAL derivation ----
adtte_final <-
  derive_vars_dy(
    adtte,
    reference_date = STARTDT,
    source_vars = vars(ADT)
  ) %>%
  mutate (AVAL=ADY)

## Merge with ADSL & derive TRTA, TRTAN, TRTP and TRTDUR ----
adtte_finalb <-
  derive_vars_merged(
    adtte_final,
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
  mutate (TRTA = TRT01A, TRTAN=TRT01AN, TRTP = TRT01P, TRTDUR=TRTDURD)

## Adapt RACEN derivation to final version of the specifications ----

adtte_finalb <- adtte_finalb %>% mutate(RACEN = format_racen(RACE))


## Final dataset, applying metadata----

var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADTTE")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
#var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adtte_fin <-  adtte_finalb %>% select(var_spec$variable)

adtte_fin <- adtte_fin %>%
  xportr_length (var_spec, "ADTTE") %>%
  xportr_order  (var_spec, "ADTTE") %>%
  xportr_label  (var_spec, "ADTTE")

## Save as XPT----
adtte_fin %>%
  xportr_format (var_spec, "ADTTE") %>%
  xportr_type   (var_spec, "ADTTE") %>%
  xportr_label  (var_spec, "ADTTE") %>%
  xportr_write  ("./adam/adtte.xpt", label = "AE Time To 1st Derm. Event Analysis")
