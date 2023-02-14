# ADTTE Prog ----

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
adsl <- read_xpt ("./adam/adsl.xpt")

## Convert blanks to NA----
XXXX <- convert_blanks_to_na(lb)

## Category functions

##Derivations----
###Predecessors / TRTA(N) / TRTP(N)----

adtte <- adsl %>%
  # Join ADSL variables with LB
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars (AGE, AGEGR1, AGEGR1N, COMP24FL, DSRAEFL, RACE,
                     RACEN, SAFFL, SEX, STUDYID, SUBJID, TRTA=TRT01A, TRTAN=TRT01AN,
                     TRTEDT, TRTP=TRT01P, TRTPN=TRT01PN, TRTSDT, USUBJID, RFENDT),
    by_vars = vars(STUDYID, USUBJID)
  )



























## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADTTE")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adtte <- adtte %>% select(var_spec$variable) %>%
  arrange(USUBJID, PARAMCD) %>%

### Applying metadata----
adtte <- adtte %>%
  xportr_length (var_spec, "ADTTE") %>%
  xportr_order  (var_spec, "ADTTE") %>%
  xportr_label  (var_spec, "ADTTE")


### Save as XPT----
adtte %>%
  xportr_format (var_spec, "ADTTE") %>%
  xportr_type   (var_spec, "ADTTE") %>%
  xportr_label  (var_spec, "ADTTE") %>%
  xportr_write  ("./adam/adtte.xpt", label = "Subject-Level Analysis Dataset")
