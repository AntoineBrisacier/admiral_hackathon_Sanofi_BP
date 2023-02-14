# ADAE Prog ----

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
ae   <- read_xpt ("./sdtm/ae.xpt")
ex   <- read_xpt ("./sdtm/ex.xpt")

## Convert blanks to NA----
ae <- convert_blanks_to_na(ae)
ex <- convert_blanks_to_na(ex)

## Category functions

##Derivations----
###Predecessors / TRTA(N) / TRTP(N)----

adae <- ae %>%
  # Join ADSL variables with LB
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars (AGE, AGEGR1, AGEGR1N, COMP24FL, DSRAEFL, RACE,
                     RACEN, SAFFL, SEX, STUDYID, SUBJID, TRTA=TRT01A, TRTAN=TRT01AN,
                     TRTEDT, TRTP=TRT01P, TRTPN=TRT01PN, TRTSDT, USUBJID, RFENDT),
    by_vars = vars(STUDYID, USUBJID)
  )



    ## Derive analysis start time ----
adae <- adae %>% derive_vars_dtm(
        dtc = AESTDTC,
        new_vars_prefix = "AST",
        highest_imputation = "M",
        min_dates = vars(TRTSDT)
      )%>%
        ## Derive analysis end time ----
      derive_vars_dtm(
        dtc = AEENDTC,
        new_vars_prefix = "AEN",
        highest_imputation = "M",
        date_imputation = "last",
        time_imputation = "last",
        # max_dates = vars(DTHDT, EOSDT)
      ) %>%
        ## Derive analysis end/start date ----
      derive_vars_dtm_to_dt(vars(ASTDTM, AENDTM)) %>%
        ## Derive analysis start relative day and  analysis end relative day ----
      derive_vars_dy(
        reference_date = TRTSDT,
        source_vars = vars(ASTDT, AENDT)
      ) %>%
        ## Derive analysis duration (value and unit) ----
      derive_vars_duration(
        new_var = ADURN,
        new_var_unit = ADURU,
        start_date = ASTDT,
        end_date = AENDT,
        in_unit = "days",
        out_unit = "days",
        add_one = TRUE,
        trunc_out = FALSE
      )



ex_ext <- derive_vars_dtm(
  ex,
  dtc = EXSTDTC,
  new_vars_prefix = "EXST",
  flag_imputation = "none"
)

adae <- adae %>%
  ## Derive last dose date/time ----
derive_var_last_dose_date(
  ex_ext,
  filter_ex = (EXDOSE > 0 | (EXDOSE == 0 & grepl("PLACEBO", EXTRT))) &
    !is.na(EXSTDTM),
  dose_date = EXSTDTM,
  analysis_date = ASTDT,
  new_var = LDOSEDTM,
  single_dose_condition = (EXDOSFRQ == "QD"),#(EXSTDTC == EXENDTC),
  output_datetime = TRUE
) %>%
  ## Derive severity / causality / ... ----
mutate(
  ASEV = AESEV,
  AREL = AEREL
) %>%
  ## Derive treatment emergent flag ----
derive_var_trtemfl(
  trt_start_date = TRTSDT,
  trt_end_date = TRTEDT,
  end_window = 30
)




















## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADAE")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adae <- adae %>% select(var_spec$variable) %>%
  arrange(USUBJID, AETERM, ASTDT, AESEQ) %>%

### Applying metadata----
adae <- adae %>%
  xportr_length (var_spec, "ADAE") %>%
  xportr_order  (var_spec, "ADAE") %>%
  xportr_label  (var_spec, "ADAE")


### Save as XPT----
adae %>%
  xportr_format (var_spec, "ADAE") %>%
  xportr_type   (var_spec, "ADAE") %>%
  xportr_label  (var_spec, "ADAE") %>%
  xportr_write  ("./adam/adae.xpt", label = "Subject-Level Analysis Dataset")
