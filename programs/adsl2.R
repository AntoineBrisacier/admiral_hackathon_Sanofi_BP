# Name: ADSL
# Label: Subject Level Analysis Dataset


library(admiral)
library(admiral.test)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyverse)

# Load source datasets ----

data("admiral_dm")
data("admiral_ds")
data("admiral_ex")
data("admiral_ae")
data("admiral_lb")
data("admiral_sv")

dm <- admiral_dm
ds <- admiral_ds
ex <- admiral_ex
ae <- admiral_ae
lb <- admiral_lb
vs <- admiral_vs
sv <- admiral_sv


dm <- convert_blanks_to_na(dm)
ds <- convert_blanks_to_na(ds)
ex <- convert_blanks_to_na(ex)
ae <- convert_blanks_to_na(ae)
lb <- convert_blanks_to_na(lb)
vs <- convert_blanks_to_na(vs)

# User defined functions ----

# Grouping
# format_racegr1 <- function(x) {
#   case_when(
#     x == "WHITE" ~ "White",
#     x != "WHITE" ~ "Non-white",
#     TRUE ~ "Missing"
#   )
# }

format_agegr1 <- function(x) {
  case_when(
    x < 65 ~ "<65",
    between(x, 65, 80) ~ "65-80",
    x > 80 ~ ">80",
    TRUE ~ "Missing"
  )
}

format_bmicat <- function(x) {
  case_when(
    x < 25 ~ "Normal",
    x >= 25 & x < 30 ~ "Overweight",
    x >= 30 ~ "Obese",
    TRUE ~ "Missing"
  )
}


format_armn <- function(x) {
  case_when(
    x == "Placebo" ~ 0,
    x == "Xanomeline Low Dose" ~ 54,
    x == "Xanomeline High Dose" ~ 81
  )
}


format_durdisc <- function(x) {
  case_when(
    x < 12 ~ "<12",
    x >= 12 ~ ">=12",
    TRUE ~ "Missing"
  )
}


# format_region1 <- function(x) {
#   case_when(
#     x %in% c("CAN", "USA") ~ "NA",
#     !is.na(x) ~ "RoW",
#     TRUE ~ "Missing"
#   )
# }

# format_lddthgr1 <- function(x) {
#   case_when(
#     x <= 30 ~ "<= 30",
#     x > 30 ~ "> 30",
#     TRUE ~ NA_character_
#   )
# }

# EOSSTT mapping
# format_eoxxstt <- function(x) {
#   case_when(
#     x %in% c("COMPLETED") ~ "COMPLETED",
#     !(x %in% c("COMPLETED", "SCREEN FAILURE")) & !is.na(x) ~ "DISCONTINUED",
#     x %in% c("SCREEN FAILURE") ~ NA_character_,
#     TRUE ~ "ONGOING"
#   )
# }

# Derivations ----
# impute start and end time of exposure to first and last respectively, do not impute date
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  )

sv_ext <- sv %>%
  derive_vars_dtm(
    new_vars_prefix = "SVST",
    dtc = SVSTDTC
  )%>%
  derive_vars_dtm_to_dt(
    source_vars = vars(SVSTDTM)
  )




# Derivations ----

dm_site <- dm %>%
  select(STUDYID,USUBJID,SITEID,ARM,ARMCD) %>%
  filter(ARMCD != "Scrnfail") %>%
  group_by(STUDYID,SITEID,ARMCD) %>%
  summarise(n=n()) %>%
  pivot_wider(names_from=ARMCD, values_from=n) %>%
  mutate(SITEGR1 = case_when(Pbo>3 & Xan_Hi>=3 & Xan_Lo>=3  ~ SITEID,
                             TRUE ~ "900")) %>%
  select(STUDYID,SITEID,SITEGR1) %>%
  ungroup()

head(sv)


adsl <- dm %>%
  derive_vars_merged(
    dataset_add = dm_site,
    new_vars = vars(SITEGR1 = SITEGR1),
    by_vars = vars(STUDYID, SITEID)
  ) %>%

  ## derive treatment variables (TRT01P, TRT01A) ----
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM,
    TRT01PN = format_armn(ARM),
    TRT01AN = format_armn(ACTARM)
  )





ex_date <- ex %>%




adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = sv_ext,
    filter_add = (VISITNUM ==3),
    new_vars = vars(TRTSDT = SVSTDT),
    by_vars = vars(STUDYID, USUBJID)
  )






  ## derive treatment start date (TRTSDTM) ----
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars = vars(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = vars(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  ## derive treatment end date (TRTEDTM) ----
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = vars(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = vars(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  ## Derive treatment end/start date TRTSDT/TRTEDT ----
  derive_vars_dtm_to_dt(source_vars = vars(TRTSDTM, TRTEDTM)) %>%
  ## derive treatment duration (TRTDURD) ----
  derive_var_trtdurd()





adsl2 <- adsl %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = (VSTESTCD == "WEIGHT" & VISITNUM == 1),
    new_vars = vars(WEIGHTBL = VSSTRESN),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = vs,
    filter_add = (VSTESTCD == "HEIGHT" & VISITNUM == 1),
    new_vars = vars(HEIGHTBL = VSSTRESN),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%
  mutate(BMIBL = (compute_bmi(height = HEIGHTBL, weight = WEIGHTBL)))







## Disposition dates, status ----
# convert character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

# Screen fail date
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(SCRFDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD == "SCREEN FAILURE"
  ) %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSDECOD != "SCREEN FAILURE"
  ) %>%
  # EOS status
  derive_var_disposition_status(
    dataset_ds = ds_ext,
    new_var = EOSSTT,
    status_var = DSDECOD,
    format_new_var = format_eoxxstt,
    filter_ds = DSCAT == "DISPOSITION EVENT"
  ) %>%
  # Last retrieval date
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(FRVDT = DSSTDT),
    filter_add = DSCAT == "OTHER EVENT" & DSDECOD == "FINAL RETRIEVAL VISIT"
  ) %>%
  # Derive Randomization Date
  derive_vars_merged(
    dataset_add = ds_ext,
    filter_add = DSDECOD == "RANDOMIZED",
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(RANDDT = DSSTDT)
  ) %>%
  # Death date - impute partial date to first day/month
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC,
    highest_imputation = "M",
    date_imputation = "first"
  ) %>%
  # Relative Day of Death
  derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  ) %>%
  # Elapsed Days from Last Dose to Death
  derive_vars_duration(
    new_var = LDDTHELD,
    start_date = TRTEDT,
    end_date = DTHDT,
    add_one = FALSE
  )

## Last known alive date ----
ae_start_date <- date_source(
  dataset_name = "ae",
  date = AESTDT
)
ae_end_date <- date_source(
  dataset_name = "ae",
  date = AEENDT
)
lb_date <- date_source(
  dataset_name = "lb",
  date = LBDT,
  filter = !is.na(LBDT)
)
trt_end_date <- date_source(
  dataset_name = "adsl",
  date = TRTEDT
)

# impute AE start and end date to first
ae_ext <- ae %>%
  derive_vars_dt(
    dtc = AESTDTC,
    new_vars_prefix = "AEST",
    highest_imputation = "M"
  ) %>%
  derive_vars_dt(
    dtc = AEENDTC,
    new_vars_prefix = "AEEN",
    highest_imputation = "M"
  )

# impute LB date to first
lb_ext <- derive_vars_dt(
  lb,
  dtc = LBDTC,
  new_vars_prefix = "LB",
  highest_imputation = "M"
)

adsl <- adsl %>%
  derive_var_extreme_dt(
    new_var = LSTALVDT,
    ae_start_date, ae_end_date, lb_date, trt_end_date,
    source_datasets = list(ae = ae_ext, lb = lb_ext, adsl = adsl),
    mode = "last"
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = vars(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "PLACEBO")))
  ) %>%
  ## Groupings and others variables ----
  mutate(
    RACEGR1 = format_racegr1(RACE),
    AGEGR1 = format_agegr1(AGE),
    REGION1 = format_region1(COUNTRY),
    LDDTHGR1 = format_lddthgr1(LDDTHELD),
    DTH30FL = if_else(LDDTHGR1 == "<= 30", "Y", NA_character_),
    DTHA30FL = if_else(LDDTHGR1 == "> 30", "Y", NA_character_),
    DTHB30FL = if_else(DTHDT <= TRTSDT + 30, "Y", NA_character_),
    DOMAIN = NULL
  )



# Save output ----

dir <- "/cloud/project/adam"
saveRDS(adsl, file = file.path(dir, "adsl.rds"), compress = "bzip2")

