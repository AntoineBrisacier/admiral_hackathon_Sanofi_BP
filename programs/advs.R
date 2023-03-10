# ADVS Prog ----
#ADMIRAL HACKATHON----
#Team SANOFI_BP----
#Date 28FEB2023----
#Label: Vital Signs Analysis Dataset


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

# table(vs$VISIT)
# table(advs$AVISIT)

## Input datasets reading----
adsl <- read_xpt ("./adam/adsl.xpt")
vs <- read_xpt ("./sdtm/vs.xpt")

## Convert blanks to NA----
vs <- convert_blanks_to_na(vs)

##Look-up tables ----
#Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble (
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "SYSBP","SYSBP","Systolic Blood Pressure (mmHg)",1,
  "DIABP","DIABP","Diastolic Blood Pressure (mmHg)",2,
  "PULSE","PULSE","Pulse Rate (beats/min)",3,
  "WEIGHT","WEIGHT","Weight (kg)",4,
  "HEIGHT","HEIGHT","Height (cm)",5,
  "TEMP","TEMP","Temperature (C)",6
)


## Category functions----
format_atptn <- function(x) {
  case_when(
    x== "AFTER LYING DOWN FOR 5 MINUTES" ~ 815,
    x== "AFTER STANDING FOR 1 MINUTE" ~ 816,
    x== "AFTER STANDING FOR 3 MINUTES" ~ 817,
  )
}

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

##Derivations----
### TRTP(N) /TRTA(N)----
advs <- vs %>%
  # Join ADSL variables with VS
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars (TRTP=TRT01P, TRTPN=TRT01PN, TRTA=TRT01A, TRTAN=TRT01AN,
                     AGE , AGEGR1 ,AGEGR1N , RACE , RACEN ,  SAFFL , SEX , SITEID , STUDYID ,
                     TRTEDT , TRTSDT , USUBJID ),
    by_vars = vars(STUDYID, USUBJID)
  )%>%
  rename(ABLFL=VSBLFL, ADY=VSDY, ATPT=VSTPT, AVAL=VSSTRESN, PARAMCD=VSTESTCD)

###ADT / PARAMCD / PARAMN----
advs <- advs %>%
  derive_vars_dt(new_vars_prefix = "A", dtc = VSDTC)

###PARAM / PARAMCD / PARAMN----
advs <- advs %>%
  ## from LOOK-UP table
  # Replace with PARAMCD lookup function
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars (PARAM,PARAMN),
    by_vars = vars (PARAMCD),
    check_type = "none",
    print_not_mapped = FALSE
  )

### ANL01FL----
advs <- advs %>%
  mutate(ANL01FL= if_else(str_detect(VISIT, "WEEK") | VISIT=="BASELINE","Y","")
        )

### AVISIT / AVISITN----
advs <- advs %>%
              mutate(AVISIT= case_when(
                        ANL01FL=='Y'~ str_to_title(VISIT),
                        VISIT=='BASELINE'~ "Baseline",
                        TRUE ~ "")
                    ) %>%
              mutate(AVISITN= case_when(
                        AVISIT=='Baseline'~ 0,
                        ANL01FL=='Y'~ as.numeric (str_remove(AVISIT,'Week ')),
                        TRUE ~ NA_real_)
                    )

#Retrieve EOT visit
advs <- advs %>%
  derive_extreme_records(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, ATPT),
    order = vars(AVISITN),
    mode = "last",
    filter = (AVISITN > 2 & !is.na(AVISITN)),
    set_values_to = vars(
      AVISIT = "End of Treatment",
      AVISITN = 99
    ))

### ATPTN----
advs <- advs %>% mutate(ATPTN=format_atptn(ATPT))


### BASE / CHG / PCHG----
advs <- advs %>%
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD, ATPTN),
    source_var = AVAL,
    new_var = BASE
  ) %>%
  derive_var_chg() %>%
  derive_var_pchg()

### BASETYPE
advs <- advs %>% mutate(BASETYPE= ATPT)

# RACEN----
advs <- advs %>% mutate(RACEN = format_racen(RACE))

## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
            dplyr::rename(type = "Data Type") %>%
            rlang::set_names(tolower) %>% filter (dataset=="ADVS")

var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)

advs <- advs %>% select(var_spec$variable) %>%
                     arrange(USUBJID, PARAMCD, AVISIT, ATPT)

### Applying metadata----
advs <- advs %>%
  xportr_length (var_spec, "ADVS") %>%
  xportr_order  (var_spec, "ADVS") %>%
  xportr_label  (var_spec, "ADVS")


### Save as XPT----
advs %>%
  xportr_format (var_spec, "ADVS") %>%
  xportr_type   (var_spec, "ADVS") %>%
  xportr_label  (var_spec, "ADVS") %>%
  xportr_write  ("./adam/advs.xpt", label = "Subject-Level Analysis Dataset")
