#ADADAS Prog ----
#ADMIRAL HACKATHON----
#Team SANOFI_BP----
#Date 28FEB2023----
#Label: ADAS-Cog Analysis


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
qs <- read_xpt ("./sdtm/qs.xpt")
## Convert blanks to NA----
qs <- convert_blanks_to_na(qs)

## Category functions
format_avisit <- function(x) {
  case_when(
    x <=1             ~ "Baseline",
    x >=2  & x <= 84  ~ "Week 8",
    x >=85 & x <=140  ~ "Week 16",
    x >140            ~ "Week 24"
  )
}

format_avisitn <- function(x) {
  case_when(
    x <=1             ~ 0,
    x >=2  & x <= 84  ~ 8,
    x >=85 & x <=140  ~ 16,
    x >140            ~ 24
  )
}

format_AWR <- function(x) {
  case_when(
    x <=1             ~ "<=1",
    x >=2  & x <= 84  ~ "2-84",
    x >=85 & x <=140  ~ "85-140",
    x >140            ~ ">140"
  )
}

format_AWT <- function(x) {
  case_when(
    x <=1             ~ 1,
    x >=2  & x <= 84  ~ 56,
    x >=85 & x <=140  ~ 112,
    x >140            ~ 168
  )
}

format_AWLO <- function(x) {
  case_when(
    x <=1             ~ NA_real_,
    x >=2  & x <= 84  ~ 2,
    x >=85 & x <=140  ~ 85,
    x >140            ~ 141
  )
}

format_AWHI <- function(x) {
  case_when(
    x <=1             ~ 1,
    x >=2  & x <= 84  ~ 84,
    x >=85 & x <=140  ~ 140,
    x >140            ~ NA_real_
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

##Look-up tables ----
# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble (
  ~QSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "ACITM01","ACITM01","Word Recall Task","1",
  "ACITM02","ACITM02","Naming Objects And Fingers (Refer To 5 C","2",
  "ACITM03","ACITM03","Delayed Word Recall","3",
  "ACITM04","ACITM04","Commands","4",
  "ACITM05","ACITM05","Constructional Praxis","5",
  "ACITM06","ACITM06","Ideational Praxis","6",
  "ACITM07","ACITM07","Orientation","7",
  "ACITM08","ACITM08","Word Recognition","8",
  "ACITM09","ACITM09","Attention/Visual Search Task","9",
  "ACITM10","ACITM10","Maze Solution","10",
  "ACITM11","ACITM11","Spoken Language Ability","11",
  "ACITM12","ACITM12","Comprehension Of Spoken Language","12",
  "ACITM13","ACITM13","Word Finding Difficulty In Spontaneous S","13",
  "ACITM14","ACITM14","Recall Of Test Instructions","14",
  "ACTOT"  ,"ACTOT"  ,"Adas-Cog(11) Subscore","15"
  )

# locf for ACTOT records
expected_ACTOT_records<- tibble::tribble (
                          ~PARAMCD,   ~AVISIT,   ~AVISITN,
                          "ACTOT",   "Baseline",   0,
                          "ACTOT",   "Week 8",     8,
                          "ACTOT",   "Week 16",    16,
                          "ACTOT",   "Week 24",    24,
)

##Derivations----
###Predecessors / TRTP(N)----
adadas <- qs %>%
  # Join ADSL variables with LB
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars (AGE, AGEGR1, AGEGR1N, COMP24FL, RACE,
                     RACEN, EFFFL, ITTFL, SEX, STUDYID, USUBJID,
                     TRTEDT, TRTP=TRT01P, TRTPN=TRT01PN, TRTSDT,
                     SITEID, SITEGR1),
    by_vars = vars(STUDYID, USUBJID)
  )

### ADT / ADY ----
adadas <- adadas %>% derive_vars_dt(new_vars_prefix = "A", dtc = QSDTC) %>%
                    derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT))


###PARAM / PARAMCD / PARAMN----
adadas <- adadas %>%
  ## from LOOK-UP table
  # Replace with PARAMCD lookup function
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars (PARAMCD, PARAM, PARAMN),
    by_vars = vars (QSTESTCD),
    check_type = "none",
    print_not_mapped = FALSE
  )
  # Remove unused QSTESTCD
adadas <- adadas %>% filter (QSTESTCD %in% unique((adadas$PARAMCD[!is.na(adadas$PARAMCD)])))


###AVAL / AVALC ----
adadas <- adadas %>% mutate (AVAL= QSSTRESN, AVALC=QSSTRESC)

### ABLFL / BASE / CHG / PCHG  ----
adadas <- adadas %>% mutate(ABLFL=QSBLFL) %>%
                      derive_var_base(
                        by_vars = vars(STUDYID, USUBJID, PARAMCD),
                        source_var = AVAL,
                        new_var = BASE
                        ) %>%
                        derive_var_chg() %>%
                        derive_var_pchg()


### AVISIT / AVISITN ----
adadas <- adadas %>%
  mutate(AVISIT   = format_avisit (ADY),
         AVISITN  = format_avisitn(ADY),)

### DTYPE----
adadas <- adadas %>% restrict_derivation(
    derivation = derive_locf_records,
    args = params(
      dataset_expected_obs = expected_ACTOT_records,
      by_vars = vars(STUDYID, USUBJID, PARAMCD),
      order = vars(AVISITN, AVISIT)
    ),
    filter = (PARAMCD=='ACTOT')
  )

  #complete LOCF records
  # Retrieve LOCF data
date_actot <- adadas %>% select(USUBJID, AVISIT,AVISITN,ADT) %>%
    arrange (USUBJID,AVISITN,ADT) %>%
    distinct(USUBJID,AVISITN,AVISIT, ADT) %>%
    mutate (ADT_LOCF = ADT, PARAMCD='ACTOT')

while(sum (is.na(date_actot$ADT_LOCF))>1){
    date_actot<-date_actot %>%
      mutate (ADT_LOCF= if_else (is.na(ADT_LOCF),lag(ADT_LOCF),ADT_LOCF))
  }

date_actot <- date_actot %>% filter (is.na(ADT))

date_actot_formerg <- date_actot %>% select(-c(ADT)) %>% rename(ADT=ADT_LOCF)

locf_val <- adadas %>% select(-c(AVISIT, AVISITN))

locf_records <- inner_join (locf_val,date_actot_formerg, by=c("USUBJID", "ADT", "PARAMCD")) %>% mutate(DTYPE='LOCF')

adadas <- adadas %>% filter (is.na(DTYPE)) %>%
                       bind_rows(locf_records) %>%
                       mutate (ABLFL=(if_else (!is.na(DTYPE), NA_character_, ABLFL)))

#update CHG and PCHG
adadas <- adadas%>% mutate  (CHG=if_else(AVISIT != 'Baseline',CHG, NA_real_),
            PCHG=if_else(AVISIT != 'Baseline',PCHG, NA_real_)
            )

###AWRANGE / AWTARGET / AWU / AWLO /AWHI----
adadas <- adadas %>% mutate ( AWRANGE  = if_else(!is.na(DTYPE), format_AWR (AVISITN*7), format_AWR  (ADY)),
                              AWTARGET = if_else(!is.na(DTYPE), format_AWT (AVISITN*7), format_AWT  (ADY)),
                              AWLO     = if_else(!is.na(DTYPE), format_AWLO(AVISITN*7), format_AWLO (ADY)),
                              AWHI     = if_else(!is.na(DTYPE), format_AWHI(AVISITN*7), format_AWHI (ADY)),
                              AWU      = "DAYS"
                            )

## AWTDIFF
adadas <- adadas %>% mutate (AWTDIFF = abs(ADY-AWTARGET))

## ANL01FL
adadas <- adadas %>%
      derive_var_extreme_flag (
        by_vars = vars(USUBJID, PARAMCD, AVISIT),
        order = vars(AWTDIFF),
        new_var = ANL01FL,
        mode = "first"
    )

# RACEN----
adadas <- adadas %>% mutate(RACEN = format_racen(RACE))



## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
            dplyr::rename(type = "Data Type") %>%
            rlang::set_names(tolower) %>% filter (dataset=="ADADAS")

var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adadas <- adadas %>% select(var_spec$variable) %>%
                     arrange(USUBJID, PARAMCD, AVISIT, ADT)

### Applying metadata----
adadas <- adadas %>%
  xportr_length (var_spec, "ADADAS") %>%
  xportr_order  (var_spec, "ADADAS") %>%
  xportr_label  (var_spec, "ADADAS")


### Save as XPT----
adadas %>%
  xportr_format (var_spec, "ADADAS") %>%
  xportr_type   (var_spec, "ADADAS") %>%
  xportr_label  (var_spec, "ADADAS") %>%
  xportr_write  ("./adam/adadas.xpt", label = "Subject-Level Analysis Dataset")
