# ADLBC Prog ----

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
lb <-  read_xpt ("./sdtm/lb.xpt")%>% filter(LBCAT=='CHEMISTRY')
adsl <- read_xpt ("./adam/adsl.xpt")

## Convert blanks to NA----
lb <- convert_blanks_to_na(lb)

## Category functions
format_rind <- function(x) {
  case_when(
    str_detect(x, "LOW") ~ "L",
    str_detect(x, "HIGH") ~ "H",
    str_detect(x, "NORMAL") ~ "N"
  )
}

##Look-up tables ----
# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~LBTESTCD, ~PARAMCD, ~PARAM,                           ~PARAMN,
  "ALB",    "ALB",    "Albumin (g/L)",                    33,
  "ALP",    "ALP",    "Alkaline Phosphatase (U/L)",       22,
  "ALT",    "ALT",    "Alanine Aminotransferase (U/L)",   24,
  "AST",    "AST",    "Aspartate Aminotransferase (U/L)", 25,
  "BILI",   "BILI",   "Bilirubin (umol/L)",               21,
  "BUN",    "BUN",    "Blood Urea Nitrogen (mmol/L)",     26,
  "CA",     "CA",     "Calcium (mmol/L)",                 30,
  "CHOL",   "CHOL",   "Cholesterol (mmol/L)",             34,
  "CK",     "CK",     "Creatine Kinase (U/L)",            35,
  "CL",     "CL",     "Chloride (mmol/L)",                20,
  "CREAT",  "CREAT",  "Creatinine (umol/L)",              27,
  "GGT",    "GGT",    "Gamma Glutamyl Transferase (U/L)", 23,
  "GLUC",   "GLUC",   "Glucose (mmol/L)",                 31,
  "K",      "K",      "Potassium (mmol/L)",               19,
  "PHOS",   "PHOS",   "Phosphate (mmol/L)",               29,
  "PROT",   "PROT",   "Protein (g/L)",                    32,
  "SODIUM", "SODIUM", "Sodium (mmol/L)",                  18,
  "URATE",  "URATE",  "Urate (umol/L)",                   28,
  )

##Derivations----
###Predecessors / TRTA(N) / TRTP(N)----

adlbc <- lb %>%
  # Join ADSL variables with LB
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars (AGE, AGEGR1, AGEGR1N, COMP24FL, DSRAEFL, RACE,
                     RACEN, SAFFL, SEX, STUDYID, SUBJID, TRTA=TRT01A, TRTAN=TRT01AN,
                     TRTEDT, TRTP=TRT01P, TRTPN=TRT01PN, TRTSDT, USUBJID, RFENDT),
    by_vars = vars(STUDYID, USUBJID)
  ) %>%

### ADT / ADY ----
   derive_vars_dt(new_vars_prefix = "A", dtc = LBDTC) %>%
   derive_vars_dy(reference_date = TRTSDT, source_vars = vars(ADT))

###PARAM / PARAMCD / PARAMN----
adlbc <- adlbc %>%
## from LOOK-UP table
    # Replace with PARAMCD lookup function
    derive_vars_merged_lookup(
      dataset_add = param_lookup,
      new_vars = vars(PARAMCD, PARAM, PARAMN),
      by_vars = vars(LBTESTCD),
      check_type = "none",
      print_not_mapped = FALSE
    )%>%
    ### PARCAT1 / AVAL / A1HI / A1HLO ----
    mutate(
      PARCAT1 = "CHEM",
      AVAL = LBSTRESN,
      A1LO = LBSTNRLO,
      A1HI = LBSTNRHI,
      ANRLO = LBSTNRLO,
      ANRHI = LBSTNRHI,
    ### R2A1LO  / R2A1HI ----
     R2A1LO = AVAL / A1LO,
     R2A1HI = AVAL / A1HI
    )



###ANRIND ----
#requires the reference ranges ANRLO, ANRHI
adlbc <- adlbc %>%
  derive_var_anrind() %>%
  mutate(ANRIND = format_rind(ANRIND))

### AVISIT / AVISITN----

adlbc <- adlbc %>%
  # Derive Timing
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "UNSCHED") ~ NA_character_,
      VISITNUM==1 ~ "Baseline",
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = case_when(
      is.na(AVISIT) ~ NA_real_,
      AVISIT == "Baseline" ~ 0,
      TRUE ~ as.numeric (str_remove(AVISIT,'Week '))
    )
  )
#Retrieve EOT visit
adlbc <- adlbc %>%
  derive_extreme_records(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    order = vars(ADT, AVISITN, AVAL),
    mode = "last",
    filter = (AVISITN > 0 & AVISITN <= 24),
    set_values_to = vars(
      AVISIT = "End of Treatment",
      AVISITN = 99
    )) %>%
    filter (!(AVISIT %in% c("Ambul Ecg Removal", "Retrieval")))



###[PARAM / PARAMCD / PARAMN for changes]----
adlbc_prev <- adlbc %>% mutate( ori_AVAL=AVAL,
                                ori_PARAM=PARAM,
                                ori_PARAMCD=PARAMCD,
                                ori_PARAMN=PARAMN
                                ) %>%
  select (-c(R2A1LO,R2A1HI, AVAL, starts_with("PARAM"))) %>%
  group_by (USUBJID, ori_PARAMCD) %>%
  arrange (USUBJID, ori_PARAMCD, AVISITN) %>%
  mutate(AVAL    = round((ori_AVAL-lag(ori_AVAL))/(ANRHI-ANRLO),5),
         PARAM   = paste0(ori_PARAM," change from previous visit, relative to normal range"),
         PARAMCD = paste0("_",ori_PARAMCD),
         PARAMN  = ori_PARAMN+100
         ) %>%
         ungroup() %>%
         select(-c(starts_with("ori"), A1LO, A1HI,ANRLO , ANRHI ))

adlbc <- bind_rows(adlbc_prev, adlbc)




### ABLFL / BASE / BNRIND / CHG ----
adlbc <- adlbc %>% restrict_derivation(
                  derivation = derive_var_extreme_flag,
                  args = params(
                    by_vars = vars(STUDYID, USUBJID, PARAMCD),
                    order = vars(ADT,VISITNUM),
                    new_var = ABLFL,
                    mode = "last"
                  ),
                  filter = (!is.na(AVAL) & ADT <= TRTSDT & VISITNUM==1)
                  ) %>%
                  derive_var_base(
                      by_vars = vars(STUDYID, USUBJID, PARAMCD),
                      source_var = AVAL,
                      new_var = BASE
                  ) %>%
                  derive_var_base(
                    by_vars = vars(STUDYID, USUBJID, PARAMCD),
                    source_var = ANRIND,
                    new_var = BNRIND
                  )


adlbc <- adlbc %>% derive_var_chg()%>%
  mutate (CHG=if_else(VISITNUM>1,CHG, NA_real_))


### BR2A1LO / BR2A1HI----
adlbc <-  adlbc %>% derive_var_base(
          by_vars = vars(STUDYID, USUBJID, PARAMCD),
          source_var = R2A1LO,
          new_var = BR2A1LO
        ) %>%
          derive_var_base(
            by_vars = vars(STUDYID, USUBJID, PARAMCD),
            source_var = R2A1HI,
            new_var = BR2A1HI
        )


### ALBTRVAL----
# Maximum of [LBSTRESN-(1.5*ULN)] and [(.5*LLN) - LBSTRESN]
# ==> pas clair les valeurs absolues _ mais ok avec compare!!!!

adlbc <- adlbc %>%
        mutate(ALBTRVAL_2= abs((0.5*LBSTNRLO)-LBSTRESN )) %>%
        mutate(ALBTRVAL_1= abs(LBSTRESN - (1.5*LBSTNRHI))) %>%
        rowwise() %>%
        mutate(ALBTRVAL= max(c(ALBTRVAL_2,ALBTRVAL_1))) %>% select(-c(ALBTRVAL_1,ALBTRVAL_2))


### AENTMTFL----
#Did not reach WEEk 24
Last_obs1 <- adlbc %>% filter (COMP24FL == 'N' & !is.na(AVISIT)) %>%
                        distinct(USUBJID,PARAMCD, VISITNUM)

#Completed WEEk 24 and performed visit 12
Comp24_and_Vis12 <- adlbc %>% filter (COMP24FL == 'Y' & VISITNUM==12) %>%
                    distinct(USUBJID,PARAMCD, VISITNUM) %>% mutate(C24_Vis12='Y')

#Completed WEEk 24 (even if visit 12 has not been performed)
#in case visit 12 has not been performed, keep only the visit before visit 12
Last_obs2 <- adlbc %>% filter (COMP24FL == 'Y' & !is.na(AVISIT))%>%
                        distinct(USUBJID,PARAMCD, VISITNUM) %>%
                        derive_vars_merged(
                            dataset_add = Comp24_and_Vis12,
                            new_vars = vars(C24_Vis12),
                            by_vars = vars(USUBJID,PARAMCD)
                        ) %>% filter(C24_Vis12=='Y' | (is.na(C24_Vis12) & VISITNUM < 12))

#Put data together and retrieve last visit with treatment
Last_obs <- bind_rows(Last_obs1, Last_obs2) %>%
                    filter_extreme( by_vars  = vars(USUBJID, PARAMCD),
                      order      = vars(VISITNUM),
                      mode       = "last",
                      check_type = "warning") %>%
                      mutate(VISITNUM = if_else(VISITNUM>12, 12, VISITNUM))

#Assign flag
adlbc <- adlbc %>% restrict_derivation(
                            derivation = derive_var_merged_exist_flag,
                            args = params(
                              dataset_add = Last_obs,
                              by_vars = vars(USUBJID, PARAMCD,VISITNUM),
                              new_var = AENTMTFL,
                              condition = (!is.na(VISITNUM))
                            ),
                            filter = (!is.na(AVISIT) & AVISIT != "Baseline")
                          )

###ANL01FL [A DERIVER] ----
min_val <-  adlbc %>%
            filter (!is.na(AVAL) & AVISITN > 0) %>%
            derive_summary_records(
              by_vars = vars(USUBJID, PARAMCD),
              analysis_var = AVAL,
              summary_fun = function(x)
                min(x, na.rm = TRUE),
              set_values_to = vars(ANL01FL = "A DERIVER")
            ) %>%
            filter(ANL01FL == 'A DERIVER') %>%
            distinct(USUBJID, PARAMCD, AVAL, ANL01FL)


adlbc <- adlbc %>%
  derive_vars_merged(
    dataset_add = min_val,
    new_vars = vars(ANL01FL),
    by_vars = vars(USUBJID, PARAMCD, AVAL)
  )

## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADLBC")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adlbc <- adlbc %>% select(var_spec$variable) %>%
  arrange(USUBJID, PARAMCD, AVISITN, LBSEQ) %>%
  filter(!(str_starts(PARAMCD, "_")))

### Applying metadata----
adlbc <- adlbc %>%
  xportr_length (var_spec, "ADLBC") %>%
  xportr_order  (var_spec, "ADLBC") %>%
  xportr_label  (var_spec, "ADLBC")


### Save as XPT----
adlbc %>%
  xportr_format (var_spec, "ADLBC") %>%
  xportr_type   (var_spec, "ADLBC") %>%
  xportr_label  (var_spec, "ADLBC") %>%
  xportr_write  ("./adam/adlbc.xpt", label = "Subject-Level Analysis Dataset")
