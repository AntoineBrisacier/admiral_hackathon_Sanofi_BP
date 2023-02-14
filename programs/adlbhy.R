# ADLBHY Prog ----

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
adlbhy <- read_xpt ("./adam/adlbc.xpt")%>% filter(PARAMCD %in% c("ALT","AST","BILI")) %>% select(-ANL01FL)

## Convert blanks to NA----
adlbhy <- convert_blanks_to_na(adlbhy)

## Category functions
format_shift <- function(x) {
  case_when(
    toupper(x)=='HIGH TO NORMAL'   ~ 'High to Normal',
    toupper(x)=='NORMAL TO NORMAL' ~ 'Normal to Normal',
    toupper(x)=='NORMAL TO HIGH'   ~ 'Normal to High'
    #HIGH HIGH and LOW-LOW !!----
  )
}


format_shiftn <- function(x) {
  case_when(
    x=='High to Normal' ~ 0,
    x=='Normal to Normal' ~ 1,
    x=='Normal to High' ~ 2
    #HIGH HIGH and LOW-LOW !!----
  )
}

##Look-up tables ----
# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~LBTESTCD, ~PARAMCD,  ~PARAM,                                           ~PARAMN, ~PARCAT1
  # NA_character_, "BILIHY",  "Bilirubin 1.5 x ULN",                             4,  HYLAW,
  # NA_character_, "TRANSHY", "Transaminase 1.5 x ULN",                          5,  HYLAW,
  # NA_character_, "HYLAW",   "Total Bili 1.5 x ULN and Transaminase 1.5 x ULN", 6,  HYLAW
  )

#TO DELETE:----
adlbhy <- adlbhy %>%  filter( USUBJID == "01-701-1015")


##Derivations----
### CRIT1 / CRIT1FL /CRIT1FN

adlbhy <- adlbhy %>%
  mutate (CRIT1 = "R2A1HI > 1.5",
          CRIT1FL = if_else((R2A1HI > 1.5),"Y","N"),
          CRIT1FN = if_else((R2A1HI > 1.5),1,0),
  )

###SHIFT(N)----

adlbhy <- adlbhy %>%
      derive_var_shift(      new_var = SHIFT1,
                             from_var = BNRIND,
                             to_var = ANRIND
    ) %>%
  mutate (SHIFT1 = format_shift(SHIFT1),SHIFT1N=format_shiftn(SHIFT1))





#
# %>%
# ### PARCAT1 / AVAL / A1HI / A1HLO ----
# mutate(
#   PARCAT1 = "CHEM",
#   AVAL = LBSTRESN,
#   A1LO = LBSTNRLO,
#   A1HI = LBSTNRHI,
#   ANRLO = LBSTNRLO,
#   ANRHI = LBSTNRHI,
# ### R2A1LO  / R2A1HI ----
#  R2A1LO = AVAL / A1LO,
#  R2A1HI = AVAL / A1HI
#
# )
#
# ###ANRIND ----
# #requires the reference ranges ANRLO, ANRHI
# adlbhy <- adlbhy %>%
#   derive_var_anrind() #%>%
#   #mutate(ANRIND = format_rind(ANRIND))
#
# ### AVISIT / AVISITN
# adlbhy <- adlbhy %>%
#   # Derive Timing
#   mutate(
#     AVISIT = case_when(
#       str_detect(VISIT, "UNSCHED") ~ NA_character_,
#       str_detect(VISIT, "SCREEN") ~ "Baseline",
#       RFENDT == ADT ~ "End of Treatment",
#       !is.na(VISIT) ~ str_to_title(VISIT),
#       TRUE ~ NA_character_
#     ),
#     AVISITN = case_when(
#       is.na(AVISIT) ~ NA_real_,
#       AVISIT == "Baseline" ~ 0,
#       AVISIT == "End of Treatment" ~ 99,
#       TRUE ~ as.numeric (str_remove(AVISIT,'Week '))
#           )
#   )
#
#
# ### ABLFL / BASE / BNRIND / CHG ----
#
# adlbhy <- adlbhy %>% restrict_derivation(
#                   derivation = derive_var_extreme_flag,
#                   args = params(
#                     by_vars = vars(STUDYID, USUBJID, PARAMCD),
#                     order = vars(ADT,VISITNUM),
#                     new_var = ABLFL,
#                     mode = "last"
#                   ),
#                   filter = (!is.na(AVAL) & ADT <= TRTSDT & VISITNUM==1)
#                   ) %>%
#                   derive_var_base(
#                       by_vars = vars(STUDYID, USUBJID, PARAMCD),
#                       source_var = AVAL,
#                       new_var = BASE
#                     ) %>%
#                   derive_var_base(
#                     by_vars = vars(STUDYID, USUBJID, PARAMCD),
#                     source_var = ANRIND,
#                     new_var = BNRIND
#                    )   #%>%
#                   # mutate(BNRIND=format_rind(BNRIND_)) %>%
#
# adlbhy <- adlbhy %>% derive_var_chg()
#
# ### BR2A1LO / BR2A1HI----
# adlbhy <- adlbhy %>% derive_var_base(
#           by_vars = vars(STUDYID, USUBJID, PARAMCD),
#           source_var = R2A1LO,
#           new_var = BR2A1LO
#         ) %>%
#           derive_var_base(
#             by_vars = vars(STUDYID, USUBJID, PARAMCD),
#             source_var = R2A1HI,
#             new_var = BR2A1HI
#         )





## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADLBHY")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adlbhy <- adlbhy %>% select(var_spec$variable) %>%
  arrange(USUBJID, PARAMCD, AVISITN)


### Applying metadata----
adlbhy <- adlbhy %>%
  xportr_length (var_spec, "ADLBHY") %>%
  xportr_order  (var_spec, "ADLBHY") %>%
  xportr_label  (var_spec, "ADLBHY")


### Save as XPT----
adlbhy %>%
  xportr_format (var_spec, "ADLBHY") %>%
  xportr_type   (var_spec, "ADLBHY") %>%
  xportr_label  (var_spec, "ADLBHY") %>%
  xportr_write  ("./adam/adlbhy.xpt", label = "Subject-Level Analysis Dataset")





