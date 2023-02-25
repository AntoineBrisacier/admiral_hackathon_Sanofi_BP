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
lb <-  read_xpt ("./sdtm/lb.xpt") %>% filter(LBTESTCD %in% c("ALT","AST","BILI"))
adsl <- read_xpt ("./adam/adsl.xpt")

## Convert blanks to NA----
lb <- convert_blanks_to_na(lb)

## Category functions
format_shift <- function(x) {
  case_when(
    toupper(x)=='HIGH TO NORMAL'   ~ 'High to Normal',
    toupper(x)=='NORMAL TO NORMAL' ~ 'Normal to Normal',
    toupper(x)=='NORMAL TO HIGH'   ~ 'Normal to High'
    #HIGH HIGH and LOW-LOW !!----
  )
}

format_rind <- function(x) {
  case_when(
    str_detect(x, "LOW") ~ "L",
    str_detect(x, "HIGH") ~ "H",
    str_detect(x, "NORMAL") ~ "N"
  )
}

format_shiftn <- function(x) {
  case_when(
    x=='High to Normal' ~ 0,
    x=='Normal to Normal' ~ 1,
    x=='Normal to High' ~ 2
  )
}



##Look-up tables ----
# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~ori_PARAMCD, ~LBTESTCD,     ~PARAMCD,  ~PARAM,                                           ~PARAMN, ~PARCAT1,
  "ALT",         "ALT",         "ALT",     "Alanine Aminotransferase (U/L)",                  1,       "CHEM",
  "AST",         "AST",         "AST",     "Aspartate Aminotransferase (U/L)",                2,       "CHEM",
  "BILI",        "BILI",       "BILI",     "Bilirubin (umol/L)",                              3,       "CHEM",
)

##Derivations----
###Predecessors / TRTA(N) / TRTP(N)----

adlbhy <- lb %>%
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


###PARAM / PARAMCD [ALT-AST-BILI]----

adlbhy <- adlbhy %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars(PARAMCD, PARAM, PARAMN,PARCAT1),
    by_vars = vars(LBTESTCD),
    check_type = "none",
    print_not_mapped = FALSE) %>%
    ### AVAL [CHEM]----
    mutate(AVAL = LBSTRESN,
           A1LO = LBSTNRLO,
           ### A1HI / A1LO ----
           A1HI = LBSTNRHI,
           ANRLO = LBSTNRLO*0.5,
           ANRHI = LBSTNRHI*1.5,
           ### R2A1LO / R2A1HI ----
           R2A1LO = AVAL / A1LO,
           R2A1HI = AVAL / A1HI)

###ANRIND ----
#requires the reference ranges ANRLO, ANRHI
adlbhy <- adlbhy %>%
  derive_var_anrind() %>%
  mutate(ANRIND = if_else( !is.na(ANRIND), format_rind(ANRIND),"N"))

### AVISIT / AVISITN----
adlbhy <- adlbhy %>%
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


adlbhy <- adlbhy %>% filter (!is.na(AVISITN) & AVISITN<=24)

### CRIT1 / CRIT1FL /CRIT1FN----
adlbhy <- adlbhy %>%
  mutate (CRIT1 = "R2A1HI > 1.5",
          CRIT1FL = if_else((R2A1HI > 1.5),"Y","N"),
          CRIT1FN = if_else((R2A1HI > 1.5),1,0),
  )

###PARAM / PARAMCD [HYLAW PARAMETERS]----

adlbhy_AST_0 <-
  adlbhy %>% select(USUBJID, PARAMCD, AVAL, AVISITN, AVISIT, A1LO, A1HI) %>%
  filter(PARAMCD=="AST") %>% rename(AST=AVAL, A1LO_AST=A1LO, A1HI_AST=A1HI) %>%
  select(-PARAMCD)

adlbhy_ALT_0 <-
  adlbhy %>% select(USUBJID, PARAMCD, AVAL, AVISITN, AVISIT,A1LO, A1HI) %>%
  filter(PARAMCD=="ALT") %>% rename(ALT=AVAL, A1LO_ALT=A1LO, A1HI_ALT=A1HI) %>%
  select(-PARAMCD)


adlbhy_BILI_0 <-
  adlbhy %>% select(USUBJID, PARAMCD, AVAL, AVISITN,AVISIT, A1LO, A1HI) %>%
  filter(PARAMCD=="BILI") %>% rename(BILI=AVAL, A1LO_BILI=A1LO, A1HI_BILI=A1HI) %>%
  select(-PARAMCD)

adlbhy_HYLAW_0 <- adlbhy_AST_0 %>% left_join(adlbhy_ALT_0,  by=c("USUBJID","AVISITN","AVISIT")) %>%
                               left_join(adlbhy_BILI_0, by=c("USUBJID","AVISITN","AVISIT"))

bilihy <- adlbhy_HYLAW_0 %>%
        mutate(AVAL=(if_else(BILI > (1.5*A1HI_BILI) ,1 , 0))) %>%
        select(-c("ALT","AST", starts_with("A1"))) %>%
        mutate(PARAMCD= "BILIHY", PARAM = "Bilirubin 1.5 x ULN", PARAMN = 4 , PARAMTYP = "DERIVED", PARCAT1= "HYLAW")

transhy <- adlbhy_HYLAW_0 %>%
  mutate(AVAL=(if_else(ALT > (1.5*A1HI_ALT) | AST > (1.5*A1HI_AST),1, 0)))%>%
  select(-c("BILI", starts_with("A1"))) %>%
  mutate(PARAMCD= "TRANSHY", PARAM = "Transaminase 1.5 x ULN", PARAMN = 5 , PARAMTYP = "DERIVED",PARCAT1= "HYLAW")

HYLAW <- bilihy %>% left_join(transhy , by=c("USUBJID","AVISITN", "AVISIT")) %>%
  mutate(AVAL=(if_else(AVAL.x == 1 & AVAL.y == 1,1, 0))) %>%
    select(c("USUBJID","BILI","AVISITN","AVISIT", "AVAL")) %>%
    rename(HYLAW=BILI) %>%
    mutate(PARAMCD= "HYLAW", PARAM = "Total Bili 1.5 x ULN and Transaminase 1.5 x ULN", PARAMN = 6, PARAMTYP = "DERIVED",PARCAT1= "HYLAW")


adlbhy_prev <- bind_rows(bilihy ,transhy,HYLAW) %>%
  mutate( ADY=NA_integer_,
          ADT=NA,
          VISITNUM=NA_integer_,
          VISIT="",
          A1LO=NA_real_,
          A1HI=NA_real_,
          R2A1LO=NA_real_,
          R2A1HI=NA_real_,
  )

adlbhy_prev <- adlbhy_prev %>%
  # Join ADSL variables with LB
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = vars (AGE, AGEGR1, AGEGR1N, COMP24FL, DSRAEFL, RACE,
                     RACEN, SAFFL, SEX, STUDYID, SUBJID, TRTA=TRT01A, TRTAN=TRT01AN,
                     TRTEDT, TRTP=TRT01P, TRTPN=TRT01PN, TRTSDT, USUBJID, RFENDT),
    by_vars = vars(USUBJID)
)

adlbhy <- bind_rows(adlbhy_prev, adlbhy)

### ABLFL----
adlbhy <- adlbhy %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, PARAMCD),
      order = vars(ADT,VISITNUM),
      new_var = ABLFL,
      mode = "last"
    ),
    filter = (ADT <= TRTSDT & VISITNUM==1)
  )

adlbhy <- adlbhy %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, PARAMCD),
      order = vars(ADT,AVISITN),
      new_var = ABLFL,
      mode = "last"
    ),
    filter = (AVISITN<=0 & PARAMCD %in% c("BILIHY", "HYLAW", "TRANSHY"))
  )

### BR2A1LO / BR2A1HI / BASE / BNRIND----
adlbhy <- adlbhy %>%
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = R2A1LO,
    new_var = BR2A1LO
  ) %>%
  derive_var_base(
    by_vars = vars(STUDYID, USUBJID, PARAMCD),
    source_var = R2A1HI,
    new_var = BR2A1HI
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

adlbhy <- adlbhy %>% mutate(BASE=if_else(PARAMCD %in% c("BILIHY", "HYLAW", "TRANSHY") & is.na(AVAL), NA_real_, BASE))

###SHIFT(N)----

adlbhy <- adlbhy %>%
  restrict_derivation(
    derivation =derive_var_shift,
    args = params(
      new_var = SHIFT1,
      from_var = BNRIND,
      to_var = ANRIND),
    filter = (PARAMCD %in% c("ALT", "AST", "BILI"))
    )

adlbhy <- adlbhy %>%
  mutate(SHIFT1=(case_when(
                      PARAMCD %in% c("BILIHY", "HYLAW", "TRANSHY") & AVAL==1 & BASE==0 ~ 'Normal to High',
                      PARAMCD %in% c("BILIHY", "HYLAW", "TRANSHY") & AVAL==0 & BASE==0 ~ 'Normal to Normal',
                      PARAMCD %in% c("BILIHY", "HYLAW", "TRANSHY") & AVAL==0 & BASE==1 ~ 'High to Normal')
                )
        )%>%
  mutate (SHIFT1 = format_shift(SHIFT1),SHIFT1N=format_shiftn(SHIFT1))


## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADLBHY")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
# var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adlbhy <- adlbhy %>% select(var_spec$variable) %>%
  arrange(USUBJID, PARAMCD, AVISIT) %>%
  filter(!(str_starts(PARAMCD, "_")))

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





