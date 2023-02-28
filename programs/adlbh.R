# ADLBH Prog ----
#ADMIRAL HACKATHON----
#Team SANOFI_BP----
#Date 28FEB2023----
#Label: Analysis Dataset Lab Hematology

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
lb <-  read_xpt ("./sdtm/lb.xpt")%>% filter(LBCAT=='HEMATOLOGY')
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
param_lookup <- tibble::tribble(
  ~LBTESTCD, ~PARAMCD, ~PARAM,~PARAMN,
  "ANISO","ANISO","Anisocytes",13,
  "BASO","BASO","Basophils (GI/L)",10,
  "EOS","EOS","Eosinophils (GI/L)",9,
  "HCT","HCT","Hematocrit",2,
  "HGB","HGB","Hemoglobin (mmol/L)",1,
  "LYM","LYM","Lymphocytes (GI/L)",7,
  "MACROCY","MACROCY","Macrocytes",14,
  "MCH","MCH","Ery. Mean Corpuscular Hemoglobin (fmol(Fe))",4,
  "MCHC","MCHC","Ery. Mean Corpuscular HGB Concentration (mmol/L)",5,
  "MCV","MCV","Ery. Mean Corpuscular Volume (fL)",3,
  "MICROCY","MICROCY","Microcytes",15,
  "MONO","MONO","Monocytes (GI/L)",8,
  "PLAT","PLAT","Platelet (GI/L)",11,
  "POIKILO","POIKILO","Poikilocytes",16,
  "POLYCHR","POLYCHR","Polychromasia",17,
  "RBC","RBC","Erythrocytes (TI/L)",12,
  "WBC","WBC","Leukocytes (GI/L)",6
  )

##Derivations----
###Predecessors / TRTA(N) / TRTP(N)----

adlbh <- lb %>%
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
adlbh <- adlbh %>%
  ## from LOOK-UP table
  # Replace with PARAMCD lookup function
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = vars(PARAMCD, PARAM, PARAMN),
    by_vars = vars(LBTESTCD),
    check_type = "none",
    print_not_mapped = FALSE
  )%>%
  ### PARCAT1 / AVAL / A1HI / A1LO ----
mutate(
  PARCAT1 = "HEM",
  AVAL = LBSTRESN,
  A1LO = if_else(LBSTNRLO!=0,LBSTNRLO,NA_real_),
  A1HI = LBSTNRHI,
  ANRLO = LBSTNRLO*0.5,
  ANRHI = LBSTNRHI*1.5,
  ### R2A1LO  / R2A1HI ----
  R2A1LO = AVAL / A1LO,
  R2A1HI = AVAL / A1HI
)

###ANRIND ----
#requires the reference ranges ANRLO, ANRHI
adlbh <- adlbh %>%
  derive_var_anrind() %>%
  mutate(ANRIND_ = if_else( !is.na(ANRIND), format_rind(ANRIND),"N"),
         #If A1LO and A1HI are missing and LB.LBNRIND=='ABNORMAL' then  set to 'H'
         ANRIND  = if_else ((is.na(A1LO)|is.na(A1HI)) & LBNRIND=='ABNORMAL','H',ANRIND_)
        )




### AVISIT / AVISITN----

adlbh <- adlbh %>%
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
adlbh <- adlbh %>% filter(VISIT !="BASELINE")

###[PARAM / PARAMCD / PARAMN for changes]----
adlbh_prev <- adlbh %>% mutate( ori_AVAL=AVAL,
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

adlbh <- bind_rows(adlbh_prev, adlbh)

### ABLFL / BASE / BNRIND / CHG ----
adlbh <- adlbh %>% restrict_derivation(
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


adlbh <- adlbh %>% derive_var_chg()%>%
  mutate (CHG=if_else(VISITNUM>1,CHG, NA_real_))


### BR2A1LO / BR2A1HI----
adlbh <-  adlbh %>% derive_var_base(
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
# to match with compare, absolute values have been taken

adlbh <- adlbh %>%
  mutate(ALBTRVAL_2= abs((0.5*LBSTNRLO)-LBSTRESN )) %>%
  mutate(ALBTRVAL_1= abs(LBSTRESN - (1.5*LBSTNRHI))) %>%
  rowwise() %>%
  mutate(ALBTRVAL= max(c(ALBTRVAL_2,ALBTRVAL_1))) %>% select(-c(ALBTRVAL_1,ALBTRVAL_2))


###ANL01FL----

adlbh <- adlbh %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(STUDYID, USUBJID, PARAMCD),
      order = vars(desc(ALBTRVAL), AVISITN, LBSEQ),
      new_var = ANL01FL,
      mode = "first",
      check_type = "none"
    ),
    filter = !is.na(ALBTRVAL) & AVISITN > 0  & AVISITN <= 24
  )


#Retrieve EOT visit
adlbh <- adlbh %>%
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



### AENTMTFL----
#Did not reach WEEk 24
Last_obs1 <- adlbh %>% filter (COMP24FL == 'N' & !is.na(AVISIT)) %>%
  distinct(USUBJID,PARAMCD, VISITNUM)

#Completed WEEk 24 and performed visit 12
Comp24_and_Vis12 <- adlbh %>% filter (COMP24FL == 'Y' & VISITNUM==12) %>%
  distinct(USUBJID,PARAMCD, VISITNUM) %>% mutate(C24_Vis12='Y')

#Completed WEEk 24 (even if visit 12 has not been performed)
#in case visit 12 has not been performed, keep only the visit before visit 12
Last_obs2 <- adlbh %>% filter (COMP24FL == 'Y' & !is.na(AVISIT))%>%
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

#Flag assignment
adlbh <- adlbh %>% restrict_derivation(
  derivation = derive_var_merged_exist_flag,
  args = params(
    dataset_add = Last_obs,
    by_vars = vars(USUBJID, PARAMCD,VISITNUM),
    new_var = AENTMTFL,
    condition = (!is.na(VISITNUM))
  ),
  filter = (!is.na(AVISIT) & AVISIT != "Baseline")
)

# RACEN----
adlbh <- adlbh %>% mutate(RACEN = format_racen(RACE))


## Final dataset----
var_spec <- readxl::read_xlsx("./metadata/specs.xlsx", sheet = "Variables") %>%
  dplyr::rename(type = "Data Type") %>%
  rlang::set_names(tolower) %>% filter (dataset=="ADLBH")


var_spec$type   <- recode(var_spec$type, text="Char")
var_spec$type [str_ends(var_spec$variable, "DT")] <- "Date"
var_spec$format <- recode(var_spec$format,DATE9.="DATE.")
var_spec$length <- as.numeric(var_spec$length)
var_spec$order  <- as.numeric(var_spec$order)


adlbh <- adlbh %>% select(var_spec$variable) %>%
  arrange(USUBJID, PARAMCD, AVISITN, LBSEQ) %>%
  filter(!(str_starts(PARAMCD, "_")))

### Applying metadata----
adlbh <- adlbh %>%
  xportr_length (var_spec, "ADLBH") %>%
  xportr_order  (var_spec, "ADLBH") %>%
  xportr_label  (var_spec, "ADLBH")


### Save as XPT----
adlbh %>%
  xportr_format (var_spec, "ADLBH") %>%
  xportr_type   (var_spec, "ADLBH") %>%
  xportr_label  (var_spec, "ADLBH") %>%
  xportr_write  ("./adam/adlbh.xpt", label = "Subject-Level Analysis Dataset")



