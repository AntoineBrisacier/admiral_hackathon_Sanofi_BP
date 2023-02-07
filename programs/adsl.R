library(haven)
library(admiral)
library(dplyr)
library(tidyr)
library(metacore)
library(metatools)
library(xportr)

dm <- read_xpt("sdtm/dm.xpt")
ex <- read_xpt("sdtm/ex.xpt")

xportr_write(adsl, "adam/adsl.xpt")

admiral::use_ad_template("adsl",save_path = "programs/adsl2.R")
admiral::use_ad_template("advs",save_path = "programs/advs.R")

admiral::list_all_templates()
