library(readxl)
specs_vars <- read_xlsx("metadata/specs.xlsx",sheet="Variables")
specs_vars$ID <- paste(specs_vars$Dataset,specs_vars$Variable,sep=".")
specs_meth <- read_xlsx("metadata/specs.xlsx",sheet="Methods")

specs <- merge(specs_vars,specs_meth,by="ID",all.x=T, all.y=T)
specs$Order <- as.numeric((specs$Order))

specs <- specs %>%
  dplyr::na_if("<NA>")

# specs_adsl <- specs[which(specs$Dataset %in% c("ADSL",NA)),colSums(is.na(specs_adsl))<nrow(specs_adsl)]
specs_adsl <- specs[which(specs$Dataset %in% c("ADSL",NA)),c("Order","Variable","Method","Description")]




#### ADSL function specs

type <- specs_adsl

px <- function(var,df=type){
  x <- df[which(df$Variable == var),]
  x <- x[,colSums(is.na(x))<nrow(x)]
  print(t(x))
}

pn <- function(order,df=type){
  x <- df[which(df$Order == order),]
  x <- x[,colSums(is.na(x))<nrow(x)]
  print(t(x))
}


pn(1)
px("AGEGR1N")
