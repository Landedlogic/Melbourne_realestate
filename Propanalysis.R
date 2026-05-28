#Set up

getwd()
setwd("/Users/godwinkavaarpuo/Library/CloudStorage/GoogleDrive-gkavaarpuo@gmail.com/My Drive/Research/R")
list.files()

library("pacman")
pacman::p_load(arrow, sandwich, haven, modelsummary, officer, modelsummary, skimr, dplyr, data.table)

# Data load ---------------------------------------------

df<-iris
View(df)
df["Species"]
df$sep<-df$Sepal.Length*3

df2<- df%>%
  select(starts_with("Sepal"))
mean(df2$Sepal.Width)
mean(df2[["Sepal.Width"]])


iap <- read_dta("IAP_master.dta")
View(iap)

skim(iap$ServiceProvide)

prop <- read.csv("~/Library/CloudStorage/GoogleDrive-gkavaarpuo@gmail.com/My Drive/Research/Housing transaction data Melbourne/apm_point_sold_vic 2019 to 2024.csv")


write_parquet(FullData_stata, "propdatavic.parquet")

pardata<-read_parquet("propdatavic.parquet")
df <- read_parquet(
  "https://huggingface.co/datasets/thehooklab/nsw-property/resolve/main/nsw_property_sales_master.parquet")


# Wrangling and descriptive stats -----------------------

View(prop)
glimpse(prop)

DT<-prop
DT[,mean_price:=mean(eventprice), by=.(suburb,year)]

FullData_stata<-FullData_stata %>%
  group_by(suburb,year)%>%
  mutate(mean_price=mean(eventprice, na.rm=TRUE))


pardata %>%
  select(the_geom)%>%
  filter(!is.na(the_geom)) %>%
  
  
  data.frame(names(df))


#Results ------------------------------------------------

fit1 <- lm(Logfinalresultprice ~ LOGAREA+baths+hasstudy, data = pardata, na.action = na.omit)

fit2 <- lm(eventprice ~ LOGAREA+baths+hasstudy, data = pardata, na.action = na.omit)
summary(fit)



# Diagnositic checks ------------------------------------

# 1. Breusch–Pagan test for homoskedasticity
library(lmtest)
bptest(fit1)
#if p-value (< 0.05), reject homoskedasticity, provide
#heteroskedastic robust standard errors



# Variance inflation factor
library(car) #don't loan car as if conflicts with recode from dyplr
car:: vif(fit1) #values less than 5 are generally acceptable.




# Robustness check --------------------------------------

# Export results --------------------------------------


#plot(fit)  # always check diagnostics
modelsummary(list(fit1,fit2), vcov ="HC1", 
             estimate = "{estimate}{stars}",
             statistic = "({std.error})",
             notes = "HC1 is robust standard errors",
             output="propresults.docx" )

#alternative for one model
modelsummary(fit1,vcov="HC1",
             estimate = "{estimate}{stars}",
             statistic="({std.error})",
             notes = "HC1 is robust standard errors",
             output="propresutls1.docx")


df %>%
  summarise(
    median_income = median(suburb_median_income, na.rm = TRUE),
    .by = property_type)
