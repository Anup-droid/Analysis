library(tidyverse)
library(haven)
library(labelled)
library(haven)
library(naniar) #to replace values with NA
library(sjlabelled)
library(matrixStats) # for weightedMedian function
library(expss)
library(xlsx)
library(here)
library(skimr)
library(lubridate)
KRdata <- read_dta("D:/NFHS-5/IAKR7DDT/IAKR7DFL.DTA")
# weight variable 
KRdata <- KRdata %>%
  mutate(wt = v005/1000000)

# age of child. If b19 is not available in the data use v008 - b3

if ("TRUE" %in% (!("b19" %in% names(KRdata))))
  KRdata [[paste("b19")]] <- NA
if ("TRUE" %in% all(is.na(KRdata$b19)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  KRdata <- KRdata %>%
    mutate(age = b19)
} else {
  KRdata <- KRdata %>%
    mutate(age = v008 - b3)
}


# # Two age groups "0-23" ,"Other"

# KRdata <- KRdata %>%
#   mutate(agegroup =
#            case_when(
#              age>=0 & age<=23 ~ 1,
#              TRUE ~ 2  )) %>%
#   set_value_labels(agegroup = c("0-23" = 1, "Other"=2)) %>%
#   set_variable_labels(agegroup = "age group of child for vaccination")

# # # Two age groups "0-59" ,"Other"

KRdata <- KRdata %>%
  mutate(agegroup =
           case_when(
             age>=0 & age<=59 ~ 1,
             TRUE ~ 2  )) %>%
  set_value_labels(agegroup = c("0-59" = 1, "Other"=2)) %>%
  set_variable_labels(agegroup = "age group of child for vaccination")


# # Two age groups "12-23" ,"Other"

# KRdata <- KRdata %>%
#   mutate(agegroup =
#            case_when(
#              age>=12 & age<=23 ~ 1,
#              TRUE ~ 2  )) %>%
#   set_value_labels(agegroup = c("12-23" = 1, "Other"=2)) %>%
#   set_variable_labels(agegroup = "age group of child for vaccination")

# Selecting children
# Create subset of KRfile to select for children for VAC indicators
# Select agegroup 1 or agegroup 2

KRvac <- KRdata %>%
  subset(agegroup==1 & b5==1) # select age group and live children 

# ***************************************************************

# Source of vaccination information.
KRvac <- KRvac %>%
  mutate(source = 
           case_when(h1==1 ~ 1, h1==0 | h1==2 | h1==3 ~ 2  )) %>%
  set_value_labels(source = c("card" = 1, "mother"=2)) %>%
  set_variable_labels(source = "source of vaccination information")

# *** Polio ***
# //polio 0, 1, 2, 3 either source
KRvac <- KRvac %>%
  mutate(polio1 = case_when(h4%in%c(1,2,3) ~ 1, h4%in%c(0,8) ~ 0  )) %>%
  mutate(polio2 = case_when(h6%in%c(1,2,3) ~ 1, h6%in%c(0,8) ~ 0  )) %>%
  mutate(polio3 = case_when(h8%in%c(1,2,3) ~ 1, h8%in%c(0,8) ~ 0  )) %>%
  mutate(poliosum=polio1 + polio2 + polio3)

# //polio 0, 1, 2, 3 by card
KRvac <- KRvac %>%
  mutate(ch_polio0_card = case_when(h0%in%c(1,2,3) & source==1 ~ 1, TRUE ~ 0 )) %>%
  set_value_labels(ch_polio0_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio0_card = "Polio at birth vaccination according to card") %>%
  mutate(ch_polio1_card = case_when(poliosum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio1_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio1_card = "Polio 1st dose vaccination according to card") %>%
  mutate(ch_polio2_card = case_when(poliosum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio2_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio2_card = "Polio 2nd dose vaccination according to card") %>%
  mutate(ch_polio3_card = case_when(poliosum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_polio3_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_polio3_card = "Polio 3rd dose vaccination according to card")

# *** Pentavalent ***
# //Pentavalent 1, 2, 3 either source
KRvac <- KRvac %>%
  mutate(pent1 = case_when(h51%in%c(1,2,3) ~ 1, h51%in%c(0,8) ~ 0  )) %>%
  mutate(pent2 = case_when(h52%in%c(1,2,3) ~ 1, h52%in%c(0,8) ~ 0  )) %>%
  mutate(pent3 = case_when(h53%in%c(1,2,3) ~ 1, h53%in%c(0,8) ~ 0  )) %>%
  mutate(pentsum = pent1 + pent2 + pent3)

# //Pentavalent 1, 2, 3 by card
KRvac <- KRvac %>%
  mutate(ch_pent1_card = case_when(pentsum >=1 & source==1~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent1_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent1_card = "Pentavalent 1st dose vaccination according to card") %>%
  mutate(ch_pent2_card = case_when(pentsum >=2 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent2_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent2_card = "Pentavalent 2nd dose vaccination according to card") %>%
  mutate(ch_pent3_card = case_when(pentsum >=3 & source==1 ~ 1, TRUE ~ 0  )) %>%
  set_value_labels(ch_pent3_card = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(ch_pent3_card = "Pentavalent 3rd dose vaccination according to card") 

#Data arrangement
Vac=KRvac %>% select(caseid,b19,v008,b3,sdist,h1,v005,v024,wt,
               age,agegroup,source,
               ch_polio0_card,h0,h0d,h0m,h0y,
               ch_polio1_card,h4,h4d,h4m,h4y,
               ch_pent1_card,h51,h51d,h51m,h51y,
               ch_polio2_card,h6,h6d,h6m,h6y,
               ch_pent2_card,h52,h52d,h52m,h52y,
               ch_polio3_card,h8,h8d,h8m,h8y,
               ch_pent3_card,h53,h53d,h53m,h53y)

# #See the pattern of missing value
Vac %>% skim()

#Making Final Data
Vac=Vac %>% mutate(sumVar = rowSums(.[15:47]))
Vac=Vac %>% filter(sumVar<75000)
Vac %>% skim()

#Vaccine based Analysis
Vac=Vac %>% mutate(opv1_pent1=case_when(h4==h51 & h4d==h51d & 
                   h4m==h51m & h4y==h51y~1,TRUE~0)) %>% 
                   set_value_labels(opv1_pent1= c("No" = 1, "Yes"=0)) %>%
                   set_variable_labels(opv1_pent1="Missed opportunity OPV1 to Penta1")
                   
Vac=Vac %>% mutate(opv2_pent2=case_when(h6==h52 & h6d==h52d & 
                   h6m==h52m & h6y==h52y~1,TRUE~0)) %>% 
                   set_value_labels(opv2_pent2= c("No" = 1, "Yes"=0)) %>%
                   set_variable_labels(opv2_pent2="Missed opportunity OPV2 to Penta2")

Vac=Vac %>% mutate(opv3_pent3=case_when(h8==h53 & h8d==h53d & 
                   h8m==h53m & h8y==h53y~1,TRUE~0)) %>% 
                   set_value_labels(opv3_pent3= c("No" = 1, "Yes"=0)) %>%
                   set_variable_labels(opv3_pent3="Missed opportunity OPV3 to Penta3")

#Table  
table(Vac$opv1_pent1)
table(Vac$opv2_pent2)
table(Vac$opv3_pent3)

# Background characteristics of respondents - women
# Proportion Table
table_temp = Vac %>%
  tab_cells(v024,opv1_pent1) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Missed Opportunity - Child")
table_temp

#Count Table 
table_temp = Vac %>%
  filter(opv1_pent1==0) %>% # Only Missed Opportunity case 
  tab_cells(v024,opv1_pent1) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cases() %>% 
  tab_pivot() %>% 
  tab_caption("Missed Opportunity - Child")
table_temp

#Export result table in xlsx
write.xlsx(table_temp, "Tables_VAC_MO.xlsx", sheetName = "Vac_MO",append=TRUE)

#Visit Based Analysis

Vac=Vac %>% mutate(opv1_d = str_c(h4d,h4m,h4y,sep="-"),
                   opv2_d = str_c(h6d,h6m,h6y,sep="-"),
                   opv3_d = str_c(h8d,h8m,h8y,sep="-"),
                   penta1_d = str_c(h51d,h51m,h51y,sep="-"),
                   penta2_d = str_c(h52d,h52m,h52y,sep="-"),
                   penta3_d = str_c(h53d,h53m,h53y,sep="-"))
#To find out the no of visit
visit=data.frame(visit=apply(Vac[52:57],1,function(x) length(unique(x))))
Vac=cbind(Vac,visit) 

#Visit
Vac=Vac %>% mutate(ch_visit=case_when(visit<=3~1,TRUE~0)) %>% 
  set_value_labels(ch_visit= c("No" = 1, "Yes"=0)) %>%
  set_variable_labels(ch_visit="Missed opportunity visit wise")

#No of visit max and min
min(visit)
max(visit)

#Table
table(Vac$ch_visit)

# Background characteristics of respondents - women
# Proportion Table
table_temp = Vac %>%
  filter(ch_visit==0) %>% 
  tab_cells(sdist,ch_visit) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cpct() %>% 
  tab_pivot() %>% 
  tab_caption("Missed Opportunity - Visit Based")
table_temp
write.xlsx(table_temp, "Tables_VAC_MO.xlsx", sheetName = "Vac_MO",append=TRUE)

#Count Table 
table_temp = Vac %>%
  filter(ch_visit==0) %>% #only for MO
  tab_cells(v024,ch_visit) %>%
  tab_cols()  %>%
  tab_weight(wt) %>% 
  tab_stat_cases() %>% 
  tab_pivot() %>% 
  tab_caption("Missed Opportunity - Visit Based")
table_temp


# To make a date variable convert to numeric
Vac=Vac %>% 
          mutate_at(c(18:47), as.numeric) %>% 
          mutate(dt_OPV1 = make_date(h4y,h4m,h4d),
                   dt_OPV2 = make_date(h6y,h6m,h6d),
                   dt_OPV3 = make_date(h8y,h8m,h8d),
                   dt_penta1 = make_date(h51y,h51m,h51d),
                   dt_penta2 = make_date(h52y,h52m,h52d),
                   dt_penta3 = make_date(h53y,h53m,h53d)) %>% 
         mutate(df_OPV1_Penta1 = dt_OPV1-dt_penta1,
                df_OPV2_Penta2 = dt_OPV2-dt_penta2,
                df_OPV3_Penta3 = dt_OPV3-dt_penta3) %>% 
         mutate(mo_OPV1_Penta1=case_when(df_OPV1_Penta1 < 0 ~ 1,# MO-Penta1
                                         df_OPV1_Penta1 > 0 ~ 2,# MO-OPV1
                                         TRUE~0),# MO-No (OPV1-Penta1)
               mo_OPV2_Penta2=case_when(df_OPV2_Penta2 < 0 ~ 1,# MO-Penta2
                                        df_OPV2_Penta2 > 0 ~ 2,# MO-OPV2
                                        TRUE~0),# MO-No (OPV2-Penta2)
               mo_OPV3_Penta3=case_when(df_OPV3_Penta3 < 0 ~ 1,# MO-Penta3
                                        df_OPV3_Penta3 > 0 ~ 2,# MO-OPV3
                                        TRUE~0))# MO-No (OPV3-Penta3)
#Table Visit wise and antigen wise
table(Vac$ch_visit,Vac$mo_OPV1_Penta1)


      
     
          
  







          

