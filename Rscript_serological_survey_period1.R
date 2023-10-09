##################
# R script for data analysis of the 
# Manuscript "Serological screening in a large-scale municipal survey in Cascais, Portugal, during the first waves of the COVID-19 pandemic: lessons for the future"
### Authors: Sofia G. Seabra1†, Francisco Merca1,3†, Bernardo Pereira1, Ivo Fonseca1, Ana Cláudia Carvalho2, Vera Brito2, Daniela Alves1, Pieter Libin3, M. Rosário O. Martins1, Mafalda N. S. Miranda1, Marta Pingarilho1, Victor Pimentel1, Ana B. Abecasis1*
### Addresses
# 1 Global Health and Tropical Medicine, GHTM, Associate Laboratory in Translation and Innovation Towards Global Health, LA-REAL, Instituto de Higiene e Medicina Tropical, IHMT, Universidade NOVA de Lisboa, UNL, Rua da Junqueira 100, 1349-008 Lisboa, Portugal.
# † These authors share first authorship
# 2 Câmara Municipal de Cascais, Portugal
# 3 Artificial Intelligence Research Lab, Vrije Universiteit Brussels (VUB), Pleinlaan 2, 1050 Brussel, Belgium  

# Open packages
library(reshape2)
library(grid)
library(scales)
library(ggthemes)
library(openxlsx)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(gridExtra)
library(lubridate)
library(naniar)
library(DescTools)
library("epitools")
library(Amelia)
library(caret)
library(MASS)
library("ResourceSelection")


# Set working directory
setwd('/USER_FOLDER')

# Read dataframe from xlsx file (header line with variable names)
df<-read.xlsx("./data_input.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# Get column names (variable names)
colnames(df)

# Convert to date and numeric formats
df[,"Data_contacto"] <- openxlsx::convertToDateTime(df[,"Data_contacto"]) # Convert numeric to Date format
df[,"Data_teste_positivo"] <- openxlsx::convertToDateTime(df[,"Data_teste_positivo"]) # Convert numeric to Date format
df[,"Ano_nascimento"] <- as.numeric(df[,"Ano_nascimento"])
df[,"Data_teste_serologico"] <- openxlsx::convertToDateTime(df[,"Data_teste_serologico"]) # Convert numeric to Date format
df[,"week_serological_test"] <- openxlsx::convertToDateTime(df[,"week_serological_test"]) # Convert numeric to Date format
df[,"week_positivePCR"] <- openxlsx::convertToDateTime(df[,"week_positivePCR"]) # Convert numeric to Date format
df[,"N_Dcronicas"] <- as.numeric(df[,"N_Dcronicas"])
df[,"N_sintomas"] <- as.numeric(df[,"N_sintomas"])
df[,"N_sintomas_INSA"] <- as.numeric(df[,"N_sintomas_INSA"])
df[,"Ndias_serol_PCR"] <- as.numeric(df[,"Ndias_serol_PCR"])

glimpse(df)
table(df$Resultado_teste_serologico,useNA = "always")
colSums(is.na(df))
table(df$Resultado_teste_serologico,df$Data_teste_serologico, useNA="always")

# Use only surveys done until October (and remove those without date)
df<-df[(as.Date(df$Data_teste_serologico) < "2020-10-01"),]
# Remove those without date of serological test
df<-df[!is.na(df$Data_teste_serologico),]
########################################################################################
glimpse(df)
table(df$Resultado_teste_serologico,useNA = "always")

min(sort(df$Data_teste_serologico))
max(sort(df$Data_teste_serologico))


#########################################
# CONFIDENCE INTERVALS, ODDS RATIO and association tests for SEROPREVALENCE in each group
##########################################

colnames(df)

variables_all<- c("Sexo","Grupo_idade_10_ref70","Escolaridade_groups",
                  "Situacao_profissional_groups_refReformados_5groups","Prof_saude","Origem","Localidade_inside",
                  "Agregado_familiar_grupo","Viajou",
                  "Ocorrencia_Dcronicas","Ocorrencia_2plusDcronicas",
                  "Diabetes","DPulmunarCronica","Asma","DCardiovascular","Obesidade","DAutoimune","DRenal",
                  "Contacto_covid","Contacto_covid_binary",
                  "Realizou_testePCR","Realizou_testePCR_binary",
                  "Resultado_testePCR","Resultado_testePCR_binary",
                  "Ocorrencia_sintomas","Ocorrencia_sintomas_INSA",
                  "Ocorrencia_Febre","Ocorrencia_Arrepios","Ocorrencia_Fadiga","Ocorrencia_Dores_musculares","Ocorrencia_Dores_articulares",
                  "Ocorrencia_Dor_garganta","Ocorrencia_Tosse","Ocorrencia_Congestao_nasal","Ocorrencia_Falta_ar","Ocorrencia_Pieira",
                  "Ocorrencia_Dor_toracica","Ocorrencia_Outras_respiratorias","Ocorrencia_Conjuntivite","Ocorrencia_Dor_cabeca",
                  "Ocorrencia_Alteracao_consciencia","Ocorrencia_Convulsoes","Ocorrencia_Outras_neurologicas",
                  "Ocorrencia_Nausea_vomitos","Ocorrencia_Diarreia","Ocorrencia_Dor_abdominal",
                  "Ocorrencia_Perda_cheiro","Ocorrencia_Perda_paladar","Ocorrencia_Perda_apetite","Ocorrencia_Hemorragia_nasal",
                  "Ocorrencia_Outras_queixas",
                  "Apoio_medico","Faltou","Hospitalizado","Cuidados_intensivos")

sink(file="./Routput_seroprevalence_confidence_intervals_Period1.txt", append=FALSE, type="output")

# total
t<-as.data.frame(table(df$Resultado_teste_serologico))
t_reactive <- t %>% 
  spread(key=Var1, value=Freq) %>% 
  mutate(p_reactive=100*binconf(Reativo,(Reativo + `Não Reativo`)))
colnames(t_reactive)<-c("Non_reactive","Reactive","p_reactive")
print(noquote("Total"))
print(t_reactive)

for(i in variables_all){
  print(noquote(i))
  t<-as.data.frame(table(df[,i]))
  p<-as.data.frame(100*prop.table(table(df[,i])))
  freq<-cbind(t,p[,"Freq"])
  colnames(freq)<-c(i,"N","%")
  
  t<-as.data.frame(table(df$Resultado_teste_serologico,df[,i]))
  t_reactive <- t %>% 
    spread(key=Var1, value=Freq) %>% 
    mutate(p_reactive=100*binconf(Reativo,(Reativo + `Não Reativo`)))
  colnames(t_reactive)<-c(i,"Non_reactive","Reactive","p_reactive")
  
  
  m<-as.matrix(table(df[,i],df$Resultado_teste_serologico))
  m # "positive" has to come on second column (negative-0, positive-1); control in first row;
  if(any(m==0)){
    tbl<-cbind(freq,t_reactive)
    print(tbl)
    print("values of 0 in matrix - odds ratio not calculated")
    
  } else {
    or.out <- oddsratio(m)
    tbl<-cbind(freq,t_reactive,or.out$measure,or.out$p.value)
      print(tbl)
  }
  chisq<-chisq.test(m)
  print("chisq$p.value")
  print(chisq$p.value)
  
  fisher_test<-fisher.test(m, hybrid=TRUE, simulate.p.value = FALSE) # hybrid chi-square approximation for table larger than 2x2; Monte Carlo simulation for table larger than 2x2
  print("fisher_test$p.value")
  print(fisher_test$p.value)
}


sink()

#########################################
# SIMPLE LOGISTIC REGRESSION - dependent: Resultado_teste_serologico
##########################################

sink(file="./Routput_simple_logistic_regression_Period1.txt", append=FALSE, type="output")

###### Simple Logistic regression
print("### SIMPLE LOGISTIC REGRESSION - dependent: Resultado_teste_serologico")
for(i in variables_all){
  
  variables_list<-c(i,"Resultado_teste_serologico_binary")
  df_regress<-df
  df_regress<-df_regress[,variables_list]
  print(colnames(df_regress))
  
  df_regress<-na.omit(df_regress) #remove rows with NAs
  df_regress<-sapply(df_regress,as.factor)
  df_regress<-as.data.frame(df_regress)
  
  nrow(df_regress)
  summary(df_regress)
  
  model <- glm(as.numeric(Resultado_teste_serologico_binary) ~ ., 
               data = df_regress, family = binomial) 
  
  print(summary(model))
  
}

sink()


#########################################
# SIMPLE LOGISTIC REGRESSION - dependent: Resultado_teste_serologico ADJUSTED FOR SEX AND AGE
##########################################

sink(file="./Routput_simple_logistic_regression_adjusted_sex_age_Period1.txt", append=FALSE, type="output")

###### Simple Logistic regression
print("### SIMPLE LOGISTIC REGRESSION - dependent: Resultado_teste_serologico ADJUSTED FOR SEX AND AGE")

for(i in variables_all){
  
  variables_list<-c(i,"Resultado_teste_serologico_binary","Sexo","Grupo_idade_10_ref70")
  df_regress<-df
  df_regress<-df_regress[,variables_list]
  print(colnames(df_regress))
  
  df_regress<-na.omit(df_regress) #remove rows with NAs
  df_regress<-sapply(df_regress,as.factor)
  df_regress<-as.data.frame(df_regress)
  
  nrow(df_regress)
  summary(df_regress)

  model <- glm(as.numeric(Resultado_teste_serologico_binary) ~ . + Sexo + Grupo_idade_10_ref70, 
               data = df_regress, family = binomial) 
  
  print(summary(model))
  
}

sink()


###############################################################
# MULTIVARIATE
##############################################################
variables_socio1<-c("Sexo","Grupo_idade_10_ref70","Escolaridade_groups","Situacao_profissional_groups_refReformados_5groups",
                    "Localidade_inside",
                    "Resultado_teste_serologico_binary")

variables_socio2<-c("Sexo","Grupo_idade_10_ref70","Escolaridade_groups","Situacao_profissional_groups_refReformados_5groups",
                    "Localidade_inside","Origem", "Agregado_familiar_grupo",
                    "Resultado_teste_serologico_binary")


sink(file="./Routput_multivariate_logistic_regression_without_stepwise_Period1.txt", append=FALSE, type="output")

print("### MULTIVARIATE LOGISTIC REGRESSION - without stepwise - dependent: Resultado_teste_serologico_binary")

for(i in list(variables_socio1,variables_socio2,variables_chronic,variables_covid,variables_symptoms,
              variables_to_include1,variables_to_include2)){
  
  df_regress<-df
  df_regress<-df_regress[,i]
  print(colnames(df_regress))
  
  df_regress<-na.omit(df_regress) #remove rows with NAs
  df_regress<-sapply(df_regress,as.factor)
  df_regress<-as.data.frame(df_regress)
  print(nrow(df_regress))
  
  if(!is.null(df_regress$N_Dcronicas)){
    df_regress$N_Dcronicas<-as.numeric(df_regress$N_Dcronicas)
  }
  if(!is.null(df_regress$N_sintomas)){
    df_regress$N_sintomas<-as.numeric(df_regress$N_sintomas)
  }
  if(!is.null(df_regress$N_agregado_familiar)){
    df_regress$N_agregado_familiar<-as.numeric(df_regress$N_agregado_familiar)
  }
  if(!is.null(df_regress$week_serological_test)){
    library(lubridate)
    df_regress$week_serological_test<-lubridate::week(ymd(df_regress$week_serological_test))
  }
  
  nrow(df_regress)
  summary(df_regress)
  
  model <- glm(as.numeric(Resultado_teste_serologico_binary) ~ ., 
               data = df_regress, family = binomial)
  
  print(summary(model))
  
  print("### Hosmer and Lemeshow goodness of fit (GOF) test")
  
  print(hoslem.test(model$y,fitted(model),g=10)$p.value)
  
}

sink()
