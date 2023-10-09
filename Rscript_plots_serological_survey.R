#######################
# R script for plots
# Manuscript "Serological screening in a large-scale municipal survey in Cascais, Portugal, during the first waves of the COVID-19 pandemic: lessons for the future"
### Authors: Sofia G. Seabra1†, Francisco Merca1,3†, Bernardo Pereira1, Ivo Fonseca1, Ana Cláudia Carvalho2, Vera Brito2, Daniela Alves1, Pieter Libin3, M. Rosário O. Martins1, Mafalda N. S. Miranda1, Marta Pingarilho1, Victor Pimentel1, Ana B. Abecasis1*
### Addresses
# 1 Global Health and Tropical Medicine, GHTM, Associate Laboratory in Translation and Innovation Towards Global Health, LA-REAL, Instituto de Higiene e Medicina Tropical, IHMT, Universidade NOVA de Lisboa, UNL, Rua da Junqueira 100, 1349-008 Lisboa, Portugal.
# † These authors share first authorship
# 2 Câmara Municipal de Cascais, Portugal
# 3 Artificial Intelligence Research Lab, Vrije Universiteit Brussels (VUB), Pleinlaan 2, 1050 Brussel, Belgium  


library(openxlsx)
library(tidyverse)
library(dplyr)
library(lubridate)
library(Hmisc)
library(grid)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(scales)
library(ggthemes)
library(ggplot2)
library(plotly)
library(naniar)
library(DescTools)
library("epitools")
library(Amelia)

# Choose colour schemes
#https://colorbrewer2.org/#type=diverging&scheme=Spectral&n=9

# Set working directory
setwd('/USER_FOLDER/')

# Read dataframe from xlsx file (header line with variable names)
df<-read.xlsx("./data_input.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# Get column names (variable names)
colnames(df)

# Convert to date and numeric formats
df[,"Data_contacto"] <- openxlsx::convertToDateTime(df[,"Data_contacto"]) # Convert numeric to Date format
df[,"Data_teste_positivo"] <- openxlsx::convertToDateTime(df[,"Data_teste_positivo"]) # Convert numeric to Date format
df[,"Ano_nascimento"] <- as.numeric(df[,"Ano_nascimento"])

df[,"Data_teste_serologico"] <- openxlsx::convertToDateTime(df[,"Data_teste_serologico"]) # Convert numeric to Date format

glimpse(df)

#####################################################################3
# Figure 1
#####################################################################3

# Serological sruveys done per week
df_date_tests<- as.data.frame(table(df$Data_teste_serologico))
df_date_tests$Var1<-as.Date(df_date_tests$Var1)
colnames(df_date_tests) <- c("Data_teste_serologico","Number_of_tests")
# per week
df_week_N<-df_date_tests %>% 
  mutate(week=floor_date(Data_teste_serologico, "week")) %>%
  group_by(week) %>%
  summarise(Number_of_tests=sum(Number_of_tests))

# Test results per week
df_date_results<- as.data.frame(table(df$Data_teste_serologico,df$Resultado_teste_serologico))
df_date_results$Var1<-as.Date(df_date_results$Var1)
colnames(df_date_results) <- c("Data_teste_serologico","Resultado_teste_serologico","Number_of_tests")

df_week_tests<-df_date_results %>% 
  mutate(week=floor_date(Data_teste_serologico, "week")) %>%
  group_by(Resultado_teste_serologico,week) %>%
  summarise(Number_of_tests=sum(Number_of_tests))

df_reactive<-df_week_tests %>% 
  spread(key=Resultado_teste_serologico, value=Number_of_tests) %>% 
  mutate(p_reactive=binconf(Reativo,(Reativo +`Não Reativo`)))


jpeg(file = "./N_participants_reactives_per_week.jpeg", bg="white", antialias = "default",
     width = 8, height = 6,  
     units = "in", res = 100)

p1 <- ggplot(data= df_week_N, aes(y=Number_of_tests, x=week, group=1)) + 
  geom_line() +
  geom_point() +
  xlab("") +ylab("Number of participants") +
   theme(text=element_text(size=12)) +
  theme(plot.margin = unit(c(0.5,0.5,0.1,0.5), "in")) + 
  theme_light()

p2<- ggplot(data= df_reactive, aes(week,p_reactive[,"PointEst"]*100)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=p_reactive[,"Lower"]*100, ymax=p_reactive[,"Upper"]*100), width=.1) +
  ylim(0,100) +
  xlab("Week") + ylab("Percentage of reactive tests")+
  theme(text=element_text(size=12)) +
  theme(plot.margin = unit(c(0.5,0.5,0.1,0.5), "in")) + 
  theme_light()

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

dev.off() 

#####################################################################3
# Figure 2
####################################################################
# ODDS RATIO PLOTS
############################################################################

library(openxlsx)
library(ggplot2)

######################
# Sociodemographic variables
t<-read.xlsx("./Oddsratio_table_sociodemographic.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

# definir a ordem que aparece no gráfico
t$Category<-factor(t$Category, levels=rev(c("Sex_Male",
                                            "Age_0-9", "Age_10-19", "Age_20-29", "Age_30-39", "Age_40-49",
                                            "Age_50-59", "Age_60-69",
                                            "Education_No scholarity",
                                            "Education_Basic2",
                                            "Education_Basic3",
                                            "Education_Secondary",
                                            "Education_Medium",
                                            "Education_Superior",
                                            "Activity_Employed",
                                            "Activity_Unemployed",
                                            "Activity_Student",
                                            "Activity_Other",
                                            "HealthProfession_Yes",
                                            "CountryBirth_Portugal",
                                            "Locality_Carcavelos/Parede",
                                            "Locality_Cascais/Estoril",
                                            "Locality_São Domingos de Rana",
                                            "Household_2-3",
                                            "Household_4-5",
                                            "Household_5+"
                                            )))

t$Period<-factor(t$Period, levels=rev(c("Period1", "Period2")))
  
# Save to file
png(file = "./plot_oddsratio_sociodemographic.png", bg="white", width = 8, height = 6,  
    units = "in", res = 100)

ggplot(data= t, aes(x=Category,y=Odds, fill=Period)) + 
  geom_point(aes(x=Category,y=Odds, color=Period),position=position_dodge(width=0.8)) +
  geom_errorbar(aes(x=Category,ymin=inferior, ymax=superior, color=Period), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  geom_hline(yintercept = 1) +
  xlab("") + ylab("Odds ratio") +
  ggtitle("")+
  coord_flip() + 
  scale_colour_grey(start = 0.7, end = 0) +
  theme_bw()

dev.off()

#####################################################################3
# Figure 3
#####################################################################3
jpeg(file = "./Per_week_proportion_contact_PCR.jpeg", bg="white", antialias = "default", width = 8, height = 8 , units = "in", res = 100)

#############################################################3
# PER WEEK - Previous contact

df_date_tests<-as.data.frame(table(df$Data_teste_serologico,df$Contacto_covid_binary))
df_date_tests$Var1<-as.Date(df_date_tests$Var1)
colnames(df_date_tests) <- c("Data_teste_serologico","Contacto_covid","Number_of_tests")

df_week_tests<-df_date_tests %>% 
  mutate(week=floor_date(Data_teste_serologico, "week")) %>%
  group_by(Contacto_covid,week) %>%
  summarise(Number_of_tests=sum(Number_of_tests))

df_proportion<-df_week_tests %>% 
  spread(key=Contacto_covid, value=Number_of_tests) %>% 
  mutate(proportion=binconf(`1`,(`1`+ `0`)))

p1 <- ggplot(data= df_proportion, aes(as.Date(week),proportion[,"PointEst"]*100)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=proportion[,"Lower"]*100, ymax=proportion[,"Upper"]*100)) +
  xlab("") + ylab("Previous contact (%)") +
  ylim(0,100) +
    theme_light()

#############################################################3
# PER WEEK - PCR tests performed
df_date_tests<-as.data.frame(table(df$Data_teste_serologico,df$Realizou_testePCR_binary))
df_date_tests$Var1<-as.Date(df_date_tests$Var1)
colnames(df_date_tests) <- c("Data_teste_serologico","Realizou_testePCR","Number_of_tests")

df_week_tests<-df_date_tests %>% 
  mutate(week=floor_date(Data_teste_serologico, "week")) %>%
  group_by(Realizou_testePCR,week) %>%
  summarise(Number_of_tests=sum(Number_of_tests))

df_proportion<-df_week_tests %>% 
  spread(key=Realizou_testePCR, value=Number_of_tests) %>% 
  mutate(proportion=binconf(`1`,(`1` + `0`)))

p2 <- ggplot(data= df_proportion, aes(as.Date(week),proportion[,"PointEst"]*100)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=proportion[,"Lower"]*100, ymax=proportion[,"Upper"]*100), width=3) +
  xlab("") + ylab("Previous PCR test (%)") +
  ylim(0,100) +
  theme_light()



########################################
# PER WEEK - Resultado PCR test - proportion from all participants
df_date_tests<-as.data.frame(table(df$Data_teste_serologico))
df_date_tests$Var1<-as.Date(df_date_tests$Var1)
colnames(df_date_tests) <- c("Data_teste_serologico","Number_of_tests")

df_week_sero<-df_date_tests %>% 
  mutate(week=floor_date(Data_teste_serologico, "week")) %>%
  group_by(week) %>%
  summarise(Number_of_tests=sum(Number_of_tests))

df_date_pos<-as.data.frame(table(df$Data_teste_serologico,df$Resultado_testePCR_binary))
df_date_pos$Var1<-as.Date(df_date_pos$Var1)
colnames(df_date_pos) <- c("Data_teste_serologico","Resultado_testePCR","Number_of_tests")
df_date_pos<-df_date_pos[df_date_pos$Resultado_testePCR=="1",]

df_week_pos<-df_date_pos %>% 
  mutate(week=floor_date(Data_teste_serologico, "week")) %>%
  group_by(week) %>%
  summarise(Number_of_positives=sum(Number_of_tests))

df_week_tests<-cbind(df_week_sero,df_week_pos[match(df_week_sero$week,df_week_pos$week),"Number_of_positives"])
colnames(df_week_tests)

df_week_tests<-df_week_tests[!is.na(df_week_tests$Number_of_positives),]

df_proportion<-df_week_tests %>% 
  mutate(proportion=binconf(`Number_of_positives`,(`Number_of_tests`)))

p3<-ggplot(data= df_proportion, aes(as.Date(week),proportion[,"PointEst"]*100)) + 
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=proportion[,"Lower"]*100, ymax=proportion[,"Upper"]*100)) +
  xlab("Week") + ylab("Positive PCR (%)")+
  ylim(0,100) +
  theme_light()


###
grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size = "last"))

dev.off()





#####################################################
#####################################################
## SUPPLEMENTARY FIGURES
##########################################################
# Fig. S1 - COMPARE proportions of categories with PORDATA 2021
##########################################################
##########################################################
png(file = "./Comparison_PORDATA.jpeg", bg="white", width = 8, height = 10,  
    units = "in", res = 300)

### STACKED BARPLOTS - Sexo inqueritos e PORDATA
# https://stackoverflow.com/questions/41648462/draw-lines-between-different-elements-in-a-stacked-bar-plot
t <- read.xlsx("./Tabela_Sexo_PORDATA.xlsx", sheet =1, startRow = 1, colNames = TRUE)
m <- melt(t[,c("Sex","This_study","Census_2021")], id.vars=1)
colnames(m)<-c("Sex","Type","Percentage")

p1<-ggplot(data= m, aes(y=Percentage, x=Type)) + 
  geom_bar(aes(fill=Sex), colour="black",position="fill", stat="identity") +
  xlab("") + ylab("") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),name="Sex") 

### STACKED BARPLOTS - Idades inqueritos e PORDATA
# https://stackoverflow.com/questions/41648462/draw-lines-between-different-elements-in-a-stacked-bar-plot
t <- read.xlsx("./Tabela_Idades_PORDATA.xlsx", sheet =1, startRow = 1, colNames = TRUE)
m <- melt(t[,c("Classe_etaria","This_study","Census_2021")], id.vars=1)
colnames(m)<-c("Age","Type","Percentage")

p2<-ggplot(data= m, aes(y=Percentage, x=Type)) + 
  geom_bar(aes(fill=Age), colour="black",position="fill", stat="identity") +
  xlab("") + ylab("") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#e69f00","#f3c587","#f3d9ba",
                             "#eeeeee","#c0cddf","#92add0","#5f8fc1","#0072b2"),name="Age") 


### STACKED BARPLOTS - Escolaridade inqueritos e PORDATA
# https://stackoverflow.com/questions/41648462/draw-lines-between-different-elements-in-a-stacked-bar-plot
t <- read.xlsx("./Tabela_Escolaridade_PORDATA.xlsx", sheet =1, startRow = 1, colNames = TRUE)
m <- melt(t[,c("Education","This_study","Census_2021")], id.vars=1)
colnames(m)<-c("Education","Type","Percentage")
m$Education<-factor(m$Education,levels=c("No scholarity",
                                         "Basic1",
                                         "Basic2",
                                         "Basic3",
                                         "Secondary",
                                         "Medium",
                                         "Superior"))

p3<-ggplot(data= m, aes(y=Percentage, x=Type)) + 
  geom_bar(aes(fill=Education), colour="black",position="fill", stat="identity") +
  xlab("") + ylab("") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#e69f00","#f3c587","#f3d9ba",
                             "#eeeeee","#c0cddf","#92add0","#5f8fc1","#0072b2"),name="Education") 


### STACKED BARPLOTS - Situação profissional inqueritos e PORDATA
# https://stackoverflow.com/questions/41648462/draw-lines-between-different-elements-in-a-stacked-bar-plot
t <- read.xlsx("./Tabela_Situacao_profissional_PORDATA.xlsx", sheet =1, startRow = 1, colNames = TRUE)
m <- melt(t[,c("Employment","This_study","Census_2021")], id.vars=1)
colnames(m)<-c("Employment","Type","Percentage")
m$Employment<-factor(m$Employment,levels=c("Employed",
                                           "Unemployed",
                                           "Student",
                                           "Retired",
                                           "Domestic",
                                           "Incapacitated",
                                           "Other inactives"))


p4<-ggplot(data= m, aes(y=Percentage, x=Type)) + 
  geom_bar(aes(fill=Employment), colour="black",position="fill", stat="identity") +
  xlab("") + ylab("") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#e69f00","#f3c587","#f3d9ba",
                             "#eeeeee","#c0cddf","#92add0","#5f8fc1","#0072b2"),name="Employment status") 




### STACKED BARPLOTS - Origem inqueritos e PORDATA
# https://stackoverflow.com/questions/41648462/draw-lines-between-different-elements-in-a-stacked-bar-plot
t <- read.xlsx("./Tabela_Origem_PORDATA.xlsx", sheet =1, startRow = 1, colNames = TRUE)
m <- melt(t[,c("Origin","This_study","Census_2021")], id.vars=1)
colnames(m)<-c("Origin","Type","Percentage")


p5<-ggplot(data= m, aes(y=Percentage, x=Type)) + 
  geom_bar(aes(fill=Origin), colour="black",position="fill", stat="identity") +
  xlab("") + ylab("") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values=c("#f3d9ba",
                             "#92add0"),name="Country of birth") 

grid.newpage()
grid.arrange(p1, p2, p3, p4, p5, nrow=3, ncol=2)


dev.off()


##########################################################
# Fig. S2 - Seroprevalence Immigrants vs Portuguese
##########################################################
##########################################################
#############################################################################
# Resultado de teste Portugal vs outros paises
#############################################################################

###############3
# First period


df<-read.xlsx("./data_input.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

colnames(df)

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


#############################################################################
# Resultado de teste Portugal vs outros paises
#############################################################################

# Resultado de teste por Pais de Nascimento (Origem) e Idade
t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Grupo_idade_10))
colnames(t)=c("Resultado_teste_serologico","Origem","Idade","Freq")

prop_serol <- t %>%
  group_by(Origem, Idade) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p1<-ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Idade, desc(Idade)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Idade, desc(Idade)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,40) +
  
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  xlab("Age") + ylab("") +
  labs(title="") +
  coord_flip() +
  ggtitle("First period") + theme(plot.title = element_text(size = 14, face = "bold", hjust=0.5))

# Resultado de teste por Pais de Nascimento (Origem) e Escolaridade

t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Escolaridade_groups))
colnames(t)=c("Resultado_teste_serologico","Origem","Escolaridade","Freq")

prop_serol <- t %>%
  group_by(Origem, Escolaridade) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

prop_serol$Escolaridade<-factor(prop_serol$Escolaridade,levels=c("Sem nível de escolaridade",
                                                                 "Básico 1º ciclo",
                                                                 "Básico 2º ciclo",
                                                                 "Básico 3º ciclo",
                                                                 "Secundário",
                                                                 "Médio",
                                                                 "Superior"))

p2<-ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Escolaridade, desc(Escolaridade)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Escolaridade, desc(Escolaridade)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,40) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  scale_x_discrete(labels=c("Superior","Medium","Secondary","Basic3","Basic2","Basic1","No scholarity")) +
  xlab("Education") + ylab("") +
  labs(title="") +
  coord_flip() 


# Resultado de teste por ORIGEM e Situacao profissional

t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Situacao_profissional_groups_refReformados_5groups))
colnames(t)=c("Resultado_teste_serologico","Origem","Situacao_profissional","Freq")

prop_serol <- t %>%
  group_by(Origem, Situacao_profissional) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p3 <-ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Situacao_profissional, desc(Situacao_profissional)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Situacao_profissional, desc(Situacao_profissional)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,40) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  scale_x_discrete(labels=c("Other","Unemployed","Student","Employed","Retired")) +
  xlab("Employment status") + ylab("") +
  labs(title="") +
  coord_flip() 

# Resultado de teste por ORIGEM e Localidade
t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Localidade_inside))

colnames(t)=c("Resultado_teste_serologico","Origem","Localidade","Freq")

prop_serol <- t %>%
  group_by(Origem, Localidade) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p4 <- ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Localidade, desc(Localidade)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Localidade, desc(Localidade)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,40) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  scale_x_discrete(labels=c("S. Domingos Rana","Cascais/Estoril","Carcavelos/Parede","Alcabideche")) +
  xlab("Locality") + ylab("") +
  labs(title="") +
  coord_flip() 



# Resultado de teste por ORIGEM e Agregado_familiar_grupo
t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Agregado_familiar_grupo))

colnames(t)=c("Resultado_teste_serologico","Origem","Agregado_familiar_grupo","Freq")

prop_serol <- t %>%
  group_by(Origem, Agregado_familiar_grupo) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p5 <- ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Agregado_familiar_grupo, desc(Agregado_familiar_grupo)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Agregado_familiar_grupo, desc(Agregado_familiar_grupo)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,40) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  xlab("Size of household") + ylab("Seroprevalence (%)") +
  labs(title="") +
  coord_flip() 


###############3
# Second period

df<-read.xlsx("./data_input.xlsx", sheet = 1, startRow = 1, colNames = TRUE)

colnames(df)

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

# Use only surveys done from October to February (and remove those without date)
df<-df[(as.Date(df$Data_teste_serologico) > "2020-09-30"),]
# Remove those without date of serological test
df<-df[!is.na(df$Data_teste_serologico),]
########################################################################################
glimpse(df)
table(df$Resultado_teste_serologico,useNA = "always")


#############################################################################
# Resultado de teste Portugal vs outros paises
#############################################################################

# Resultado de teste por Pais de Nascimento (Origem) e Idade
t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Grupo_idade_10))
colnames(t)=c("Resultado_teste_serologico","Origem","Idade","Freq")

prop_serol <- t %>%
  group_by(Origem, Idade) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p6<-ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Idade, desc(Idade)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Idade, desc(Idade)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,75) +
  ggtitle("Second period") +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  xlab("") + ylab("") +
  labs(title="") +
  coord_flip() +
  ggtitle("Second period") + theme(plot.title = element_text(size = 14, face = "bold", hjust=0.5))


# Resultado de teste por Pais de Nascimento (Origem) e Escolaridade

t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Escolaridade_groups))
colnames(t)=c("Resultado_teste_serologico","Origem","Escolaridade","Freq")

prop_serol <- t %>%
  group_by(Origem, Escolaridade) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

prop_serol$Escolaridade<-factor(prop_serol$Escolaridade,levels=c("Sem nível de escolaridade",
                                                                 "Básico 1º ciclo",
                                                                 "Básico 2º ciclo",
                                                                 "Básico 3º ciclo",
                                                                 "Secundário",
                                                                 "Médio",
                                                                 "Superior"))

p7<-ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Escolaridade, desc(Escolaridade)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Escolaridade, desc(Escolaridade)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  scale_x_discrete(labels=c("Superior","Medium","Secondary","Basic3","Basic2","Basic1","No scholarity")) +
  xlab("") + ylab("") +
  labs(title="") +
  coord_flip() 


# Resultado de teste por ORIGEM e Situacao profissional

t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Situacao_profissional_groups_refReformados_5groups))
colnames(t)=c("Resultado_teste_serologico","Origem","Situacao_profissional","Freq")

prop_serol <- t %>%
  group_by(Origem, Situacao_profissional) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p8 <-ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Situacao_profissional, desc(Situacao_profissional)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Situacao_profissional, desc(Situacao_profissional)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  scale_x_discrete(labels=c("Other","Unemployed","Student","Employed","Retired")) +
  xlab("") + ylab("") +
  labs(title="") +
  coord_flip() 

# Resultado de teste por ORIGEM e Localidade
t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Localidade_inside))

colnames(t)=c("Resultado_teste_serologico","Origem","Localidade","Freq")

prop_serol <- t %>%
  group_by(Origem, Localidade) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p9 <- ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Localidade, desc(Localidade)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Localidade, desc(Localidade)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  scale_x_discrete(labels=c("S. Domingos Rana","Cascais/Estoril","Carcavelos/Parede","Alcabideche")) +
  xlab("") + ylab("") +
  labs(title="") +
  coord_flip() 


# Resultado de teste por ORIGEM e Agregado_familiar_grupo
t<-as.data.frame(table(df$Resultado_teste_serologico_binary,df$Origem,df$Agregado_familiar_grupo))

colnames(t)=c("Resultado_teste_serologico","Origem","Agregado_familiar_grupo","Freq")

prop_serol <- t %>%
  group_by(Origem, Agregado_familiar_grupo) %>%
  spread(key=Resultado_teste_serologico, value=Freq) %>% 
  mutate(p_reactive=binconf(`1`,(`1` +`0`)))

p10 <- ggplot(data= prop_serol,aes(y=p_reactive[,1]*100,x=reorder(Agregado_familiar_grupo, desc(Agregado_familiar_grupo)), fill=Origem)) + 
  geom_bar(stat = "identity", position= "dodge") +
  geom_errorbar(aes(x=reorder(Agregado_familiar_grupo, desc(Agregado_familiar_grupo)),ymin=p_reactive[,2]*100, ymax=p_reactive[,3]*100), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  ylim(0,75) +
  theme_bw() + theme(panel.grid.minor = element_blank(), legend.position="none") +
  scale_fill_manual("Country of birth",labels=c("Other countries", "Portugal"),values=c("#92add0","#0072b2")) +
  xlab("") + ylab("Seroprevalence (%)") +
  labs(title="") +
  coord_flip() 


jpeg(file = "./Percentage_reactive_Origin_categories.jpeg", 
     bg="white", antialias = "default",width = 8, height = 13,  
     units = "in", res = 100)


p_all <- plot_grid(p1, p6, p2, p7, p3, p8, p4, p9, p5, p10, nrow=5, ncol=2,
          labels = "AUTO",
          label_size = 14,
          label_x = 0.92, label_y = 0.75,
          hjust = -0.5, vjust = -0.5,
          align="v")

legend <- get_legend(p1 + 
                       guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom"))

plot_grid(p_all, legend, ncol = 1, rel_heights = c(1, .1))

dev.off()


# Gravar para ficheiro
png(file = "./plot_oddsratio_chronic.png", bg="white", width = 5, height = 3,  
    units = "in", res = 100)

ggplot(data= t, aes(x=Category,y=Odds, fill=Period)) + 
  geom_point(aes(x=Category,y=Odds, color=Period),position=position_dodge(width=0.8)) +
  geom_errorbar(aes(x=Category,ymin=inferior, ymax=superior, color=Period), 
                width=0.2, size=0.5,position=position_dodge(width=0.8))+
  geom_hline(yintercept = 1) +
  xlab("") + ylab("Odds ratio") + ylim(0,5) +
  ggtitle("")+
  coord_flip() 

dev.off()
