# 
# Description:
#
# Script to generate tables for results of assignment from different methods such as DAPC, GeneClass2 or STRUCTURE
#

rm(list=ls())

library("stringr")
library("dplyr")

# Input like Geneclass
# Select columns from Assigned sample to the score 2 and all rows with individuals

datos<-read.table(file="clipboard",sep = "\t",dec = ".",header = F)
datos<-datos[,c(1,2,3,5,6)]
datos$Pop<- str_c(str_sub(datos$V1,1,4))

### Change group name asigned by GeneClass to your species names

datos[datos == "HLCL15_48"] <- "Mch"
datos[datos == "PRUY16_59"] <- "Mp"
datos[datos == "RBMX14_48"] <- "Mg"

colnames(datos)<-c("Hibrido","E1","Score1","E2","Score2","Pop")
datos<-datos %>%
  rowwise() %>%      # 
  mutate(Especies = paste(sort(c(E1, E2)), collapse = " - ")) %>% 
  ungroup()          # 

minus90<-subset(datos,subset = Score1<90) # Set your score threshold for assignment to group, in this case 90%
over90<-subset(datos,subset = Score1>=90) # Set your score threshold for assignment to group, in this case 90%

table(over90$Pop,over90$E1)
table(minus90$Pop,minus90$Especies)

# Input like STRUCTURE, DAPC, FLOCK

#Input data must be in format ID probability1 probability2
#ID: code of 4 letters for sampling locations, plus individual number. Example: WXYZ01
#probability: Value between 0 to 1.

datos<-read.table(file="clipboard",sep = "\t",dec = ",",header = T)
datos$Pop<- str_c(str_sub(datos$IND,1,6))
datos$Mayor<-colnames(datos)[apply(datos,1,which.max)]
datos$Mayor2<-colnames(datos[,c(2:4)])[apply(datos[,c(2:4)],1,function(x)which(x==sort(x,partial=2)[2])[1])]

datos<-datos %>%
  rowwise() %>%      
  mutate(Hibrido = paste(sort(c(Mayor, Mayor2)), collapse = " - ")) %>%  
  ungroup()          
datos$Max1<-apply(X=datos[,c(2:4)], MARGIN=1, FUN=max)
datos<-datos %>% 
  rowwise() %>% 
  mutate(Max2 = sort(c(Mg, Mch, Mp),decreasing = TRUE)[2]) %>% 
  ungroup()

minus90<-subset(datos,subset = Score1<90) # Set your score threshold for assignment to group, in this case 90%
over90<-subset(datos,subset = Score1>=90) # Set your score threshold for assignment to group, in this case 90%

table(over90$Pop,over90$E1)
table(minus90$Pop,minus90$Especies)

#<90
bajo90<-subset(datos,Max1<0.9,select = c(IND,Pop,Hibrido,Mch,Mg,Mp))
table(bajo90$Pop,bajo90$Hibrido)
