library("data.table")

## CHAMANDO O ARQUIVO ----

setwd("/home/marcelo/Downloads/")

file_list = list.files(pattern="*.csv")
file_list = file_list[!grepl('Table|table', file_list)]  # todos os csv menos o table


balanco <- function(eplus_output){
  df = read.csv(eplus_output)
  
  ## incluindo o dia e o mes nos dfs
  df$mes = as.numeric(substr(df$Date.Time, 2,3))
  df$dia = as.numeric(substr(df$Date.Time, 5,6))
  
  ## pegando o dia mais quente e o mais frio
  dia_max <- as.numeric(df[which.max(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),"dia"])
  mes_max <- as.numeric(df[which.max(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
  dia_min <- as.numeric(df[which.min(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
  mes_min <- as.numeric(df[which.min(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
  
  ## criando os dfs com os dias que eu quero
  timestepsxdia = nrow(df[df$dia == dia_max, ])
  
  max <- subset(df, ((df$dia >= (dia_max-1)) & (df$dia <= (dia_max)) & (df$mes == mes_max)))
  min <- subset(df, ((df$dia >= (dia_min-1)) & (df$dia <= (dia_min)) & (df$mes == mes_min)))
  
  ## pegando o dia mais quente e o mais frio
  dia_max <- as.numeric(df[which.max(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly.),"dia"])
  mes_max <- as.numeric(df[which.max(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly.),c("mes")])
  dia_min <- as.numeric(df[which.min(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly.),c("dia")])
  mes_min <- as.numeric(df[which.min(df$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..Hourly.),c("mes")])
  
  ## criando os dfs com os dias que eu quero
  timestepsxdia = nrow(df[df$dia == dia_max & df$mes == mes_max, ])
  
  max <- df[dia==dia_max]
  min <- subset(df, ((df$dia >= (dia_min-1)) & (df$dia <= (dia_min)) & (df$mes == mes_min)))
}

## incluindo a coluna com o nome max e min para eu poder pegar no df que eu quero

max_01$temp <- "Max"
min_01$temp <- "Min"
max_02$temp <- "Max"
min_02$temp <- "Min"
max_03$temp <- "Max"
min_03$temp <- "Min"

## rbind do dia mais quente e mais frio, first pq depois eu vou criar outro

env_01_first <- rbind(max_01, min_01)
env_02_first <- rbind(max_02, min_02)
env_03_first <- rbind(max_03, min_03)

rm(max_01, min_01, max_02, min_02, max_03, min_03)

## criando o df com todas as informacoes necessarias para a criacao do grafico - temperatura operativa para os APPs ----

## envoltoria 1 para a sala

S_01 <- data.frame(Data=c(1:nrow(env_01_first)),temp=env_01_first$temp, Envoltoria=c("Pesada"),Portas=c(1:nrow(env_01_first)),Janelas=c(1:nrow(env_01_first)),Par_Internas=c(1:nrow(env_01_first)),Par_Externas=c(1:nrow(env_01_first)),
                      Cobertura=c(1:nrow(env_01_first)),Piso=c(1:nrow(env_01_first)),Cargas_Internas=c(1:nrow(env_01_first)))

S_01$Portas <- ((env_01_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                     (env_01_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                     (env_01_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Janelas <- ((env_01_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Internas <- ((env_01_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                           (env_01_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Externas <- ((env_01_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                           (env_01_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Cobertura <- ((env_01_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Piso <- env_01_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_01$Cargas_Internas <- ((env_01_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_01-1)) & (Date$dia <= (dia_max_01)) & (Date$mes == mes_max_01))))

S_01$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 2 para a sala

S_02 <- data.frame(Data=c(1:nrow(env_02_first)),temp=env_02_first$temp, Envoltoria=c("Tijolo"),Portas=c(1:nrow(env_02_first)),Janelas=c(1:nrow(env_02_first)),Par_Internas=c(1:nrow(env_02_first)),Par_Externas=c(1:nrow(env_02_first)),
                   Cobertura=c(1:nrow(env_02_first)),Piso=c(1:nrow(env_02_first)),Cargas_Internas=c(1:nrow(env_02_first)))

S_02$Portas <- ((env_02_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Janelas <- ((env_02_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Internas <- ((env_02_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Externas <- ((env_02_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Cobertura <- ((env_02_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Piso <- env_02_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_02$Cargas_Internas <- ((env_02_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_02-1)) & (Date$dia <= (dia_max_02)) & (Date$mes == mes_max_02))))

S_02$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 3 para a sala

S_03 <- data.frame(Data=c(1:nrow(env_03_first)),temp=env_03_first$temp, Envoltoria=c("Leve"),Portas=c(1:nrow(env_03_first)),Janelas=c(1:nrow(env_03_first)),Par_Internas=c(1:nrow(env_03_first)),Par_Externas=c(1:nrow(env_03_first)),
                   Cobertura=c(1:nrow(env_03_first)),Piso=c(1:nrow(env_03_first)),Cargas_Internas=c(1:nrow(env_03_first)))

S_03$Portas <- ((env_03_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Janelas <- ((env_03_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Internas <- ((env_03_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Externas <- ((env_03_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Cobertura <- ((env_03_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Piso <- env_03_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_03$Cargas_Internas <- ((env_03_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_03-1)) & (Date$dia <= (dia_max_03)) & (Date$mes == mes_max_03))))

S_03$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## rbind arquivos sala ----

sala <- rbind(S_01, S_02, S_03)

sala$Temp_Dia[sala$temp == "Max"] <- "Dia 8/12 e 9/12"
sala$Temp_Dia[sala$temp == "Min"] <- "Dia 17/02 e 18/02"

## grafico sala ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/resultados")

library(ggplot2)

png(filename = 'Macapa_conduction.png', width = 32, height = 18, units = "cm", res = 500)
plot(
  ggplot(sala) + 
    geom_line(aes(x = Hora, y=Portas, colour = "Portas")) + 
    geom_line(aes(x = Hora, y=Janelas, colour = "Janelas"))+
    geom_line(aes(x = Hora, y=Par_Internas, colour = "Paredes Internas"))+
    geom_line(aes(x = Hora, y=Par_Externas, colour = "Paredes Externas"))+
    geom_line(aes(x = Hora, y=Cobertura, colour = "Cobertura"))+
    geom_line(aes(x = Hora, y=Piso, colour = "Piso"))+
    geom_line(aes(x = Hora, y=Cargas_Internas, colour = "Cargas Internas"))+
        labs(x="Horas nos Dias", y="Fluxo de Calor(?) [W]")+
    theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
          legend.position = "bottom",
          #legend.box = "vertical",
          #legend.key = element_rect(size = 5),
          # axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.title.x = element_text(size=12, vjust = 0.75),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.title = element_blank(),
          legend.text = element_text(size=12, hjust = 0.5),
          strip.text = element_text(size=13)) +
    #  scale_x_continuous(breaks = seq(from = 0, to = 600, by = 12))+  #  scale_x_discrete()+
    scale_x_datetime(date_breaks='1 hour',date_labels='%H:%M',expand = c(0, 0))+
    ylim(-1500,2000)+
    #geom_vline(xintercept = 8, colour="green", linetype = "longdash")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[97]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[265]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[385]), linetype=4, color="black")+
    facet_grid(Envoltoria~Temp_Dia)
)
dev.off()

rm(list = ls())

## CHAMANDO O ARQUIVO ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/BRA_PR_Curitiba-Bacacheri.AP.838420_TMYx.2003-2017/")

file_list = list.files(pattern="*1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:3){ ## eh de 1 a 3 pois nao quero os 2 ultimos csv da pasta
  assign(file_list[i], read.csv(file_list[i]))
}

## incluindo o dia e o mes nos dfs

dia_mes <- data.frame(mes=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 2,3)), dia=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 5,6)))

construction_01_comp_02_ocup_01_1.csv <- cbind(construction_01_comp_02_ocup_01_1.csv, dia_mes)
construction_01_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_01_comp_02_ocup_01_1.csv$dia)
construction_01_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_01_comp_02_ocup_01_1.csv$mes)

construction_02_comp_02_ocup_01_1.csv <- cbind(construction_02_comp_02_ocup_01_1.csv, dia_mes)
construction_02_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_02_comp_02_ocup_01_1.csv$dia)
construction_02_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_02_comp_02_ocup_01_1.csv$mes)

construction_03_comp_02_ocup_01_1.csv <- cbind(construction_03_comp_02_ocup_01_1.csv, dia_mes)
construction_03_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_03_comp_02_ocup_01_1.csv$dia)
construction_03_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_03_comp_02_ocup_01_1.csv$mes)

rm(dia_mes)

## pegando o dia mais quente e o mais frio

dia_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

## criando os dfs com os dias que eu quero

max_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_max_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_max_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_max_01)))

min_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_min_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_min_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_min_01)))

max_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_max_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_max_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_max_02)))

min_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_min_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_min_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_min_02)))

max_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_max_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_max_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_max_03)))

min_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_min_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_min_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_min_03)))

## incluindo a coluna com o nome max e min para eu poder pegar no df que eu quero

max_01$temp <- "Max"
min_01$temp <- "Min"
max_02$temp <- "Max"
min_02$temp <- "Min"
max_03$temp <- "Max"
min_03$temp <- "Min"

## rbind do dia mais quente e mais frio, first pq depois eu vou criar outro

env_01_first <- rbind(max_01, min_01)
env_02_first <- rbind(max_02, min_02)
env_03_first <- rbind(max_03, min_03)

rm(max_01, min_01, max_02, min_02, max_03, min_03)

## criando o df com todas as informacoes necessarias para a criacao do grafico - temperatura operativa para os APPs ----

## envoltoria 1 para a sala

S_01 <- data.frame(Data=c(1:nrow(env_01_first)),temp=env_01_first$temp, Envoltoria=c("Pesada"),Portas=c(1:nrow(env_01_first)),Janelas=c(1:nrow(env_01_first)),Par_Internas=c(1:nrow(env_01_first)),Par_Externas=c(1:nrow(env_01_first)),
                   Cobertura=c(1:nrow(env_01_first)),Piso=c(1:nrow(env_01_first)),Cargas_Internas=c(1:nrow(env_01_first)))

S_01$Portas <- ((env_01_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Janelas <- ((env_01_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Internas <- ((env_01_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Externas <- ((env_01_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Cobertura <- ((env_01_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Piso <- env_01_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_01$Cargas_Internas <- ((env_01_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_01-1)) & (Date$dia <= (dia_max_01)) & (Date$mes == mes_max_01))))

S_01$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 2 para a sala

S_02 <- data.frame(Data=c(1:nrow(env_02_first)),temp=env_02_first$temp, Envoltoria=c("Tijolo"),Portas=c(1:nrow(env_02_first)),Janelas=c(1:nrow(env_02_first)),Par_Internas=c(1:nrow(env_02_first)),Par_Externas=c(1:nrow(env_02_first)),
                   Cobertura=c(1:nrow(env_02_first)),Piso=c(1:nrow(env_02_first)),Cargas_Internas=c(1:nrow(env_02_first)))

S_02$Portas <- ((env_02_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Janelas <- ((env_02_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Internas <- ((env_02_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Externas <- ((env_02_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Cobertura <- ((env_02_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Piso <- env_02_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_02$Cargas_Internas <- ((env_02_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_02-1)) & (Date$dia <= (dia_max_02)) & (Date$mes == mes_max_02))))

S_02$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 3 para a sala

S_03 <- data.frame(Data=c(1:nrow(env_03_first)),temp=env_03_first$temp, Envoltoria=c("Leve"),Portas=c(1:nrow(env_03_first)),Janelas=c(1:nrow(env_03_first)),Par_Internas=c(1:nrow(env_03_first)),Par_Externas=c(1:nrow(env_03_first)),
                   Cobertura=c(1:nrow(env_03_first)),Piso=c(1:nrow(env_03_first)),Cargas_Internas=c(1:nrow(env_03_first)))

S_03$Portas <- ((env_03_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Janelas <- ((env_03_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Internas <- ((env_03_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Externas <- ((env_03_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Cobertura <- ((env_03_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Piso <- env_03_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_03$Cargas_Internas <- ((env_03_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_03-1)) & (Date$dia <= (dia_max_03)) & (Date$mes == mes_max_03))))

S_03$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## rbind arquivos sala ----

sala <- rbind(S_01, S_02, S_03)

sala$Temp_Dia[sala$temp == "Max"] <- "Dia 27/11 e 28/11"
sala$Temp_Dia[sala$temp == "Min"] <- "Dia 27/08 e 28/08"

## grafico sala ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/resultados")

library(ggplot2)

png(filename = 'Curitiba_conduction.png', width = 32, height = 18, units = "cm", res = 500)
plot(
  ggplot(sala) + 
    geom_line(aes(x = Hora, y=Portas, colour = "Portas")) + 
    geom_line(aes(x = Hora, y=Janelas, colour = "Janelas"))+
    geom_line(aes(x = Hora, y=Par_Internas, colour = "Paredes Internas"))+
    geom_line(aes(x = Hora, y=Par_Externas, colour = "Paredes Externas"))+
    geom_line(aes(x = Hora, y=Cobertura, colour = "Cobertura"))+
    geom_line(aes(x = Hora, y=Piso, colour = "Piso"))+
    geom_line(aes(x = Hora, y=Cargas_Internas, colour = "Cargas Internas"))+
    labs(x="Horas nos Dias", y="Fluxo de Calor(?) [W]")+
    theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
          legend.position = "bottom",
          #legend.box = "vertical",
          #legend.key = element_rect(size = 5),
          # axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.title.x = element_text(size=12, vjust = 0.75),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.title = element_blank(),
          legend.text = element_text(size=12, hjust = 0.5),
          strip.text = element_text(size=13)) +
    #  scale_x_continuous(breaks = seq(from = 0, to = 600, by = 12))+  #  scale_x_discrete()+
    scale_x_datetime(date_breaks='1 hour',date_labels='%H:%M',expand = c(0, 0))+
    ylim(-1500,2000)+
        #geom_vline(xintercept = 8, colour="green", linetype = "longdash")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[97]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[265]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[385]), linetype=4, color="black")+
    facet_grid(Envoltoria~Temp_Dia)
)
dev.off()

rm(list = ls())

## CHAMANDO O ARQUIVO ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/BRA_RJ_Rio.de.Janeiro-Santos.Dumont.AP.837550_TMYx.2003-2017/")

file_list = list.files(pattern="*1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:3){ ## eh de 1 a 3 pois nao quero os 2 ultimos csv da pasta
  assign(file_list[i], read.csv(file_list[i]))
}

## incluindo o dia e o mes nos dfs

dia_mes <- data.frame(mes=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 2,3)), dia=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 5,6)))

construction_01_comp_02_ocup_01_1.csv <- cbind(construction_01_comp_02_ocup_01_1.csv, dia_mes)
construction_01_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_01_comp_02_ocup_01_1.csv$dia)
construction_01_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_01_comp_02_ocup_01_1.csv$mes)

construction_02_comp_02_ocup_01_1.csv <- cbind(construction_02_comp_02_ocup_01_1.csv, dia_mes)
construction_02_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_02_comp_02_ocup_01_1.csv$dia)
construction_02_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_02_comp_02_ocup_01_1.csv$mes)

construction_03_comp_02_ocup_01_1.csv <- cbind(construction_03_comp_02_ocup_01_1.csv, dia_mes)
construction_03_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_03_comp_02_ocup_01_1.csv$dia)
construction_03_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_03_comp_02_ocup_01_1.csv$mes)

rm(dia_mes)

## pegando o dia mais quente e o mais frio

dia_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

## criando os dfs com os dias que eu quero

max_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_max_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_max_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_max_01)))

min_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_min_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_min_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_min_01)))

max_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_max_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_max_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_max_02)))

min_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_min_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_min_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_min_02)))

max_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_max_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_max_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_max_03)))

min_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_min_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_min_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_min_03)))

## incluindo a coluna com o nome max e min para eu poder pegar no df que eu quero

max_01$temp <- "Max"
min_01$temp <- "Min"
max_02$temp <- "Max"
min_02$temp <- "Min"
max_03$temp <- "Max"
min_03$temp <- "Min"

## rbind do dia mais quente e mais frio, first pq depois eu vou criar outro

env_01_first <- rbind(max_01, min_01)
env_02_first <- rbind(max_02, min_02)
env_03_first <- rbind(max_03, min_03)

rm(max_01, min_01, max_02, min_02, max_03, min_03)

## criando o df com todas as informacoes necessarias para a criacao do grafico - temperatura operativa para os APPs ----

## envoltoria 1 para a sala

S_01 <- data.frame(Data=c(1:nrow(env_01_first)),temp=env_01_first$temp, Envoltoria=c("Pesada"),Portas=c(1:nrow(env_01_first)),Janelas=c(1:nrow(env_01_first)),Par_Internas=c(1:nrow(env_01_first)),Par_Externas=c(1:nrow(env_01_first)),
                   Cobertura=c(1:nrow(env_01_first)),Piso=c(1:nrow(env_01_first)),Cargas_Internas=c(1:nrow(env_01_first)))

S_01$Portas <- ((env_01_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Janelas <- ((env_01_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Internas <- ((env_01_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Externas <- ((env_01_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Cobertura <- ((env_01_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Piso <- env_01_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_01$Cargas_Internas <- ((env_01_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_01-1)) & (Date$dia <= (dia_max_01)) & (Date$mes == mes_max_01))))

S_01$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 2 para a sala

S_02 <- data.frame(Data=c(1:nrow(env_02_first)),temp=env_02_first$temp, Envoltoria=c("Tijolo"),Portas=c(1:nrow(env_02_first)),Janelas=c(1:nrow(env_02_first)),Par_Internas=c(1:nrow(env_02_first)),Par_Externas=c(1:nrow(env_02_first)),
                   Cobertura=c(1:nrow(env_02_first)),Piso=c(1:nrow(env_02_first)),Cargas_Internas=c(1:nrow(env_02_first)))

S_02$Portas <- ((env_02_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Janelas <- ((env_02_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Internas <- ((env_02_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Externas <- ((env_02_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Cobertura <- ((env_02_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Piso <- env_02_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_02$Cargas_Internas <- ((env_02_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_02-1)) & (Date$dia <= (dia_max_02)) & (Date$mes == mes_max_02))))

S_02$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 3 para a sala

S_03 <- data.frame(Data=c(1:nrow(env_03_first)),temp=env_03_first$temp, Envoltoria=c("Leve"),Portas=c(1:nrow(env_03_first)),Janelas=c(1:nrow(env_03_first)),Par_Internas=c(1:nrow(env_03_first)),Par_Externas=c(1:nrow(env_03_first)),
                   Cobertura=c(1:nrow(env_03_first)),Piso=c(1:nrow(env_03_first)),Cargas_Internas=c(1:nrow(env_03_first)))

S_03$Portas <- ((env_03_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Janelas <- ((env_03_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Internas <- ((env_03_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Externas <- ((env_03_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Cobertura <- ((env_03_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Piso <- env_03_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_03$Cargas_Internas <- ((env_03_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_03-1)) & (Date$dia <= (dia_max_03)) & (Date$mes == mes_max_03))))

S_03$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## rbind arquivos sala ----

sala <- rbind(S_01, S_02, S_03)

sala$Temp_Dia[sala$temp == "Max"] <- "Dia 23/11 e 24/11"
sala$Temp_Dia[sala$temp == "Min"] <- "Dia 05/09 e 06/09"

## grafico sala ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/resultados")

library(ggplot2)

png(filename = 'Rio_conduction.png', width = 32, height = 18, units = "cm", res = 500)
plot(
  ggplot(sala) + 
    geom_line(aes(x = Hora, y=Portas, colour = "Portas")) + 
    geom_line(aes(x = Hora, y=Janelas, colour = "Janelas"))+
    geom_line(aes(x = Hora, y=Par_Internas, colour = "Paredes Internas"))+
    geom_line(aes(x = Hora, y=Par_Externas, colour = "Paredes Externas"))+
    geom_line(aes(x = Hora, y=Cobertura, colour = "Cobertura"))+
    geom_line(aes(x = Hora, y=Piso, colour = "Piso"))+
    geom_line(aes(x = Hora, y=Cargas_Internas, colour = "Cargas Internas"))+
    labs(x="Horas nos Dias", y="Fluxo de Calor(?) [W]")+
    theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
          legend.position = "bottom",
          #legend.box = "vertical",
          #legend.key = element_rect(size = 5),
          # axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.title.x = element_text(size=12, vjust = 0.75),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.title = element_blank(),
          legend.text = element_text(size=12, hjust = 0.5),
          strip.text = element_text(size=13)) +
    #  scale_x_continuous(breaks = seq(from = 0, to = 600, by = 12))+  #  scale_x_discrete()+
    scale_x_datetime(date_breaks='1 hour',date_labels='%H:%M',expand = c(0, 0))+
    ylim(-1500,2000)+
        #geom_vline(xintercept = 8, colour="green", linetype = "longdash")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[97]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[265]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[385]), linetype=4, color="black")+
    facet_grid(Envoltoria~Temp_Dia)
)
dev.off()

rm(list = ls())

## CHAMANDO O ARQUIVO ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/BRA_RS_Santa.Maria.AB.839370_TMYx.2003-2017/")

file_list = list.files(pattern="*1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:3){ ## eh de 1 a 3 pois nao quero os 2 ultimos csv da pasta
  assign(file_list[i], read.csv(file_list[i]))
}

## incluindo o dia e o mes nos dfs

dia_mes <- data.frame(mes=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 2,3)), dia=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 5,6)))

construction_01_comp_02_ocup_01_1.csv <- cbind(construction_01_comp_02_ocup_01_1.csv, dia_mes)
construction_01_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_01_comp_02_ocup_01_1.csv$dia)
construction_01_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_01_comp_02_ocup_01_1.csv$mes)

construction_02_comp_02_ocup_01_1.csv <- cbind(construction_02_comp_02_ocup_01_1.csv, dia_mes)
construction_02_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_02_comp_02_ocup_01_1.csv$dia)
construction_02_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_02_comp_02_ocup_01_1.csv$mes)

construction_03_comp_02_ocup_01_1.csv <- cbind(construction_03_comp_02_ocup_01_1.csv, dia_mes)
construction_03_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_03_comp_02_ocup_01_1.csv$dia)
construction_03_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_03_comp_02_ocup_01_1.csv$mes)

rm(dia_mes)

## pegando o dia mais quente e o mais frio

dia_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

## criando os dfs com os dias que eu quero

max_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_max_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_max_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_max_01)))

min_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_min_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_min_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_min_01)))

max_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_max_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_max_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_max_02)))

min_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_min_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_min_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_min_02)))

max_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_max_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_max_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_max_03)))

min_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_min_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_min_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_min_03)))

## incluindo a coluna com o nome max e min para eu poder pegar no df que eu quero

max_01$temp <- "Max"
min_01$temp <- "Min"
max_02$temp <- "Max"
min_02$temp <- "Min"
max_03$temp <- "Max"
min_03$temp <- "Min"

## rbind do dia mais quente e mais frio, first pq depois eu vou criar outro

env_01_first <- rbind(max_01, min_01)
env_02_first <- rbind(max_02, min_02)
env_03_first <- rbind(max_03, min_03)

rm(max_01, min_01, max_02, min_02, max_03, min_03)

## criando o df com todas as informacoes necessarias para a criacao do grafico - temperatura operativa para os APPs ----

## envoltoria 1 para a sala

S_01 <- data.frame(Data=c(1:nrow(env_01_first)),temp=env_01_first$temp, Envoltoria=c("Pesada"),Portas=c(1:nrow(env_01_first)),Janelas=c(1:nrow(env_01_first)),Par_Internas=c(1:nrow(env_01_first)),Par_Externas=c(1:nrow(env_01_first)),
                   Cobertura=c(1:nrow(env_01_first)),Piso=c(1:nrow(env_01_first)),Cargas_Internas=c(1:nrow(env_01_first)))

S_01$Portas <- ((env_01_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Janelas <- ((env_01_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Internas <- ((env_01_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Externas <- ((env_01_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Cobertura <- ((env_01_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Piso <- env_01_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_01$Cargas_Internas <- ((env_01_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_01-1)) & (Date$dia <= (dia_max_01)) & (Date$mes == mes_max_01))))

S_01$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 2 para a sala

S_02 <- data.frame(Data=c(1:nrow(env_02_first)),temp=env_02_first$temp, Envoltoria=c("Tijolo"),Portas=c(1:nrow(env_02_first)),Janelas=c(1:nrow(env_02_first)),Par_Internas=c(1:nrow(env_02_first)),Par_Externas=c(1:nrow(env_02_first)),
                   Cobertura=c(1:nrow(env_02_first)),Piso=c(1:nrow(env_02_first)),Cargas_Internas=c(1:nrow(env_02_first)))

S_02$Portas <- ((env_02_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Janelas <- ((env_02_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Internas <- ((env_02_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Externas <- ((env_02_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Cobertura <- ((env_02_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Piso <- env_02_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_02$Cargas_Internas <- ((env_02_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_02-1)) & (Date$dia <= (dia_max_02)) & (Date$mes == mes_max_02))))

S_02$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 3 para a sala

S_03 <- data.frame(Data=c(1:nrow(env_03_first)),temp=env_03_first$temp, Envoltoria=c("Leve"),Portas=c(1:nrow(env_03_first)),Janelas=c(1:nrow(env_03_first)),Par_Internas=c(1:nrow(env_03_first)),Par_Externas=c(1:nrow(env_03_first)),
                   Cobertura=c(1:nrow(env_03_first)),Piso=c(1:nrow(env_03_first)),Cargas_Internas=c(1:nrow(env_03_first)))

S_03$Portas <- ((env_03_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Janelas <- ((env_03_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Internas <- ((env_03_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Externas <- ((env_03_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Cobertura <- ((env_03_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Piso <- env_03_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_03$Cargas_Internas <- ((env_03_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_03-1)) & (Date$dia <= (dia_max_03)) & (Date$mes == mes_max_03))))

S_03$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## rbind arquivos sala ----

sala <- rbind(S_01, S_02, S_03)

sala$Temp_Dia[sala$temp == "Max"] <- "Dia 01/02 e 02/02"
sala$Temp_Dia[sala$temp == "Min"] <- "Dia 20/07 e 21/07"

## grafico sala ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/resultados")

library(ggplot2)

png(filename = 'Santa_conduction.png', width = 32, height = 18, units = "cm", res = 500)
plot(
  ggplot(sala) + 
    geom_line(aes(x = Hora, y=Portas, colour = "Portas")) + 
    geom_line(aes(x = Hora, y=Janelas, colour = "Janelas"))+
    geom_line(aes(x = Hora, y=Par_Internas, colour = "Paredes Internas"))+
    geom_line(aes(x = Hora, y=Par_Externas, colour = "Paredes Externas"))+
    geom_line(aes(x = Hora, y=Cobertura, colour = "Cobertura"))+
    geom_line(aes(x = Hora, y=Piso, colour = "Piso"))+
    geom_line(aes(x = Hora, y=Cargas_Internas, colour = "Cargas Internas"))+
    labs(x="Horas nos Dias", y="Fluxo de Calor(?) [W]")+
    theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
          legend.position = "bottom",
          #legend.box = "vertical",
          #legend.key = element_rect(size = 5),
          # axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.title.x = element_text(size=12, vjust = 0.75),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.title = element_blank(),
          legend.text = element_text(size=12, hjust = 0.5),
          strip.text = element_text(size=13)) +
    #  scale_x_continuous(breaks = seq(from = 0, to = 600, by = 12))+  #  scale_x_discrete()+
    scale_x_datetime(date_breaks='1 hour',date_labels='%H:%M',expand = c(0, 0))+
    ylim(-1500,2000)+
    
    #geom_vline(xintercept = 8, colour="green", linetype = "longdash")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[97]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[265]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[385]), linetype=4, color="black")+
    facet_grid(Envoltoria~Temp_Dia)
)
dev.off()

rm(list = ls())

## CHAMANDO O ARQUIVO ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/BRA_SP_Sao.Paulo-Congonhas.AP.837800_TMYx.2003-2017/")

file_list = list.files(pattern="*1.csv") # todos os csv menos o table

library("data.table")

for (i in 1:3){ ## eh de 1 a 3 pois nao quero os 2 ultimos csv da pasta
  assign(file_list[i], read.csv(file_list[i]))
}

## incluindo o dia e o mes nos dfs

dia_mes <- data.frame(mes=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 2,3)), dia=c(substr(construction_01_comp_02_ocup_01_1.csv$Date.Time, 5,6)))

construction_01_comp_02_ocup_01_1.csv <- cbind(construction_01_comp_02_ocup_01_1.csv, dia_mes)
construction_01_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_01_comp_02_ocup_01_1.csv$dia)
construction_01_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_01_comp_02_ocup_01_1.csv$mes)

construction_02_comp_02_ocup_01_1.csv <- cbind(construction_02_comp_02_ocup_01_1.csv, dia_mes)
construction_02_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_02_comp_02_ocup_01_1.csv$dia)
construction_02_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_02_comp_02_ocup_01_1.csv$mes)

construction_03_comp_02_ocup_01_1.csv <- cbind(construction_03_comp_02_ocup_01_1.csv, dia_mes)
construction_03_comp_02_ocup_01_1.csv$dia <- as.numeric(construction_03_comp_02_ocup_01_1.csv$dia)
construction_03_comp_02_ocup_01_1.csv$mes <- as.numeric(construction_03_comp_02_ocup_01_1.csv$mes)

rm(dia_mes)

## pegando o dia mais quente e o mais frio

dia_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.max(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_01 <- as.numeric(construction_01_comp_02_ocup_01_1.csv[which.min(construction_01_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.max(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_02 <- as.numeric(construction_02_comp_02_ocup_01_1.csv[which.min(construction_02_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

dia_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_max_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.max(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])
dia_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("dia")])
mes_min_03 <- as.numeric(construction_03_comp_02_ocup_01_1.csv[which.min(construction_03_comp_02_ocup_01_1.csv$Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.),c("mes")])

## criando os dfs com os dias que eu quero

max_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_max_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_max_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_max_01)))

min_01 <- subset(construction_01_comp_02_ocup_01_1.csv, ((construction_01_comp_02_ocup_01_1.csv$dia >= (dia_min_01-1)) & (construction_01_comp_02_ocup_01_1.csv$dia <= (dia_min_01)) & 
                                                           (construction_01_comp_02_ocup_01_1.csv$mes == mes_min_01)))

max_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_max_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_max_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_max_02)))

min_02 <- subset(construction_02_comp_02_ocup_01_1.csv, ((construction_02_comp_02_ocup_01_1.csv$dia >= (dia_min_02-1)) & (construction_02_comp_02_ocup_01_1.csv$dia <= (dia_min_02)) & 
                                                           (construction_02_comp_02_ocup_01_1.csv$mes == mes_min_02)))

max_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_max_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_max_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_max_03)))

min_03 <- subset(construction_03_comp_02_ocup_01_1.csv, ((construction_03_comp_02_ocup_01_1.csv$dia >= (dia_min_03-1)) & (construction_03_comp_02_ocup_01_1.csv$dia <= (dia_min_03)) & 
                                                           (construction_03_comp_02_ocup_01_1.csv$mes == mes_min_03)))

## incluindo a coluna com o nome max e min para eu poder pegar no df que eu quero

max_01$temp <- "Max"
min_01$temp <- "Min"
max_02$temp <- "Max"
min_02$temp <- "Min"
max_03$temp <- "Max"
min_03$temp <- "Min"

## rbind do dia mais quente e mais frio, first pq depois eu vou criar outro

env_01_first <- rbind(max_01, min_01)
env_02_first <- rbind(max_02, min_02)
env_03_first <- rbind(max_03, min_03)

rm(max_01, min_01, max_02, min_02, max_03, min_03)

## criando o df com todas as informacoes necessarias para a criacao do grafico - temperatura operativa para os APPs ----

## envoltoria 1 para a sala

S_01 <- data.frame(Data=c(1:nrow(env_01_first)),temp=env_01_first$temp, Envoltoria=c("Pesada"),Portas=c(1:nrow(env_01_first)),Janelas=c(1:nrow(env_01_first)),Par_Internas=c(1:nrow(env_01_first)),Par_Externas=c(1:nrow(env_01_first)),
                   Cobertura=c(1:nrow(env_01_first)),Piso=c(1:nrow(env_01_first)),Cargas_Internas=c(1:nrow(env_01_first)))

S_01$Portas <- ((env_01_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_01_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Janelas <- ((env_01_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Internas <- ((env_01_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Par_Externas <- ((env_01_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_01_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Cobertura <- ((env_01_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_01_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_01$Piso <- env_01_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_01$Cargas_Internas <- ((env_01_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_01_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_01-1)) & (Date$dia <= (dia_max_01)) & (Date$mes == mes_max_01))))

S_01$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 2 para a sala

S_02 <- data.frame(Data=c(1:nrow(env_02_first)),temp=env_02_first$temp, Envoltoria=c("Tijolo"),Portas=c(1:nrow(env_02_first)),Janelas=c(1:nrow(env_02_first)),Par_Internas=c(1:nrow(env_02_first)),Par_Externas=c(1:nrow(env_02_first)),
                   Cobertura=c(1:nrow(env_02_first)),Piso=c(1:nrow(env_02_first)),Cargas_Internas=c(1:nrow(env_02_first)))

S_02$Portas <- ((env_02_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_02_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Janelas <- ((env_02_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Internas <- ((env_02_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Par_Externas <- ((env_02_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_02_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Cobertura <- ((env_02_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_02_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_02$Piso <- env_02_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_02$Cargas_Internas <- ((env_02_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_02_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_02-1)) & (Date$dia <= (dia_max_02)) & (Date$mes == mes_max_02))))

S_02$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## envoltoria 3 para a sala

S_03 <- data.frame(Data=c(1:nrow(env_03_first)),temp=env_03_first$temp, Envoltoria=c("Leve"),Portas=c(1:nrow(env_03_first)),Janelas=c(1:nrow(env_03_first)),Par_Internas=c(1:nrow(env_03_first)),Par_Externas=c(1:nrow(env_03_first)),
                   Cobertura=c(1:nrow(env_03_first)),Piso=c(1:nrow(env_03_first)),Cargas_Internas=c(1:nrow(env_03_first)))

S_03$Portas <- ((env_03_first$PORTAINT_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAEXT_LESTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAEXT_NORTE.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PORTAINT_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                  (env_03_first$PORTAINT_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Janelas <- ((env_03_first$JANSALA_SUL.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$JANSALA_OESTE.Surface.Window.Net.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Internas <- ((env_03_first$PARINT1_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT1_SALADORM2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PARINT2_SALABWC.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PARINT2_SALADORM1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Par_Externas <- ((env_03_first$PAREXT_LESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_NORTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) +
                        (env_03_first$PAREXT_OESTE_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$PAREXT_SUL_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Cobertura <- ((env_03_first$FORRO_SALAATICO1.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.) + (env_03_first$FORRO_SALAATICO2.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.))

S_03$Piso <- env_03_first$PISO_SALA.Surface.Average.Face.Conduction.Heat.Transfer.Rate..W..TimeStep.

S_03$Cargas_Internas <- ((env_03_first$SALA.Zone.Electric.Equipment.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.Lights.Total.Heating.Rate..W..TimeStep.) + (env_03_first$SALA.Zone.People.Total.Heating.Rate..W..TimeStep.))

Date <- data.frame(seq(ISOdate(2019,1,1,0,5,0),by='5 min',length.out=365*24*12,tz=''))
Date$dia <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 9,10))
Date$mes <- as.numeric(substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 6,7))
Date$hora <- substr(Date$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365..., 12,16)

Data_max <- data.frame(subset(Date, ((Date$dia >= (dia_max_03-1)) & (Date$dia <= (dia_max_03)) & (Date$mes == mes_max_03))))

S_03$Hora <- Data_max$seq.ISOdate.2019..1..1..0..5..0...by....5.min...length.out...365...

## rbind arquivos sala ----

sala <- rbind(S_01, S_02, S_03)

sala$Temp_Dia[sala$temp == "Max"] <- "Dia 18/09 e 19/09"
sala$Temp_Dia[sala$temp == "Min"] <- "Dia 10/06 e 11/06"

## grafico sala ----

setwd("D:/Frentes de Trabalho/simu_NBR/exposicoes_conduction/casos_com_solo_e_cob/resultados")

library(ggplot2)

png(filename = 'Sao_conduction.png', width = 32, height = 18, units = "cm", res = 500)
plot(
  ggplot(sala) + 
    geom_line(aes(x = Hora, y=Portas, colour = "Portas")) + 
    geom_line(aes(x = Hora, y=Janelas, colour = "Janelas"))+
    geom_line(aes(x = Hora, y=Par_Internas, colour = "Paredes Internas"))+
    geom_line(aes(x = Hora, y=Par_Externas, colour = "Paredes Externas"))+
    geom_line(aes(x = Hora, y=Cobertura, colour = "Cobertura"))+
    geom_line(aes(x = Hora, y=Piso, colour = "Piso"))+
    geom_line(aes(x = Hora, y=Cargas_Internas, colour = "Cargas Internas"))+
    labs(x="Horas nos Dias", y="Fluxo de Calor(?) [W]")+
    theme(axis.text.x=element_text(angle=90, vjust = 0.5, size=9),
          legend.position = "bottom",
          #legend.box = "vertical",
          #legend.key = element_rect(size = 5),
          # axis.title.x = element_blank(),
          #axis.text.x = element_blank(),
          axis.title.x = element_text(size=12, vjust = 0.75),
          axis.title.y = element_text(size=12, vjust = 2),
          axis.text.y = element_text(size=10),
          legend.title = element_blank(),
          legend.text = element_text(size=12, hjust = 0.5),
          strip.text = element_text(size=13)) +
    #  scale_x_continuous(breaks = seq(from = 0, to = 600, by = 12))+  #  scale_x_discrete()+
    scale_x_datetime(date_breaks='1 hour',date_labels='%H:%M',expand = c(0, 0))+
    ylim(-1500,2000)+
    
    #geom_vline(xintercept = 8, colour="green", linetype = "longdash")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[97]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[265]), linetype=4, color="black")+
    #  geom_vline(xintercept=as.numeric(env_01$Hora[385]), linetype=4, color="black")+
    facet_grid(Envoltoria~Temp_Dia)
)
dev.off()

rm(list = ls())