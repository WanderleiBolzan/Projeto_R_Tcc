pacotes <- c("quantmod","PerformanceAnalytics","magrittr","ggplot2",
             "fPortfolio","timeSeries","dplyr","yfR")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

pacotes <- c("readxl","plotly","tidyverse","gridExtra","forecast","TTR",
             "smooth","tidyverse", "tsibble", "fable","tsibbledata", "fpp3",
             "urca","ggplot2")

library(writexl)

library(plotly)
library(dplyr)

if(!require('rugarch')) {
  install.packages('rugarch')
  library('rugarch')
}


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T)
} else {
  sapply(pacotes, require, character = T)
}
  # fazer as normalizações de acordo com a documentação do MAIRCA e a planilha

riscoNormal1 <- min(decisao$risco) / decisao$risco[1]  
rentaNormal1 <- min(decisao$rentabilidade) / decisao$rentabilidade[1]
volatNormal1 <- min(decisao$volatilidade) / decisao$volatilidade[1]
liquiNormal1 <- min(decisao$liquidez) / decisao$liquidez[1]  

riscoNormal2 <- min(decisao$risco) / decisao$risco[2]          
rentaNormal2 <- min(decisao$rentabilidade) / decisao$rentabilidade[2]  
volatNormal2 <- min(decisao$volatilidade) / decisao$volatilidade[2]  
liquiNormal2 <- min(decisao$liquidez) / decisao$liquidez[2]       

riscoNormal3 <- min(decisao$risco) / decisao$risco[3]            
rentaNormal3 <- min(decisao$rentabilidade) / decisao$rentabilidade[3]  
volatNormal3 <- min(decisao$volatilidade) / decisao$volatilidade[3]  
liquiNormal3 <- min(decisao$liquidez) / decisao$liquidez[3]       

riscoNormal4 <- min(decisao$risco) / decisao$risco[4]          
rentaNormal4 <- min(decisao$rentabilidade) / decisao$rentabilidade[4]  
volatNormal4 <- min(decisao$volatilidade) / decisao$volatilidade[4]  
liquiNormal4 <- min(decisao$liquidez) / decisao$liquidez[4]       

riscoNormal5 <- min(decisao$risco) / decisao$risco[5]
rentaNormal5 <- min(decisao$rentabilidade) / decisao$rentabilidade[5]  
volatNormal5 <- min(decisao$volatilidade) / decisao$volatilidade[5]  
liquiNormal5 <- min(decisao$liquidez) / decisao$liquidez[5]       

riscoNormal6 <- min(decisao$risco) / decisao$risco[6]            
rentaNormal6 <- min(decisao$rentabilidade) / decisao$rentabilidade[6]  
volatNormal6 <- min(decisao$volatilidade) / decisao$volatilidade[6]  
liquiNormal6 <- min(decisao$liquidez) / decisao$liquidez[6]       

riscoNormal7 <- min(decisao$risco) / decisao$risco[7]            
rentaNormal7 <- min(decisao$rentabilidade) / decisao$rentabilidade[7]  
volatNormal7 <- min(decisao$volatilidade) / decisao$volatilidade[7]  
liquiNormal7 <- min(decisao$liquidez) / decisao$liquidez[7]       

riscoNormal8 <- min(decisao$risco) / decisao$risco[8]            
rentaNormal8 <- min(decisao$rentabilidade) / decisao$rentabilidade[8]  
volatNormal8 <- min(decisao$volatilidade) / decisao$volatilidade[8]  
liquiNormal8 <- min(decisao$liquidez) / decisao$liquidez[8]       

riscoNormal9 <- min(decisao$risco) / decisao$risco[9]            
rentaNormal9 <- min(decisao$rentabilidade) / decisao$rentabilidade[9]  
volatNormal9 <- min(decisao$volatilidade) / decisao$volatilidade[9]  
liquiNormal9 <- min(decisao$liquidez) / decisao$liquidez[9]       

riscoNormal10 <- min(decisao$risco) / decisao$risco[10]            
rentaNormal10 <- min(decisao$rentabilidade) / decisao$rentabilidade[10]  
volatNormal10 <- min(decisao$volatilidade) / decisao$volatilidade[10]  
liquiNormal10 <- min(decisao$liquidez) / decisao$liquidez[10]       

# Criando a planilha
  if (item <= 3) {
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
  if (item == 4) {
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3,riscoNormal4)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3,rentaNormal4)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3,volatNormal4)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3,liquiNormal4)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
  if (item == 5) {      
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3,riscoNormal4,riscoNormal5)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3,rentaNormal4,rentaNormal5)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3,volatNormal4,volatNormal5)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3,liquiNormal4,liquiNormal5)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
  if (item == 6) {    
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3,riscoNormal4,riscoNormal5,riscoNormal6)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3,rentaNormal4,rentaNormal5,rentaNormal6)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3,volatNormal4,volatNormal5,volatNormal6)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3,liquiNormal4,liquiNormal5,liquiNormal6)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
  if (item == 7) {            
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3,riscoNormal4,riscoNormal5,riscoNormal6,riscoNormal7)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3,rentaNormal4,rentaNormal5,rentaNormal6,rentaNormal7)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3,volatNormal4,volatNormal5,volatNormal6,volatNormal7)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3,liquiNormal4,liquiNormal5,liquiNormal6,liquiNormal6)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
  if (item == 8) {            
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3,riscoNormal4,riscoNormal5,riscoNormal6,riscoNormal7,riscoNormal8)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3,rentaNormal4,rentaNormal5,rentaNormal6,rentaNormal7,rentaNormal8)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3,volatNormal4,volatNormal5,volatNormal6,volatNormal7,volatNormal8)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3,liquiNormal4,liquiNormal5,liquiNormal6,liquiNormal7,liquiNormal8)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
  if (item == 9) {            
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3,riscoNormal4,riscoNormal5,riscoNormal6,riscoNormal7,riscoNormal8,riscoNormal9)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3,rentaNormal4,rentaNormal5,rentaNormal6,rentaNormal7,rentaNormal8,rentaNormal9)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3,volatNormal4,volatNormal5,volatNormal6,volatNormal7,volatNormal8,volatNormal9)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3,liquiNormal4,liquiNormal5,liquiNormal6,liquiNormal7,liquiNormal8,liquiNormal9)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
  if (item == 10) {            
    ativo <- c(ativos)
    risco <- c(riscoNormal1,riscoNormal2,riscoNormal3,riscoNormal4,riscoNormal5,riscoNormal6,riscoNormal7,riscoNormal8,riscoNormal9,riscoNormal10)
    rentabilidade <- c(rentaNormal1,rentaNormal2,rentaNormal3,rentaNormal4,rentaNormal5,rentaNormal6,rentaNormal7,rentaNormal8,rentaNormal9,rentaNormal10)
    volatilidade  <- c(volatNormal1,volatNormal2,volatNormal3,volatNormal4,volatNormal5,volatNormal6,volatNormal7,volatNormal8,volatNormal9,volatNormal10)
    liquidez      <- c(liquiNormal1,liquiNormal2,liquiNormal3,liquiNormal4,liquiNormal5,liquiNormal6,liquiNormal7,liquiNormal8,liquiNormal9,liquiNormal10)    
    normal <- data.frame(cbind(ativo,risco,rentabilidade,volatilidade,liquidez))
  }
#  y<-y + 1
#}
#  exportar a matriz de decisão para o excel

write_xlsx(normal,"C:\\Users\\Usuario\\Desktop\\Tcc_rendaVariavel\\Matriz_Normal\\normal.xlsx")



