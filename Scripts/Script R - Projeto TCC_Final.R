################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################

#Pacotes utilizados


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

library(plotly)
library(dplyr)
library(writexl)

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


# Dados Iniciais
###############################################################################
# As ações escolhidas a seguir NÃO SÃO RECOMENDAÇÕES DE INVESTIMENTOS POR NOSSA 
# PARTE, refletem apenas exemplos meramente ilustrativos da prática.
###############################################################################

Acoes = matrix(c('AZUL4.SA',
                 'GOLL4.SA',
                 'EMBR3.SA',
                 'ITSA4.SA',
                 'ITUB4.SA',
                 'BPAC11.SA',
                 'BBAS3.SA',
                 'ABCB4.SA',
                 'SANB11.SA',
                 'BRSR6.SA',
                 'BMGB4.SA',
                 'BEES3.SA',
                 'BAZA3.SA',
                 'ALOS3.SA',
                 'CURY3.SA',
                 'IGTI11.SA',
                 'MRVE3.SA',
                 'TEND3.SA',
                 'VALE3.SA',
                 'CMIN3.SA',
                 'AURA33.SA',
                 'MMXM3.SA',
                 'BRKM5.SA',
                 'RRRP3.SA',
                 'DEXP3.SA',
                 'PETR3.SA',
                 'PETR4.SA',
                 'ALPA4.SA',
                 'MGLU3.SA',
                 'BAIA3.SA',
                 'AMER3.SA',
                 'CEAB3.SA',
                 'LREN3.SA',
                 'AMAR3.SA'))
first_date = as.Date('2022-01-01')
last_date =  as.Date('2022-12-31')

#======================[ função de cálculo de risco ]==========================

retorno = function(Df){
  ret <- Df %>%
    Cl() %>%
    log() %>%
    diff()
  return(ret)  
}


#==============================================================================
#
# Gerando data frane 
#
#==============================================================================

x<- 0
rc <-0.00
library(data.table)
library(tidyverse)    

bov <- (yf_get(tickers = Acoes, 
               first_date,
               last_date)) 

pesquisa <- data.table(bov) 
# Elimina colunas desnecessárias    
pesquisa$ret_adjusted_prices <- NULL
pesquisa$ret_closing_prices <- NULL
pesquisa$cumret_adjusted_prices <-NULL
# Atribuindo a coluna risco para a base pesquisa, criando
# através da função sd() o Desvio padrão, "entendido como media de Risco
# a partir da coluna - price_adjusted.


# Obtendo Retornos
# Obtendo Retornos - Calculo da Rentabilidade
x<-1

while (x <= length(Acoes)) {
  
  # No código da 178, observa - se a criação do data frame Ret, que usa como argumento
  # de criação a execução da função "retorno" função que retorna o retorno referente aos
  # dados do dataframe -> pesquisa.
  
  Ret <-data.frame(retorno(pesquisa))
  ac <- Acoes[x]
  
  # Após gerado o dataframe Ret, criq - se então uma matriz de dados, 
  # através do loop abaixo, que baseado no conteúdo do objeto ac 
  # é obtido o array Rent, que contém como conteúdo os retornos obtidos 
  # em Ret$retorno.pesquisa.
  
  for (ind in ac) {
    Rent <- c(Ret$retorno.pesquisa.)
  }
  
  # Após gerado o Array Rent. é criada no dataframe pesquisa, 
  # a coluna rentabilidade
  
  pesquisa$risco <- 0.00
  pesquisa$rentabilidade <- 0.00
  
  # no laço for abaixo, observa - se um filtro, onde, o índice rentabilidade, é 
  # criado, baseando-se no conteúdo do objeto ac, que por sua vez contém os
  # índices das ações pesquisadas, conforme a matrix -> Ações, criada anteriormente
  
  for (ind in pesquisa$ticker == ac) {
    pesquisa$rentabilidade <- c(Rent[-1],dim(pesquisa))
  }  
  x <- x + 1
}  
# Cálculo do índice de Volatilidade

DF_gspec <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list("sGARCH"),
                       distribution.model = "norm")

Df_gfit <- ugarchfit(data = Ret, spec = DF_gspec)

#Aplicação da volatilidade: Teorema da separação de Tobin
# Calcula a volatilidade anualizada

volat <- sigma(Df_gfit) * sqrt(252)

pesquisa = pesquisa[-1] %>% select(ticker, ref_date, price_adjusted, risco,rentabilidade) %>%
  mutate(volatilidade <- c(volat))

volatilidade <- pesquisa$`volatilidade <- c(volat)`
pesquisa$volatilidade <- pesquisa$`volatilidade <- c(volat)`
pesquisa$`volatilidade <- c(volat)` <-NULL

# Criação do índice de liquidez

pesquisa = pesquisa %>% select(ticker, ref_date, price_adjusted, risco, volatilidade, rentabilidade) %>%
  mutate(liquidez = price_adjusted * rentabilidade)    

# 
# Calcular o valor de Venda
#
# Após criado o índice de rentabilidade, torna - se possível a criação do preço
# de venda, onde é feita uma análise através da função ifelse(), caso o valor do índice
# da rentabilidade for < 0, então é feito o calculo do preço de venda, conforme a fómula 

valorVenda <- ifelse(pesquisa$rentabilidade<0,pesquisa$price_adjusted * (1-(pesquisa$rentabilidade*-1)/100),pesquisa$price_adjusted * 1+(pesquisa$rentabilidade))
pesquisa$valorVenda <- valorVenda


# Início da criação da carteira de investimentos

# Inicialmente, deve - se criar uma arquivo CSV contendo grupos de ações
# para compor a carteira, seria algo desse tipo:

# Carteira - Setor bancário
# Nesse caso, essa carteira, conterá ações pertencentes a esse setor e que 
# também pertençam ao data frame - pesquisa, que posteriormente terá o 
# seu nome modificado, portanto esse CSV, teria o formato abaixo

# Ativo
# BBAS3.SA
# ITSA4.SA
# ITUB4.SA
# BPAC11.SA

# Uma vez lendo essas informações, o script, deverá buscar essas informações no
# Data Frame -> pesquisa, trazendo os indices calculados anteriormente:
# Risco, Rentabilidade, Volatilidade e Liquidez.

# Após feito isso, será criado então um novo data frame, contendo esses intens em
# seu conteúdo, nesse momento, inicia - se então os cálculos baseados na planilha 
# MAIRCA.

# Observação: - a cada item localizado no data frame - pesquisa, este deverá ser
# calculado para cada índice o valor médio de toda a série temporal

carteira<-data.frame(read.csv("C:\\Users\\Usuario\\Desktop\\Tcc_rendaVariavel\\carteiras\\Carteira-Varejo.csv"))
# Passo 1 - Montagem da Matriz de decisão

rm(data)

data <- data.table(carteira) 
item <- nrow(data)
ind <- 1

library(tidyverse)
library(lubridate)

rm(decisao)
rm(ativos)
rm(xdados1)
rm(xdados2)
rm(xdados3)
rm(xdados4)
rm(xdados5)
rm(xdados6)
rm(xdados7)
rm(xdados8)
rm(xdados9)
rm(xdados10)
#
rm(filtro)
rm(filtro2)
rm(filtro3)
rm(filtro4)
rm(filtro5)
rm(filtro6)
rm(filtro7)
rm(filtro8)
rm(filtro9)
rm(filtro10)    
ativos <- c(carteira$Ativo)


  # cálculo do Critério de Risco
  filtro <- pesquisa%>%
  filter(ticker == ativos[1])
  
    # cálculo do Critério de Risco
    
    risc <- getSymbols(ativos[1],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    filtro$risco <- rc
    
    xdados1 <- tibble::tibble(ativo = c(ativos[1]),
                              risco = c(mean(filtro$risco)),
                              rentabilidade = c(mean(filtro$rentabilidade)),
                              volatilidade = c(mean(filtro$volatilidade)),
                              liquidez = c(mean(filtro$liquidez)))
    filtro2 <- pesquisa%>%
    filter(ticker == ativos[2])      
      # cálculo do Critério de Risco

        risc <- getSymbols(ativos[2],
                           from = first_date,  
                           to = last_date,
                           auto.assign = FALSE)
        risc_rets <- risc %>%
          Cl() %>%
          log() %>%
          diff()
        
        mu <- risc_rets %>%
          na.omit() %>%
          mean()
        sigma <- risc_rets %>%
          na.omit() %>%
          sd()
        
        rc <- qnorm(0.025, mean = mu, sd = sigma)
        
        filtro2$risco <- rc
     
    xdados2 <- tibble::tibble(ativo = c(ativos[2]),
                              risco = c(mean(filtro2$risco)),
                              rentabilidade = c(mean(filtro2$rentabilidade)),
                              volatilidade = c(mean(filtro2$volatilidade)),
                              liquidez = c(mean(filtro2$liquidez)))
    filtro3 <- pesquisa%>%
      filter(ticker == ativos[3])      
    # cálculo do Critério de Risco
    
    risc <- getSymbols(ativos[3],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro3$risco <- rc
    
    xdados3 <- tibble::tibble(ativo = c(ativos[3]),
                              risco = c(mean(filtro3$risco)),
                              rentabilidade = c(mean(filtro3$rentabilidade)),
                              volatilidade = c(mean(filtro3$volatilidade)),
                              liquidez = c(mean(filtro3$liquidez)))
                              
    filtro4 <- pesquisa%>%
      filter(ticker == ativos[4])

      # cálculo do Critério de Risco

    risc <- getSymbols(ativos[4],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro4$risco <- rc

    xdados4 <- tibble::tibble(ativo = c(ativos[4]),
                              risco = c(mean(filtro4$risco)),
                              rentabilidade = c(mean(filtro4$rentabilidade)),
                              volatilidade = c(mean(filtro4$volatilidade)),
                              liquidez = c(mean(filtro4$liquidez)))
    filtro5 <- pesquisa%>%
      filter(ticker == ativos[5])
    
    # cálculo do Critério de Risco
   
    risc <- getSymbols(ativos[5],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro5$risco <- rc
    
    
    xdados5 <- tibble::tibble(ativo = c(ativos[5]),
                              risco = c(mean(filtro5$risco)),
                              rentabilidade = c(mean(filtro5$rentabilidade)),
                              volatilidade = c(mean(filtro5$volatilidade)),
                              liquidez = c(mean(filtro5$liquidez)))
    filtro6 <- pesquisa%>%
      filter(ticker == ativos[6])
    
    # cálculo do Critério de Risco
      
      risc <- getSymbols(ativos[6],
                         from = first_date,  
                         to = last_date,
                         auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro6$risco <- rc

    xdados6 <- tibble::tibble(ativo = c(ativos[6]),
                              risco = c(mean(filtro6$risco)),
                              rentabilidade = c(mean(filtro6$rentabilidade)),
                              volatilidade = c(mean(filtro6$volatilidade)),
                              liquidez = c(mean(filtro6$liquidez)))
    filtro7 <- pesquisa%>%
      filter(ticker == ativos[7])

    
      
      # cálculo do Critério de Risco

    risc <- getSymbols(ativos[7],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro7$risco <- rc
    
    xdados7 <- tibble::tibble(ativo = c(ativos[7]),
                              risco = c(mean(filtro7$risco)),
                              rentabilidade = c(mean(filtro7$rentabilidade)),
                              volatilidade = c(mean(filtro7$volatilidade)),
                              liquidez = c(mean(filtro7$liquidez)))
    filtro8 <- pesquisa%>%
      filter(ticker == ativos[8])
    
    # cálculo do Critério de Risco

    risc <- getSymbols(ativos[8],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro8$risco <- rc
    
    
    xdados8 <- tibble::tibble(ativo = c(ativos[8]),
                              risco = c(mean(filtro8$risco)),
                              rentabilidade = c(mean(filtro8$rentabilidade)),
                              volatilidade = c(mean(filtro8$volatilidade)),
                              liquidez = c(mean(filtro8$liquidez)))
    filtro9 <- pesquisa%>%
      filter(ticker == ativos[9])
    
    # cálculo do Critério de Risco

    risc <- getSymbols(ativos[9],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro9$risco <- rc
    
    
    xdados9 <- tibble::tibble(ativo = c(ativos[9]),
                              risco = c(mean(filtro9$risco)),
                              rentabilidade = c(mean(filtro9$rentabilidade)),
                              volatilidade = c(mean(filtro9$volatilidade)),
                              liquidez = c(mean(filtro9$liquidez)))
    filtro10 <- pesquisa%>%
      filter(ticker == ativos[10])
    
    # cálculo do Critério de Risco

    risc <- getSymbols(ativos[10],
                       from = first_date,  
                       to = last_date,
                       auto.assign = FALSE)
    risc_rets <- risc %>%
      Cl() %>%
      log() %>%
      diff()
    
    mu <- risc_rets %>%
      na.omit() %>%
      mean()
    sigma <- risc_rets %>%
      na.omit() %>%
      sd()
    
    rc <- qnorm(0.025, mean = mu, sd = sigma)
    
    filtro10$risco <- rc
    
    
    xdados10 <- tibble::tibble(ativo = c(ativos[10]), 
                               risco = c(mean(filtro10$risco)),
                               rentabilidade = c(mean(filtro10$rentabilidade)),
                               volatilidade = c(mean(filtro10$volatilidade)),
                               liquidez = c(mean(filtro10$liquidez)))

if (item <= 3) {        
  decisao <- rbind(xdados1,xdados2,xdados3)
}
if (item == 4) {
  decisao <- rbind(xdados1,xdados2,xdados3,xdados4)        
}
if (item == 5) {      
  decisao <- rbind(xdados1,xdados2,xdados3,xdados4,xdados5)      
}
if (item == 6) {            
  decisao <- rbind(xdados1,xdados2,xdados3,xdados4,xdados5,xdados6)            
}
if (item == 7) {            
  decisao <- rbind(xdados1,xdados2,xdados3,xdados4,xdados5,xdados6,xdados7)            
}
if (item == 8) {            
  decisao <- rbind(xdados1,xdados2,xdados3,xdados4,xdados5,xdados6,xdados7,xdados8)            
}
if (item == 9) {            
  decisao <- rbind(xdados1,xdados2,xdados3,xdados4,xdados5,xdados6,xdados7,xdados8,xdados9)            
}
if (item == 10) {            
  decisao <- rbind(xdados1,xdados2,xdados3,xdados4,xdados5,xdados6,xdados7,xdados8,xdados9,xdados10)            
}
#  exportar a matriz de decisão para o excel
write_xlsx(decisao,"C:\\Users\\Usuario\\Desktop\\Tcc_rendaVariavel\\Matriz_Decisao\\Matriz_Decisao.xlsx")

