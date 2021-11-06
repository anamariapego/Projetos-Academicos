
#         PROJETO INTERDISCIPLINAR 

# Desenvolvimento do modelo arima com os dados de séries temporais do covid-19 da cidade de Porto Alegre - RS. 
# Evolução dos casos de covid-19 em Porto Alegre. 

library(tidyverse)
library(lubridate) 
library(forecast) 
library(ggplot2)

data <- read.csv('dados/casos.csv', sep = ';')

data$DATA_NOTIF <- as.Date(data$DATA_NOTIF, format = '%d/%m/%Y')

# filtrando as datas e criando  mais duas colunas (dia da semana e dias úteis(0)/não úteis(1))

data_arima <- data %>% count(DATA_NOTIF) %>% na.omit() 

# removendo os meses de feveriro, março.

data_arima <- data_arima[-c(1:24),]

plot_sem_maio <- data_arima[1:395,] # removendo o mês de maio que será para validação

# transformando em uma série e depois plotando os dados

dados_covid <- ts(plot_sem_maio$n, frequency = 365, start = c(2020,04))
plot(dados_covid) 

ggplot(plot_sem_maio, aes(x = DATA_NOTIF, y = n)) +
  geom_line() +
  labs(y = 'Nº de casos confirmados') +
  labs(x = 'Data') +
  ggtitle('Números de casos confirmados de Covid-19 entre abril/2020 a abril/2021 em Porto Alegre-RS') +
  theme_test() +
  theme(plot.title = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20), size = 12,  face = 'bold'),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

  
dados_casos <- ts(data_arima$n, frequency = 365, start = c(2020,04))
plot(dados_casos) 

# ACF - auto correlação
# PACF - auto correlação parcial 
acf(dados_covid)
pacf(dados_covid)

dim(data_arima)
length(dados_casos)
frequency(dados_casos) # em semana

par(mfrow=c(2,1))
plot(dados_covid, main = 'Serie original', xlab = "", ylab =  "")
plot(log(dados_covid), main = 'Serie logaritmica', xlab = "", ylab =  "")

# aplicando função logaritima para treino o modelo arima

dado_treino <- ts(dados_casos[1:395], frequency = 7) # 395 pontos

dado_validacao <- ts(dados_casos[396:425], frequency = 7) # 30 pontos

# criando o modelo 

modelo_arima <- auto.arima(y = log(dado_treino)) 
summary(modelo_arima)

# comparando a serie original com os valores ajustados 

par(mfrow=c(2,1))
plot(dado_treino)
plot(exp(modelo_arima$fitted), col = 'orange')

# metricas para avaliação das previsões do modelo

accuracy(dado_treino, exp(modelo_arima$fitted))


# previsão nos próximos 30 dias 

previsao_casos <- forecast(modelo_arima, h = 30)
previsao_casos <- exp(previsao_casos$mean)

# criando um data frame de datas para plotar um gráfico no ggplot2

dias <- c('01/05/2021', '02/05/2021', '03/05/2021', '04/05/2021', '05/05/2021', '06/05/2021', '07/05/2021','08/05/2021',
         '09/05/2021', '10/05/2021', '11/05/2021', '12/05/2021', '13/05/2021', '14/05/2021', '15/05/2021',
         '16/05/2021', '17/05/2021', '18/05/2021', '19/05/2021', '20/05/2021', '21/05/2021', '22/05/2021',
         '23/05/2021', '24/05/2021', '25/05/2021', '26/05/2021', '27/05/2021','28/05/2021', '29/05/2021', '30/05/2021')
dias <- as.Date(dias ,format = '%d/%m/%Y')

# criando um data frame com as previsões e as datas criadas(que corresponde as datas das previsões)

dados_grafico <- data.frame(previsao_casos = as.numeric(previsao_casos),
                       datas = dias)

ggplot(dados_grafico, aes(x = datas, y = previsao_casos)) +
  geom_line() +
  labs(y = 'Nº de casos confirmados previstos') +
  labs(x = 'Maio') +
  ggtitle('Números de casos de Covid-19 previstos para maio de 2021 em Porto Alegre-RS') +
  theme(plot.title = element_text((size = 17))) +
  theme_test() +
  theme(plot.title = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20), size = 12,  face = 'bold'),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))


# as previsões com a base de validação para analisar se ele preveu bem

plot(as.numeric(dado_validacao), type = 'l')
lines(as.numeric(previsao_casos), col =  'orange')

# as previsões com a base de validação para analisar se ele preveu bem


dados_previstos <- data.frame(dado_validacao, previsao_casos, dias)

ggplot(dados_previstos, aes(x = dias)) + 
  geom_line(aes(y = dado_validacao, colour = 'Dados validação')) +
  geom_line(aes(y = previsao_casos, 
                colour = "Dados previstos")) +
  labs(y = 'Nº de casos confirmados') +
  labs(x = 'Maio/2021') +
  ggtitle('Comparação entre os dados de validação e os dados previstos') +
  theme(plot.title = element_text((size = 17))) +
  
  theme_test() +
  theme(plot.title = element_text(margin = margin(t = 20, r = 20, b = 20, l = 20), size = 20,  face = 'bold'),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))+
  scale_colour_manual("", breaks = c("Dados validação", "Dados previstos"), 
                      values = c("black","orange")) +
  theme(legend.text=element_text(size=12),
        legend.justification=c(1,2),legend.position=c(1,1),legend.title=element_blank())
  

# Verificação da acurácia da previsão com a amostra teste
accuracy(dado_validacao, as.numeric(previsao_casos)) 


