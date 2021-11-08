

#                   Problemas

# Aumento no número de usuários que desistiram do atendimento devido ao tempo de espera na fila

# Queda da qualidade de atendimento que é medido através de NPS (Net Promoter Score)

# Tempo alto para conclusão de um ticket (uma requisição aberta por um usuário para receber atendimento)

# Baixa efetividade no uso da página de FAQ do site (perguntas frequentes)


      
#                   Análise dos dados  



# Carregando os pacotes necessários


# install.packages('plotly')
# install.packages('tidyverse')

library(plotly)
library(tidyverse)

# Carregando os dados 

setwd('C:\\Users\\Usuário\\Desktop\\CURSOS\\Data_Science_2020')

dados <- read.csv('arquivo.csv', fileEncoding = 'utf-8')

View(dados)

# Resumo dos dados

head(dados)

tail(dados)

dim(dados)

# Colunas

colnames(dados)
names(dados) <- c('ID_Cliente', 'Atendente', 'Data', 'Mensagem', 
                  'Tempo_Conclusao_Atendimento_minutos', 'Clientes_Desistiu', 'NPS')
colnames(dados)


summary(dados)

# Conferindo e contando os NAs

sum(is.na(dados))

# O dataset apresenta apenas uma linha(a última) com valores faltantes. 
# Removendo a última linha

dados <- na.omit(dados)
sum(is.na(dados))


# Tranformação dos tipos dos dados

str(dados)

dados$ID_Cliente <- as.character(dados$ID_Cliente)

dados$Data <- as.Date(dados$Data,format= ('%d/%m/%Y')) 


# Clientes que desistiram

ClientesDesistiram <- filter(dados,Clientes_Desistiu == 'Sim')

nrow(ClientesDesistiram)

print(ClientesDesistiram)


# Gráfico Histograma - Clientes que desistiram 

hist_des <- plot_ly(x = dados$Clientes_Desistiu, type = 'histogram',color = 'slategrey') %>%
  layout(title = 'Desistência dos Clientes',
         barmode="stack",bargap=0.1)
hist_des


# Verificando se há registros vazios na coluna Tempo de Conclusão de Atendimento por Minuto

is.null(dados$Tempo_Conclusao_Atendimento_minutos)



# Quantidade de atendimento por atendente - Atendente

freq <- dados %>%
  count(Atendente, sort = TRUE)

names(freq) <- c('Atendente', 'Count')
freq

# Gráfico de barra

bar <- plot_ly(data = freq,
            x = ~Atendente,
            y = ~Count,
            type = 'bar',
            color = 'purple2') %>%
  layout(title = 'Frequência dos Atendentes')

bar


# NPS

# Resumo estatístico NPS

summary(dados$NPS)

moda <- function(arg) {
  valor_unico = unique(arg)
  valor_unico[which.max(tabulate(match(arg, valor_unico)))]
}

moda(dados$NPS)

teste <- dados %>% 
  count(NPS)

names(teste) <- c('nps', 'Count')
teste

# Gráfico Histograma

hist_nps <- plot_ly(x=dados$NPS,type = "histogram",color = 'slategrey') %>% 
  layout(barmode="stack",bargap=0.1,
         title = 'Variação da Avaliação do Net Promoter Score',
         xaxis = list(title = "NPS"))

hist_nps

teste$percentual <- teste$Count/sum(teste$Count)
teste$percentual_acumulado <- cumsum(teste$percentual)
teste

# A concentração do NPS aponta que a avaliação dos atendimentos não é tão ruim, 
# embora haja espaço para melhorias, especialmente levando-se em consideração que o número de NPS igual a 10 é muito baixo, o que indica que raramente um cliente está plenamente satisfeito.



# Média de Tempo por Atendente

media_tempo <- subset(dados, select =c(Atendente, Tempo_Conclusao_Atendimento_minutos))
print(media_tempo)


media_tempo <- dados %>%
  group_by(Atendente)%>%
  summarise(media_tempo = mean(Tempo_Conclusao_Atendimento_minutos)) %>%
  arrange(Atendente, media_tempo)

print(media_tempo)


# Média de tempo por NPS

media_tempo_nps <- subset(dados, select = c(Atendente, NPS))
print(media_tempo_nps)

media_tempo_nps <- dados %>%
  group_by(Atendente)%>%
  summarise(media_tempo = mean(NPS)) %>%
  arrange(Atendente, media_tempo)

print(media_tempo_nps)



# Frequência das perguntas feitas


mensagem <- dados %>%
  count(Mensagem, sort = TRUE)
  
print(mensagem)


# Demora das mensagens 


demora_meng <- subset(dados , select = c(Mensagem, Tempo_Conclusao_Atendimento_minutos))
demora_meng <- arrange(demora_meng, desc(Tempo_Conclusao_Atendimento_minutos))
print(demora_meng)

# Salvando os dados

write.csv(dados, file = 'arquivo_pronto.csv' , row.names = FALSE,fileEncoding = 'utf-8' )
