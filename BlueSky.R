#BlueSky
library(readxl)
library(tidyverse)


#Estudo de Caso BlueSky

dados <- read_excel("C:/Users/lucas/Desktop/Analise de Dados/DecisaoRisco/BlueSky_Single-Leg_demand.xls",
                    range = "A6:B371", col_types = c("date","numeric"))

colnames(dados)[2] <- "demand"
colnames(dados)[1] <- "date"

#Plots
g <- ggplot(dados, aes(x=date,y=demand)) +
  geom_point() +
  geom_line()
g

#Valor Crítico

tarifa.full <- 174
tarifa.low <- 114

valor.critico <- (tarifa.full-tarifa.low)/tarifa.full


# Modelo 1 - Normal ####

#Vamos considerar que dados seguem uma distribuição normal
#sem discrimar o dia da semana

media_1 <- mean(dados$demand)

m_int <-round(media_1,0) #o inteiro mais próximo seguindo arrendondamento

g <- g + geom_hline(yintercept =  media_1, color="blue",
                    linetype = "longdash", size = 1)
g

# sd e RMSE (considerando o valor fracionário)
rmse_1 <- sd(dados$demand)

protection_Level_1 <- qnorm(p=valor.critico,mean = media_1, sd = rmse_1 )

protection_Level_1 <- round(protection_Level_1,0)
#isso é o resultado vindo do calculo de Z


# Modelo 2 - Empírico ####

#Vamos considerar que dados seguem uma distribuição empírica
#sem discrimar o dia da semana

q.model2 <- quantile(dados$demand, c(valor.critico,0.5)) 

q.model2

protection_Level_2 <-  q.model2[[1]]

mediana_2 <-  q.model2[[2]]

g <- g + geom_hline(yintercept =  mediana_2, color="red",
                    linetype = "longdash", size = 1)
g

# RMSE

rmse_2 <- sqrt(mean((dados$demand - mediana_2)^2))

# Modelo 3 - split por dias da semana (normal)

dados$day <- weekdays(dados$date)

#Primeira maneira de fazer (para cada dia ...)
dados_domingo <- dados %>%
  filter(day == "domingo") 

mean_domingo <- mean(dados_domingo$demand)

sd_domingo   <- sd(dados_domingo$demand)

#ou 

mean.days.normal <- dados %>%
  group_by(day) %>%
  summarise(media.N = mean(demand),
            sd.N= sd(demand),
            prot.Level.3 = round(qnorm(p=valor.critico,mean = media.N, sd = sd.N)))

#Mean.days.normal é uma tabela resumo sem conexão com "dados"
#Poderia ter aproximando prot.Level.3 para inteiro em summarize()

#Usando mutate (anexa as variáveis criadas aos dados):
mean.days.normal2 <- dados %>%
  group_by(day) %>%
  mutate(media.N = mean(demand),
         sd.N= sd(demand),
         prot.Level.3 = round(qnorm(p=valor.critico,mean = media.N, sd = sd.N),0)
  )


# RMSE modelo 3

rmse_3 <- sqrt(mean((mean.days.normal2$demand - mean.days.normal2$media.N)^2))


# Modelo 4 # Empírica por dia)

#Usando mutate (anexa as variáveis criadas aos dados):
median.days.empirica <- dados %>%
  group_by(day) %>%
  mutate(mediana = median(demand),
         prot.Level.4 = as.integer(quantile(demand,valor.critico))
  )

rmse_4 <- sqrt(mean((median.days.empirica$demand - median.days.empirica$mediana)^2))

# Modelo 5 - considerando mês REGRESSÃO

dados$month <- months(dados$date)

# Para regressão: FATORES

dados$day <- factor(dados$day)

dados$month <- factor(dados$month)

summary(dados$day)
is.factor(dados$day)
levels(dados$day)
str(dados)
#Regressão

dados.fit1 <- lm(data=dados, demand ~ day + month)

anova(dados.fit1)
summary(dados.fit)


# Incluindo um termo de interação dia-mês 
dados.fit2 <- lm(data=dados, demand ~ day + month + day:month)
anova(dados.fit2)
#summary(dados.fit)

#ou formula: demand ~ day*month gera a mesma formula

dados.fit3 <- lm(data=dados, demand ~ day*month)
anova(dados.fit3)
summary(dados.fit)

#Plot residuals

plot(dados.fit)


#plotando os valores previstos

prediction <- predict(dados.fit2,newdata = dados )

g <- ggplot(dados, aes(x=date,y=demand)) +
  geom_point() +
  geom_line() 
g
#use fitted values or prediction
k <- g + geom_point(aes(x=dados$date,y=dados.fit1$fitted.values), 
               color = "yellow") #AMARELO PREVISÃO 1


k + geom_point(aes(x=dados$date,y=prediction), 
              color = "blue")

#criando uma coluna com "feriados", ou seja, altas demandas:
i=0
help("if")
for(j in 1:365) i=dados$demand
for(j in 1:365) f[j]="nao"
for(j in 1:365) if(i[j]>125) f[j]="sim"
dados$feriados <- f
dados$feriados
dados$feriados <- factor(dados$feriados)
str(dados)

dados.fit3 <- lm(data=dados, demand ~ day + month + day:month + feriados)
anova(dados.fit3)
summary(dados.fit3)

kk <- k + geom_point(aes(x=dados$date,y=dados.fit3$fitted.values), 
                    color = "red") #VERMELHO PREVISÃO 3
l <- kk + geom_point(aes(x=dados$date,y=prediction), 
                color = "blue") #AZUL PREVISÃO 2

dados.fit4 <- lm(data=dados, demand ~ day + month + day:month + feriados + feriados:day)
anova(dados.fit4)
summary(dados.fit4)
kkk <- l + geom_point(aes(x=dados$date,y=dados.fit4$fitted.values), 
                     color = "green") #VERDE PREVISÃO 4
kkk


#e se pensarmos em Ferias?
h=0

for(j in 1:365) h[j]="0"
for(j in 1:365) if(dados$month[j] == "dezembro" | dados$month[j] == "janeiro") h[j]="1"
warnings()
h

dados$ferias <- h
dados$ferias
dados$ferias <- factor(dados$ferias)
str(dados)

dados.fit5 <- lm(data=dados, demand ~ ferias + day + month + feriados +day:month + feriados:day)
anova(dados.fit5)
summary(dados.fit5)
#não apresenta nenhuma vantagem, já que os meses já foram contabilizados anteriormente.
#Adicionar feriados e a contagem feriados:days levou à melhor regressão até o momento.


# O que vc acha?
# Como podemos minimizar os residous para valores grandes de demanda?
# Talvez criando mais uma variável para os dias de feriado? Mas como saber quando é feriado?
# Mais alguma sugestão?