#Parte I. 
#Carregando os dados a partir de um arquivo csv.
data <- as.data.frame(read.csv("Censo.csv"))
head(data)


#Selecionando variáveis e dando nomes às colunas
censo <- cbind(data$Mortalidade.Infantil,data$PIB,data$Popula�ao,data$Analfabetismo, data$Urbanizacao)
colnames(censo) <- c("MF","PIB","POP","ANALF","URB")

#Transfomando em banco de dados:
censo <- as.data.frame(censo)
head(censo)

#Calculando e adicionando o PIB per capita no banco de dados:
censo <- cbind(censo, PIBPC = censo$PIB/censo$POP)
head(censo)


#Estatísticas descritivas:
summary(censo)


#Estimando o modelo linear: analfabetismo explica a mortalidade infantil?
model1=lm(censo$MF~censo$ANALF)
summary(model1)
anova(model1)

#Gráfico entre Mortalidade Infantil e cada uma das variáveis:
par(mfrow=c(2,2))
plot(censo$ANALF, censo$MF, xlab="Analfabetismo",main="Mortalidade Infantil", col='red')
plot(censo$PIBPC, censo$MF, xlab="PIB per capita",main="Mortalidade Infantil", col='blue')
plot(censo$POP, censo$MF, xlab="População",main="Mortalidade Infantil", col='green')
plot(censo$URB, censo$MF, xlab="Urbanização",main="Mortalidade Infantil", col='black')

dev.off()
plot(censo$ANALF, censo$MF, xlab="Analfabetismo",main="Mortalidade Infantil", col='red')

#Estimando o modelo linear: inclusão de variáveis adicionais?
model2=lm(censo$MF~censo$PIBPC+censo$POP+censo$ANALF+censo$URB)
summary(model2)
#Comparando com modelo 1:
summary(model1)




#Gráfico
Y=censo$MF
Y_hat=predict(model2)
par(mfrow=c(1,1))
plot(Y, col='red',main="Y and Y_hat", type='l')
lines(Y_hat, col = "blue", type='o')
legend("topleft",legend=c('Y','Y_hat'), col=c('red','blue'), lty=1, cex=0.8)


dev.off()



#Plot resíduos
par(mfrow=c(1,1))
plot(censo$MF-Y_hat, xlab="Residuos", col='red')
abline(h=0)


#Exercício 1: Adicionar ao banco de dados as variáveis em log e refazer o modelo

#Calculando os logs e criando banco de dados somente com logs.
install.packages("dplyr")
library(dplyr)

censo <- censo %>% mutate(LMF = log(MF),
                           LPIB = log(PIB),
                           LPOP = log(POP),
                           LANALF = log(ANALF),
                           LURB = log(URB),
                           LPIBPC = log(PIBPC)
                          )

head(censo)

model_log <- lm(LMF ~ LANALF + LPIBPC + LPOP, data = censo)
summary(model_log)

par(mfrow=c(2,2))
plot(censo$LANALF, censo$MF, xlab="Analfabetismo",main="Mortalidade Infantil", col='red')
plot(censo$LPIBPC, censo$MF, xlab="PIB per capita",main="Mortalidade Infantil", col='blue')
plot(censo$LPOP, censo$MF, xlab="População",main="Mortalidade Infantil", col='green')
plot(censo$LURB, censo$MF, xlab="Urbanização",main="Mortalidade Infantil", col='black')


#Estimando o modelo loglinear:
#Estimando o modelo linear (excluir log da urbanização, devido a missing)



#Parte II

#Inspeção de colinearidade
cor(censo, method="pearson")
cor(censo, method="spearman")

#Teste de heterocedástica Breusch-Pagan:
resid2=(Y-predict(model2))^2
testebp=lm(resid2~censo$PIBPC+censo$POP+censo$ANALF+censo$URB)
summary(testebp)

#Teste de heterocedástica White:
predic2=(predict(model2))^2
testew=lm(resid2~predict(model2)+predic2)
summary(testew)
anova(testew)



#Teste de especificação:
#model1: modelo restrito (apenas com analfabetismo)
#model2: modelo irrestrito (incluíndo os controles)
estat_F=(sum(model1$residuals^2-model2$residuals^2))/(sum(model2$residuals^2)/(length(model2$residuals)-4-1))
#p_valor
1-pf(estat_F,1,(length(model2$residuals)-4-1))


#####################

