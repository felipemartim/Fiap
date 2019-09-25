install.packages("readxl")
library(readxl)
data <- as.data.frame(read_excel("dados_icms_pib.xlsx", sheet="Planilha1"))
head(data,25)


class(data$mês)
class(data$icms)

#Extraíndo o mês da observação
data$month=as.numeric(substr(data$mês, start=6, stop=8)) 


head(data)

#Estatísticas descritivas
summary(data)

#Estimando o modelo linear ICMS
model_icms_1=lm(icms~pib+ibcbr, data=data)
summary(model_icms_1)
anova(model_icms_1)



#Estimando o modelo linear IPVA
model_ipva_1=lm(ipva~pib+ibcbr, data=data)
summary(model_ipva_1)
anova(model_ipva_1)



#Plotar resíduos do ICMS e do IPVA
par(mfrow=c(1,2))
plot(data$icms-predict(model_icms_1), main="Resíduos", ylab="ICMS", col='blue')
abline(h=0)
plot(data$ipva-predict(model_ipva_1), main="Resíduos", ylab="IPVA", col='red')
abline(h=0)

#Exercicio: Calculando os logs e adicionando ao banco de dados:


head(data)

#Exercicio:Arrecadação de ICMS- modelo log log
#Estimando o modelo linear ICMS

#Inserir variável dependente defesada:

#Exercicio: Arrecadação de IPVA- modelo log log



#Exercicio: Plotar resíduos do ICMS e do IPVA, e ambos em log





#Teste de heterocedástica -ICMS
#Modelo em nível - Breusch Pagan
resid21=(model_icms_1$residuals)^2
testebp1=lm(resid21~data$pib+data$ibcbr)
summary(testebp1)

#Modelo em nível - White
sqpib=(data$pib)^2
sqibcbr=(data$ibcbr)^2
cruz=data$ibcbr*data$pib
testew=lm(resid21~data$pib+data$ibcbr+sqpib+sqibcbr+cruz)
summary(testew)
anova(testew)

#Exercício: Modelo em log - Breusch Pagan


#alternativa teste BP
install.packages("lmtest")
library(lmtest)
bptest(model_icms_1)
bptest(model_icms_2)

#Teste de heterocedástica -IPVA
#Exercício: Modelo em nível - Breusch Pagan


#Exercício: Modelo em nível - White



#Exercício: Modelo em log - Breusch Pagan
resid24=(model_ipva_2$residuals)^2
summary(lm(resid24~data$ln_pib+data$ln_ibcbr))


#Exercício: Modelo em log - White
lsqpib=(data$ln_pib)^2
lsqibcbr=(data$ln_ibcbr)^2
lcruz=data$ln_ibcbr*data$ln_pib
summary(lm(resid24~data$ln_pib+data$ln_ibcbr+lsqpib+lsqibcbr+lcruz))


#alternativa teste BP
install.packages("lmtest")
library(lmtest)
bptest(model_ipva_1)
bptest(model_ipva_2)

#MQG factíveis em R:
varfunc_ols <- lm(log((model_ipva_2$resid)^2) ~ ln_pib+ln_ibcbr, data = data)
data$varfunc <- exp(varfunc_ols$fitted.values)
ipva2_gls <- lm(ln_ipva ~ ln_pib+ln_ibcbr, weights = 1/sqrt(varfunc), data = data)
summary(model_ipva_2)
summary(ipva2_gls)
bptest(model_ipva_2)
bptest(ipva2_gls)

#Teste de Durbin Watson para correlação serial
dwtest(model_icms_1, order.by = NULL, alternative = c("greater", "two.sided", "less"),
       iterations = 15, exact = NULL, tol = 1e-10, data = data)

dwtest(model_icms_1)
dwtest(model_ipva_1)



#Criando dummies para cada mês
for (i in 1:12){
  data[[paste("m", i, sep = "_")]] <- as.numeric(data$month==i)}

head(data)


#Exercício: Arrecadação de ICMS com dummies


#Exercício: Arrecadação de IPVA com dummies



######################
