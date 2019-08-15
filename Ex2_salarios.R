dados <- read.csv("Salarios.csv")
head(dados)
dados <-as.data.frame(dados[,-9])
head(dados)

summary(dados)



#Fazendo a regressão salário contra educação
summary(lm(salario~educacao, data=dados))
anova(lm(salario~educacao, data=dados))

#Fazendo o gráfico educação contra salário e plotando a reta de regressão
par (mfrow=c(1,1))
plot(dados$educacao, dados$salario, xlab = "Anos de Estudo",ylab="Salário/hora")
abline((lm(salario~educacao, data=dados))$coef,col="red")

#Subdividindo banco de dados e colocando no gráfico:
homens <- subset.data.frame(dados,dados$masculino==1)
mulheres <- subset.data.frame(dados,dados$masculino==0)

par(mfrow=c(1,2))
plot(homens$educacao, homens$salario, xlab = "Anos de Estudo",
     ylab="Salário/hora", main="Homens", ylim = c(0,10))
abline((lm(homens$salario~homens$educacao))$coef,col="blue")

plot(mulheres$educacao, mulheres$salario, xlab = "Anos de Estudo",
     ylab="Salário/hora", main="Mulheres", ylim=c(0,10))
abline((lm(mulheres$salario~mulheres$educacao))$coef,col="red")

par(mfrow=c(1,1))
plot(dados$educacao, dados$salario, xlab = "Anos de Estudo",
     ylab="Salário (R$/hora)", main="Salários e Educação", ylim = c(0,15), col="blue")
abline((lm(salario~educacao, data=dados))$coef,col="black", cex = 1)

mean(dados$salario)

"Regressão Múltipla"
reg_mult=lm(salario~educacao+feminino+experiencia+naobranco+casado+dependente, data=dados)
summary(reg_mult)
anova(reg_mult)


#Gráfico dos previstos e estimados
#Estimados
dev.off()

par(mfrow=c(1,1))
plot(dados$salario,predict(reg_mult), col=c('red','blue'))
legend("topleft", legend = c("Y", "Y_hat"))


#Inspeção de colinearidade
cor(dados, method = c("pearson", "kendall", "spearman"))

#Criando dummies para quem tem curso superior
dados$superior=as.numeric(dados$educacao>15)
head(dados)

#Refazendo a regressão para inclusão de dummies para curso superior
summary(lm(salario~educacao+feminino+experiencia+naobranco+casado+dependente+superior, data=dados))

#Incluindo mulheres casadas:
summary(lm(salario~educacao+feminino+experiencia+naobranco+casado+dependente+superior+feminino*casado, data=dados))


#Teste de especificação
#inclusão da variável experiência ao quadrado:
exp2=dados$experiencia^2
model_u <- lm(salario~educacao+feminino+experiencia+exp2+naobranco+casado+dependente+superior, data=dados)
model_r <- lm(salario~educacao+feminino+experiencia+naobranco+casado+dependente+superior, data=dados)
summary(model_u)
summary(model_r)
estat_F=(sum(model_r$residuals^2-model_u$residuals^2))/(sum(model_u$residuals^2)/(length(model_u$residuals)-7-1-1))
#p_valor
1-pf(estat_F,1,(length(model_u$residuals)-7-1-1))
# exp2 � significativo


#inclusão da variável dependente
model_u <- lm(salario~educacao+feminino+experiencia+exp2+naobranco+casado+dependente+superior, data=dados)
model_r <- lm(salario~educacao+feminino+experiencia+exp2+naobranco+casado+superior, data=dados)
summary(model_u)
summary(model_r)
estat_F=(sum(model_r$residuals^2-model_u$residuals^2))/(sum(model_u$residuals^2)/(length(model_u$residuals)-7-1-1))
#p_valor
1-pf(estat_F,1,(length(model_u$residuals)-7-1-1))



#Teste de heterocedástica:
install.packages("lmtest")
library(lmtest)
bptest(model_u)

#Tirando o log:
model_log <- lm(log(1+salario)~log(1+educacao)+feminino+experiencia+exp2+naobranco+casado+superior, data=dados)
summary(model_log)
bptest(model_log)

#Mínimos Quadrados Generalizados factiveis:
#MQG heterocedástica em R:
varfunc.ols <- lm(log(model_log$resid^2) ~ log(1+educacao)+feminino+experiencia+exp2+naobranco+casado+superior, data = dados)
dados$varfunc <- exp(varfunc.ols$fitted.values)
model_log_gls <- lm(log(1+salario)~log(1+educacao)+feminino+experiencia+exp2+naobranco+casado+superior, weights = 1/varfunc, data = dados)
summary (model_log)
summary(model_log_gls)
######################


