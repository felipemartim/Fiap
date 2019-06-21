library(dplyr)
library(corrgram)
library('GGally')
library(plotly)
library(caret)
library(e1071)
library(lmtest)
library(DAAG)
library(rpart)
library(rpart.plot)
library(rattle)

setwd('~/workspace/fiap/wine')

wines <- read.csv2(file="BaseWine_Red_e_White.csv"
                  , header=TRUE
                  , sep=";"
                  , colClasses=c("integer", rep("numeric", 5), rep("numeric", 6), "integer", "factor"))

str(wines)
summary(wines)

sum(is.na(wines))

wines %>% rename(type = Vinho)-> wines_adjusted

par (mfrow=c(4,3))
hist(wines_adjusted$chlorides)
hist(wines_adjusted$sulphates)
hist(wines_adjusted$fixedacidity)
hist(wines_adjusted$freesulfurdioxide)
hist(wines_adjusted$alcohol)
hist(wines_adjusted$volatileacidity)
hist(wines_adjusted$totalsulfurdioxide)
hist(wines_adjusted$quality)
hist(wines_adjusted$citricacid)
hist(wines_adjusted$density)
hist(wines_adjusted$pH)
hist(wines_adjusted$residualsugar)

wines_adjusted %>% ggplot(aes(x = chlorides)) + 
  geom_histogram() + 
  scale_x_log10()

wines_adjusted %>% ggplot(aes(x = residualsugar)) + 
  geom_histogram() + 
  scale_x_log10()

wines_adjusted %>%
  ggplot(aes(quality, fill = quality)) +
  geom_bar(fill = "purple")

par (mfrow=c(1,1))
boxplot(wines_adjusted$quality ~ wines_adjusted$type, main='tipo')

par (mfrow=c(2,2))
boxplot(wines_adjusted$chlorides, main='chlorides')
boxplot(wines_adjusted$sulphates, main='sulphates')
boxplot(wines_adjusted$fixedacidity, main='fixedacidity')
boxplot(wines_adjusted$residualsugar, main='residualsugar')

wines_adjusted <- wines_adjusted %>% filter(chlorides < 0.6)
wines_adjusted <- wines_adjusted %>% filter(sulphates < 1.5)
wines_adjusted <- wines_adjusted %>% filter(residualsugar < 30)
wines_adjusted <- wines_adjusted %>% filter(fixedacidity < 15)

wines_adjusted %>% arrange(desc(citricacid)) %>% head(n = 10)

boxplot(wines_adjusted$freesulfurdioxide, main='freesulfurdioxide')
boxplot(wines_adjusted$alcohol, main='alcohol')
boxplot(wines_adjusted$volatileacidity, main='volatileacidity')
boxplot(wines_adjusted$totalsulfurdioxide, main='totalsulfurdioxide')

wines_adjusted <- wines_adjusted %>% filter(freesulfurdioxide < 150)
wines_adjusted <- wines_adjusted %>% filter(alcohol > 8)
wines_adjusted <- wines_adjusted %>% filter(volatileacidity < 1.5)

boxplot(wines_adjusted$quality, main='quality')
boxplot(wines_adjusted$citricacid, main='citricacid')
boxplot(wines_adjusted$density, main='density')
boxplot(wines_adjusted$pH, main='pH')

wines_adjusted <- wines_adjusted %>% filter(citricacid < 1.3)

par (mfrow=c(1,1))

wines_adjusted %>% 
  select(-type) %>% 
  cor %>% 
  corrgram(type = "cor", lower.panel = panel.shade, upper.panel = panel.pie)

wines_adjusted %>%
  select(-tipo) %>%
  ggcorr(method = c("pairwise","spearman"), label = FALSE, angle = -0, hjust = 0.2) +
  coord_flip()

wines_adjusted %>% ggpairs()

plotWithTrendLine <- function (data, x, y) {
  reds <- data %>% filter(type == 'RED')
  whites <- data %>% filter(type == 'WHITE')
  fitReds <- reds %>% lm(.[,y]~.[,x], data=.) 
  fitWhites <- whites %>% lm(.[,y]~.[,x], data=.) 
  p <- plot_ly(data = data, x = data[, x], y = data[, y], type="scatter", mode='markers', 
               color = ~type, colors = c('#FF9999', 'lightblue'), opacity = 0.8) %>%
    add_trace(x=reds[, x], y=predict(fitReds), mode = 'lines', line = list(color = '#FF9999'),
              showlegend = F, hoverinfo="none", inherit = FALSE) %>%
    add_trace(x=whites[, x], y=predict(fitWhites), mode = 'lines', line = list(color = 'lightblue'),
              showlegend = F, hoverinfo="none", inherit = FALSE) %>%
    layout(title = paste(y, 'vs', x, sep = ' '),
           xaxis = list(title = x, showline = T),
           yaxis = list(side = 'left', title = y))
  print(p)
}

plotWithTrendLine(wines_adjusted, 'density', 'alcohol')
plotWithTrendLine(wines_adjusted, 'density', 'fixedacidity')
plotWithTrendLine(wines_adjusted, 'volatileacidity', 'quality')
plotWithTrendLine(wines_adjusted, 'alcohol', 'quality')
plotWithTrendLine(wines_adjusted, 'residualsugar', 'alcohol')
plotWithTrendLine(wines_adjusted, 'density', 'chlorides')

wines_adjusted <- wines_adjusted %>% mutate(chlorides_log = log(chlorides))
wines_adjusted <- wines_adjusted %>% mutate(residualsugar_log = log(residualsugar))

# train e test sets

smp_size <- floor(0.8 * nrow(wines_adjusted))
set.seed(2020)
train_ind <- sample(seq_len(nrow(wines_adjusted)), size = smp_size)

train <- wines_adjusted[train_ind, ]
test <- wines_adjusted[-train_ind, ]

lm_01 <- lm(quality ~ fixedacidity + volatileacidity
               + citricacid + residualsugar_log + chlorides_log
               + freesulfurdioxide + totalsulfurdioxide
               + density + pH + sulphates + alcohol + type, data=train)
summary(lm_01)


lm_02 <- lm(quality ~ fixedacidity + volatileacidity
            + residualsugar_log
            + freesulfurdioxide + totalsulfurdioxide
            + density + pH + sulphates + alcohol + type, data=train)
summary(lm_02)

qualityPredict <- predict(lm_02, test, interval = "prediction", level = 0.95)

mse <- mean((test$quality  - qualityPredict[,1])^2)
sqrt(mse)

erro_usando_media <- mean((test$quality  - mean(test$quality))^2)
sqrt(erro_usando_media)

actuals_preds <- data.frame(cbind(actuals=test$quality, predicteds=qualityPredict[,1]))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

stepwise<-step(lm_02,direction="both")

stepwise
summary(stepwise)

# grafico residuo
rs <- resid(lm_02)
plot(predict(lm_02), rs, xlab = "Preditor linear", ylab = "Residuos")
abline(h = 0, lty = 2)

hist(rs)

#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal
qqnorm(residuals(lm_02), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(lm_02))

#Teste de shapiro
shapiro.test(sample(residuals(lm_02), size = 5000)) 

#k-fold cross validation
cvResults <- suppressWarnings(CVlm(data=wines_adjusted,
    form.lm = quality ~ volatileacidity + chlorides_log
    + totalsulfurdioxide
    + pH + sulphates
    + alcohol, m=5, dots=TRUE, seed=29, legend.pos="topleft",  printit=FALSE));
attr(cvResults, 'ms')

## Árvore de Regressão


tree <- rpart (quality ~ volatileacidity
               + residualsugar_log
               + freesulfurdioxide
               + density + sulphates + alcohol + type, data=train, 
               cp = 0.001, minsplit = 15, maxdepth=10)

rpart.plot(tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE,box.palette="Grays",
           snip=FALSE)

val_pred_tree <- predict(tree, test, interval = "prediction", level = 0.95) 

mse_tree <- mean((test$quality  - val_pred_tree)^2)
sqrt(mse_tree)

actuals_preds <- data.frame(cbind(actuals=test$quality, predicteds=val_pred_tree))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

rs <- val_pred_tree - test$quality 
plot(predict(tree, test), rs, xlab = "Com Árvore de Regressão", ylab = "Residuos")
abline(h = 0, lty = 2)

hist(rs)

rs <- val_pred_tree - test$quality 
#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal
qqnorm(rs, ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(rs)

hist(rs)

library(lattice)
library(latticeExtra)
library(asbio)
library(car)

measures <- function(x) {
  L <- list(npar = length(coef(x)),
            dfres = df.residual(x),
            nobs = length(fitted(x)),
            RMSE = summary(x)$sigma,
            R2 = summary(x)$r.squared,
            R2adj = summary(x)$adj.r.squared,
            PRESS = press(x),
            logLik = logLik(x),
            AIC = AIC(x),
            BIC = BIC(x))
  unlist(L)
}

modl <- list(m0 = lm_01, m1 = lm_02)
round(t(sapply(modl, measures)), 3)


## Árvore de Decisão

wines_adjusted$good <- as.factor(ifelse(wines_adjusted$quality >= 6,1,0))
summary(wines_adjusted$good)

rpart.model01 <- rpart (train$good ~ fixedacidity + volatileacidity
                        + citricacid + residualsugar_log + chlorides_log
                        + freesulfurdioxide + totalsulfurdioxide
                        + density + pH + sulphates + alcohol + type, 
                       data = train)

rpart.plot(rpart.model01, type=4, extra=104,
           fallen.leaves=FALSE,
           digits=2, varlen=-8, faclen=10,
           tweak=1,
           compress=TRUE)

summary(rpart.model01)
print(rpart.model01)

previsto.com.modelo <- predict(rpart.model01, train, type='class')

matriz.de.confusão <- table(train$good, previsto.com.modelo)
matriz.de.confusão

diagonal <- diag(matriz.de.confusão)
Acc <-  sum(diagonal)/sum(matriz.de.confusão)
Acc

previsto.valid <- predict(rpart.model01, test, type='class')

matriz.de.confusão<-table(test$good, previsto.valid)
matriz.de.confusão

diagonal <- diag(matriz.de.confusão)
Acc <-  sum(diagonal)/sum(matriz.de.confusão)
Acc

fancyRpartPlot(rpart.model01)

## Regressão Logística



