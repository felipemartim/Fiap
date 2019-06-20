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

wines_adjusted %>% filter(type=="WHITE") -> whites
wines_adjusted %>% filter(type=="RED") -> reds

smp_size <- floor(0.8 * nrow(reds))
set.seed(2020)
train_ind <- sample(seq_len(nrow(reds)), size = smp_size)

trainReds <- reds[train_ind, ]
testReds <- reds[-train_ind, ]

lm_reds_01 <- lm(quality ~ fixedacidity + volatileacidity
               + citricacid + residualsugar_log + chlorides_log
               + freesulfurdioxide + totalsulfurdioxide
               + density + pH + sulphates + alcohol, data=trainReds)
summary(lm_reds_01)


lm_reds_02 <- lm(quality ~ volatileacidity
               + chlorides_log
               + totalsulfurdioxide
               + pH + sulphates
               + alcohol, data=trainReds)
summary(lm_reds_02)

qualityPredict <- predict(lm_reds_02, testReds, interval = "prediction", level = 0.95)

mse <- mean((testReds$quality  - qualityPredict[,1])^2)
sqrt(mse)

erro_usando_media <- mean((testReds$quality  - mean(testReds$quality))^2)
sqrt(erro_usando_media)

actuals_preds <- data.frame(cbind(actuals=testReds$quality, predicteds=qualityPredict[,1]))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

stepwise<-step(lm_reds_02,direction="both")

stepwise
summary(stepwise)

# grafico residuo
rs <- resid(lm_reds_02)
plot(predict(lm_reds_02), rs, xlab = "Preditor linear", ylab = "Residuos")
abline(h = 0, lty = 2)

hist(rs)

#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal
qqnorm(residuals(lm_reds_02), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(lm_reds_02))

#Teste de shapiro
shapiro.test(residuals(lm_reds_02))

#k-fold cross validation
cvResults <- suppressWarnings(CVlm(data=reds,
    form.lm = quality ~ volatileacidity + chlorides_log
    + totalsulfurdioxide
    + pH + sulphates
    + alcohol, m=5, dots=TRUE, seed=29, legend.pos="topleft",  printit=FALSE));
attr(cvResults, 'ms')


#whites

smp_size <- floor(0.8 * nrow(whites))
set.seed(2020)
train_ind <- sample(seq_len(nrow(whites)), size = smp_size)

trainWhites <- whites[train_ind, ]
testWhites <- whites[-train_ind, ]

lm_whites_01 <- lm(quality ~ fixedacidity + volatileacidity
               + citricacid + residualsugar_log + chlorides_log
               + freesulfurdioxide + totalsulfurdioxide
               + density + pH + sulphates + alcohol, data=trainWhites)
summary(lm_whites_01)

lm_whites_02 <- lm(quality ~ volatileacidity
               + chlorides_log
               + freesulfurdioxide + residualsugar_log +
               + pH + sulphates + density
               + alcohol, data=trainWhites)
summary(lm_whites_02)

qualityPredict <- predict(lm_whites_02, testWhites, interval = "prediction", level = 0.95)

mse <- mean((testWhites$quality  - qualityPredict[,1])^2)
sqrt(mse)

erro_usando_media <- mean((testWhites$quality  - mean(testWhites$quality))^2)
sqrt(erro_usando_media)

actuals_preds <- data.frame(cbind(actuals=testWhites$quality, predicteds=qualityPredict[,1]))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape

stepwise<-step(lm_whites_02,direction="both")

stepwise
summary(stepwise)

# grafico residuo
rs <- resid(lm_whites_02)
plot(predict(lm_whites_02), rs, xlab = "Preditor linear", ylab = "Residuos")
abline(h = 0, lty = 2)

hist(rs)

#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal
qqnorm(residuals(lm_whites_02), ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(residuals(lm_whites_02))

hist(residuals(lm_whites_02))

#Teste de shapiro
shapiro.test(residuals(lm_whites_02))

#k-fold cross validation
cvResults <- suppressWarnings(CVlm(data=whites,
                                   form.lm = quality ~ volatileacidity
                                   + chlorides_log
                                   + freesulfurdioxide + residualsugar_log +
                                   + pH + sulphates + density
                                   + alcohol, m=5, dots=TRUE, seed=29, legend.pos="topleft",  printit=FALSE));
attr(cvResults, 'ms')


## Árvore de Regressão

# Reds

reds_tree <- rpart (quality ~ volatileacidity
                              + chlorides_log
                              + freesulfurdioxide + residualsugar_log +
                                + pH + sulphates + density
                              + alcohol, data=trainReds, 
                              cp = 0.001, minsplit = 15, maxdepth=10)

rpart.plot(reds_tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE,box.palette="Grays",
           snip=FALSE)

reds_val_pred_tree <- predict(reds_tree, testReds, interval = "prediction", level = 0.95) 
str(reds_val_pred_tree)

mse_tree <- mean((testReds$quality  - reds_val_pred_tree)^2)
sqrt(mse_tree)

rs <- reds_val_pred_tree - testReds$quality 
plot(predict(reds_tree, testReds), rs, xlab = "Com Árvore de Regressão", ylab = "Residuos")
abline(h = 0, lty = 2)
hist(rs)

rs <- reds_val_pred_tree - testReds$quality 
#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal
qqnorm(rs, ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(rs)

hist(rs)

#All

smp_size <- floor(0.8 * nrow(wines_adjusted))
set.seed(2020)
train_ind <- sample(seq_len(nrow(wines_adjusted)), size = smp_size)

train <- wines_adjusted[train_ind, ]
test <- wines_adjusted[-train_ind, ]

tree <- rpart (quality ~ volatileacidity
                    + chlorides_log
                    + freesulfurdioxide + residualsugar_log +
                    + pH + sulphates + density
                    + alcohol, data=train, 
                    cp = 0.001, minsplit = 15, maxdepth=10)

rpart.plot(tree, type=4, extra=1, under=FALSE, clip.right.labs=TRUE,
           fallen.leaves=FALSE,   digits=2, varlen=-10, faclen=20,
           cex=0.4, tweak=1.7,
           compress=TRUE,box.palette="Grays",
           snip=FALSE)

val_pred_tree <- predict(tree, test, interval = "prediction", level = 0.95) 

mse_tree <- mean((test$quality  - val_pred_tree)^2)
sqrt(mse_tree)

rs <- val_pred_tree - test$quality 
plot(predict(tree, test), rs, xlab = "Com Árvore de Regressão", ylab = "Residuos")
abline(h = 0, lty = 2)

hist(rs)

rs <- val_pred_tree - test$quality 
#observa-se SE violação da suposição de que os erros aleatórios têm distribuição Normal
qqnorm(rs, ylab="Resíduos",xlab="Quantis teóricos",main="")
qqline(rs)

hist(rs)
