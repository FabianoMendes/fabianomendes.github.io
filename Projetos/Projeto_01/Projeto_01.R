# Projeto 01 Detecção de Fraudes no Tráfego de Cliques em Propagandas de Aplicações Mobile


# Setando o diretório de trabalho
setwd("D:/Estudos/DSA/Projetos/Projeto_01")

# Instalando e Carregando Pacotes
install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("caret")

library(data.table)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)

# Carregando dataset treino
df_train <- fread("dataset/train.csv")
head(df_train)

# Explorando os dados
str(df_train)
dim(df_train)

# Explorando variável target
unique(df_train$is_attributed)

sum(df_train$is_attributed == 0)
sum(df_train$is_attributed == 1)

# Coletando amostras para balanceamento dataset train
memory.limit (9999999999) # Aumentando o limite de memória para conseguir executar o sample

df_train_sample_0 <- df_train[df_train$is_attributed == 0]
df_train_sample_1 <- df_train[df_train$is_attributed == 1]

df_train_resample_0 <- sample_n(df_train_sample_0, size = 500000, replace = FALSE, set.seed(101))

df_train <- bind_rows(df_train_sample_1, df_train_resample_0)

# Gravando dataset balanceado
fwrite(df_train, file = "dataset/train_balanced.csv")

# Liberando espaço em memória
rm(df_train, df_train_sample_0, df_train_sample_1, df_train_resample_0)

#Carregando dataset
df_train_balanced <- fread("dataset/train_balanced.csv")
head(df_train_balanced)

# Explorando os dados
str(df_train_balanced)
dim(df_train_balanced)

# Verificando valores missing
sapply(df_train_balanced, function(x) sum(is.na(x)))

# Removendo coluna com valores missing
df_train_balanced$attributed_time = NULL

# Função Extração de data
extract_data <- function(df, path){
  df$date = as.Date(format(df$click_time, "%Y-%m-%d"))
  head(df)
  df$dayOfWeek = as.factor(format(df$date, "%u"))
  df$dayOfYear = as.numeric(format(df$date, "%j"))
  df$hour = as.numeric(format(df$click_time, "%H"))
  df$min = as.numeric(format(df$click_time, "%M"))
  df$sec = as.numeric(format(df$click_time, "%S"))
  df$click_time = NULL
  fwrite(df, file = path)
  str(df)
} 

# Definindo o path 
path_df_train_balanced <- "dataset/train_balanced_V2.csv"

# Extraindo as datas dos datasets 
extract_data(df_train_balanced, path_df_train_balanced)

# Liberando espaço em memória
rm(df_train_balanced, extract_data, path_df_train_balanced)

# Recarregando os datasets
df_train_balanced_V2 <- fread("dataset/train_balanced_V2.csv", colClasses = c(is_attributed="factor", dayOfWeek="factor"))
str(df_train_balanced_V2)

# Verificando valores únicos por variável
table(df_train_balanced_V2$ip)
table(df_train_balanced_V2$app)
table(df_train_balanced_V2$device)
table(df_train_balanced_V2$os)
table(df_train_balanced_V2$channel)


### Verificando Frequência por ip ###
porIp <- df_train_balanced_V2 %>%
  select(ip) %>%
  group_by(ip) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq))
  
View(head(porIp, 20))

grafico_ip <- ggplot(head(porIp, 20), aes(x= reorder(as.character(ip), -absFreq), y=absFreq)) + 
  ggtitle("Suspeita de Fraude") + xlab("Ip") + ylab("Frequência") + theme_minimal() +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') +
  geom_text(aes(label = absFreq), vjust = 1)
grafico_ip

### Verificando Frequência por app ###
porApp <- df_train_balanced_V2 %>%
  select(app) %>%
  group_by(app) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq))

View(head(porApp, 20))

grafico_app <- ggplot(head(porApp, 20), aes(x= reorder(as.character(app), -absFreq), y=absFreq)) + 
  ggtitle("Suspeita de Fraude") + xlab("App") + ylab("Frequência") + theme_minimal() +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') + scale_y_continuous(limits = c(0, 150000)) +
  geom_text(aes(label = absFreq), vjust = 1)
grafico_app

### Verificando Frequência por device ###
porDevice <- df_train_balanced_V2 %>%
  select(device) %>%
  group_by(device) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq))

View(head(porDevice, 20))

grafico_device <- ggplot(head(porDevice, 20), aes(x= reorder(as.character(device), -absFreq), y=absFreq)) + 
  ggtitle("Suspeita de Fraude") + xlab("Device") + ylab("Frequência") + theme_minimal() +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') + scale_y_continuous(limits = c(0, 1000000)) +
  geom_text(aes(label = absFreq), vjust = 1)
grafico_device

### Verificando Frequência por os ###
porOs <- df_train_balanced_V2 %>%
  select(os) %>%
  group_by(os) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq))

View(head(porOs, 20))

grafico_os <- ggplot(head(porOs, 20), aes(x= reorder(as.character(os), -absFreq), y=absFreq)) + 
  ggtitle("Suspeita de Fraude") + xlab("Os") + ylab("Frequência") + theme_minimal() +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') +
  geom_text(aes(label = absFreq), vjust = 1)
grafico_os

### Verificando Frequência por channel ###
porChannel <- df_train_balanced_V2 %>%
  select(channel) %>%
  group_by(channel) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq))

View(head(porChannel, 20))

grafico_channel <- ggplot(head(porChannel, 20), aes(x= reorder(as.character(channel), -absFreq), y=absFreq)) + 
  ggtitle("Suspeita de Fraude") + xlab("Channel") + ylab("Frequência") + theme_minimal() +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') +
  geom_text(aes(label = absFreq), vjust = 1)
grafico_channel

# Exibindo a distribuição da váriável target
grafico_target <- ggplot(df_train_balanced_V2, aes(x=is_attributed)) + ggtitle("Suspeita de Fraude") + xlab("Fraude") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentual") + coord_flip() + theme_minimal()
grafico_target

# Dividindo os datasets em treino e teste
intrain <- createDataPartition(df_train_balanced_V2$is_attributed,p=0.7,list=FALSE)
set.seed(2017)
training <- df_train_balanced_V2[intrain,]
testing <- df_train_balanced_V2[-intrain,]

# Confirmando se a divisão está correta
dim(training); dim(testing)
View(training)
str(training)




##### Regressão Logística #####
LogModel <- glm(is_attributed ~ ., family=binomial(link="logit"), data=training)
print(summary(LogModel))

# Análise de Variância - ANOVA
anova(LogModel, test="Chisq")

# Avaliando a performance do Modelo
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$is_attributed)
print(paste('Logistic Regression Accuracy',1-misClasificError))

# Confusion Matrix Regressão Logística
print("Confusion Matrix Para Logistic Regression"); table(testing$is_attributed, fitted.results > 0.5)




##### Random Forest #####
modelo <- randomForest(is_attributed ~ ., data = training)
print(modelo)
plot(modelo)

# Recursos mais importantes
varImpPlot(modelo, sort=T, n.var = 10, main = 'Top 10 Feature Importance')

# Prevendo valores com dados de teste
pred <- predict(modelo, testing)

# Confusion Matrix Random Forest
print("Confusion Matrix Para Random Forest"); table(testing$is_attributed, pred)
