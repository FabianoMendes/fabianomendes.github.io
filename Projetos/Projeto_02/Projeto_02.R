# Projeto 02 Prevendo Demanda de Estoque com Base em Vendas

# Setando o diretório de trabalho
setwd("D:/Estudos/DSA/Projetos/Projeto_02")

# Instalando e Carregando Pacotes

#install.packages("data.table")
#install.packages("dplyr")
#install.packages("caret")
#install.packages("rpart")
#install.packages("ggplot2")

library(data.table)
library(dplyr)
library(caret)
library(rpart)
library(ggplot2)

# Carregando datasets
df_cliente <- fread("dataset/cliente_tabla.csv")
View(df_cliente)

df_produto <- fread("dataset/producto_tabla.csv")
View(df_produto)

df_town_state <- fread("dataset/town_state.csv")
View(df_town_state)

df_test <- fread("dataset/test.csv")
View(df_test)

df_sample <- fread("dataset/sample_submission.csv")
View(df_sample)

df_train <- fread("dataset/train.csv")
View(df_train)

# Visualizando as variáveis
str(df_cliente)
dim(df_cliente)

str(df_produto)
dim(df_produto)

str(df_town_state)
dim(df_town_state)

str(df_test)
dim(df_test)

str(df_sample)
dim(df_sample)

str(df_train)
dim(df_train)

# Verificando valores missing
sapply(df_cliente, function(x) sum(is.na(x)))
sapply(df_produto, function(x) sum(is.na(x)))
sapply(df_town_state, function(x) sum(is.na(x)))
sapply(df_test, function(x) sum(is.na(x)))
sapply(df_sample, function(x) sum(is.na(x)))
sapply(df_train, function(x) sum(is.na(x)))

# Verificando valores únicos por variável
sapply(df_cliente, function(x) length(unique(x)))
table(duplicated(df_cliente))

sapply(df_produto, function(x) length(unique(x)))
table(duplicated(df_produto))

sapply(df_town_state, function(x) length(unique(x)))
table(duplicated(df_town_state))

sapply(df_test, function(x) length(unique(x)))
table(duplicated(df_test))

sapply(df_train, function(x) length(unique(x)))
table(duplicated(df_train))


### Verificando Frequência por Cliente ###
porCliente <- df_train %>%
  select(Cliente_ID) %>%
  group_by(Cliente_ID) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq)) %>%
  inner_join(df_cliente, by = "Cliente_ID")

head(porCliente, 10)

ggplot(head(porCliente, 10), aes(x= reorder(NombreCliente, -absFreq), y=absFreq)) +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') +
  labs(title = "Por Clientes",
       x = "Nome Cliente",
       y = "Frequência") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Verificando Frequência por Agência ###
porAgencia <- df_train %>%
  select(Agencia_ID) %>%
  group_by(Agencia_ID) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq)) %>%
  inner_join(df_town_state, by = "Agencia_ID")

head(porAgencia, 10)

ggplot(head(porAgencia, 10), aes(x= reorder(Town, -absFreq), y=absFreq)) +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') +
  scale_y_continuous(limits = c(0,900000)) +
  labs(title = "Por Agencia",
       x = "Nome Vendedor",
       y = "Frequência") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Vericicando Frequência por Canal ###
porCanal <- df_train %>%
  select(Canal_ID) %>%
  group_by(Canal_ID) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq))

head(porCanal)

ggplot(porCanal, aes(x= reorder(Canal_ID, -absFreq), y=absFreq)) +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') +
  labs(title = "Por Canal",
       x = "Canal",
       y = "Frequência")


### Verificando Frequência por Produto ###
porProduto <- df_train %>%
  select(Producto_ID) %>%
  group_by(Producto_ID) %>%
  summarise(absFreq = n()) %>%
  arrange(desc(absFreq)) %>%
  inner_join(df_produto, by = "Producto_ID")

head(porProduto, 10)

ggplot(head(porProduto, 20), aes(x= reorder(NombreProducto, -absFreq), y=absFreq)) +
  geom_bar(stat = 'identity', alpha = 0.75, fill = '#086788') +
  labs(title = "Por Produto",
       x = "Nome Produto",
       y = "Frequência") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Verificando Frequência por Semana #
table(df_train$Semana)

# Medidas de Tendência Central
summary(df_train$Demanda_uni_equil)

# Medidas de Dispersão
var(df_train$Demanda_uni_equil)
sd(df_train$Demanda_uni_equil)

var(df_train$Semana)
sd(df_train$Semana)

# Explorando relacionamento entre as variáveis
cor(df_train)

# Dividindo os datasets em treino e teste
intrain <- createDataPartition(df_train$Demanda_uni_equil,p=0.7,list=FALSE)
set.seed(2017)
training <- df_train[intrain,]
testing <- df_train[-intrain,]

# Confirmando se a divisão está correta
dim(training); dim(testing)

str(training)
str(testing)

# Gravando dataset
fwrite(training, file = "dataset/training.csv")
fwrite(testing, file = "dataset/testing.csv")

# Liberando espaço em memória
rm(list=ls(all=TRUE))


# Funções para recarregar o datasets 
dataset_treino <- function() {
  training <<- fread("dataset/training.csv")
} 

dataset_teste <- function() {
  testing <<- fread("dataset/testing.csv")
}

# Recarregando dataset treino  
dataset_treino()

str(training)

# Aumentando o limite de memória para conseguir executar
memory.limit (9999999) 



##### Treinamento do Modelo de Regressão Linear #####
modelo <- lm(Demanda_uni_equil ~ ., data = training)

# Liberando espaço em memória
rm(training)

# Previsões nos dados de teste
dataset_teste()
previsao <- predict(modelo, testing)
View(previsao)

# Avaliando a Performance do Modelo
summary(modelo)

# Liberando espaço em memória
rm(modelo, testing, previsao)

# Recarregando dataset treino  
dataset_treino()



##### Treinamento do Modelo Random Forest #####
modelo2 <- rpart(Demanda_uni_equil ~ ., data = training)

# Recarregando dataset teste
dataset_teste()

# Previsões nos dados de teste
tree_pred = predict(modelo2, testing)

# Percentual de previsões com dataset de teste
percentual <- (100 - round(mean(abs(tree_pred - testing$Demanda_uni_equil)), digits = 2))

percentual
