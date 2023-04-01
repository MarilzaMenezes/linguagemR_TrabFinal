library(dplyr)
library(ggplot2)
library(purrr)
library (magrittr)
library(stringr)
library(tidyr)
library(readr)

dados <- read_csv2("C:/Users/Windows/Documents/dataset_preco_gas_brasil.csv", col_names = TRUE)

#Filtrando os Estados com o maior preço de revenda
dados %>% 
  filter(ESTADO %in% c("MATO GROSSO DO SUL", "SANTA CATARINA"))


dados %>%
  group_by(ESTADO, PRCMEDIOREV) %>%
  summarise(soma_valor = sum(PRCMEDIOREV))


dados$PRCMEDISTR <- as.numeric(dados$PRCMEDISTR)

#dados <- dados %>%
# sample_n(size = nrow(.)*0.5)

dados$PRCMEDISTR[dados$PRCMEDISTR == "NA"] <- NA
dados$PRCMEDISTR <- as.numeric(dados$PRCMEDISTR)
dados <- dados[complete.cases(dados$PRCMEDISTR), ]

dados <- dados %>% 
  filter(PRCMEDISTR > 0)

dados %>% 
  mutate(MARGEM_LUCRO = PRCMEDIOREV - PRCMEDISTR)

#Gráfico de barras dos preços médios de revenda por estado, ordenado por preço médio:
ggplot(dados, aes(x = reorder(ESTADO, PRCMEDIOREV), y = PRCMEDIOREV)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Estado", y = "Preço Médio de Revenda (R$/l)") +
  theme_bw()

#Variação do preço médio do produto ao longo do tempo em São Paulo
dados %>%
  filter(PRODUTO == "GASOLINA COMUM" & ESTADO == "SAO PAULO") %>%
  mutate(DATA = as.Date(DTINICIAL, "%d/%m/%Y")) %>%
  ggplot(aes(x = DATA, y = PRCMEDIOREV)) +
  geom_line(color = "blue") +
  labs(x = "Data", y = "Preço médio da gasolina", title = "Variação do preço médio da gasolina em São Paulo") +
  theme_minimal()

#Preço medio ao longo do tempo
dados %>%
  filter(PRODUTO == "GASOLINA COMUM") %>%
  select(DTINICIAL, REGIAO, PRCMEDIOREV) %>%
  mutate(DTINICIAL = as.Date(DTINICIAL, "%d/%m/%Y")) %>%
  ggplot(aes(x = DTINICIAL, y = PRCMEDIOREV, color = REGIAO)) +
  geom_jitter() +
  labs(x = "Data", y = "Preço médio de revenda (R$/l)",
       color = "Região") +
  theme_minimal()


ggplot(dados, aes(x = PRCMEDISTR, y = PRCMEDIOREV)) + 
  geom_point() + 
  labs(x = "Preço médio de distribuição", y = "Preço médio de revenda")

modelo <- lm(PRCMEDIOREV ~ PRCMEDISTR, data = dados)
summary(modelo)

ggplot(dados, aes(x = PRCMEDISTR, y = PRCMEDIOREV)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Preço médio de distribuição", y = "Preço médio de revenda")

novo_dado <- data.frame(PRCMEDISTR = 2.50)
previsao <- predict(modelo, newdata = novo_dado)
previsao