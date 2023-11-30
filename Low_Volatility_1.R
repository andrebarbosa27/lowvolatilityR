library(dplyr)
library(tidyquant)
library(lifecycle)
library(tidyr)
if(!require(lubridate)) install.packages("lubridate")
library(lubridate)
if(!require(zoo)) install.packages("zoo")
library(zoo)
if(!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
install.packages("ggplot2")

rm(list=ls())

load("C:/Users/Andre/Documents/LowVolatilityR/Precos_Ajustados_Andre.RData")
load("C:/Users/Andre/Documents/LowVolatilityR/Financeiro_Andre.RData")
load("C:/Users/Andre/Documents/LowVolatilityR/Negocios_Andre.RData")
load("C:/Users/Andre/Documents/LowVolatilityR/Ibov_Andre.RData")


## Corte Temporal
Precos_Corte      <- Raw_Prices_Data2 %>%
filter(Datas >= as.Date("2000-01-01", format = "%Y-%m-%d")) %>%
filter(Datas <= as.Date("2020-12-31", format = "%Y-%m-%d")) %>%
gather("Codigo", "Preco", -Datas) %>%
mutate(Ano = year(Datas)) %>%
group_by(Codigo,Ano) %>%
mutate(Pregoes = sum(!is.na(Preco), na.rm = TRUE)) %>%
mutate(Pregoes_Percentual = Pregoes/sum(!is.na(Codigo))) %>%
filter(Pregoes_Percentual > 0.90) %>%
mutate(Preco = na.locf0(Preco)) %>%
mutate(Retorno =  (Preco - lag(Preco,1))/lag(Preco,1)) %>%
mutate(Risco = sqrt(Pregoes)*sd(Retorno, na.rm = TRUE)) %>%
mutate(Retorno_positivos = sum(Retorno > 0, na.rm= TRUE)) %>%
mutate(Retorno_negativos = sum(Retorno < 0, na.rm= TRUE)) %>%
mutate(Perc_positivos = Retorno_positivos/
(Retorno_positivos+Retorno_negativos)) %>%
mutate(Perc_negativos = Retorno_negativos/
(Retorno_positivos+Retorno_negativos)) %>%
mutate(Retorno = replace_na(Retorno, replace = 0)) %>%
mutate(Retorno_Anual = Return.cumulative(Retorno, geometric = TRUE)) %>%
mutate(FIP = Retorno_Anual*(Perc_negativos - Perc_positivos)) %>%
mutate(Ret_Acumulado_Diario =cumprod(1+Retorno) -1) %>%
ungroup(Codigo) %>%
mutate(Decil_Retorno=ntile(x = Retorno_Anual, n = 10)) %>%
mutate(Decil_Risco=ntile(x = Risco, n = 10)) %>%
mutate(Decil_FIP=ntile(x = FIP, n = 10)) %>%
group_by(Codigo)

Negocios_Corte    <- Negocios_Data2 %>%
filter(Datas >= as.Date("2000-01-01", format = "%Y-%m-%d")) %>%
filter(Datas <= as.Date("2020-12-31", format = "%Y-%m-%d")) %>%
gather("Codigo", "Negocios", -Datas) %>%
mutate(Ano = year(Datas)) %>%
group_by(Codigo,Ano) %>%
mutate(Media_Negocios_Acao = mean(Negocios, na.rm = TRUE)) %>%
mutate(Dias_Negocios = sum(!is.na(Negocios), na.rm = TRUE)) %>%
mutate(Negocios_Percentual = Dias_Negocios/sum(!is.na(Codigo))) %>%
filter(Negocios_Percentual > 0.9) %>%
ungroup(Codigo) %>%
mutate(Media_Negocios = mean(Negocios, na.rm = TRUE)) %>%
mutate(Razao_Media_Negocios = Media_Negocios_Acao/Media_Negocios)

Financeiro_Corte  <- Financeiro_Data2 %>%
filter(Datas >= as.Date("2000-01-01", format = "%Y-%m-%d")) %>%
filter(Datas <= as.Date("2020-12-31", format = "%Y-%m-%d")) %>%
gather("Codigo", "Financeiro", -Datas) %>%
mutate(Ano = year(Datas)) %>%
group_by(Codigo,Ano) %>%
mutate(Media_Financeiro_Ativo_Ano = mean(Financeiro, na.rm=TRUE)) %>%
mutate(Financeiro_Dias = sum(!is.na(Financeiro), na.rm = TRUE)) %>%
mutate(Financeiro_Dias_Percentual = Financeiro_Dias/sum(!is.na(Codigo))) %>%
filter(Financeiro_Dias_Percentual > 0.7) %>%
ungroup(Codigo) %>%
mutate(Media_Financeiro_Ano = mean(Financeiro, na.rm = TRUE)) %>%
mutate(DP_Financeiro_Ano = sd(Financeiro, na.rm = TRUE)) %>%
group_by(Codigo,Ano) %>%
mutate(Razao_Financeiro = Media_Financeiro_Ativo_Ano/
Media_Financeiro_Ano)

DF <- left_join(Precos_Corte, Negocios_Corte)
DF2 <- left_join(DF, Financeiro_Corte)


Ibovespa_df <- Ibovespa_Data2 %>%
filter(Datas >= as.Date("2000-01-01", format = "%Y-%m-%d")) %>%
filter(Datas <= as.Date("2020-12-31", format = "%Y-%m-%d")) %>%
mutate(Pontos = na.locf0(Pontos)) %>%
mutate(Retorno =  (Pontos - lag(Pontos,1))/lag(Pontos,1)) %>%
mutate(Retorno = replace_na(Retorno, replace = 0)) %>%
#mutate(Ano = year(Datas)) %>%
#group_by(Ano) %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno) -1)


# Criando uma lista separando os DFs por Ano
Lista_DFs <- DF2 %>%
filter(Razao_Financeiro > 0.05) %>%
filter(Razao_Media_Negocios > 0.01) %>%
ungroup() %>%
group_split(Ano)


### Criando as Carteiras de Mercado - Portfólio Neutro
Lista_Carteira_Mercado <- list()
for (i in seq_along(Lista_DFs)){
Lista_Carteira_Mercado[[i]] <- Lista_DFs[[i]] %>%
group_by(Codigo) %>%
summarise(mean(Risco),
mean(Retorno_Anual),
mean(Decil_Retorno),
mean(Decil_Risco),
mean(Ano))
}

Lista_Carteira_Mercado

### Criando as Carteiras da Estratégia 1 - Momentum
Lista_Carteira_1 <- list()
for (i in seq_along(Lista_DFs)){
Lista_Carteira_1[[i]] <- Lista_DFs[[i]] %>%
group_by(Codigo) %>%
filter(Decil_Risco <= 1) %>%
summarise(mean(Risco),mean(Retorno_Anual),mean(Decil_Retorno),mean(Decil_Risco),mean(Ano))
}

Lista_Carteira_2 <- list()
for (i in seq_along(Lista_DFs)){
Lista_Carteira_2[[i]] <- Lista_DFs[[i]] %>%
group_by(Codigo) %>%
filter(Decil_Risco <= 2) %>%
filter(Decil_Risco > 1) %>%
summarise(mean(Risco),mean(Retorno_Anual),mean(Decil_Retorno),mean(Decil_Risco),mean(Ano))
}

Lista_Carteira_3 <- list()
for (i in seq_along(Lista_DFs)){
Lista_Carteira_3[[i]] <- Lista_DFs[[i]] %>%
group_by(Codigo) %>%
filter(Decil_Risco <= 3) %>%
filter(Decil_Risco > 2) %>%
summarise(mean(Risco),mean(Retorno_Anual),mean(Decil_Retorno),mean(Decil_Risco),mean(Ano))
}

Lista_Carteira_4 <- list()
for (i in seq_along(Lista_DFs)){
Lista_Carteira_4[[i]] <- Lista_DFs[[i]] %>%
group_by(Codigo) %>%
filter(Decil_Risco <= 4) %>%
filter(Decil_Risco > 3) %>%
summarise(mean(Risco),mean(Retorno_Anual),mean(Decil_Retorno),mean(Decil_Risco),mean(Ano))
}

Lista_Carteira_5 <- list()
for (i in seq_along(Lista_DFs)){
Lista_Carteira_5[[i]] <- Lista_DFs[[i]] %>%
group_by(Codigo) %>%
filter(Decil_Risco <= 5) %>%
filter(Decil_Risco > 4) %>%
summarise(mean(Risco),mean(Retorno_Anual),mean(Decil_Retorno),mean(Decil_Risco),mean(Ano))
}


### Operações OUT of SAMPLE 2001

Lista_Trade_Neutro <- list()
for (i in  seq(1,20)){

Lista_Trade_Neutro[[i]] <- Lista_DFs[[i+1]] %>%
filter(Codigo %in% Lista_Carteira_Mercado[[i]]$Codigo) %>%
ungroup() %>%
group_by(Datas) %>%
summarise(Retorno_Carteira = mean(Retorno, na.rm=TRUE)) %>%
ungroup() %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno_Carteira) -1)
}

Lista_Trade_1 <- list()
for (i in seq(1,20)){
Lista_Trade_1[[i]] <- Lista_DFs[[i+1]] %>%
filter(Codigo %in% Lista_Carteira_1[[i]]$Codigo) %>%
ungroup() %>%
group_by(Datas) %>%
summarise(Retorno_Carteira = mean(Retorno, na.rm=TRUE)) %>%
ungroup() %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno_Carteira) -1)
}

Lista_Trade_2 <- list()
for (i in seq(1,20)){
Lista_Trade_2[[i]] <- Lista_DFs[[i+1]] %>%
filter(Codigo %in% Lista_Carteira_2[[i]]$Codigo) %>%
ungroup() %>%
group_by(Datas) %>%
summarise(Retorno_Carteira = mean(Retorno, na.rm=TRUE)) %>%
ungroup() %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno_Carteira) -1)
}

Lista_Trade_3 <- list()
for (i in seq(1,20)){
Lista_Trade_3[[i]] <- Lista_DFs[[i+1]] %>%
filter(Codigo %in% Lista_Carteira_3[[i]]$Codigo) %>%
ungroup() %>%
group_by(Datas) %>%
summarise(Retorno_Carteira = mean(Retorno, na.rm=TRUE)) %>%
ungroup() %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno_Carteira) -1)
}

Lista_Trade_4 <- list()
for (i in seq(1,20)){
Lista_Trade_4[[i]] <- Lista_DFs[[i+1]] %>%
filter(Codigo %in% Lista_Carteira_4[[i]]$Codigo) %>%
ungroup() %>%
group_by(Datas) %>%
summarise(Retorno_Carteira = mean(Retorno, na.rm=TRUE)) %>%
ungroup() %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno_Carteira) -1)
}

Lista_Trade_5 <- list()
for (i in seq(1,20)){
Lista_Trade_5[[i]] <- Lista_DFs[[i+1]] %>%
filter(Codigo %in% Lista_Carteira_5[[i]]$Codigo) %>%
ungroup() %>%
group_by(Datas) %>%
summarise(Retorno_Carteira = mean(Retorno, na.rm=TRUE)) %>%
ungroup() %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno_Carteira) -1)
}


Lista_Ibovespa <- Ibovespa_df %>%
mutate(Ano = year(Datas)) %>%
ungroup() %>%
group_split(Ano)

Lista_Ibovespa_2 <- list()
for (i in seq(1,20)){
Lista_Ibovespa_2[[i]] <- Lista_Ibovespa[[i+1]] %>%
mutate(Ret_Acumulado_Carteira =cumprod(1+Retorno) -1)
}


### Plot dos Resultados
#par(mfrow = c(3,3))

par(oma = c(4,1,1,1), mfrow = c(3, 3), mar = c(2, 2, 1, 1))
for (i in seq(1,20)){

png("plot1.png")

png("plot2.png")

png("plot3.png")

png("plot4.png")

png("plot5.png")

png("plot6.png")

png("plot7.png")

png("plot1.png")

png("plot2.png")

png("plot3.png")

png("plot4.png")

png("plot5.png")
png("plot1.png")
plot(
dev.off()
dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),

dev.off(),
Lista_Trade_Neutro[[i]]$Datas,
Lista_Trade_Neutro[[i]]$Ret_Acumulado_Carteira,
type = "l",
lwd = 2,
ylab = "Retorno Acumulado",
xlab = "Datas",
main = paste(mean(Lista_DFs[[i+1]]$Ano)),
ylim = c(min(min(Lista_Trade_Neutro[[i]]$Ret_Acumulado_Carteira),
min(Lista_Trade_1[[i]]$Ret_Acumulado_Carteira),
min(Lista_Trade_2[[i]]$Ret_Acumulado_Carteira),
min(Lista_Trade_3[[i]]$Ret_Acumulado_Carteira),
min(Lista_Trade_4[[i]]$Ret_Acumulado_Carteira),
min(Lista_Trade_5[[i]]$Ret_Acumulado_Carteira),
min(Lista_Ibovespa_2[[i]]$Ret_Acumulado_Carteira)),
max(max(Lista_Trade_Neutro[[i]]$Ret_Acumulado_Carteira),
max(Lista_Trade_1[[i]]$Ret_Acumulado_Carteira),
max(Lista_Trade_2[[i]]$Ret_Acumulado_Carteira),
max(Lista_Trade_3[[i]]$Ret_Acumulado_Carteira),
max(Lista_Trade_4[[i]]$Ret_Acumulado_Carteira),
max(Lista_Trade_5[[i]]$Ret_Acumulado_Carteira),
max(Lista_Ibovespa_2[[i]]$Ret_Acumulado_Carteira))))
lines(Lista_Trade_1[[i]]$Datas,Lista_Trade_1[[i]]$Ret_Acumulado_Carteira,
col = "red", lwd = 2)
lines(Lista_Trade_2[[i]]$Datas,Lista_Trade_2[[i]]$Ret_Acumulado_Carteira,
col = "blue", lwd = 2)
lines(Lista_Trade_3[[i]]$Datas,Lista_Trade_3[[i]]$Ret_Acumulado_Carteira,
col = "magenta", lwd = 2)
lines(Lista_Trade_4[[i]]$Datas,Lista_Trade_4[[i]]$Ret_Acumulado_Carteira,
col = "cyan", lwd = 2)
lines(Lista_Trade_5[[i]]$Datas,Lista_Trade_5[[i]]$Ret_Acumulado_Carteira,
col = "purple", lwd = 2)
#  lines(Lista_Ibovespa_2[[i]]$Datas,Lista_Ibovespa_2[[i]]$Ret_Acumulado_Carteira,
#        col = "green", lwd = 2)
abline(h = 0, lty = 2, col = "grey")
}
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
png("plot2.png")
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n', cex = 0.5)
dev.off()

png("plot6.png")

png("plot7.png")
legend(
dev.off(),

dev.off(),
'bottom',legend = c("Estratégia Neutra",
"D1",
"D2",
"D3",
"D4",
"D5"),
col = c("black","red", "blue", "magenta", "cyan", "purple"),
lwd = 5, xpd = TRUE, horiz = TRUE, cex = 0.8, seg.len=1, bty = 'n')


### Análise Período Completo

Port_Trade_Neutro = do.call(rbind, Lista_Trade_Neutro)
Port_Trade_Neutro$Ret_Acumulado_Carteira = cumprod(1+Port_Trade_Neutro$Retorno_Carteira) -1

Port_Trade_1 = do.call(rbind, Lista_Trade_1)
Port_Trade_1$Ret_Acumulado_Carteira = cumprod(1+Port_Trade_1$Retorno_Carteira) -1

Port_Trade_2 = do.call(rbind, Lista_Trade_2)
Port_Trade_2$Ret_Acumulado_Carteira = cumprod(1+Port_Trade_2$Retorno_Carteira) -1

Port_Trade_3 = do.call(rbind, Lista_Trade_3)
Port_Trade_3$Ret_Acumulado_Carteira = cumprod(1+Port_Trade_3$Retorno_Carteira) -1

Port_Trade_4 = do.call(rbind, Lista_Trade_4)
Port_Trade_4$Ret_Acumulado_Carteira = cumprod(1+Port_Trade_4$Retorno_Carteira) -1

Port_Trade_5 = do.call(rbind, Lista_Trade_5)
Port_Trade_5$Ret_Acumulado_Carteira = cumprod(1+Port_Trade_5$Retorno_Carteira) -1


par(mfrow = c(1,1))

## Plot do Resultado do Portfólio
png("plot3.png")
plot(Port_Trade_Neutro$Datas,
dev.off()
Port_Trade_Neutro$Ret_Acumulado_Carteira,
type = "l",
lwd = 2,
ylim =  c(min(min(Port_Trade_Neutro$Ret_Acumulado_Carteira),
min(Port_Trade_1$Ret_Acumulado_Carteira),
min(Port_Trade_2$Ret_Acumulado_Carteira),
min(Port_Trade_3$Ret_Acumulado_Carteira),
min(Port_Trade_4$Ret_Acumulado_Carteira),
min(Port_Trade_5$Ret_Acumulado_Carteira)),
max(max(Port_Trade_Neutro$Ret_Acumulado_Carteira),
max(Port_Trade_1$Ret_Acumulado_Carteira),
max(Port_Trade_2$Ret_Acumulado_Carteira),
max(Port_Trade_3$Ret_Acumulado_Carteira),
max(Port_Trade_4$Ret_Acumulado_Carteira),
max(Port_Trade_5$Ret_Acumulado_Carteira))),
ylab = "Retorno Acumulado",
xlab = "Datas",
main = "Retorno das Estratégias ao Longo do Tempo",
panel.first = grid())
lines(Port_Trade_1$Datas,
Port_Trade_1$Ret_Acumulado_Carteira,
type = "l", lwd = 2, col = "red",
lty = 1)

lines(Port_Trade_2$Datas,
Port_Trade_2$Ret_Acumulado_Carteira,
type = "l", lwd = 2, col = "blue",
lty = 1)

lines(Port_Trade_3$Datas,
Port_Trade_3$Ret_Acumulado_Carteira,
type = "l", lwd = 2, col = "magenta",
lty = 1)

lines(Port_Trade_4$Datas,
Port_Trade_4$Ret_Acumulado_Carteira,
type = "l", lwd = 2, col = "cyan",
lty = 1)

lines(Port_Trade_5$Datas,
Port_Trade_5$Ret_Acumulado_Carteira,
type = "l", lwd = 2, col = "purple",
lty = 1)

#lines(Ibovespa_df$Datas,
#      Ibovespa_df$Ret_Acumulado_Carteira,
#      type = "l", lwd = 2, col = "green",
#      lty = 1)
legend("topleft",c("Neutro","D1-DP","D2-DP","D3-DP", "D4-DP", "D5-DP"),
cex = 0.8, lty = 1, col = c("black","red","blue","magenta","cyan","purple"),
lwd = 2, inset = 0.05)



abline(h = 0, lty = 2, col = "grey")


### Exportando as Carteiras

path0 <- "C:/Users/Andre/Documents/LowVolatilityR/Carteira_Neutra.xlsx"
path1 <- "C:/Users/Andre/Documents/LowVolatilityR/Carteira1.xlsx"
path2 <- "C:/Users/Andre/Documents/LowVolatilityR/Carteira2.xlsx"
path3 <- "C:/Users/Andre/Documents/LowVolatilityR/Carteira3.xlsx"
path4 <- "C:/Users/Andre/Documents/LowVolatilityR/Carteira4.xlsx"
path5 <- "C:/Users/Andre/Documents/LowVolatilityR/Carteira5.xlsx"


anos <- seq(2000, 2020, by = 1)

library("xlsx")

# Write the first data set in a new workbook
write.xlsx(Lista_Carteira_Mercado[[1]], file = path0,
sheetName = "2000", append = FALSE)
# Add a second data set in a new worksheet
for (i in 2:21) {
write.xlsx(Lista_Carteira_Mercado[[i]], file = path0,
sheetName = as.character(anos[i]), append = TRUE)
}
# Write the first data set in a new workbook
write.xlsx(Lista_Carteira_1[[1]], file = path1,
sheetName = "2000", append = FALSE)
# Add a second data set in a new worksheet
for (i in 2:21) {
write.xlsx(Lista_Carteira_1[[i]], file = path1,
sheetName = as.character(anos[i]), append = TRUE)
}

write.xlsx(Lista_Carteira_2[[1]], file = path2,
sheetName = "2000", append = FALSE)
# Add a second data set in a new worksheet
for (i in 2:21) {
write.xlsx(Lista_Carteira_2[[i]], file = path2,
sheetName = as.character(anos[i]), append = TRUE)
}

write.xlsx(Lista_Carteira_3[[1]], file = path3,
sheetName = "2000", append = FALSE)
# Add a second data set in a new worksheet
for (i in 2:21) {
write.xlsx(Lista_Carteira_3[[i]], file = path3,
sheetName = as.character(anos[i]), append = TRUE)
}

write.xlsx(Lista_Carteira_4[[1]], file = path4,
sheetName = "2000", append = FALSE)
# Add a second data set in a new worksheet
for (i in 2:21) {
write.xlsx(Lista_Carteira_4[[i]], file = path4,
sheetName = as.character(anos[i]), append = TRUE)
}

write.xlsx(Lista_Carteira_5[[1]], file = path5,
sheetName = "2000", append = FALSE)
# Add a second data set in a new worksheet
for (i in 2:21) {
write.xlsx(Lista_Carteira_5[[i]], file = path5,
sheetName = as.character(anos[i]), append = TRUE)
}


Data_DF_Final <- data.frame(Datas = Port_Trade_1$Datas,
Trade1=Port_Trade_1$Retorno_Carteira,
Trade2=Port_Trade_2$Retorno_Carteira,
Trade3=Port_Trade_3$Retorno_Carteira,
Trade4=Port_Trade_4$Retorno_Carteira,
Trade5=Port_Trade_5$Retorno_Carteira,
Neutro=Port_Trade_Neutro$Retorno_Carteira)


xts_Trade1      = xts(Port_Trade_1$Retorno_Carteira, order.by = Port_Trade_1$Datas)
xts_Trade2      = xts(Port_Trade_2$Retorno_Carteira, order.by = Port_Trade_2$Datas)
xts_Trade3      = xts(Port_Trade_3$Retorno_Carteira, order.by = Port_Trade_3$Datas)
xts_Trade4      = xts(Port_Trade_4$Retorno_Carteira, order.by = Port_Trade_4$Datas)
xts_Trade5      = xts(Port_Trade_5$Retorno_Carteira, order.by = Port_Trade_5$Datas)
xts_TradeNeutro = xts(Port_Trade_Neutro$Retorno_Carteira, order.by = Port_Trade_Neutro$Datas)
xts_Ibov        = xts(Ibovespa_df$Retorno, order.by = Ibovespa_df$Datas)


xts_vector <- cbind(xts_Trade1,xts_Trade2,xts_Trade3,xts_Trade4,xts_Trade5,
xts_TradeNeutro,xts_Ibov)
xts_vector <- xts_vector[index(xts_vector) > "2000-12-31",]


table.DownsideRisk(xts_vector)
colnames(xts_vector) <- c("D1", "D2", "D3", "D4", "D5" ,"Neutro", "Ibovespa")


charts.PerformanceSummary(xts_vector, plot.engine = "ggplot2")



table.AnnualizedReturns(xts_vector)

PerformanceAnalytics::
png("plot8.png")

png("plot9.png")

png("plot10.png")

png("plot11.png")

png("plot12.png")
png("plot4.png")
chart.
dev.off()

dev.off()

dev.off()

dev.off()

dev.off()
RelativePerformance(xts_Trade1,xts_TradeNeutro)
png("plot5.png")
PerformanceAnalytics::chart.RelativePerformance(xts_Trade1,xts_Trade2)
dev.off()

png("plot6.png")
PerformanceAnalytics::chart.RollingPerformance(xts_vector, width = 252)
dev.off()


#install.packages('qcc')
#install.packages("RcppRoll")
library(qcc)
library(RcppRoll)


ret_sq <- xts_vector^2
EWMA_Estimado <- qcc::ewma(ret_sq, lambda = 0.95)

Mov_SD <- rollapply(xts_vector, width = 252, FUN = sd, na.pad = TRUE)
#Mov_CumProd <- RcppRoll::roll_prod(1+xts_vector, n = 252)

png("plot7.png")
plot(Mov_SD, col = c("red","blue","magenta","cyan","purple","black","green"),legend.loc = TRUE)
dev.off()
png("plot8.png")
#plot(Mov_CumProd[,1]-1, type = "l", col = "red")
dev.off()
#lines(Mov_CumProd[,2]-1, type = "l", col = "blue")

library(corrplot)

png("plot9.png")
corrplot(cor(xts_vector,xts_vector), method = "number")
dev.off()

png("plot10.png")
chart.RollingCorrelation(xts_vector[,-4],xts_Ibov,
dev.off()
width = 252, legend.loc = "bottomright")

png("plot11.png")
chart.RollingRegression(xts_vector[,-4],xts_Ibov,
dev.off()
width = 252, legend.loc = "bottomright")
