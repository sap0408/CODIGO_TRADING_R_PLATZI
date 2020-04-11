#clase 5

library(quantmod)
library(PerformanceAnalytics)

#serie de tiempo y conseguir datos desde enero 15, 2019
date <- "2019-1-15"           

#Obtener símbolos de yahoo. por ej Televisa
tv <- getSymbols.yahoo("TV", from = date)

TVClose <- getSymbols.yahoo("TV", from=date, auto.assign = F)[,6]
TVClose

TVRets <- na.omit(dailyReturn(TVClose, type="log")) 
#TVRets <- na.omit(dailyReturn(TVClose, type="arithmetic")) 
chartSeries(TVRets)  

#Ahora lo mismo para KOF, GFNORTEO.MX(Banorte), WALMEX.MX Y 
#BBD (bradesco)  

#clase 6
#Importar los datasets de cinco empresas de latam que seleccionamos 
#en yahoo finance 
#Despues de importar los archivos, unirlos por filas y despues 
#convertir a numeric
#crear columna a cada dataset con nombre de cia (ticker)
library(dplyr)
library(caret)
library(tidyverse)     

#dataset FEMSA
FEMSA <- mutate (FEMSA, Company= "Femsa", Company_Ticker="KOF") 
#cambiamos el orden de las variables (columnas) para tener primero
#la compañia
FEMSA_2 <- FEMSA[,c(9,8,1,2,3,4,5,6,7)]         

#dataset GFBANORTE
GFBANORTE.MX <- mutate (GFBANORTE.MX, Company= "Grupo Financiero Banorte", 
                        Company_Ticker="GFNORTEO.MX") 
#cambiamos el orden de las variables (columnas) para tener primero
#la compañia
GFBANORTE.MX_2 <- GFBANORTE.MX[,c(9,8,1,2,3,4,5,6,7)]  

#Televisa
TELEVISA <- mutate (TELEVISA, Company= "Televisa", 
                    Company_Ticker="TV") 
TELEVISA_2 <- TELEVISA[,c(9,8,1,2,3,4,5,6,7)] 

#walmart México
WALMEX.MX <- mutate (WALMEX.MX, Company= "Walmart Méx", 
                     Company_Ticker="WALMEX.MX") 
WALMEX.MX_2 <- WALMEX.MX[,c(9,8,1,2,3,4,5,6,7)] 

#Bradesco. banco brasil
BBD.bradesco <- mutate (BBD.bradesco, Company= "Banco Bradesco", 
                        Company_Ticker="BBD") 
BBD.bradesco_2 <- BBD.bradesco[,c(9,8,1,2,3,4,5,6,7)] 

#Crear un gran dataset agregando filas (rows)
Latam_GEI_Index <- rbind(FEMSA_2, GFBANORTE.MX_2, TELEVISA_2, WALMEX.MX_2, 
                BBD.bradesco_2)      

#ver estructura de dataset
str (Latam_GEI_Index)   

#si algo estuviera en caracter, entonces tocaria usar el convertidor as.numeric asi
#ej: si open no estuviera numerico
Latam_GEI_Index$Open = gsub(",", "", Latam_GEI_Index$Open) %>%
  as.numeric()  

#promedios y desviaciones de los precios de cierre ajustados de las cinco acciones
summary(Latam_GEI_Index)

#desviacion estandar de precio de cierre ajustado de las cinco companias
sd(Latam_GEI_Index$Adj.Close)   

#subseting para sacar promedio, desviacion estandar y coeficiente por compania
library("dplyr")
  
FEMSA_3 <- Latam_GEI_Index %>%
  select(Company, Adj.Close) %>%
  filter(Company == "Femsa")     

WALMART_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Walmart Méx")

BBD.bradesco_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Banco Bradesco")

TELEVISA_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Televisa")   

GFBANORTE.MX_3 <- Latam_GEI_Index %>%  
  select(Company, Adj.Close) %>%
  filter(Company == "Grupo Financiero Banorte")

#promedio, desviacion y coeficiente para cada cia.   
meanWALMART_3<- mean(WALMART_3$Adj.Close)
sdWALMART_3<- sd(WALMART_3$Adj.Close)  
Coef_W <- (sdWALMART_3/meanWALMART_3)*100  

#Clase 9      
#analisis que resume comportamiento historico de las cinco cias 
#desde marzo 2, 2019. Con precio cierre ajustado

date <- "2019-3-2"  
tickers <- c("GFNORTEO.MX", "TV", "WALMEX.MX", "BBD", "KOF")       

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
        from="2019-3-2", periodicity="daily",auto.assign=FALSE)[,6])  
}

portfolioPrices 

view(portfolioPrices)

#Clase 10
#Graficar lineas con serie de tiempo precios cierre. con ggplot2
library ("ggplot2")
#Antes vamos a incluir el titulo de la variable tiempo en la columna 0  
#exportar archivo excel
library("dplyr")
colnames(portfolioPrices)
str(portfolioPrices)
library("tibble")

portfolioPrices <- as.data.frame(portfolioPrices)
class(portfolioPrices)    

portfolioPrices <- rownames_to_column(portfolioPrices, var="fecha")
view(portfolioPrices)     
   
#extraer este dataset.al escritorio
#windows
write.csv2(portfolioPrices , "C:/Users/Desktop/portfolioPrices.csv")
#linux - Ejemplo de mi escritorio
write.csv2(portfolioPrices , "/home/sonia/Desktop/portfolioPrices.csv")  

#ajustar el dataframe para hacer la grafica de lineas
library("tidyverse") 
df <- portfolioPrices %>% select(fecha,GFNORTEO.MX.Adjusted,TV.Adjusted,
      WALMEX.MX.Adjusted,BBD.Adjusted, KOF.Adjusted) %>%
      gather (key="variable", value="value", -fecha)
head(df)

#clase 11
#ahora grafica de lineas para ver comportamiento de precio de
#cierre ajustado 
ggplot(df, aes(x=fecha, y=value)) + 
  geom_line(aes(group=variable, linetype=variable))+
  scale_color_manual(values=c("red", "blue", "black", "purple", "green"))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank())+
  labs(title="Fluctuación Precio Cierre Ajustado desde Marzo 2019")                

#clase 12         
#Vamos a ver el historico de RETORNO DIARIO SOBRE precios de cierre QUANTMOD
date <- "2019-3-2"    
TVClose <- getSymbols.yahoo("TV", from=date, auto.assign = F)[,6]
TVClose

TVRets <- na.omit(dailyReturn(TVClose, type="log")) 
chartSeries(TVRets)    

#Femsa    
date <- "2019-3-2"  
KOFClose <- getSymbols.yahoo("KOF", from=date, auto.assign = F)[,6]
KOFClose   

KOFRets <- na.omit(dailyReturn(KOFClose, type="log")) 
chartSeries(KOFRets) 

#Grupo financiero banorte
date <- "2019-3-2"  
GFNORTEO.MXClose <- getSymbols.yahoo("GFNORTEO.MX", from=date, auto.assign = F)[,6]
GFNORTEO.MXClose

GFNORTEO.MXRets <- na.omit(dailyReturn(GFNORTEO.MXClose, type="log")) 
chartSeries(GFNORTEO.MXRets) 

#Walmart México
date <- "2019-3-2"  
WALMEX.MXClose <- getSymbols.yahoo("WALMEX.MX", from=date, auto.assign = F)[,6]
WALMEX.MXClose

WALMEX.MXRets <- na.omit(dailyReturn(WALMEX.MXClose, type="log")) 
chartSeries(WALMEX.MXRets) 

#Bradesco
date <- "2019-3-2"  
BBDClose <- getSymbols.yahoo("BBD", from=date, auto.assign = F)[,6]
BBDClose

BBDCloseRets <- na.omit(dailyReturn(BBDClose, type="log")) 
chartSeries(BBDCloseRets)     

#Ver con velas japonesas 
getSymbols( Symbols="BBF", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(BBF, theme = "white")  

candleChart(BBF, multi.col = TRUE, theme = "white")   
#
getSymbols( Symbols="WALMEX.MX", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(WALMEX.MX, theme = "white")  

candleChart(WALMEX.MX, multi.col = TRUE, theme = "white") 

getSymbols( Symbols="TV", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(TV, theme = "white")  

candleChart(TV, multi.col = TRUE, theme = "white") 

#
getSymbols( Symbols="BFNORTEO.MX", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(BFNORTEO.MX, theme = "white")  

candleChart(BFNORTEO.MX, multi.col = TRUE, theme = "white") 

#
getSymbols( Symbols="KOF", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(KOF, theme = "white")  

candleChart(KOF, multi.col = TRUE, theme = "white") 
install.packages("xts")

#clase 13 histogramas
hist(TV$TV.Close, breaks= 60, col="blue")      

hist(KOF$KOF.Close, breaks= 60, col="blue") 

hist(BBF$BBF.Close, breaks= 60, col="blue") 

hist(BFNORTEO.MX$BFNORTEO.MX.Close, breaks= 60, col="blue") 

hist(WALMEX.MX$WALMEX.MX.Close, breaks= 60, col="blue") 

#clase 14
#Analisis tecnico 
#TA: Technical analysis
#BBands es bandas de bollinger. Vo es volumen: 
#Nivel de actividad de un mercado. 
#MACD es moving average convergence divergence
KOF%>%Ad()%>%chartSeries()
KOF%>%chartSeries(TA='addBBands();addVo();addMACD()',
subset='2020')  
#Clase 15: solo slides

#Clase 16
#Ahora ademas el RSI
KOF%>%chartSeries(TA='addBBands();addVo();addMACD();
                  addRSI()',subset='2020') 


#y ATR
KOF%>%chartSeries(TA='addBBands();addMACD();addRSI();
                  addATR()',subset='2020')   

#clase 17 slides

#clase 18 medir rentabilidad de portafolio segun riesgo
#asumir que cada accion tiene el mismo peso
#LIBRERIA
library(PerformanceAnalytics)
library(PortfolioAnalytics)
tickers <- c("WALMEX.MX", "BBD", "KOF", "BBF", "TV")      

weights <- c(.20, .20, 0.20, 0.20, 0.20)     

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
          from="2019-3-2", periodicity="daily",auto.assign=FALSE)[,6])  
}

portfolioPrices 
#ROC: Rate of change. % de variacion entre precio actual y precio de
#periodo anterior. 
portfolioReturns <- na.omit(ROC(portfolioPrices))  
portfolioReturns

#Benchmark: con ILF> ishares Latin Funds

benchmarkPrices <- getSymbols.yahoo('ILF',
from='2019-3-2', periodicity='daily', auto.assign=FALSE)[,6]
  
benchmarkReturns <- na.omit(ROC(benchmarkPrices))     
benchmarkReturns      

#Retornos del portafolio segun el peso de cada accion. 
#Sale una sola columna
#porque son los retornos del portafolio segun los pesos. 
#Funcion de returno de portafolio del paquete Performance analytics

RetornosPortafolio <- Return.portfolio(portfolioReturns)
RetornosPortafolio

#Grafica ROC
RetornosPortafolio %>% chartSeries(TA="addROC()",subset="2020")
benchmarkReturns %>% chartSeries(TA="addROC()",subset="2020")

#Clase 19
#Midiendo rentabilidad segun retorno esperado y riesgo 
CAPM.beta(portfolioReturns, benchmarkReturns, .035/252)     

#Calcular retornos al año          
table.AnnualizedReturns(portfolioReturns)
 
#Clase 20      
#optimizacion portafolio. Maximizar retornos
#modificar los pesos
#los inputs: assets, constraints, objectives   
#objetivo:
library(PortfolioAnalytics)
portf <- portfolio.spec(colnames(portfolioReturns))
   
portf <- add.constraint(portf, type="weight_sum", min_sum=1,
                        max_sum=1)
portf <- add.constraint(portf, type="box", min=.10, max=.40)
portf <- add.objective(portf, type="return", name="mean")
portf <- add.objective(portf, type="risk", name="StdDev")

library(GenSA)
?GenSA

optPort <- optimize.portfolio(portfolioReturns, portf, 
                              optimize_method ="GenSA")
optPort  

#Clase 21 - Back testing
# 1. Load libraries.
library(Quandl)
library(scales)
library(gridExtra)   
library(TTR)
library(jsonlite)
library(xtable)
library(gtable)
library(grid)
library(dplyr)

# 2. Load  
library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(reshape2)
library(quantmod)

# 3. Set working directory. Estoy llamando mi codigo. 
setwd("/home/sap/Desktop/R/trading_r_taller_sap/")

# 4. Quandl authentication key. Se debe generar una api key en Quandl
#Quandl.api_key("9zUtYEM3Q9LZ39SGEv") # mostrar. Esta no funciona. Es un ejemplo.

#Evaluar la estrategia de long y short en cada cia.
PFL <- getSymbolsYahoo(c("KOF"))
filter(date >= "2015-01-01")

ILF <- getSymbolsYahoo("ILF") %>% 
  filter(date >= "2015-01-01")

# 5. Calcular senal de trading para la estrategia M01. 
PFL <- PFL %>%
  mutate(signal = ifelse(as.numeric(format(date, "%m")) <= 9,1,-1), ticker = "M01")

# 6. Calcular retornos diarios, senal de returno, retornos acumulados, 
#rolling retornos acumulados, drawdown, 
# y sharpe ratio. 
PFL <- PFL %>%
  mutate(daily_return = ifelse(row_number() == 1, 0, adjusted_close / lag(adjusted_close, 1) - 1), 
         signal_return = daily_return * signal, 
         cum_return = cumprod(1 + signal_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(signal_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 252)

ILF <- ILF %>% 
  mutate(daily_return = ifelse(row_number() == 1, 0, adjusted_close / lag(adjusted_close, 1) - 1), 
         cum_return = cumprod(1 + daily_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(daily_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 252)

combined <- bind_rows(PFL, ILF)

# 7. Plot equity curve versus benchmark.
(p1 <- ggplot(combined, aes(x = date, y = cum_return)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Equity Curve Versus Benchmark", 
         subtitle = "Evaluación de desempeno de la estrategia", 
         y = "Cumulative Return", 
         x = "Date") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

#Clase 22
(p2 <- ggplot(PFL, aes(x = date, y = signal)) + 
    geom_line(size = 1, colour = "blue") + 
    labs(title = "Trading Signal", 
         subtitle = "Senal de mi estrategia.Entre  +1 y -1.", 
         y = "Position", 
         x = "Date") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

(p3 <- ggplot(PFL, aes(x = date, y = adjusted_close)) + 
    geom_line(aes(colour = signal)) + 
    scale_colour_gradient(low = "red") +
    labs(title = "PFL Closing Price con Trading Signal", 
         subtitle = "Cuando short y cuando long", 
         y = "Precio Cierre Ajustado", 
         x = "Fecha") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

#Clase 23
(p4 <- ggplot(combined, aes(x = date, y = cum_return_3m)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Rolling Returns (3 Meses)", 
         y = "Retorno", 
         x = "Fecha") + 
    scale_y_continuous(labels = percent, limits = c(-0.5, 0.75)) + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

(p5 <- ggplot(combined, aes(x = date, y = drawdown)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Drawdown", 
         subtitle = "Frecuencia de caidas, tamano de maximas caidas y tiempo de recuperacion", 
         y = "Porcentaje drawdown", 
         x = "Fecha") + 
    scale_y_continuous(labels = percent) + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

#clase 24
(p6 <- ggplot(combined, aes(x = date, y = sharpe_12m)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Sharpe Ratio (12 Meses)", 
         subtitle = "Sharpe ratio. Retornos por unidad de riesgo.", 
         y = "Sharpe Ratio", 
         x = "Fecha") + 
    geom_hline(yintercept = 0) + 
    theme_alphaplot())

#clase 25 - Evaluando rentabilidad   
library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

stocks <- c("KOF", "TV", "BBD", "WALMEX.MX", "GFNORTEO.MX")

stock_data <- tq_get(stocks,
                     get = "stock.prices",
                     from = Sys.Date() - months(12),
                     to = Sys.Date())

init.investment <- 1000
growth <- mo_returns %>% arrange(date) %>%
  mutate(final_value = init.investment * cumprod(1 + returns)) %>%
  arrange(desc(final_value))
growth %>% filter(date == max(date)) %>% select(-date)

growth %>% ggplot(aes(x = date, y = final_value, color = symbol)) +
  geom_line() +
  # geom_smooth(method = "loess") +
  labs(
    title = "Portafolio individual: Comparando el crecimiento de US1000",
    subtitle = "Visualizacion de desempeno",
    x = "",
    y = "Valor inversion"
  ) +
  theme_tq() + theme(legend.position = "right") +
  scale_y_continuous(labels = scales::dollar)

growth %>% ungroup() %>% filter(date == max(date)) %>% 
  mutate(rank = row_number()) %>% top_n(5, final_value) %>% 
  select(rank, symbol, final_value)     

#Clase 26 slides yield.
#clase 27 slides final


        
