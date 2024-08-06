library(xlsx)
library(dplyr)
library(tidyverse)
library(GetDFPData2)
library(PerformanceAnalytics)
library(quantmod)  # (https://cran.r-project.org/web/packages/quantmod/quantmod.pdf)
library(rbcb)      # (https://cran.r-project.org/web/packages/rbcb/rbcb.pdf)

#library(dplyr)    # (https://cran.r-project.org/web/packages/dplyr/dplyr.pdf)

#Cleaning
rm(list=ls())

#Downloading information about the companies

df_info <- get_info_companies()

search_company('Ambev S.A')

####Period

#Downloading data using GetDFPData2
data_inicial <- c("2018")
data_final <- c("2023")

MyData <- get_dfp_data(
  companies_cvm_codes =23264,
  first_year = data_inicial,
  last_year = data_final,
  type_docs = c("BPA", "BPP", "DRE", "DFC_MD", "DFC_MI"),
  type_format = c("con"),
  clean_data = TRUE,
  use_memoise = FALSE,
  cache_folder = "gdfpd2_cache",
  do_shiny_progress = FALSE
)

#FINDING THE FCFE (FCFE = CFO - CAPEX - Working capital investment)

#CFO (Cash Flow from Operations)
FCO <- MyData$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)` %>%
  filter(CD_CONTA==6.01)

#Taxes

Tributos <- MyData$`DF Consolidado - Demonstração do Resultado`%>%
  filter(CD_CONTA==3.08)

#CAPEX (Capital Expenditures)

CAPEX <- MyData$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)` %>%
  filter(CD_CONTA== 6.02)

#Working capital (Operational Current Assets - Operational Current Liabilities)
Passivo_circ <- MyData$`DF Consolidado - Balanço Patrimonial Passivo` %>%
  filter(CD_CONTA==2.01)
Ativo_circ <- MyData$`DF Consolidado - Balanço Patrimonial Ativo` %>%
  filter(CD_CONTA==1.01)
Invest_giro <- data.frame(Ativo_circ$VL_CONTA - Passivo_circ$VL_CONTA)

Delta_giro <- data.frame(diff(Invest_giro$Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA))

#FCDE

FCDE <- FCO$VL_CONTA[-1] + Tributos$VL_CONTA[-1] - CAPEX$VL_CONTA[-1] - Delta_giro

cresc_FCDE <- data.frame(diff(FCDE$diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA.)/tail(FCDE$diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA.,1))

media <- mean(cresc_FCDE$diff.FCDE.diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA...tail.FCDE.diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA...)

print(media)

#Weighted Average Cost of Capital: WACC = cost of equity + cost of debt

# Year-Month-Day
start_d <- as.Date("2022-01-01")
end_d <- as.Date("2023-11-30")

#Estimating Ke via CAPM

stocklist <- c("^BVSP","ABEV3.SA")

#Gathering Stocks Data
getSymbols(stocklist, from=start_d,to=end_d, src="yahoo")

#Gathering Risk Free Asset (CDI) Data 

cdi_daily <- rbcb::get_series(code = 12, start_date = start_d, end_date = end_d)
tibble::glimpse(cdi_daily)

## function (code, start_date = NULL, end_date = NULL, last = 0, 
##     as = c("tibble", "xts", "ts", "data.frame", "text")) 

data1<- merge(ABEV3.SA$ABEV3.SA.Close, BVSP$BVSP.Close, cdi_daily, by="date")

#Returns Calculations

data1$ret_ambev <- diff(log(ABEV3.SA$ABEV3.SA.Close))[-1] 

data1$ret_bvsp <- diff(log(BVSP$BVSP.Close))[-1] 

# Excess Returns

data1$ex_ambev <- data1$ret_ambev - (data1$cdi_daily/100)

data1$ex_bvsp <- data1$ret_bvsp - (data1$cdi_daily/100)

#Results

reg_capm <- lm(ex_ambev ~ ex_bvsp, data = data1)

cf <- coef(reg_capm)

summary(reg_capm)

data1 <- head(data1, - 1) 

#Calculating Ke
Ke <- mean(data1$cdi_daily) + cf[2]*mean(tail(data1$ex_ambev,30))

print(Ke)

#Calculando Kd

DespJuros <- MyData$`DF Consolidado - Balanço Patrimonial Passivo` %>%
  filter(DS_CONTA=="Juros a Pagar")

DividaBruta <- MyData$`DF Consolidado - Balanço Patrimonial Passivo` %>%
  filter(CD_CONTA==2)

Kd <- mean(DespJuros$VL_CONTA/DividaBruta$VL_CONTA)

print(Kd)

ValorMercado <- c(186670000000)

WACC = Ke*(ValorMercado/(ValorMercado + mean(DividaBruta$VL_CONTA))) + Kd*(mean(DespJuros$VL_CONTA) /(ValorMercado+DividaBruta$VL_CONTA))

print(WACC)

#Dados Futuros 

FCDE_base <- data.frame(c(tail(FCDE$diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA.,1),tail(FCDE$diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA.,1),tail(FCDE$diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA.,1)))

g <- data.frame(mean(diff(FCO$VL_CONTA)/tail(FCO$VL_CONTA,1)))
print(g)

Cresc_g <- data.frame(c((1+g), (1+g)^2, (1+g)^3))

FCDE_base$Cresc <- FCDE_base*t(Cresc_g)

gPIB <- 0.025

FatorDesconto <- data.frame(c((1/(1+mean(WACC))), 1/((1+mean(WACC)^2)), 1/((mean(WACC)-gPIB)*(1+mean(WACC)^3)) ))

FCDE_base$Final <- FCDE_base$Cresc$c.tail.FCDE.diff.Invest_giro.Ativo_circ.VL_CONTA...Passivo_circ.VL_CONTA...*FatorDesconto

VP <- sum(FCDE_base$Final) 

print(paste("The value of AMBEV is:", VP))


