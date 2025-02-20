library(tidyverse)
library(lubridate)
library(quantmod)


# load data ---------------------------------------------------------------
getSymbols("XDWD.L")

# MSCI WORLD ---------------------------------------------------------------
msci_world <- XDWD.L
# msci_world <- msci_world %>% 
#   as.data.frame() %>% 
#   select(XDWD.L.Adjusted) %>% 
#   rownames_to_column(var = "date") %>% 
#   rename(kurs = XDWD.L.Adjusted) %>% 
#   mutate(date = as.Date(date),
#          year = year(date),
#          date_md = paste0(month(date),"-",day(date)) %>% as.Date(format="%m-%d")) %>% 
#   as_tibble()

# anpassung namen
# names(msci_world) <- names(msci_world) %>% str_replace("ISAC.L.", "") %>% tolower()

# löschen der NA's
msci_world <- na.omit(msci_world)
chart_Series(msci_world, col = "black")

library(funModeling)
library(Hmisc)
 
# ersetzen der 0, weil bei log INF herauskommt
msci_world$XDWD.L.Volume[msci_world$XDWD.L.Volume==0] <- .1
msci_world_log <- log(msci_world)

# diverse plots und werte
head(msci_world_log, n = 10)
df_status(msci_world_log)
describe(msci_world_log)


acf_log <- acf(msci_world_log, lag.max = 320)
diff.acf <- acf(msci_world_log)


# lineares model von opening zu adjusted ----------------------------------
models <- lm(XDWD.L.Open ~ XDWD.L.Adjusted, data = msci_world)
ggplot(msci_world, aes(x = XDWD.L.Adjusted, y = XDWD.L.Open)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "XDWD.L.Adjusted", y = "XDWD.L.Open", title = "Linear Regression") +
  theme_minimal()


# arima modell ------------------------------------------------------------
library(caTools)
library(forecast)

# training set
train_data <- msci_world_log[1:2140, "XDWD.L.Close"]  
set.seed(123)
arima_model <- auto.arima(train_data, stationary = TRUE, 
                          ic = c("aicc", "aic", "bic"), 
                          trace = TRUE)
summary(arima_model)
checkresiduals(arima_model)

# model mit den werten aus auto.arima
arima <- arima(train_data, order = c(0, 0, 5))
summary(arima)


close_prices <- Cl(msci_world)


forecast1 <- forecast(arima, h = 100)
plot(forecast1)

# modellierung
train_datas <- msci_world_log[1:2140, "XDWD.L.Close"]
arima <- arima(train_datas, order = c(0, 0, 5))
forecast_ori <- forecast(arima, h = 7)
a <- ts(train_datas)


# plots -------------------------------------------------------------------
forecast_ori %>% autoplot() + 
  autolayer(a) +
  labs(title="log forecast from ARIMA(0,0,5) with non-zero mean") +
  theme_minimal()

ggsave("www/msci_world_arima_total.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)

forecast_ori %>% autoplot() + 
  autolayer(a) +
  xlim(2100, 2150) +
  labs(title="log forecast from ARIMA(0,0,5) with non-zero mean") +
  theme_minimal()

ggsave("www/msci_world_arima.png", units = "px", bg = "white",
       height = 500, width = 1000, dpi = 120)











