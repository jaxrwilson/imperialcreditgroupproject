---
  title: "GroupAss"
output: html_document
---
  
rm(CREDIT)

knitr::opts_chunk$set(echo = TRUE)

library(forecast)
library(tsibble)
library(lubridate)
library(fpp3)



CREDIT <- credit

CREDIT$month <- seq(as.Date('2021/12/1'), by = "-1 month", length.out = 492)
CREDIT$month <- yearmonth(CREDIT$month)
CREDIT <- tsibble(CREDIT, index = month)

CREDIT_Ex <- credit
CREDIT_Ex$month <- seq(as.Date('2021/12/1'), by = "-1 month", length.out = 492)
CREDIT_Ex$month <- yearmonth(CREDIT$month)
CREDIT_Ex <- tsibble(CREDIT, index = month)


gg_tsdisplay(CREDIT_Ex, plot_type = 'partial')


fit <- CREDIT_Ex %>%
  model(ARIMA(credit_in_millions ~ pdq(0,0,0)))
report(fit)

CREDIT_Ex %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ARIMA(credit_in_millions),
    ETS(credit_in_millions)
  ) %>%
  forecast(h = 1) %>%
  accuracy(CREDIT_Ex) %>%
  select(.model, RMSE)


fit %>%
  forecast(h=12) %>%
  autoplot(CREDIT_Ex) +
  labs(title = "Imperial Credit Prediction",
       y = "Credit (millions)")

