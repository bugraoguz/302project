# 302 paper notes --------------------

fwrite(last_data_for_plm, file = "C:/Users/90535/Desktop/Buğra/econ 7th/302/paper data/302_data.csv")

# 302 paper applications ------------------

library(data.table)
library(ggplot2)
library(stargazer)
library(dplyr)
library(readxl)
library(tidyverse)

path <- "C:/Users/90535/Desktop/Buğra/econ 7th/302/paper data/iki.xlsx"
mainData <- read_excel(path)
setDT(mainData)
mainData <- mainData[c(1:22)]
mainDataCopy <- mainData
melt(mainData, id.vars= "Country Name", measure.vars= c("Series Name", "Series Code"))


# Wide-to-long -----------------------

str(mainData)

mainData[, c(5:41) := lapply(.SD, as.numeric), .SDcols = c(5:41)]


mainDataLong <- mainData %>%
  pivot_longer(
    cols = matches("\\d{4}"),
    names_to = "year",
    values_to = "value"
  )

mainDataLongCopy <- mainDataLong

setDT(mainDataLong)
mainDataLong[, "year" := lapply(.SD, as.numeric), .SDcols = "year"]

mainDataLong <- mainDataLong %>%
  # Clean up the year column by removing [YR****]
  mutate(
    year = as.numeric(str_extract(year, "\\d{4}"))
  ) %>%
  # Arrange the data by country and year
  arrange(`Country Name`, `year`)

str(mainDataLong)

mainDataLong2 <- NULL
mainDataLongCopy <- mainDataLong

class(mainDataLong)

mainDataLong$country <- mainDataLong$`Country Name`
mainDataLong$`Country Name` <- NULL
mainDataLong$country_code <- mainDataLong$`Country Code`
mainDataLong$`Country Code` <- NULL
mainDataLong$series_name <- mainDataLong$`Series Name`
mainDataLong$`Series Name` <- NULL
mainDataLong$series_code <- mainDataLong$`Series Code`
mainDataLong$`Series Code` <- NULL

class(mainDataLong)

gdp_growth <- mainDataLong[series_name == "GDP growth (annual %)"]
gross_capital_formation <- mainDataLong[series_name == "Gross capital formation (% of GDP)"]
patent_resident <- mainDataLong[series_name == "Patent applications, residents"]
patent_n_resident <- mainDataLong[series_name == "Patent applications, nonresidents"]
fdi <- mainDataLong[series_name == "Foreign direct investment, net inflows (% of GDP)"]
unemp <- mainDataLong[series_name == "Unemployment, total (% of total labor force) (national estimate)"]
fdi_bop <- mainDataLong[series_name == "Foreign direct investment, net (BoP, current US$)"]
net_trade <- mainDataLong[series_name == "Net trade in goods (BoP, current US$)"]
gov_exp_edu <- mainDataLong[series_name == "Government expenditure on education, total (% of GDP)"]
inf <- mainDataLong[series_name == "Inflation, GDP deflator (annual %)"]
pop <- mainDataLong[series_name == "Population, total"]


p_mainDataLong$country_code <- NULL
p_mainDataLong$series_code <- NULL
  

rm(p_mainDataLongFinal)
p_mainDataLongFinal <- p_mainDataLong %>%
  pivot_wider(
    names_from = series_name,
    values_from = value
  )

# Regression --------------------

library(plm)
library(lme4)
library(dynlm)

last_data_for_plm <- pdata.frame(p_mainDataLongFinal, index = c("country", "year"))
colnames(last_data_for_plm)

last_data_for_plm <- last_data_for_plm %>%
  rename(gdp_growth = "GDP.growth..annual...",
         gross_capital_fromation = "Gross.capital.formation....of.GDP.",
         patent_resident = "Patent.applications..residents",
         patent_n_resident = "Patent.applications..nonresidents",
         fdi = "Foreign.direct.investment..net.inflows....of.GDP.",
         unemp = "Unemployment..total....of.total.labor.force...national.estimate.",
         fdi_bop = "Foreign.direct.investment..net..BoP..current.US..",
         net_trade = "Net.trade.in.goods..BoP..current.US..",
         gov_exp_edu = "Government.expenditure.on.education..total....of.GDP.",
         inf = "Inflation..GDP.deflator..annual...",
         pop = "Population..total")

class(last_data_for_plm)

######################

class(last_data_for_plm)

last_data_for_plm$patent_total <- last_data_for_plm$patent_resident + last_data_for_plm$patent_n_resident
last_data_for_plm$patent_total_p_pop <- last_data_for_plm$patent_total / last_data_for_plm$pop
last_data_for_plm$pop_growth <- (last_data_for_plm$pop - shift(last_data_for_plm$pop))/last_data_for_plm$pop
last_data_for_plm$patent_resident_p_pop <- last_data_for_plm$patent_resident / last_data_for_plm$pop
last_data_for_plm$patent_n_resident_p_pop <- last_data_for_plm$patent_n_resident / last_data_for_plm$pop


## pop_growth bakacaksam 1986dan itibaren bakmalıyım   !!!!!!!!

#model 1 === patent_total + investment

last_data_for_plm_us <- last_data_for_plm[last_data_for_plm$country == "United States", ,]


# descriptive stats -----------
summary_china <- as.data.frame(summary(last_data_for_plm_china))
summary_us <- as.data.frame(summary(last_data_for_plm_us))
summary(last_data_for_plm)

ggplot(last_data_for_plm, aes(year, patent_total)) +
  geom_point(aes(group = country, color = country)) +
  geom_line(aes(group = country))


# model 1 ------------

#model 1 === patent_total
#model 1.1 === patent_total_p_pop

model1 <- lm( gdp_growth ~ patent_total, data = last_data_for_plm)
summary(model1)
model1.1 <- lm( gdp_growth ~ patent_total_p_pop, data = last_data_for_plm)
summary(model1.1)
model1.1.china <- lm( gdp_growth ~ patent_total_p_pop, data = last_data_for_plm_china)
summary(model1.1.china)
model1.1.us <- lm( gdp_growth ~ patent_total_p_pop, data = last_data_for_plm_us)
summary(model1.1.us)
model1.china <- lm( gdp_growth ~ patent_total, data = last_data_for_plm_china)
summary(model1.china)
plot(model1.china)
model1.us <- lm( gdp_growth ~ patent_total, data = last_data_for_plm_us)
summary(model1.us)


# model 2 --------------

#model 2 === patent_resident + patent_n_resident
#model 2.1 === patent_resident_p_pop + patent_n_resident_p_pop

model2 <- lm( gdp_growth ~ patent_resident + patent_n_resident, data = last_data_for_plm)
summary(model2)
model2.1 <- lm( gdp_growth ~ patent_resident_p_pop + patent_n_resident_p_pop, data = last_data_for_plm)
summary(model2.1)
model2.1.china <- lm( gdp_growth ~ patent_resident_p_pop + patent_n_resident_p_pop, data = last_data_for_plm_china)
summary(model2.1.china)
model2.1.us <- lm( gdp_growth ~ patent_resident_p_pop + patent_n_resident_p_pop, data = last_data_for_plm_us)
summary(model2.1.us)
model2.china <- lm( gdp_growth ~ patent_resident + patent_n_resident, data = last_data_for_plm_china)
summary(model2.china)
model2.us <- lm( gdp_growth ~ patent_resident + patent_n_resident, data = last_data_for_plm_us)
summary(model2.us)

stargazer(model1, model1.1, model2, model2.1, type = "text")
stargazer(model1.china, model1.1.china, model2.china, model2.1.china, type = "text")
stargazer(model1.us, model1.1.us, model2.us, model2.1.us, type = "text")

plot(model1.us, model1.1.us)

#model 3 ------------

#model 3 === patent_total_p_pop + gross_capital_formation + fdi + unemp + net_trade + inf

model3 <- lm( gdp_growth ~ patent_total_p_pop + gross_capital_fromation + fdi +
                unemp + net_trade +inf, data = last_data_for_plm)
summary(model3)
model3.china <-lm( gdp_growth ~ patent_total_p_pop + gross_capital_fromation + fdi +
                                unemp + net_trade +inf, data = last_data_for_plm_china)
summary(model3.china)
model3.us <-lm( gdp_growth ~ patent_total_p_pop + gross_capital_fromation + fdi +
                     unemp + net_trade +inf, data = last_data_for_plm_us)
summary(model3.us)

#model 4 ----------

#model 4 === patent total_p_pop + gross_capital_formation

model4 <- lm( gdp_growth ~ patent_total_p_pop + gross_capital_fromation,
              data = last_data_for_plm)
summary(model4)
model4.china <- lm( gdp_growth ~ patent_total_p_pop + gross_capital_fromation,
                    data = last_data_for_plm_china)
summary(model4.china)
model4.us <- lm( gdp_growth ~ patent_total_p_pop + gross_capital_fromation,
              data = last_data_for_plm_us)
summary(model4.us)


# new model ------------

model_patent_investment <- lm( gdp_growth ~ patent_total + gross_capital_fromation, data = last_data_for_plm_china)
summary(model_patent_investment)
plot(model_patent_investment)


# tests --------------

#adf test

library(tseries)
diff_patent_china <- na.omit(diff(last_data_for_plm_china$patent_total))
diff_growth_china
adf.test(log(last_data_for_plm_china$patent_total), k=0)

diff2_patent_china <- na.omit(diff(diff_patent_china))


growth_diff_patent_investment_china <- data.table(shift(last_data_for_plm_china$gdp_growth, n=-1L), diff_patent_china, diff_gross_capital_formation)
growth_diff_patent_investment_china <- growth_diff_patent_investment_china[c(1:36)]
names(growth_diff_patent_investment_china) <- c("gdp_growth", "diff_patent_china", "diff_gross_capital_formation")
model1.china.updated <- lm( gdp_growth ~ diff_patent_china, data = growth_diff_patent_china)
summary(model1.china.updated)

#####

adf.test(last_data_for_plm_china$gross_capital_fromation, k=2)
diff_gross_capital_formation <- na.omit(diff(last_data_for_plm_china$gross_capital_fromation))
diff_gross_capital_formation

adf.test(growth_diff_patent_investment_china$diff_gross_capital_formation, k=2)

diff2_gross_capital_formation <- na.omit(diff(diff_gross_capital_formation))
adf.test(diff2_gross_capital_formation, k=2)
adf.test(log(last_data_for_plm_china$gross_capital_fromation))

#new model -----------

model_patent_investment_updated <- lm( gdp_growth ~ diff_patent_china + diff_gross_capital_formation, data = growth_diff_patent_investment_china)
summary(model_patent_investment_updated)

#tets continue ---------

# durbin watson test
library(car)
durbinWatsonTest(model1.china)

durbinWatsonTest(model_patent_investment)

#Breusch-Godfrey test
library(lmtest)
bgtest(model1.china.updated)

bgtest(model_patent_investment_updated)

#acf, pacf plots
library("forecast")
acf(residuals(model1.china))
pacf(residuals(model1.china))

acf(residuals(model1.china.updated))
pacf(residuals(model1.china.updated))


acf(residuals(model_patent_investment))
pacf(residuals(model_patent_investment))

acf(residuals(model_patent_investment_updated))
pacf(residuals(model_patent_investment_updated))

# arima applications new

ma1_old <- Arima(residuals(model_patent_investment), order = c(0, 0, 1), include.mean = F, include.drift = F)
ma1_old
checkresiduals(ma1_old)

auto.arima(residuals(model_patent_investment))

Arima(residuals(model_patent_investment), order = c(0, 0, 2), include.mean = F, include.drift = F)
checkresiduals(Arima(residuals(model_patent_investment), order = c(0, 0, 2), include.mean = F, include.drift = F))


arma12_new <- Arima(residuals(model_patent_investment_updated), order = c(1, 0, 2), include.mean = F, include.drift = F)
arma12_new
checkresiduals(arma12_new)

ma1_new <- auto.arima(residuals(model_patent_investment_updated))
checkresiduals(ma1_new)


#arima application old -------------

auto.arima(residuals(model1.china))
checkresiduals(auto.arima(residuals(model1.china)))

arma11_model1_china <- Arima(residuals(model1.china), order = c(1, 0 ,1))
arma11_model1_china
checkresiduals(arma11_model1_china)

arma12_model1_china <- Arima(residuals(model1.china), order = c(1, 0 ,2))
arma12_model1_china
checkresiduals(arma12_model1_china)

arma22_updated1_model_china <- Arima(residuals(model1.china.updated), order = c(2, 0, 2))
arma22_updated1_model_china
checkresiduals(arma22_updated1_model_china)

ma2_updated_model1_china <- auto.arima(residuals(model1.china.updated))
checkresiduals(auto.arima(residuals(model1.china.updated)))

ma2_updated_model1_china


# breusch-pagan test -------------
bptest(model1.china.updated)

bptest(model_patent_investment)
bptest(model_patent_investment_updated)

# white test
bptest(model1.china.updated, studentize = FALSE)

bptest(model_patent_investment, studentize = F)
bptest(model_patent_investment_updated, studentize = F)

# goldfeld-quandt test
gqtest(model1.china.updated)

gqtest(model_patent_investment)
gqtest(model_patent_investment_updated)


# hcse 
library(sandwich)

coeftest(model_patent_investment_updated, vcov = vcovHC(model_patent_investment_updated, type = "HC1"))
summary(model_patent_investment_updated)

# variance inflation factor

vif(model_patent_investment_updated)

# correlation matrix
install.packages("corrplot")
library(corrplot)

cor_matrix_2 <- cor(last_data_for_plm_china[, c("patent_total", "gross_capital_fromation")])
cor_matrix_2

# ramsey RESET test

?resettest
resettest(model_patent_investment_updated)
summary(model_patent_investment_updated)
# ARCH test
install.packages("vars")
library("vars")
arch.test(model_patent_investment)

# cochrane orcutt test
install.packages("orcutt")
library(orcutt)
cochrane.orcutt.test(model_patent_investment)

# engle-granger co-integration
install.packages("urca")
library(urca)

cointegration_data <- cbind(growth_diff_patent_investment_china$gdp_growth, growth_diff_patent_investment_china$diff_patent_china, growth_diff_patent_investment_china$diff_gross_capital_formation, residuals)

cointegration_test <- ca.jo(cointegration_data)
summary(cointegration_test)



# comparisons



#China
stargazer(model1.1.china, model1.china, model2.china, model2.1.china, model3.china, model4.china, type = "text")

#fixed models -------------

fixed_model1 <- plm( gdp_growth ~ patent_total, data = last_data_for_plm, model = "within")
summary(fixed_model1)
str(fixed_model1)


# Fit a fixed effects model
fe_model1 <- plm(gdp_growth ~ inf + pop + (patent_n_resident + patent_resident),
                data = last_data_for_plm, model = "within")

## adding factor(Year) as a variable controls for shocks that effect each entity

re_model1 <- plm(gdp_growth ~ inf + pop + (patent_n_resident + patent_resident),
                 data = last_data_for_plm, model = "pooling")

##### diğer modellere de bak bakayım nasıl geliyor.




# instrumental variable models for innovation - patent ---------------
install.packages("AER")
library(AER)














#model 1 === patent_total
#model 1.1 === patent_total_p_pop
#model 2 === patent_resident + patent_n_resident
#model 2.1 === patent_resident_p_pop + patent_n_resident_p_pop
#model 3 === patent_total_p_pop + gross_capital_formation + fdi + unemp + net_trade + inf
#model 4 === patent total_p_pop + gross_capital_formation
# diğer modelleri model3'te çıkan sonuçlara göre seçebilirm


