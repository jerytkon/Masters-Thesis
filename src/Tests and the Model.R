### Here is the augmented dickey fuller tests of the data

urtest <- ur.df(diff(log_NetWeight.ts, 1), type = "trend", lags = 1)
summary(urtest)
urtest <- ur.df(log_NetWeight.ts, type = "trend", lags = 1)
summary(urtest)


urtest <- ur.df(diff(log_BSE.ts, 1), type = "trend", lags = 1)
summary(urtest)
urtest <- ur.df(log_BSE.ts, type = "trend", lags = 1)
summary(urtest)

urtest <- ur.df(diff(log_UnitPrice.ts, 1), type = "trend", lags = 1)
summary(urtest)
urtest <- ur.df(log_UnitPrice.ts, type = "trend", lags = 1)
summary(urtest)
ts.plot(diff(log_gdp.ts, 1))

urtest <- ur.df(diff(log_gdp.ts, 1), lags = 1)
summary(urtest)

### Zivot-Andrews test for GDP due to ADF showing that Order of Integration would be greater than 1
za.est <- ur.za(diff(log_gdp.ts, 1), model = "both", lag = 0)  # white noise
summary(za.est)




### Creating the dataframe with all the variables. In order to test different areas for imports the data needs to be changed in data section.
df.ts <- ts.union(log_UnitPrice.ts, log_gdp.ts, log_NetWeight.ts, log_BSE.ts,log_RoWunitprice.ts, log_slaughterings.ts, log_EUNetWeight.ts, EUShare)

### ARDL model specification
models <- auto_ardl(log_NetWeight.ts ~ log_UnitPrice.ts + log_gdp.ts + log_BSE.ts + log_slaughterings.ts, data = df.ts, starting_order = c(1,1, 1, 1, 1), max_order = c(2,2,2,2,2), selection = "AIC")


### Deciding the lag length of the variables
models$top_orders
AICc(models$best_model)


### Specifying the ARDL model
ardl_111 <- ardl(log_NetWeight.ts ~ log_UnitPrice.ts + log_BSE.ts + log_gdp.ts + log_slaughterings.ts, data = df.ts, order = c(1,1, 1, 1,2))
summary(ur.df(diff(log_BSE.ts, 1)))

### Inspecting the residuals for serial correlation
acf(ardl_111$residuals)


uecm_111 <- uecm(ardl_111)
summary(uecm_111)

### Testing for cointegration
bounds_f_test(ardl_555, case = 2)
tbounds <- bounds_t_test(uecm_111, case = 3, alpha = 0.10)
tbounds



