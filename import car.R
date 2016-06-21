cars <­ read.table('cars.txt', header = TRUE) 
library(MASS) 
cars$symboling[cars$symboling == 2 | cars$symboling == 3] = 3 cars$symboling[cars$symboling == 0 | cars$symboling == 1] = 2 cars$symboling[cars$symboling == ­1 | cars$symboling == ­2] = 1 
cars[which(cars$symboling == 1 & cars$driveWheels == 'rwd' & cars$aspiration == 'std'),]$driveWheels[1] = '4wd' 
cars[which(cars$symboling == 1 & cars$driveWheels == 'rwd' & cars$aspiration == 'turbo'),]$driveWheels[1] = '4wd' 
cars[which(cars$symboling == 2 & cars$driveWheels == 'fwd' & cars$aspiration == 'std'),]$aspiration[1:20] = 'turbo' 
car_table <­ data.frame(expand.grid(A = factor(c('std', 'turbo')), D = factor(c('4wd', 'fwd', 'rwd')), 
S = factor(c('1', '2', '3'))), count <­ c(1, 1, 7, 1, 8, 7, 4, 3, 52, 
names(car_table)[4] <­ 'count' 
car_table 
# Fit the log­linear # Start with (A), (D), (S) 
models 
fit0 <­ loglm(count ~ A + D + S, data = car_table, param = TRUE) # Fit (DS) 
fit1 <­ loglm(count ~ A + D + S + D:S, data = 
# Fit (AS) fit2 <­ loglm(count ~ A + D + S + A:S, data = 
# Fit (AD) fit3 <­ loglm(count ~ A + D + S + A:D, data = 
# Fit (AS, DS) fit4 <­ loglm(count ~ A + D + S + A:S + D:S, 
data = car_table, param = TRUE) 
# Fit (AD, DS) fit5 <­ loglm(count ~ A + D + S + A:D + D:S, 
data = car_table, param = TRUE) 
# Fit (AD, AS) fit6 <­ loglm(count ~ A + D + S + A:D + A:S, 
data = car_table, param = TRUE) 
# Fit (AD, AS, DS) fit7 <­ loglm(count ~ A + D + S + A:D + A:S + D:S, 
data = car_table, param = TRUE) 
# Fit (ADS) fit8 <­ loglm(count ~ A + D + S + A:D + A:S + D:S + A:D:S, 
data = car_table, param = TRUE) library(stats) 
extractAIC(fit0) extractAIC(fit1) extractAIC(fit2) extractAIC(fit3) extractAIC(fit4) extractAIC(fit5) extractAIC(fit6) extractAIC(fit7) extractAIC(fit8) 
fit0 fit1 fit2 fit3 fit4 fit5 fit6
fit7 fit8
stepAIC(fit8, scope = list(lower = ~1, upper = ~ A + D + S + A:D + A:S + D:S + A:D:S), 
direction = "both") 
stepAIC(fit8, scope = list(lower = ~1, upper = ~ A + D + S + A:D + A:S + D:S + A:D:S), 
direction = "both", k = log(205.5)) anova(fit0, fit4, test = "Chisq") 
 


