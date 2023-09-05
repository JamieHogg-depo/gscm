
m1 <- 'f1 =~ 1*activityleiswkpl + alcohol + diet + overweight+ smoking
       f2 =~ 1*alcohol + diet + overweight+ smoking'
fit <- cfa(m1, data = data,
           std.lv = TRUE,
           auto.cov.lv.x=FALSE)
t <- summary(fit, fit.measures=TRUE, standardized=TRUE)
l1 <- predict(fit)
t$fit["aic"]

m1 <- 'f1 =~ 1*smoking + alcohol + diet + overweight+ activityleiswkpl
       f2 =~ 1*alcohol + diet + overweight+ activityleiswkpl'
fit <- cfa(m1, data = data,
           std.lv = TRUE,
           auto.cov.lv.x=FALSE)
t <- summary(fit, fit.measures=TRUE, standardized=TRUE)
l2 <- predict(fit)
t$fit["aic"]

m1 <- 'f1 =~ 1*smoking + alcohol + diet + activityleiswkpl+overweight
       f2 =~ 1*alcohol + diet + activityleiswkpl+overweight'
fit <- cfa(m1, data = data,
           std.lv = TRUE,
           auto.cov.lv.x=FALSE)
t <- summary(fit, fit.measures=TRUE, standardized=TRUE)
l3 <- predict(fit)
t$fit["aic"]

## Single Factor ## ------------------------------------------------------------

# not identified
m1 <- 'f1 =~ 1*activityleiswkpl + alcohol + diet + overweight+ smoking'
fit1 <- cfa(m1, data = data, std.lv = TRUE)
t1 <- summary(fit1, fit.measures=TRUE)
l1 <- predict(fit1)
t1$fit["aic"]

m2 <- 'f1 =~ 1*alcohol + diet + overweight+ smoking + activityleiswkpl'
fit2 <- cfa(m2, data = data, std.lv = TRUE)
t2 <- summary(fit2, fit.measures=TRUE)
l2 <- predict(fit2)
t2$fit["aic"]

# Compare factor
plot(l1[,1], l2[,1]); abline(a=0, b= 1)

'
Using marker method - first loading equal to 1
- different aic and latent values

Different orders but no fixing of first loading
- same aic and predicted values

Only the order of the first two matter
'

## Based on least correlation ## -----------------------------------------------

tg <- cor(data)
which(tg == (-1*min(abs(tg))), arr.ind=TRUE)

# diet and alcohol are the least correlated
# Based on P0389 we would put: diet, alcohol

# New development: Diet and smoking are the least correlated
# Based on P0389 we would put: smoking, diet - smoking is first as it has the largest variance

## Based on PCA ## -------------------------------------------------------------

pp <- prcomp(data, scale. = TRUE)
pp$rotation
summary(pp)

# Based on PCA we would put: smoking, alcohol
# First two principal components capture 73% of variation

