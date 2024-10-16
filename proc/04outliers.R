#---- Revisión outliers ----

library(tidyverse)
library(openxlsx)

load('input/data/database_FDL_AC.RData')

names(db)

rmw <- ggplot(db, aes(rmw)) + geom_freqpoly()

summary(db$GDP)

boxplot(db$GDP, main="Boxplot GDP", ylab="Valores")

Q1 <- quantile(db$LF_RATE_M, 0.25, na.rm = TRUE)
Q3 <- quantile(db$LF_RATE_M, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers <- db$LF_RATE_M[db$LF_RATE_M< (Q1 - 1.5 * IQR) | db$LF_RATE_M > (Q3 + 1.5 * IQR)]
