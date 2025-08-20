
rm(list=ls())

x <- data.frame(
  year = 1:20,
  salary = 100
)

for(i in 2:nrow(x)){
  x$salary[i] <- x$salary[i-1] * 1.02
}

x$savings <- x$salary*0.06
x$TRS <- ifelse(x$year < 10, x$savings, x$savings + x$savings*0.2114)
x$TRS_total <- ifelse(x$year < 10, cumsum(x$savings), cumsum(x$savings + x$savings*0.2114))
  

x$ORP <- x$savings + x$savings*0.0924
x$ORP_total <- cumsum(x$ORP)

plot(x$year, x$TRS_total, type = "l", lwd = 4)
lines(x$year, x$ORP_total, lty = 2, lwd = 4)

x$diff <- x$TRS_total - x$ORP_total

plot(x$year, x$diff, type = "l", lwd = 4)
abline(v = 10, lty = 2, lwd = 2)
abline(h = 0, lty = 2, lwd = 2)

x$diff <- x$TRS - x$ORP

x$diff[20] / x$salary[20]

mean(x$diff*1000)



plot(x$year, x$diff)
