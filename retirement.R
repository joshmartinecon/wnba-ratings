
rm(list=ls())

x <- data.frame(
  year = 1:30,
  salary = 100
)

for(i in 2:nrow(x)){
  x$salary[i] <- x$salary[i-1]*1.02
}

#### TRS #####

x$savings <- x$salary*0.06
x$TRS <- x$savings
x$TRS_total <- cumsum(x$TRS)
x$TRS_10 <- x$savings + x$salary*0.2114
x$TRS_10_total <- cumsum(x$TRS_10)
x$TRS_10_total[1:9] <- x$TRS_total[1:10]
x$TRS_benefit <- 0.02*x$salary*x$year*20
x$TRS_benefit[1:9] <- 0

x$TRS_10_total <- ifelse(x$year < 10, x$TRS_10_total, x$TRS_benefit)

##### ORP #####

x$ORP <- x$savings + x$salary*0.0924

x$ORP_total_low <- x$ORP[1] * 1.03
for(i in 2:nrow(x)){
  x$ORP_total_low[i] <- (x$ORP_total_low[i-1] + x$ORP[i])*1.03
}

x$ORP_total_med <- x$ORP[1] * 1.06
for(i in 2:nrow(x)){
  x$ORP_total_med[i] <- (x$ORP_total_med[i-1] + x$ORP[i])*1.06
}

x$ORP_total_high <- x$ORP[1] * 1.09
for(i in 2:nrow(x)){
  x$ORP_total_high[i] <- (x$ORP_total_high[i-1] + x$ORP[i])*1.09
}

x$diff_low <- ifelse(x$year < 10, x$TRS_10_total, x$TRS_10_total / (1+0.03)^(30-x$year)) - 
  x$ORP_total_low 
x$diff_med <- ifelse(x$year < 10, x$TRS_10_total, x$TRS_10_total / (1+0.06)^(30-x$year)) - 
  x$ORP_total_med 
x$diff_high <- ifelse(x$year < 10, x$TRS_10_total, x$TRS_10_total / (1+0.09)^(30-x$year)) - 
  x$ORP_total_high

par(mar = c(4.5, 4.5, 1, 1))
plot(rep(x$year, 3), c(x$diff_low, x$diff_med, x$diff_high),
     xlab = "Years at UWG", ylab = "Difference in Earnings ($100k): TRS vs ORP",
     cex.axis = 1.25, cex.lab = 1.5)
abline(h = 0, lwd = 1000000, col = "aliceblue")
abline(h = seq(-1500, 0, 500), lwd = 4, col = "white")
abline(v = seq(0, 30, 5), lwd = 4, col = "white")
abline(h = 0, lwd = 4, lty = 2)
lines(x$year, x$diff_low, lwd = 8, col = "red")
lines(x$year, x$diff_med, lwd = 8, col = "yellow")
lines(x$year, x$diff_high, lwd = 8, col = "green")

x$prob_leave <- log(seq_len(nrow(x))) / log(nrow(x))

x$ev_diff_low <- x$diff_low * (x$prob_leave)
x$ev_diff_med <- x$diff_med * (x$prob_leave)
x$ev_diff_high <- x$diff_high * (x$prob_leave)

plot(rep(x$year, 3), c(x$ev_diff_low, x$ev_diff_med, x$ev_diff_high),
     xlab = "Years at UWG", ylab = "Exp. Difference in Earnings ($100k): TRS vs ORP",
     cex.axis = 1.25, cex.lab = 1.5)
abline(h = 0, lwd = 1000000, col = "aliceblue")
abline(h = seq(-60, 20, 20), lwd = 4, col = "white")
abline(v = seq(0, 30, 5), lwd = 4, col = "white")
abline(h = 0, lwd = 4, lty = 2)
lines(x$year, x$ev_diff_low, lwd = 8, col = "red")
lines(x$year, x$ev_diff_med, lwd = 8, col = "yellow")
lines(x$year, x$ev_diff_high, lwd = 8, col = "green")
