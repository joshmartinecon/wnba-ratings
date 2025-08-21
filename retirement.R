
rm(list=ls())

x <- data.frame(
  year = 1:30,
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

par(mar = c(4.5, 4.5, 1, 1))
plot(x$year, x$TRS_total, type = "l", lwd = 4,
     xlab = "Years at UWG", ylab = "Retirement Savings ($1k)",
     cex.axis = 1.25, cex.lab = 1.5)
lines(x$year, x$ORP_total, lty = 2, lwd = 4)
legend("topleft",
       legend = c("TRS", "ORP"),
       lwd = 2, lty = c(1, 2), bty = "n",
       cex = 1.25)

x$diff <- x$TRS_total - x$ORP_total

plot(x$year, x$diff, type = "l", lwd = 4,
     cex.axis = 1.25, cex.lab = 1.5,
     xlab = "Years at UWG", ylab = "Diff. in Savings ($1k)")
abline(v = 10, lty = 2, lwd = 2)
abline(h = 0, lty = 2, lwd = 2)

x$diff <- x$TRS_total - x$ORP_total
z <- list()
for(i in seq(0, 1, 0.01)){
  x$prob_leave <- i * log(seq_len(nrow(x))) / log(nrow(x))
  z[[length(z)+1]] <- data.frame(
    prob_leave = i,
    ev_trs = sum(x$diff*(1-x$prob_leave))
  )
}
z <- as.data.frame(do.call(rbind, z))

plot(x$year, x$prob_leave, type = "l", lwd = 4,
     xlab = "Years at UWG", ylab = "Cumulative Prob. of Exit",
     cex.axis = 1.25, cex.lab = 1.5)

plot(x$year, (1-x$prob_leave)*x$diff, type = "l", lwd = 4,
     xlab = "Years at UWG", ylab = "E[Diff. in Savings ($1k)]",
     cex.axis = 1.25, cex.lab = 1.5)
abline(h = 0, lty = 2)
abline(v = 10, lty = 2)

x$break_even <- cumsum((1-x$prob_leave)*x$diff)
# x$break_even <- cumsum(x$diff)
plot(x$year, x$break_even, type = "l", lwd = 4,
     xlab = "Years at UWG", ylab = "Sum(E[Diff. in Savings])",
     cex.axis = 1.25, cex.lab = 1.5)
abline(h = 0, lty = 2)
abline(v = 14, lty = 2)
