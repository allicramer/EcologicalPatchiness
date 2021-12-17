simulate <- function(x1, r, n) {
  x <- rep(0, n)
  x[1] <- x1
  for (i in 2:n) {
    x[i] <- r * x[i-1] * (1 - x[i-1])
  }
  return(x)
}

png("graphics/logistic_map.png", width=4, height=3.8, units="in", res=300)
n = 15
x = simulate(0.5, 0.5, n)
plot(x, ty='l', xlim=c(0, 20), ylim=c(0, 1.1), lty=3, col="dark grey", bty="n",
    xlab="Time", ylab="Population")
text(n, x[n], paste("r = ", 0.5), pos=4, col="dark grey")
r = c(2.7, 3.1, 3.9)
col = c("black", "dark grey", "black")
lty = c(3,1,1)
for (i in 1:3) {
  x = simulate(0.5, r[i], n)
  lines(x, col=col[i], lty=lty[i])
  text(n, x[n], paste("r = ", r[i]), pos=4, col=col[i])
}
dev.off()
