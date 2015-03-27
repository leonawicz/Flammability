


##
##
## fseMLE.R

Maximum likelihood estimation.




### Maximum likelihood estimation


```r
# test with statewide forest all years observed data
d.sf <- subset(d, Domain == "Statewide" & Vegetation == "Forest" & Replicate %in% 
    c("Observed"), select = c(2, 5, 7))
x <- d.sf$FSE + runif(nrow(d.sf), -0.95, 0.95)
lx <- log(x)

n2loglik <- function(fun, x, params) -2 * sum(log(do.call(fun, list(x = x, params))))
pars <- optim(fn = n2loglik, fun = dlnorm, x = x, par = c(meanlog = 1, sdlog = 1))$par

drlx <- diff(range(lx))
xlm <- range(lx) + 0.3 * drlx * c(-1, 1)
sq <- seq(xlm[1], xlm[2], length = 200)
hist(lx, freq = F, xlim = xlm, main = "log(1950-2009 obs. sw. forest fire size)", 
    col = "gray40", cex.lab = 1.3, cex.axis = 1.3)
lines(sq, dnorm(x = sq, pars[1], pars[2]), lwd = 2)
legend("topright", lwd = 2, paste("MLE: meanlog =", round(pars[1], 2), "\nMLE: sdlog =", 
    round(pars[2], 2)), bty = "n")
```

![](fse_mle_files/figure-html/mle-1.png) 
