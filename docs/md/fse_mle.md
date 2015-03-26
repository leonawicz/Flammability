


##
##
## fseMLE.R

Maximum likelihood estimation.




### Maximum likelihood estimation


```r
n2loglik <- function(fun, params = list()) -2 * sum(log(do.call(fun, params)))


# p04a <- ggplot(data=d.sf, aes(x=FSE)) + geom_histogram() + facet_wrap(~
# Decade, ncol=2, scale='free_y') #+ scale_fill_manual(values=cbpal) +
# theme(axis.text.x=element_text(angle=45, hjust=1)) p04a

# if(i<=ncol(vars.p[[j]])){ tmp.pars <- optim(fn=n2loglik,par=c(5,0.2))$par;
# tmp.pars } else { tmp.pars <- optim(fn=n2loglik,par=c(100,2.5))$par;
# tmp.pars }
```
