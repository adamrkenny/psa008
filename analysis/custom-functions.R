## create custom function to truncate values
## as bias ranges from 0 to 10
rtruncnorm <- function(N, mean = 0, sd = 1, min = -Inf, max = Inf) {

    if (min > max) stop("Error: Truncation range is empty");
  
    U <- runif(N, pnorm(min, mean, sd), pnorm(max, mean, sd));
    
    qnorm(U, mean, sd); 

}


## custom dotplot, from: https://stackoverflow.com/a/16511206
## re = object of class ranef.mer
gg_caterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
    require(ggplot2)
    f <- function(x) {
        pv   <- attr(x, "postVar")
        cols <- 1:(dim(pv)[1])
        se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
        ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
        pDf  <- data.frame(y=unlist(x)[ord],
                           ci=1.96*se[ord],
                           nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                           ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                           ind=gl(ncol(x), nrow(x), labels=names(x)))

        if(QQ) {  ## normal QQ-plot
            p <- ggplot(pDf, aes(nQQ, y))
            p <- p + facet_wrap(~ ind, scales="free")
            p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
        } else {  ## caterpillar dotplot
            p <- ggplot(pDf, aes(ID, y)) + coord_flip()
            if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
                p <- p + facet_wrap(~ ind)
            } else {           ## different scales for random effects
                p <- p + facet_grid(ind ~ ., scales="free_y")
            }
            p <- p + xlab("Levels") + ylab("Random effects")
        }

        p <- p + theme_bw()
        p <- p + theme(legend.position="none")
        p <- p + geom_hline(yintercept=0)
        p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
        p <- p + geom_point(aes(size=1.2), colour="grey30") 
        return(p)
    }

    lapply(re, f)
}