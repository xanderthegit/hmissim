library(pryr)

validateVar <- function(var, compendium, simdata, threshold=.05, include.plot=F) {
    # review simulation of variable, test simulated vs theoretical values
    # plot simulated vs theoretical w/ p value for given test
    #
    # Args: 
    #   var: simulated variable
    #   compendium:   compendium used to simulate data
    #   simdata:   data simulated
    #   threshold:  sigma for rejecting null hypothesis
    #
    # Returns:
    #   p:  plot 
    type <- compendium[['TYPE']][compendium[['VARIABLE']] == var]
    vector <- simdata[[var]]
    vector <- na.omit(vector)
    
    titleHelper <- function() {
        # prep string for plot title
        if (p < threshold) {
            flag <- "\n Reject N_0"
        } else {flag <- ""}
        title <- paste0("Var=", var, "\n p=", round(p, 6), flag)
        title
    }
    
    if (type == "enum") {
        vector <- factor(vector, 
                         levels=(convertToList(
                             compendium[['CHOICES']][compendium[['VARIABLE']] == var])))
        prob <- as.numeric(convertToList(compendium[['PROBS']][compendium[['VARIABLE']] == var]))
        tab.Enum <- table(vector)
        
        # validation test using chisquare test
        chi.test <- chisq.test(tab.Enum, p=prob)
        p <- chi.test$p.value
        
        # plot
        title <- titleHelper()
        pt %<a-% {
            dfbar <- barplot(prop.table(tab.Enum), ylab="", main=title, las=2)
            lines(x = dfbar, y = prob, col="red")
            points(x = dfbar, y = prob, col="red")
        }
        
    } else if (type == "boolean") {
        vector <- factor(vector, levels=c(TRUE, FALSE))
        prob <- as.numeric(convertToList(compendium[['PROBS']][compendium[['VARIABLE']] == var]))
        tab.Bool <- table(vector)
        
        # validation test using binomial test
        bi.test <- binom.test(tab.Bool[[1]], length(vector), prob[[1]],alternative="two.sided")
        p <- bi.test$p.value
        
        # plot 
        title <- titleHelper()
        pt %<a-% {
            pp <- barplot(prop.table(tab.Bool), main=title)
            lines(x = pp, y = prob, col="red")
            points(x = pp, y = prob, col="red")
        }
        
    } else if (type == "number") {
        dist <- compendium[['DISTRIB']][compendium[['VARIABLE']] == var]
        row <- compendium[compendium[['VARIABLE']]==var,]
        dist.vals <- distPrep(row, full.output = TRUE)
        
        if (dist == "normal") {
            # validation test using Kolmogorov-Smirnov test
            k.test <- ks.test(vector, "pnorm", dist.vals[[1]], dist.vals[[2]])
            p <- k.test$p.value
            
            # plot
            title <- titleHelper()
            pt %<a-% {
                pp <- hist(vector, freq=F, main=title, xlab="", ylab="")
                curve(dnorm(x, dist.vals[[1]], dist.vals[[2]]), add=TRUE, col='red')
            }
            
        } else if (dist == "uniform") {
            # validation test using Kolmogorov-Smirnov test
            k.test <- ks.test(vector, "punif", dist.vals[[1]], dist.vals[[2]])
            p <- k.test$p.value
            
            # plot
            title <- titleHelper()
            pt %<a-% {
                pp <- hist(vector, freq=F, main=title, xlab="", ylab="")
                curve(dunif(x, dist.vals[[1]], dist.vals[[2]]), add=TRUE, col='red')
            }
            
        } else if (dist == "exponential") {
            # validation test using Kolmogorov-Smirnov test
            k.test <- ks.test(vector, "pexp", dist.vals[[1]])
            p <- k.test$p.value
            
            # plot
            title <- titleHelper()
            pt %<a-% {
                pp <- hist(vector, freq=F, main=title, xlab="", ylab="")
                curve(dexp(x, dist.vals[[1]]), add=TRUE, col='red')
            }
        }
        
    } else if (type == "string") {
        #pt <- 1
        p <- 1
    } else if (type == "integer") {
        
        dist <- compendium[['DISTRIB']][compendium[['VARIABLE']] == var]
        row <- compendium[compendium[['VARIABLE']]==var,]
        dist.vals <- distPrep(row, full.output = TRUE)
        
        if (dist == "normal") {
            p <- 1    
            
        } else if (dist == "poisson") {
            # validation test using chisquare test
            tab.Pois <- table(vector)
            nam <- names(tab.Pois)
            l <- unlist(c(as.list(tab.Pois), 0))
            probs <- dpois(as.numeric(unlist(nam)), dist.vals[[1]])
            comp <- 1 - sum(probs)
            chi.test <- chisq.test(l, p=c(probs, comp), simulate.p.value = T)
            p <- chi.test$p.value
            
            # plot
            title <- titleHelper()
            pt %<a-% {
                pp <- plot(table(vector)/length(vector), 
                           main=title, xlab="", ylab="")
                curve(dpois(x, dist.vals[[1]]), add=TRUE, col='red',
                      from=0, to=9, n=10)
            }
            
        } else if (dist == "uniform") {
            # validation test using chisquare test
            tab.Unif <- table(vector)
            v <- dist.vals[[2]] - dist.vals[[1]] + 1
            probs <- rep(1 / v, v)
            chi.test <- chisq.test(tab.Unif, p=probs)
            p <- chi.test$p.value
            
            # plot
            title <- titleHelper()
            pt %<a-% {
                pp <- plot(table(vector)/length(vector), 
                           main=title, xlab="", ylab="")
                curve(dunif(x, dist.vals[[1]], dist.vals[[2]]), add=TRUE, col='red')
            }
        } 
        
    }
    if (include.plot) {
        return(pt)
    } else {
        return(p)
    }
    
}
