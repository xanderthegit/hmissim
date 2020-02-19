library(stringr)
library(stringi)
library(charlatan)
source('ValidateFunction.R')

## Helper for handling numeric / integer distributions in compendium
distPrep <- function(row, n=0, full.output=FALSE) {
    # reviews strings associated with distr for a variable and prepares for use
    #
    # Args: 
    #   row:  row in compendium representing a variable
    #   n:   number of times to sim var
    # 
    # Returns:
    #   val:  list of simulated inputs based on distribution type
    str <- row[['DISTRIB.INPUTS']]
    if (row[['DISTRIB']] == "normal") {
        str <- row[['DISTRIB.INPUTS']]
        mean <- as.numeric(str_extract(str_extract(str, "(mean = )[-]{0,1}[0-9]*"), "([-]{0,1}[0-9]+)"))
        sd <- as.numeric(str_extract(str_extract(str, "(sd = )[0-9]*"), "([0-9]+)"))
        full <- list(mean=mean, sd=sd)
        val <- rnorm(n, mean, sd)
    } else if (row[['DISTRIB']] == "poisson") {
        lambda <- as.numeric(str_extract(str_extract(str, "(lambda = )[0-9]*"), "([0-9]+)"))
        full <- list(lambda=lambda)
        val <- rpois(n, lambda)
    } else if (row[['DISTRIB']] == "uniform") {
        min <- as.numeric(str_extract(str_extract(str, "(min = )[0-9]*"), "([0-9]+)"))
        max <- as.numeric(str_extract(str_extract(str, "(max = )[0-9]*"), "([0-9]+)"))
        full <- list(min=min, max=max)
        if (row[['TYPE']] == "number") {
            val <- runif(n, min, max)
        } else {
            val <- sample(min:max, n, replace=T)
        }
    } else if (row[['DISTRIB']] == "exponential") {
        rate <- as.numeric(sub('rate = ', '', str))
        full <- list(rate=rate)
        val <- rexp(n, rate)
    }
    
    if (full.output) {
        return(full)
    } else { return(val) }
}

convertToList <- function(l) {
    # quick helper to handle lists for categorical vars
    as.list(str_trim(strsplit(l, '\\|')[[1]]))
}

## Helper to choose correct simulation method for a var
simVar <- function(row, n,include.na=TRUE, reject=FALSE, threshold=.05) {
    # reviews row, selects and implements a method for simulating variable
    #
    # Args: 
    #   row:  row in compendium representing a variable
    #   n:   number of times to sim var
    #   include.na:   simulate NA values based on compendium probabilities
    #   reject:  run validation and immediately resim if threshold not met
    #   threshold:  desired threshold for validation
    # 
    # Returns:
    #   val:  list of simulated inputs based on distribution type
    if (!reject) {threshold <- 0; check <- 1}
    repeat {
        if (row[['TYPE']] == "enum") {
            
            val <- unlist(sample(convertToList(row[['CHOICES']]), 
                                 n, T, as.numeric(convertToList(row[['PROBS']]))))
            #if (all(grepl('^-?[0-9.]+$', convertToList(row[['CHOICES']])))){
            #    val <- as.numeric(val) 
            #}
        } else if (row[['TYPE']] == "boolean"){
            val <- unlist(sample(c(TRUE, FALSE), 
                                 n, T, as.numeric(convertToList(row[['PROBS']]))))
        } else if (row[['TYPE']] == "ssn"){
            area = stri_rand_strings(n, 3, pattern = "[0-9]")
            specify = stri_rand_strings(n, 2, pattern = "[0-9]")
            group = stri_rand_strings(n, 4, pattern = "[0-9]")
            val <- paste(area, specify, group, sep = "-") #stri_rand_strings(n, 9, pattern = "[0-9]") #as.numeric(stri_rand_strings(n, 9, pattern = "[0-9]")) #replicate(sample(100000000:999999999,n,F))
        } else if (row[['TYPE']] == "number"){
            if (row[['DISTRIB']] == "poisson") {
                val <- rep("Not a valid number distribution", n)
            } else { 
                val <- distPrep(row, n)
            }
        } else if (row[['TYPE']] == "integer"){
            if (row[['DISTRIB']] == "exponential") {
                val <- rep("Not a valid integer distribution", n)
            } else { 
                val <- round(distPrep(row, n), 0)
                if (is.na(row[['POSITIVEONLY']])) {
                    val
                } else { 
                    val[val < 0] <- 0
                }
            }
        } else if (row[['TYPE']] == "string"){
            if(row[['CHOICES']] != ''){
                if(grepl('md5sum', row[['VARIABLE']])){
                    pat <- str_extract(row[['CHOICES']], "\\[(.+?)\\]")
                    size <- as.integer(str_match(row[['CHOICES']], "\\{(.+?)\\}")[,2])
                    val <- stri_rand_strings(n, size, pattern = pat)
                }
                else{
                    if(grepl(':', row['CHOICES'])){
                        val <- replicate(n, paste(sample(10:23, 1),sample(10:59,1),sample(10:59,1),sep=":")) 
                    }
                    else if(grepl('x', row['CHOICES'])){
                        val <- replicate(n, paste(sample(0:9, 1),sample(0:9,1),sample(0:9,1),sep="x")) 
                    }
                    else{
                        val <- replicate(n, paste(stri_rand_strings(1, 4, '[0-9]'), stri_rand_strings(1, 2, '[0-9]'), stri_rand_strings(1, 2, '[0-9]'),sep="-"))
                    }
                }
            }
            else{
                 val <- stri_rand_strings(n, 12, pattern = "[A-Za-z0-9]")
            }
        } else if (row[['TYPE']] == "array") {
              val <- data.frame()
              for (i in 1:n) {
                  tmp1 = data.frame("")
                  names(tmp1) = c('c1')
                  tmp1[1]$c1 = list(stri_rand_strings(sample(1:3,1), 4, pattern = "[A-Za-z0-9]"))
                  val <- rbind(val,tmp1)
              }
        } else if (row[['TYPE']] == "name") {
              persons <- PersonProvider$new()
              val <- replicate(n, persons$last_name()) #paste(persons$first_name(), persons$last_name())) #replicate(n, persons$render()) #data.frame()
              #for (i in 1:n) {
              #    tmp1 = data.frame("")
              #    names(tmp1) = c('c1')
              #    tmp1[1]$c1 = list(stri_rand_strings(sample(1:3,1), 4, pattern = "[A-Za-z0-9]"))
              #    val <- rbind(val,tmp1)
        } else if (row[['TYPE']] == "date") {
              val <- as.Date("2010-1-1")+sample(1:1000,n) #replicate(n, as.Date("2010-1-1") + sample(1:3685, 1))
              #if row[['VARIABLE']] == "end"){
               # val <- +sample(1:1000,n)
              #}
        } else {
              val <- rep("Something Went Wrong", n)
        }
      
        # add NAS
        if (include.na) {
            na.prob <- row[['NAS']]
            ind <- sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(1-na.prob, na.prob))
            val[!ind] <- NA
        }
        
        # check MAX and MIN columns
        if ('MAX' %in% names(row) & 'MIN' %in% names(row)){
            if (!is.na(row[['MAX']]) & !is.na(row[['MIN']])){
                 val[val>row[['MAX']]] <- row[['MAX']]
                 val[val<row[['MIN']]] <- row[['MIN']]
            }
        }

        names <- c(row[["VARIABLE"]])
        df <- data.frame(val)
        names(df) <- names
        if (reject) {
            check <- validateVar(row[['VARIABLE']], compendium, df, threshold)
        }
        if (check > threshold) break
    
    }
    
    return(df)
}

simData <- function(compendium, sample_numbers, include.na=TRUE, reject=FALSE, threshold=.05) {
    # helper that runs simulation for each row in variable compendium
    #
    # Args: 
    #   row:  dictionary representing variables to simulate and methods to use
    #   n:   number of observations to simulate
    #   include.na:   simulate NA values based on compendium probabilities
    #   reject:  run validation and immediately resim if threshold not met
    #   threshold:  desired threshold for validation
    # 
    # Returns:
    #   df:   a simulated dataset
    df <- list()
    names <- c()
    for (i in 1:nrow(compendium)) {
        v <- compendium[i,][['VARIABLE']]
        node <- compendium[i,][['NODE']]
        if (i==1) {
            var <- simVar(compendium[i,], sample_numbers, include.na, reject, threshold)
            #df <- append(df, var)
            names[length(names)+1] = names(var)
            df[length(df)+1] <- var
        } else {
            tried <- try(simVar(compendium[i,], sample_numbers, include.na, reject, threshold), silent=T)
            if(inherits(tried, "try-error")) {
                print(paste0("Variable: ", v, " | Error: ", tried))
            } else {
                var <- simVar(compendium[i,], sample_numbers, include.na, reject, threshold)
                names[length(names)+1] = names(var)
                df[length(df)+1] <- var
            }
        }
    }
    names(df) = names
    return(df)
}
