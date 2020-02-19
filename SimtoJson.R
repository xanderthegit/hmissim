source('SimData.R')
library(jsonlite)

convert2dataframe <- function(subdata) {
  sub = data.frame()
  for(i in 1:length(subdata[[names(subdata)[1]]])){
    first = names(subdata)[1]
    tmp1 = data.frame("")
    names(tmp1) = names(subdata)[1]
    for(name in names(subdata)) {
        tmp1[[name]] = subdata[[name]][i]
    }
    sub = rbind(sub, tmp1)
  }
  return(sub)
}
SimtoJson <- function(simdata, compendium, nodelinks, sorted_nodes, project_name, path) {
    # takes simulated data and creates json
    # Args:
    #   simdata:   Simulated values
    #   compendium:  initial values for nodes
    #   nodelinks:  table describing relationship betweeen nodes
    #   path:   output for files
    #
    # Returns:
    #   creates and saves of json files representing data simulated for each node

    for (node in sorted_nodes) {
        varlist <- compendium[['VARIABLE']][compendium[['NODE']]==node]
        subdata = simdata[varlist]
        
        sub <- convert2dataframe(subdata)
       
        # Remove project_id
        if(length(grep("project_id", colnames(sub))) > 0){
            sub <- sub[-grep("project_id", colnames(sub))]
        }
        
        new_names <- names(sub)
        new_names <- gsub("\\..*", "", new_names)
        names(sub) <- new_names

        # Add type
        if(!("type" %in% colnames(sub))){
          sub <- cbind(sub, type=rep(node, nrow(sub)))
        }
        else{
          sub$type <- rep(node, nrow(sub))
        }

        # Add submitter_id
        submitter_id <- c()
        for (v in 1:nrow(sub)){ 
            num <- paste0(node, "_00", v)
            submitter_id <- c(submitter_id, num)
        }
        
        if("submitter_id" %in% colnames(sub)){
            sub$submitter_id <- submitter_id
        }
        else{
            sub <- cbind(sub, submitter_id=submitter_id)
        }

        link_name <- as.character(nodelinks[['LINK_NAME']][nodelinks[['NODE']]==node])
        targets <- nodelinks[['TARGET']][nodelinks[['NODE']]==node]
        multiplicity <- nodelinks[['MULTIPLICITY']][nodelinks[['NODE']]==node]

        #if (multiplicity == "many_to_one") {
        #    sub[[link_name]] <- toJSON(target_id, pretty=T, auto_unbox = T)
        #} else {
        #    sub[[link_name]] <- toJSON(target_id, pretty=T, auto_unbox = T)
        #}
        
        link_type <-  nodelinks[['MULTIPLICITY']][nodelinks[['NODE']]==node]
        names(link_type) <- targets
        # Add links
        l <- c()
        adjusted_number = nrow(sub)
        for(target in targets) {
            varlist2 <- compendium[['VARIABLE']][compendium[['NODE']]==target]
            sub2 = convert2dataframe(simdata[varlist2])
            if (link_type[[target]] %in% c("one_to_one","one_to_many")) {
              adjusted_number = min(adjusted_number, nrow(sub2))
            }
        }
        
        if (adjusted_number < nrow(sub)) {
            for (var in varlist) {
              simdata[var][[var]] = simdata[var][[var]][1:adjusted_number]
            }
        }
        
        for (v in 1:adjusted_number) {
            for(target in targets) {
                varlist2 <- compendium[['VARIABLE']][compendium[['NODE']]==target]
                sub2 = convert2dataframe(simdata[varlist2])
                if (link_type[[target]] %in% c("one_to_one","one_to_many") == FALSE) {
                  l <- append(l,paste0(target, "_00", sample(1:nrow(sub2),1)))
                }
                else {
                    if (v <= nrow(sub2)) {
                      l <- append(l, paste0(target, "_00", v))
                    }
                }
                
            }
        }
        finlist <- c()
        submitter_ids <- c()
        for (m in 1:adjusted_number) {
            x <- as.list(sub[m,])
            for(name in names(x)) {
                if (typeof(x[[name]]) == 'list')
                {
                  x[[name]] = unlist(x[[name]])
                }
            }
            for(ln in 1:length(link_name)){
                if(link_name[ln] == "projects"){
                   x[[link_name[ln]]] <- list(code=project_name)
                }
                else{
                   pos = (m-1)*length(link_name) + ln
                   x[[link_name[ln]]] <- list(submitter_id=l[pos])
                }
            }
            
            if (link_type[1] %in% c("one_to_one","one_to_many") ) {
                if (x[[link_name[ln]]] %in% submitter_ids == FALSE) {
                   submitter_ids <- append(submitter_ids, x[[link_name[ln]]])
                   finlist <- append(finlist, list(x))
                }
            }
            else
                finlist <- append(finlist, list(x))
        }
        
        json <- toJSON(finlist, pretty=T, auto_unbox=T)
              
        filepath <- paste0(path, node, ".json")
        write(json, filepath)
    }

    # Write file descriptions
    node_descriptions <- list()
    for (i in seq_along(sorted_nodes)) {
      node_name <- sorted_nodes[i]
      this_node <- list()
      this_node$NODE <- unbox(node_name)
      this_node$ORDER <- unbox(i)
      this_node$TARGET <- as.character(nodelinks[['TARGET']][nodelinks[['NODE']]==node_name])
      this_node$CATEGORY <- unbox(as.character(unique(nodelinks[nodelinks[['NODE']]==node_name, 'CATEGORY'])))
      node_descriptions[[i]] <- this_node
    }
    fileDescr <- toJSON(node_descriptions, pretty=T)
    write(fileDescr, paste0(path, 'NodeDescriptions.json'))
}

## Example to run
#source('https://raw.githubusercontent.com/occ-data/data-simulator/master/SimData.R')
#n <- 3
compendium <- read.csv('https://raw.githubusercontent.com/occ-data/data-simulator/master/SampleCompendium/sampleClinical.csv',
                       header=T, stringsAsFactors = F)
#nodelinks <- read.csv('https://raw.githubusercontent.com/occ-data/data-simulator/master/SampleCompendium/sampleClinical_Nodes.csv',
#                      header = T, stringsAsFactors = F)
#simdata <- simData(compendium, n, 
#                         include.na = FALSE, 
#                         reject= FALSE)

#SimtoJson(simdata, compendium, nodelinks, 'SampleJsonOutput/')
