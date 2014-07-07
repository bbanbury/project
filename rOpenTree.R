##  -------------------------------  ##
##  -- OpenTree API betaVers 0.1 --  ##
##  --    Written by B.Banbury   --  ##
##  --         7 June 2014           ##
##  -------------------------------  ##


#get newick tree from ottid

GetOpenTree <- function(format, subtreeID){
return(fromJSON(postForm("http://api.opentreeoflife.org/treemachine/v1/getSyntheticTree",
         .opts = list(postfields = toJSON(list(format = format, subtreeNodeID = subtreeID)),
                      httpheader = c('content-type' = 'application/json'),
                      ssl.verifypeer = FALSE))))
}


#get OTTids from names
GetOTTid <- function(taxon){
  OTTids <- NULL
  web <- "http://api.opentreeoflife.org/taxomachine/v1/autocompleteBoxQuery"
  res <- fromJSON(postForm(web,
         .opts = list(postfields = toJSON(list(queryString = taxon, contextName = "All life")),
                      httpheader = c('content-type' = 'application/json'),
                      ssl.verifypeer = FALSE)))
  for(i in sequence(length(res))){
    OTTids <- c(OTTids, res[[i]]$ottId)
  }
  return(OTTids)
}


#Get list of studies:

GetStudyList <- function(){
  web <- "http://api.opentreeoflife.org/phylesystem/v1/study_list"
  res <- suppressWarnings(fromJSON(getForm(web)))
  return(res)
}


# Get a particular Study

GetParticularStudy <- function(study){
  web <- paste("http://api.opentreeoflife.org/phylesystem/v1/study", study, sep="/")
  res <- suppressWarnings(fromJSON(getForm(web)))
  return(res)
}


library(RCurl)
library(RJSONIO)
GetOpenTree("newick", "3534540")
GetOTTid("Aratinga")
GetOpenTree("newick", "671156")

GetStudyList()
GetParticularStudy("pg_551")
























