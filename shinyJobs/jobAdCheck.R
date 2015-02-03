#Job Ad Analitics
library(XML)
devtools::install_github("duncantl/RGoogleDocs")
library(RGoogleDocs)

getWords <- function(textFile, removeFillers=TRUE){
  words <- scan(textFile, what="character")  
  words <- tolower(words)
  if(any(words == "·"))
    words <- words[-which(words == "·")]
  if(any(getLink(words) == words))
    words <- words[-which(getLink(words) == words)]
  if(any(grep("[.,:;()-]", words)))
    words <- words[-grep("[.,:;()-]", words)]
  if(any(grep("\\+", words))){
    for(i in grep("\\+", words)){
      words[i] <- paste(strsplit(words[i], "+")[[1]], collapse="\\")
    }
  }
  fillerWords <- c("and", "the", "of", "with", "to", "in", "you", "your", "us", "will", "or", "it", "is", "a", "at", "as", "on", "our", "for", "an")
  if(any(words %in% fillerWords))
    words <- words[-which(words %in% fillerWords)]
  return(words)  
}

getLink <- function(text){
  if(length(text) == 1){
    text <- scan(text, what="character")
    text <- tolower(text)
  }
  whichLink <- grep("http", text)
  return(text[whichLink])
}

wordCounts <- function(wordVector, num=NULL){
  uniqueWords <- unique(wordVector)
  wordCount <- rep(NA, length(uniqueWords))
  names(wordCount) <- uniqueWords
  for(i in sequence(length(wordCount))){
    wordCount[i] <- length(which(names(wordCount)[i] == wordVector))
  }
  if(!is.null(num))
    wordCount <- sort(wordCount, decreasing=TRUE)[1:num]
  return(wordCount)
}

CalculateMatch <- function(textAd, textResume){
  ad <- getWords(textAd)
  res <- getWords(textResume)
  #this resume has X percent words in common with ad
  return(length(which(res %in% ad))/length(res))
}


WordsThatMatchAd <- function(textAd, textResume){
  ad <- getWords(textAd)
  res <- getWords(textResume)
  return(res[which(res %in% ad)])
}

WordsToAddToResume <- function(textAd, textResume, num=NULL){
  ad <- getWords(textAd)
  adcounts <- wordCounts(ad)
  res <- getWords(textResume)
  toadd <- names(adcounts[-which(names(adcounts) %in% res)])
  if(!is.null(num))
    toadd <- toadd[1:num]
  return(toadd)
}


textFile <- "~/Dropbox/jobAppStuff/CVstuffs/Industry/CodingResume.txt"
getLink(textFile)
words <- getWords(textFile)
wordCounts(words)

textAd <- "~/Dropbox/jobAppStuff/industryAds/amazonResearcher.txt"
textResume <- "~/Dropbox/jobAppStuff/CVstuffs/Industry/CodingResume.txt"

CalculateMatch(textAd, textResume)


googleAd <- "~/Dropbox/jobAppStuff/industryAds/googleResearcher.txt"
CalculateMatch(googleAd, textResume)

WordsThatMatchAd(googleAd, textResume)

WordsToAddToResume(googleAd, textResume)



##  Now to look online for jobs:

# higher ed

CheckHigherEd <- function(keywords){
##this will only check portland/seattle metro
  keywords <- paste(keywords)
  web <- paste("http://www.higheredjobs.com/search/advanced_action.cfm?JobCat=&PosType=1&PosType=2&InstType=1&InstType=2&InstType=3&Keyword=", keywords, "&Remote=1&Region=7&SubRegions=48&Metros=249&Submit=Search+Jobs", sep="")
  web2 <- readLines(web, warn=FALSE)
  web3 <- web2[grep("jobTitle", web2)]
  jobs <- rep(NA, length(web3))
  for(i in sequence(length(jobs))){
    jobs[i] <- paste0("http://www.higheredjobs.com/search/", strsplit(web3[i], '"')[[1]][4])
    names(jobs)[i] <- strsplit(strsplit(web3[i], '"')[[1]][5], "[<>]")[[1]][2]
  }
  return(jobs)
}

CheckEvolDir <- function(keywords){
#keywords are null
  web <- "http://evol.mcmaster.ca/cgi-bin/my_wrap/brian/evoldir/Jobs/"
  web2 <- readLines(web, warn=FALSE)
  web3 <- web2[grep("href", web2)]
  jobs <- rep(NA, length(web3))
  for(i in sequence(length(jobs))){
    jobs[i] <- paste0("http://evol.mcmaster.ca", strsplit(web3[i], '"')[[1]][4])
    names(jobs)[i] <- strsplit(strsplit(web3[i], '"')[[1]][length(strsplit(web3[i], '"')[[1]])], "[<>]")[[1]][2]
  }
  return(jobs)
}


http://www.nature.com/naturejobs/science/


web <- "https://docs.google.com/spreadsheets/d/13K6ISCNKieS1itBJLpqXhBNph38zui7HrTl-qJ01FU0/pubhtml"
urltable <- getURL(web)
read.csv(textConnection(urltable))
#these come in as a list of however many html tables there are on the web, to go through each, use [[]]
tables[[1]]
colnames(tables[[1]])
#Can then rearrange data however you see fit (changing to numeric, factors, etc.)



GetJobDescriptions <- function(VectorOfWebsites){
#This will take output from the check functions and download the job descriptions as a list.  
  desc <- list()
  for(i in sequence(length(VectorOfWebsites))){
    #readLines(VectorOfWebsites[i])
    #htmlTreeParse(VectorOfWebsites[i], asText=TRUE)
    l <- xmlToList(xmlRoot(htmlTreeParse(readLines("http://www.higheredjobs.com/search/details.cfm?JobCode=175938815&Title=Assistant%20Professor%20in%20Biology%20%28Molecular%20Genetics%29"))))[[2]]




  }

}


#CheckVitae <- function(keywords){   #not working because of https
##this will only check portland/seattle metro
#  keywords <- paste(keywords, collapse="+")
#  web <- paste("http://chroniclevitae.com/job_search?job_search%5Bdistance_from_zip%5D=10&job_search%5Bemployment_type%5D=&job_search%5Binstitution_type%5D=&job_search%5Bkeywords%5D=", keywords,"&job_search%5Blocation%5D=45&job_search%5Bposition_type%5D=&job_search%5Bzip_code%5D=&utf8=%E2%9C%93", sep="")
#  web2 <- readLines(web)
#  web3 <- web2[grep("jobTitle", web2)]
#  jobs <- rep(NA, length(web3))
#  for(i in sequence(length(jobs))){
#    jobs[i] <- strsplit(web3[i], '"')[[1]][4]
#    names(jobs)[i] <- strsplit(strsplit(web3[i], '"')[[1]][5], "[<>]")[[1]][2]
#  }
#  return(jobs)
#}

CheckHigherEd("biology")
CheckHigherEd(c("research", "biology"))

CheckEvolDir()


















