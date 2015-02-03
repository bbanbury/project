##  --------------------------------  ##
##  ---         shinyJobs        ---  ##
##  ---    written by B.Banbury  ---  ##
##  ---     v 1.0  27August2014   ---  ##
##  --------------------------------  ##


##  ---         Load Code        ---  ##
library(shiny)
library(XML)

vers <- "1.0"


##  ---     Server Functions     ---  ##

getWords <- function(words, removeFillers=TRUE){
  words <- tolower(words)
  if(any(words == "·"))
    words <- words[-which(words == "·")]
  if(any(getLink(words) == words))
    words <- words[-which(getLink(words) == words)]
  if(any(grep("[.,:;()-]", words)))
    words <- words[-grep("[.,:;()-]", words)]
  if(any(grep("\\+", words)))
      words <- words[-grep("\\+", words)]
  fillerWords <- c("and", "the", "of", "with", "to", "in", "you", "your", "us", "will", "or", "it", "is", "a", "at", "as", "on", "our", "for", "an", "such", "about", "this", "ads")
  if(any(words %in% fillerWords))
    words <- words[-which(words %in% fillerWords)]
  return(words)  
}

getLink <- function(text){
  #if(length(text) == 1){
  #  text <- scan(text, what="character")
  #  text <- tolower(text)
  #}
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

CheckHigherEd <- function(keywords, outside=FALSE){
##this will only check portland/seattle metro
  keywords <- paste(keywords)
  web <- paste("http://www.higheredjobs.com/search/advanced_action.cfm?JobCat=&PosType=1&PosType=2&InstType=1&InstType=2&InstType=3&Keyword=", keywords, "&Remote=1&Region=7&SubRegions=48&Metros=249&Submit=Search+Jobs", sep="")
  if(outside)
    web <- paste("http://www.higheredjobs.com/search/advanced_action.cfm?JobCat=&PosType=1&PosType=2&InstType=1&InstType=2&InstType=3&Keyword=", keywords, "&Remote=1&Region=7&SubRegions=0&Submit=Search+Jobs", sep="")
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





##  ---   Server Communication   ---  ##
shinyServer(function(input, output, session) {
  
  output$jobversion <- renderText({
    paste("Version", vers)
  })


  ##  ---   Match Quality   ---  ##

  initializeJob <- reactive({
    text <- NULL
    if(!is.null(input$fileJob))
      text <- scan(input$fileJob$datapath, what="character")
    if(!is.null(input$textJob) && input$textJob != "")
      text <- strsplit(input$textJob, " ")[[1]]
    words <- getWords(text)
    return(words)
  })

  initializeResume <- reactive({
    text <- NULL
    if(!is.null(input$fileRes))
      text <- scan(input$fileRes$datapath, what="character")
    if(!is.null(input$textRes) && input$textRes != "")
      text <- strsplit(input$textRes, " ")[[1]]
    words <- getWords(text)
    return(words)
  })
  
  output$match <- renderText({
    ad <- initializeJob()
    res <- initializeResume()
    if(length(ad)==0 || length(res) == 0)
      return(NULL)
    match <- CalculateMatch(ad, res)
    paste(match)
  })
 
  output$toAdd <- renderText({
    ad <- initializeJob()
    res <- initializeResume()
    if(length(ad)==0 || length(res) == 0)
      return(NULL)
    add <- WordsToAddToResume(initializeJob(), initializeResume())
    add[1:input$wordsToAdd]
  })


  ##  ---   Find A Job   ---  ##

  JobSearch <- reactive({
    jobs <- NULL
print(input$keywords)

    if(input$HigherEd)
      jobs <- c(jobs, CheckHigherEd(input$keywords, input$outside))      
    if(input$EvolDir)
      jobs <- c(jobs, CheckEvolDir())
 print(jobs)
    return(jobs)
  })

  output$jobsList <- renderText({
    jobsList <- JobSearch()
    paste(jobsList)
  })

  output$GetAJob <- renderText({
    paste("Get A Job You BUM!")
  })

  output$test <- renderText({
    HTML(paste("See phrynomics GitHub for full code base", tags$link("https://github.com/bbanbury/phrynomics")))
  })


  ##  ---   Tab Control   ---  ##

  observe({
    if (input$method == "") {    
      updateTabsetPanel(session, inputId="methodtabs", selected="panel1")
    }      
    else if (input$method == "Match a resume to an ad") {    
      updateTabsetPanel(session, inputId="methodtabs", selected="panel2")
    }      
    else if (input$method == "Find a job!") {    
      updateTabsetPanel(session, inputId="methodtabs", selected="panel3")
    }  
  })





})














