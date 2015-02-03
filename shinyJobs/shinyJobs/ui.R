##  --------------------------------  ##
##  ---         shinyJobs        ---  ##
##  ---    written by B.Banbury  ---  ##
##  ---     v 1.0  27August2014   ---  ##
##  --------------------------------  ##


##  Load packages and source code
library(shiny)
library(XML)

##  shiny user interface
shinyUI(pageWithSidebar(  

##  ---          header          ---  ##
  headerPanel(
    title=HTML(paste(tags$span(style="color:#A9D0F5", "shiny"), tags$span(style="color:#FAAC58", "Jobs"), sep="")), windowTitle="shinyJobs"),




##  ---         side bar         ---  ##
  sidebarPanel(
    helpText("When you submit your resume someplace, they will often times filter it through software that compares keywords. This software will do the same, by calculating the amount of overlap between the two ('Match Quality'). You can use this to add more words to your resume, thereby increasing the amount of overlap or to find other jobs that match better."),
    br(),
    br(),
  h4("What would you like to do?"),
  radioButtons(inputId="method", label="Choose A Method", choices = list("Be jobless forever...", "Match a resume to an ad", "Find a job!")),
  conditionalPanel("input.method == 'Match a resume to an ad' ", 
    textInput("textJob", "Paste job ad text:"), 
    fileInput("fileJob", "OR choose a text file with the job ad:", accept=c(".txt")),
    br(),
    br(), 
    textInput("textRes", "Paste resume/CV text:"), 
    fileInput("fileRes", "OR choose a resume/CV text file:", accept=c(".txt"))),
    conditionalPanel("input.method == 'Find a job!' ", 
      textInput("keywords", "Keywords (ex:'biology')"), 
      h4("Search the following Databases:"),
      checkboxInput("EvolDir", "EvolDir"),
      checkboxInput("HigherEd", "HigherEd")),

    br(), 
    br(), 
    verbatimTextOutput("jobversion")),


##  ---         main bar         ---  ##
  mainPanel(tabsetPanel(id="methodtabs",
    tabPanel(title="Get A Job", value="panel1", verbatimTextOutput("GetAJob")),
    tabPanel(
      title="Match A Job",
      value="panel2",
      h4("Match Quality"),
      verbatimTextOutput("match"),
      br(),
      h4("You might consider adding the following to boost match quality:"),
      sliderInput("wordsToAdd", "How many word suggestions do you want?", min=1, max=100, value=5),
      verbatimTextOutput("toAdd")),

    tabPanel(
      title="Find A Job",
      value="panel3",
      htmlOutput("test"),
      #textInput("keywords", "Keywords (ex:'biology')"), 
      #h4("Search the following Databases:"),
      #checkboxInput("EvolDir", "EvolDir"),
      #checkboxInput("HigherEd", "HigherEd"),
      #verbatimTextOutput("match"),
      #br(),
      #h4("You might consider adding the following to boost match quality:"),
      #sliderInput("wordsToAdd", "How many word suggestions do you want?", min=1, max=100, value=5),
      #verbatimTextOutput("toAdd"))
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      br(),
      checkboxInput("outside", "Click to expand the search outside Seattle/Portland (but think about this choice!)"))         
  )) #tabpanel/mainPanel

)) #shinyUI 














