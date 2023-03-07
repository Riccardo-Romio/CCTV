library(shiny)
library(tm)
library(dplyr)

cleanPackage <- function(x, depTree){
  temp = finalDataSet
  keepWords = colnames(temp)
  
  text = filter(crandb, Package == x)$Description

  corpus <- Corpus(VectorSource(text)) %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(gsub, pattern = "`r[^`]*`", replacement = " ") %>% 
    tm_map(gsub, pattern = "`[^`]*`", replacement = " ") %>% 
    tm_map(gsub, pattern = "\\(https:[^()]*\\)", replacement = " ") %>% 
    tm_map(gsub, pattern = "\\(http:[^()]*\\)", replacement = " ") %>% 
    tm_map(gsub, pattern = "\\(http:.*", replacement = " ") %>% 
    tm_map(gsub, pattern = "\\(https:.*", replacement = " ") %>% 
    tm_map(gsub, pattern = "\\(http:[^()]*\\)", replacement = " ") %>% 
    tm_map(gsub, pattern = "\\([^()]*\\)", replacement = " ") %>% 
    tm_map(gsub, pattern = "-", replacement = " ") %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(stemDocument) %>% 
    tm_map(stripWhitespace)
  
  dtm <- DocumentTermMatrix(corpus)
  finalInputSet <- as.data.frame(as.matrix(dtm))
  commonCols = intersect(names(finalInputSet), names(finalDataSet))
  finalInputSet = finalInputSet[, commonCols]
  extraCols = setdiff(names(finalDataSet), names(finalInputSet))
  finalInputSet[, extraCols] = 0
  
  colOrder = match(names(finalDataSet), names(finalInputSet))
  finalInputSet = finalInputSet[, colOrder]
  finalInputSet = rbind(finalDataSet, finalInputSet)
  len = length(finalInputSet[,1])

  for(i in seq(length(finalInputSet[1,]))){
    tf = (finalInputSet[len, i]/sum(finalInputSet[len,] != 0))
    idf = (log2((len)/sum(finalInputSet[,i] != 0))) 
    finalInputSet[len, i] = tf * idf
  }
  
  finalInputSet = finalInputSet[len,]
  finalVector = c()
  for(j in seq(length(taskViewWordsOnly[,1]))){
    finalVector = append(finalVector, sum(finalInputSet[1,] * taskViewWordsOnly[j,])/((sqrt(sum(finalInputSet[1,]^2)))*(sqrt(sum(taskViewWordsOnly[j,]^2)))))
  }  
  
  for(i in taskViewNames){
    finalVector = append(finalVector, dependenceIndex(depTree, i))
  }
  
  return(as.matrix(finalVector))
  
}

depTree <- function(x){
  return(build_dependence_tree(package_network, package = x))
}

visualHelp <- function(x){

  par(mar=c(15,4,4,2))
  return(barplot(x[1:length(x)], names.arg = unique(allDescCore$taskView), cex.names = 0.9, las = 2))
}


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

ui <- fluidPage(
  #dashboardHeader(),
  #dashboardSidebar(),
  #dashboardBody(),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  navbarPage("CCTV",
    tabPanel("Classify Package", fluid = TRUE,
      tags$style(button_color_css),
        mainPanel(
          fluidRow(
            column(3,
              selectizeInput("package", choices = NULL, label = "Select Package to Classify"),#look at server side selectize
              actionButton("classify", "Classify"),          
              tableOutput("static"),
              textOutput("verdict")
            )
          ),
          fluidRow(
            column(12,
              plotOutput("classifications")
            )
          )
        )
    ),
    tabPanel("Upload Package", fluid = TRUE,
    titlePanel("COMING SOON...")
    )
  )
)
verdict = 0

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "package", choices = crandb$Package, server = TRUE)
  
  observeEvent(input$package, {
    label = paste0("Classify ", input$package, "!")
    updateActionButton(inputId = "classify", label = label)
  })
  
  dependenceTree <- eventReactive(input$classify, {
    depTree(input$package)
  })
  
  text <- eventReactive(input$classify, {
   cleanPackage(input$package, dependenceTree())
    
  })

  #output$dependenceTree <- renderPlot(plot(dependenceTree(), legend = FALSE, title = FALSE, height = 300, width = 150)) 
  verdict <- reactive(exp(predict(model, newx = t(text()), s = min(modelLambda$lambda))))
  output$verdict <- renderText(paste("The predicted Task view for the selected package is: ", taskViewNames[which.max(verdict())]))
  output$classifications <- renderPlot(visualHelp(verdict()))
}

shinyApp(ui = ui, server = server)
