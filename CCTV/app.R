library(shiny)
library(tm)

cleanText <- function(x){
  temp = subset(finalDataSet, select = -taskView)
  keepWords = colnames(temp)
  
  text = filter(crandb, Package == x)$Description

  corpus <- Corpus(VectorSource(text)) %>% 
    tm_map(content_transformer(tolower)) %>% 
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
  return(finalInputSet)
  
}

ui <- fluidPage(
  
  selectizeInput("package", choices = NULL, label = "Select Package to Classify"), #look at server side selectize
  
  tableOutput("static"),
  textOutput("verdict")
  
)

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "package", choices = crandb$Package, server = TRUE)
  
  text <- reactive(cleanText(input$package))
  
  verdict <- reactive(which.max(predict(model, newdata = text(), "probs")))
  
  output$verdict <- renderText(paste("The predicted Task view for the selected package is: ", taskViewNames[verdict()]))
}

shinyApp(ui = ui, server = server)
