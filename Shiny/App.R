library(shiny)
library(vroom)
library(tidyverse)
library(ggplot2)

# injuries <- vroom::vroom("injuries.tsv")
# products <- vroom::vroom("products.tsv.txt")
# population <- vroom::vroom("population.tsv.txt")
# selected <- injuries %>% filter(prod_code == 649)
# selected %>% count(location, wt = weight, sort = TRUE)
# selected %>% count(body_part, wt = weight, sort = TRUE)
# selected %>% count(diag, wt = weight, sort = TRUE)
# summary <- selected %>%
#   count(age, sex, wt = weight) %>%
#   left_join(population, by = c("age", "sex")) %>%
#   mutate(rate = n / population * 1e4)
# print(n = 208, summary)
# summary %>%
#   ggplot(aes(age, rate, colour = sex)) +
#   geom_line(na.rm = TRUE) +
#   labs(y = "Injuries per 10,000")
prod_codes <- setNames(products$prod_code, products$title)

ui <- fluidPage(
  fluidRow(
    column(
      6, selectInput("code", "Product", choices = prod_codes)
    )
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))  
  ),
  fluidRow(
    column(
      12, plotOutput("age_sex")
    )
  )
)

server <- function(input, output, session) {
  selected <- reactive(injuries %>%  filter(prod_code == input$code))

  output$diag <- renderTable(
    selected() %>% count(diag, wt = weight, sort = TRUE)
  )  
  
  output$body_part <- renderTable(
    selected() %>% count(body_part, wt = weight, sort = TRUE)
  )
  
  output$location <- renderTable(
    selected() %>% count(location, wt = weight, sort = TRUE)
  )
  
  summary <- reactive({
    selected() %>% 
      count(age, sex, wt = weight) %>% 
      left_join(population, by = c("age", "sex")) %>% 
      mutate(rate = n / population * 1e4)
  })

  output$age_sex <- renderPlot({
    summary() %>% 
      ggplot(aes(age, n, colour = sex)) +
      geom_line(na.rm = TRUE) +
      labs(y = "Estimated Number of Injuries")
  }, res = 96)  
}


shinyApp(ui, server)