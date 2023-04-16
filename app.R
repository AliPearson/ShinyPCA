#####################
library(shiny)
library(plotly)
library(tidyverse)
library(clipr)

#################### Modify to your data...
pca_df <- readRDS("pca_df.rds")
pca_cap_df <- readRDS("pca_cap_df.rds")
num_pcs<-2
datasets<-list("pca_df"=pca_df, "pca_cap_df"=pca_cap_df)
###################

ui <- fluidPage(
  titlePanel("ShinyPCA"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("data",
                     label = "data",
                     #choices = c("Shotgun"="1", "Capture"="2")
                     choices = c(names(datasets))),
      selectizeInput("colour",
                     label = "colours",
                     choices = colnames(pca_df)[(num_pcs+1):ncol(pca_df)]),
      selectizeInput("xaxis",
                     label = "xaxis",
                     choices = colnames(pca_df)[1:num_pcs]),
      selectizeInput("yaxis",
                     label = "yaxis",
                     choices = colnames(pca_df)[1:num_pcs])
    ),
    mainPanel(
      plotlyOutput("plot", height = "800px"),
      verbatimTextOutput("hover"),
      verbatimTextOutput("selected"),
      br(),
      actionButton("copy", "Copy"),
      downloadButton('downloadData', 'Download data')
    )
  )
)

server <- function(input, output, session) {
  datasetInput <- reactive({
    temp <- data.frame(datasets[[input$data]])
  })
  
  selected <- reactiveValues(selected=NA)
  
  output$plot <- renderPlotly({
    p1 <-ggplotly(ggplot(datasetInput(), aes(x=datasetInput()[,input$xaxis],y=datasetInput()[,input$yaxis],colour=datasetInput()[,input$colour], text=datasetInput()[,input$colour])) +
                   geom_point()+xlab(input$xaxis)+ylab(input$yaxis)+theme(legend.position='none')+ggtitle(input$data), tooltip=c("text"))
    p1 %>% 
      layout(dragmode = "select") %>%
      event_register("plotly_selecting")
  })
  
  #output$hover <- renderPrint({
   ## d <- event_data("plotly_hover")
   # if (is.null(d)) "Hover events appear here (unhover to clear)" else d
  #})
  
  output$selected <- renderPrint({
    d <- event_data("plotly_selected")
    # topleft <- c(min(d$x), max(d$y))
    # bottomright <- c(max(d$x), min(d$y))
    if (is.null(d)) {"Nothing selected yet"} 
    else {
      df_display <- datasetInput() %>%
        filter(pc1 %in% d$x) %>% 
        filter(pc2 %in% d$y)
      selected$selected <- df_display
      print(select(selected$selected, Genetic_ID, input$colour))
    }
  })
  
  observeEvent(input$copy, {
    write_clip(event_data("plotly_selected"), object_type = "table")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { 
      paste("dataset-", input$data, ".csv", sep="")
    },
    content = function(file) {
      write.csv(selected$selected, file)
    })
  
}

shinyApp(ui, server)
##################
