library(tidyverse)
library(readxl)
library(qdap)
library(shiny)
library(scales)
library(plotly)

#load dataset from Dropbox----
#main <- read.csv('https://www.dropbox.com/s/7ub8ka24fzy7t82/main.csv?raw=1')
main <- read.csv("https://www.dropbox.com/s/59djyf59jerehlp/main_v1.csv?raw=1")

#Tidying dataset----
#Visuals Setting
theme_set(theme_bw())
tol9qualitative=c("#332288", "#88CCEE", "#44AA99", "#117733", "#999933", 
                  "#DDCC77", "#CC6677", "#882255", "#AA4499")

#tidy datasets
#collect Var Names that fulfill data type conditions
ctgr <- main %>% 
  summarise_all(funs(is.factor(.)==TRUE)) %>% 
  unlist() %>% 
  names(.)[.]

cont <- main %>% 
  summarise_all(funs(is.numeric(.)==TRUE & !is.integer(.))) %>% 
  unlist() %>% 
  names(.)[.]

test <- as.list(main)
#define UI----
ui <- fluidPage(
  titlePanel("Hands & Minds"),
  
  navbarPage("", selected = 'Demographics',
             tabPanel("Demographics",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Select desired variables"),
                          
                          selectInput('xcol', 'X Variable', names(main)),
                          selectInput('ycol', 'Y Variable', names(main),
                                      selected = names(main)[[3]]),
                          selectInput('color', 'Color', c('None', names(main))),
                          
                          checkboxInput('jitter', 'Jitter'),
                          checkboxInput('smooth', 'Smooth'),
                          checkboxInput('flip', 'Flip'),
                          
                          selectInput('facet_row', 'Facet Row', c(None='.', names(main))),
                          selectInput('facet_col', 'Facet Column', c(None='.', names(main)))
                        ),
                        
                        mainPanel(
                          plotlyOutput('scatter', height = '600px')))),
             
             tabPanel("Livestock",
                      sidebarLayout(
                        sidebarPanel(
                          # selectInput('facet_row2', 'Facet Row', c(None='.', names(livestock))),
                          # selectInput('facet_col2', 'Facet Column', c(None='.', names(livestock)))
                        ),
                        
                        mainPanel(
                          plotlyOutput('bar', height = '600px')
                        ))
             )
  )
)

# Define server function ----
server <- function(input, output){
  
  #Reactivity
  selectedData <- reactive({main})
  
  output$scatter <- renderPlotly({
    if (input$xcol %in% ctgr & input$ycol %in% cont)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol,y=input$ycol)) + 
        geom_boxplot()
    else if (input$xcol %in% cont & input$ycol %in% ctgr)
      g3 <- ggplot(selectedData(), aes_string(x=input$ycol,y=input$xcol)) + 
        geom_boxplot() + coord_flip() #why this doesnt work???
    else if (input$xcol %in% ctgr & input$ycol %in% ctgr)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol)) + 
        geom_bar(width = .5, fill='#CC79A7') +
        #geom_text(stat='count',aes(label=..count..),vjust=-0.6) +
        labs(x='Values')
    else if (input$xcol %in% cont & input$ycol %in% cont & input$xcol==input$ycol)
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol)) + 
        geom_density()
    else
      g3 <- ggplot(selectedData(), aes_string(x=input$xcol,y=input$ycol)) + 
        geom_point()
    
    if (input$color != 'None')
      g3 <- g3 + aes_string(color=input$color)
    
    facets1 <- paste(input$facet_row, '~', input$facet_col)
    if (facets1 != '. ~ .')
      g3 <- g3 + facet_grid(facets1)
    
    if (input$flip)
      g3 <- g3 + coord_flip()
    
    if (input$jitter)
      g3 <- g3 + geom_jitter()
    
    if (input$smooth)
      g3 <- g3 + geom_smooth(method = 'loess')
    
    #print(g3)
    ggplotly(g3)
  })
  
  # output$bar <- renderPlot({
  #   g4 <- ggplot(livestock, aes(HHID,number,fill=livestock)) +
  #     theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +   #no tick marks
  #     labs(x = 'Households', y = 'Percentage') + 
  #     geom_bar(position = 'fill', stat = 'identity') +
  #     scale_y_continuous(labels = percent_format()) +
  #     scale_fill_manual(values=tol9qualitative)
  #   
  #   facets2 <- paste(input$facet_row2, '~', input$facet_col2)
  #   if (facets2 != '. ~ .')
  #     g4 <- g4 + facet_grid(facets2, space="free_x", scales="free_x")
  #   
  #   print(g4)
  # }, height = 600)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)