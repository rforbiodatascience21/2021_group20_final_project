# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggplot2)

# Load data ---------------------------------------------------------------
st_jude_top40 <- read_tsv(file = "data/03_stjude_top40.tsv.gz")
gene_names <-  st_jude_top40 %>% select(-sampleID,-leukemia) %>% colnames()

#Shiny App
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # Application title
                titlePanel("Gene expression"),
                hr(),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    selectInput('plot_type','Select the plot type', c('Boxplot','Densitogram')
                    ),
                    selectInput('gene','Select the gene you want to be displayed', gene_names)
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    plotOutput("genePlot"),
                    p("Yeoh et al. (2002) acquired the diagnostic bone marrow 
                      samples from 248 pediatric acute lymphoblastic leukemia
                      (ALL) patients.")
                  ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$genePlot <- renderPlot({
    if (input$plot_type == 'Boxplot') {
      ggplot(data = st_jude_top40,
             mapping = aes_string(x = "leukemia", 
                                  y = input$gene, 
                                  fill = "leukemia")) +
        geom_boxplot(alpha = 0.5) +
        labs(title = sprintf("Expression of %s", input$gene),
             x = "Leukemia type",
             y = sprintf("%s expression",input$gene),
             caption = "Data source: Yeoh et al. (2002)")+
        guides(fill=guide_legend(title="Leukemia type"))+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
    else if (input$plot_type == 'Densitogram'){
      ggplot(data = st_jude_top40,
             mapping = aes_string(x = input$gene, 
                                  colour = "leukemia")) +
        geom_density(size=1.2) +
        labs(title = sprintf("Densitogram of %s", input$gene),
             y = 'Density',
             caption = "Data source: Yeoh et al. (2002)")+
        guides(colour=guide_legend(title="Leukemia type"))+
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)