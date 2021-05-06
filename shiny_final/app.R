# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinythemes)
library(ggplot2)

# Load data ---------------------------------------------------------------
st_jude_dwsz <- read_tsv(file = "data/03_stjude_downsized.tsv.gz")
gene_names <-  st_jude_dwsz %>% select(-sampleID,-leukemia) %>% colnames()
leukemia <- 'leukemia'

#Shiny App
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Gene expression boxplot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput('gene','Select the gene you want to be displayed', gene_names),
            selectInput('leukemia','Select the gene you want to be displayed', leukemia)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("genePlot"),
           p("Yeoh et al. (2002) acquired the diagnostic bone marrow samples from 248 pediatric acute lymphoblastic leukemia (ALL) patients who were determined to have one and only one of the six known pediatric ALL prognostic subtypes, which include T-cell lineage ALL (T-ALL), E2A-PBX1, TEL-AML1, MLL rearrangements, BCR-ABL, and hyperdiploid karyotypes with more than 50 chromosomes (HK50). The 248 patients included 43 T-ALL, 27 E2A-PBX1, 79 TEL-AML1, 15 BCR-ABL, 20 MLL, and 64 HK50 patients.")
    ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$genePlot <- renderPlot({
        ggplot(data = st_jude_dwsz,
               mapping = aes_string(x = leukemia, 
                             y = input$gene, 
                             fill = input$leukemia)) +
            geom_boxplot(alpha = 0.5) +
            labs(#title = sprintf("Expression of %s", input$gene),
                     x = "Leukemia type",
                     y = sprintf("%s expression",input$gene),
                     caption = "Data source: Yeoh et al. (2002)")+
            guides(fill=guide_legend(title="Leukemia type"))+
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        })
}


# Run the application 
shinyApp(ui = ui, server = server)
