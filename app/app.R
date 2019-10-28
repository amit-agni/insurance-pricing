
library(data.table)
library(tidyverse)
library(shiny)
# p_load(rdrop2)
# saveRDS(DT,here('110_data_intermediate','DT.Rds'))
# drop_upload(here('110_data_intermediate','DT.Rds'))

# Loading required package: pacman
# Warning in utils::install.packages(package, ...) :
#     'lib = "/opt/R/3.6.1/lib/R/library"' is not writable
# Error in value[[3L]](cond) : unable to install packages
# Calls: local ... tryCatch -> tryCatchList -> tryCatchOne -> <Anonymous>
#     Execution halted

DT <- readRDS(gzcon(url('https://www.dropbox.com/s/hv1hb42q1k7p7tc/DT.Rds?dl=1')))
#premium_cols <- grep('PREMIUM',names(DT),value = T)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Premium Pricing : How it is related to different factors ?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width = 2
            ,radioButtons('feature','Factor:',choices = c("YDAGE","OWNERAGE","VEHAGE",
                                                          "MAKE","MODEL","YEARLYKM","LICCANCNT","MVINSURER","REGUSE","NRMAASST" 
                                                          ,"NOYRLICOBT","MVINSTYPE","DEMERITPTS","NCBPCT","COMREGVEH","YDGENDER","ATFAULT5YRS","SHAPE"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plt")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plt <- renderPlot({

        cols <- c(grep(input$feature,names(DT)),19:22)
        
        DT[,..cols] %>%  melt(id.vars = input$feature) %>% 
            ggplot(aes_string(y='value',x = input$feature,group = input$feature)) +
            geom_boxplot() +
            facet_grid(variable~.)
        
        
    },height=600)


    
}

# Run the application 
shinyApp(ui = ui, server = server)
