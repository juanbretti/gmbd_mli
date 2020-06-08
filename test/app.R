#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    cars2 <- reactive({
        
        cars2 <- datasets::cars
        # cars2 <- cars[1:input$bins,]
        return(cars2)
        
    })
    
    output$table <- renderDataTable({
        
        datatable(cars2(), options=list(columnDefs = list(list(visible=FALSE, targets=c(4)))))
            
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
