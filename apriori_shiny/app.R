library(shiny)
library(visNetwork)
library(tidyverse)
library(data.table)
library(arules)
library(arulesViz)
library(plotly)

options(DT.options = list(pageLength = 50))

## Load data ----
data <- read.delim("data/grocery_transactional.txt", sep = ',', stringsAsFactors = FALSE)

# Functions
rules_metrics <- function(rules, tr) {
    capture.output(
        out <- bind_cols(
            inspect(rules), 
            interestMeasure(rules, c("oddsRatio", "leverage"), transactions = tr),
            tibble(lhs_length = size(rules@lhs))
        )
    )
    out <- mutate_if(out, is.double, function(x) round(x, 4))
    colnames(out)[2] <- '_'
    return(out)
}

# Prepare data
set.seed(42)
df1 <- data %>%
    select(CUSTOMER, PRODUCT) %>%
    mutate(
        PRODUCT = trimws(PRODUCT),
        value = 1) %>%
    spread(PRODUCT, value, fill = 0) %>% 
    sample_frac(0.5)

tr1 <- as(as.matrix(df1[, -1]), 'transactions')
summary_ <- summary(tr1)

## Apriori ----

# Clean up
association_rules <- apriori(tr1, parameter = list(support=0.009, confidence=0.25, minlen=3, maxtime = 0))

# Saved for solving some issue with the Shiny app server
# table_association_rules <- rules_metrics(association_rules, tr1)
# saveRDS(table_association_rules, 'data/table_association_rules.RDS')
table_association_rules <- readRDS('data/table_association_rules.RDS')

# table_association_rules['confidence'] 

probs <- c(0, 0.075, 0.225, 0.4, 0.225, 0.075)
probs_label <- paste0(LETTERS[1:5], ' ', probs[2:6]*100, '%')

# Whole milk case
# association_rules_whole_milk <- apriori(tr1, parameter = list(support=0.01, confidence=0.1, minlen=2, maxtime = 0), appearance = list(lhs="whole milk", default="rhs"))
# inspect(association_rules_whole_milk)

## Define UI for application ----
ui <- navbarPage(title = "Fresh.Shop",
    tabPanel("Descriptive",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br()
                 ),
                 mainPanel(
                     plotOutput('plot_items_ticket'),
                     plotOutput('plot_relative'),
                     plotOutput('plot_absolute')
                 )
             )
    ),
    tabPanel("All rules",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br()
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Interactive", 
                                          plotlyOutput('plot_plotly', height = '800px')
                                 ),
                                 tabPanel("Additional", 
                                          plotOutput('plot_grouped', height = '800px'),
                                          # plotOutput('plot_scatterplot'),
                                          plotOutput('plot_two_key')
                                 )
                     )
                 )
             )
    ),
    tabPanel("Strata table",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     selectInput('by_split','Strata criteria:', choices=c('confidence', 'lift', 'support'), selected = 'confidence')
                 ),
                 mainPanel(
                      DT::dataTableOutput('table_association_rules', height = '800px')
                 )
             )
    ),
    tabPanel("Top rules",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     sliderInput('top', 'Top rules:', min = 1, max = 50, value = 10),
                     selectInput('by_sort','Sorting criteria:', choices=c('confidence', 'lift', 'support'), selected = 'confidence')
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Network", 
                                          visNetworkOutput('plot_graph_html', height = '800px')
                                 ),
                                 tabPanel("Additional", 
                                          plotlyOutput('plot_matrix'),
                                          plotOutput('plot_graph', height = '800px'),
                                          plotOutput('plot_paracord')
                                 )
                     )
                 )
             )
     )
)

## Define server logic ----
server <- function(input, output) {

    # Items per ticket histogram
    output$plot_items_ticket <- renderPlot(
        tibble(`Items per ticket` = factor(names(summary_@lengths), levels = names(summary_@lengths)), Frequency = as.numeric(summary_@lengths)) %>% 
            ggplot() +
            geom_bar(aes(x = `Items per ticket`, y = Frequency), stat="identity") +
            labs(title="Items per ticket")
    )
    
    #Fix plots
    output$plot_absolute <- renderPlot(itemFrequencyPlot(tr1, topN=20, type="absolute", main="Absolute Item Frequency Plot"))
    output$plot_relative <- renderPlot(itemFrequencyPlot(tr1, topN=20, type="relative", main="Relative Item Frequency Plot"))
    
    observeEvent(input$by_split, ignoreNULL = FALSE, ignoreInit = FALSE, {
        table_association_rules_ <- table_association_rules
        q_ <- quantile(table_association_rules_[[input$by_split]], probs=cumsum(probs))
        q_[1] <- 0
        table_association_rules_$cuts <- cut(table_association_rules_[[input$by_split]], breaks=as.numeric(q_), labels = probs_label[5:1], include.lowest = FALSE)

        out <- datatable(table_association_rules_) %>%
            formatStyle(
                'cuts',
                target = 'row',
                backgroundColor = styleEqual(probs_label, heat.colors(5))
            )
        output$table_association_rules <- DT::renderDataTable(out, rownames = FALSE, width = 0.9)
    })
    
    # Get subset rules in vector
    # rules_subset <- which(colSums(is.subset(association_rules, association_rules)) > 1)
    # rules_subset <- association_rules[-rules_subset] # remove subset rules.
    rules_subset <- association_rules # remove subset rules.
    rules_subset_filtered<-rules_subset[
        quality(rules_subset)$lift >= 1 & 
            quality(rules_subset)$confidence >= 0.25 &
            quality(rules_subset)$support >= 0.005]
    
    # All the rules following the previous criteria
    output$plot_scatterplot <- renderPlot(plot(rules_subset_filtered, method = "scatterplot", jitter = 0))
    output$plot_two_key <- renderPlot(plot(rules_subset_filtered, method = "two-key plot", jitter = 0))
    output$plot_grouped <- renderPlot(plot(rules_subset_filtered, method = "grouped"))
    output$plot_plotly <- renderPlotly(plot(rules_subset_filtered, engine = "plotly", jitter = 0))
    
    observeEvent(c(input$top, input$by_sort), ignoreNULL = FALSE, ignoreInit = FALSE, {
        set.seed(42)
        
        rules_subset_filtered_top <- head(rules_subset_filtered, n = input$top, by = input$by_sort)
        
        # Top 10 rules
        output$plot_matrix <- renderPlotly(plot(rules_subset_filtered_top, method = "matrix", measure = "lift", engine = "plotly"))
        output$plot_graph <- renderPlot(plot(rules_subset_filtered_top, method = "graph"))
        output$plot_paracord <- renderPlot(plot(rules_subset_filtered_top, method = "paracoord"))
        output$plot_graph_html <- renderVisNetwork(plot(rules_subset_filtered_top, method = "graph",  engine = "htmlwidget"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
