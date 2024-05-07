library(shiny)
library(visNetwork)
library(tidyverse)
library(data.table)
library(DT)
library(arules)
library(arulesViz)
library(plotly)

## Load data ----
data <- read.delim("data/grocery_transactional.txt", sep = ',', stringsAsFactors = FALSE)

# Additional metrics
rules_metrics <- function(rules, by_split= 'support') {
    capture.output(
        out <- bind_cols(
            inspectDT(rules)$x$data, 
            interestMeasure(rules, c("oddsRatio", "leverage", "chiSquared"), significance=TRUE),
            tibble(lhs_length = size(rules@lhs))
        )
    )
    # 4 decimals
    out <- mutate_if(out, is.double, function(x) round(x, 4))
    # Quantiles
    out$cuts <- cut(out[[by_split]], breaks=4, labels = LETTERS[4:1], include.lowest=TRUE)
    
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
association_rules <- apriori(tr1, parameter = list(support=0.005, confidence=0.25, minlen=3, maxtime = 0), control = list(verbose = FALSE))
# Clean up
association_rules <- arules::sort(association_rules, by = "support")
association_rules <- association_rules[!is.redundant(association_rules, measure  = 'support')]
association_rules <- association_rules[is.significant(association_rules)]

## Define UI for application ----
ui <- navbarPage(title = "Fresh.Shop",
    tabPanel("Top rules",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     sliderInput('top', 'Top rules:', min = 1, max = 50, value = 10),
                     selectInput('by_sort','Sorting criteria:', choices=c('confidence', 'lift', 'support'), selected = 'support'),
                     br(),
                     span("Select the top rules based on the proposed sorting criteria. It's recommended the 'support'."),
                     br(),
                     br(),
                     span("The 'Network' plot, shows the rules LHS and RHS."),
                     br(),
                     span("The size represents the 'support', the color represents the 'lift'")
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Network", 
                                          span("Use the drop down or click to selct a node."),
                                          br(),
                                          br(),
                                          visNetworkOutput('plot_graph_html', height = '800px')
                                 ),
                                 tabPanel("Additional", 
                                          plotlyOutput('plot_matrix'),
                                          br(),
                                          span("Click the objects to display the association rule description."),
                                          br(),
                                          plotOutput('plot_grouped_top'),
                                          # plotOutput('plot_graph', height = '800px'),
                                          plotOutput('plot_paracord')
                                 )
                     )
                 )
             )
    ),
    tabPanel("All rules",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     span("These rules includes subsets, this means longer rules are included."),
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Interactive", 
                                          span("Click the objects to display the association rule description."),
                                          br(),
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
    tabPanel("Rules list",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     selectInput('by_split','Strata criteria:', choices=c('confidence', 'lift', 'support'), selected = 'support'),
                     br(),
                     span("These rules had removed the possible redundant and no-significance"),
                     br(),
                     span("The color represents how 'hot' is the rule."),
                     br(),
                     span("Hotter rule should be taken action first"),
                     br(),
                     br(),
                     span("The rightmost column 'cut', clusters the associations rules")
                     
                 ),
                 mainPanel(
                      DT::dataTableOutput('table_association_rules', height = '800px')
                 )
             )
    ),
    tabPanel("Descriptive study",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     span("General descriptive information of the transactions and rules")
                 ),
                 mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Plots", 
                                     plotOutput('plot_items_ticket'),
                                     plotOutput('plot_relative'),
                                     plotOutput('plot_absolute')
                                 ),
                                 tabPanel("Summary",
                                     h3("Transactions"),
                                     verbatimTextOutput('summary_tr'),
                                     br(),
                                     h3("All association rules"),
                                     verbatimTextOutput('summary_association_rules')
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
        # https://rstudio.github.io/DT/options.html
        out <- DT::datatable(rules_metrics(association_rules, input$by_split),
                             rownames = FALSE,
                             list(pageLength = 30, order = list(12, 'desc'))) %>%
            formatStyle('cuts', target = 'row', backgroundColor = styleEqual(LETTERS[1:4], heat.colors(4)))
        output$table_association_rules <- DT::renderDataTable(out)
    })
    
    # All the rules following the previous criteria
    output$plot_scatterplot <- renderPlot(plot(association_rules, method = "scatterplot", jitter = 0))
    output$plot_two_key <- renderPlot(plot(association_rules, method = "two-key plot", jitter = 0))
    output$plot_grouped <- renderPlot(plot(association_rules, method = "grouped"))
    output$plot_plotly <- renderPlotly(plot(association_rules, engine = "plotly", jitter = 0))
    
    observeEvent(c(input$top, input$by_sort), ignoreNULL = FALSE, ignoreInit = FALSE, {
        set.seed(42)
        
        # Top 10 rules
        association_rules_top <- head(association_rules, n = input$top, by = input$by_sort)
        output$plot_matrix <- renderPlotly(plot(association_rules_top, method = "matrix", measure = "lift", engine = "plotly"))
        output$plot_graph <- renderPlot(plot(association_rules_top, method = "graph"))
        output$plot_paracord <- renderPlot(plot(association_rules_top, method = "paracoord"))
        output$plot_graph_html <- renderVisNetwork(plot(association_rules_top, method = "graph",  engine = "htmlwidget"))
        output$plot_grouped_top <- renderPlot(plot(association_rules_top, method = "grouped"))
    })
    
    #Summaries
    output$summary_tr <- renderPrint(summary_)
    output$summary_association_rules <- renderPrint(summary(association_rules))

}

# Run the application 
shinyApp(ui = ui, server = server)
