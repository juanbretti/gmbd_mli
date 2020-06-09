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

# Probabilities for groups
probs <- c(0, 0.075, 0.225, 0.4, 0.225, 0.075)
probs_label <- paste0(LETTERS[1:5], ' ', probs[2:6]*100, '%')

# Functions
rules_metrics <- function(rules, tr, by_split) {
    capture.output(
        out <- bind_cols(
            inspectDT(rules)$x$data, 
            interestMeasure(rules, c("oddsRatio", "leverage"), transactions = tr),
            tibble(lhs_length = size(rules@lhs))
        )
    )
    # 4 decimals
    out <- mutate_if(out, is.double, function(x) round(x, 4))
    # Quantiles
    q_ <- quantile(out[[by_split]], probs=cumsum(probs))
    q_[1] <- 0
    out$cuts <- cut(out[[by_split]], breaks=as.numeric(q_), labels = probs_label[5:1], include.lowest = FALSE)
    
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
# Clean up subset rules
rules_subset <- which(colSums(is.subset(association_rules, association_rules)) > 1)
rules_subset <- association_rules[-rules_subset] # remove subset rules.

## Define UI for application ----
ui <- navbarPage(title = "Fresh.Shop",
    tabPanel("Top rules",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     sliderInput('top', 'Top rules:', min = 1, max = 50, value = 10),
                     selectInput('by_sort','Sorting criteria:', choices=c('confidence', 'lift', 'support'), selected = 'support')
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
    tabPanel("List of rules",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br(),
                     selectInput('by_split','Strata criteria:', choices=c('confidence', 'lift', 'support'), selected = 'support')
                 ),
                 mainPanel(
                      DT::dataTableOutput('table_association_rules', height = '800px')
                 )
             )
    ),
    tabPanel("Descriptive",
             sidebarLayout(
                 sidebarPanel(
                     div(img(src="logo3.png",height=110,width=300), style="text-align: center;"),
                     br()
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
                                     h3("Association rules"),
                                     verbatimTextOutput('summary_rules')
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
        out <- DT::datatable(rules_metrics(rules_subset, tr1, input$by_split),
                             rownames = FALSE,
                             list(pageLength = 30, order = list(11, 'desc'))) %>%
            formatStyle('cuts', target = 'row', backgroundColor = styleEqual(probs_label, heat.colors(5)))
        output$table_association_rules <- DT::renderDataTable(out)
    })
    
    # All the rules following the previous criteria
    output$plot_scatterplot <- renderPlot(plot(rules_subset, method = "scatterplot", jitter = 0))
    output$plot_two_key <- renderPlot(plot(rules_subset, method = "two-key plot", jitter = 0))
    output$plot_grouped <- renderPlot(plot(rules_subset, method = "grouped"))
    output$plot_plotly <- renderPlotly(plot(rules_subset, engine = "plotly", jitter = 0))
    
    observeEvent(c(input$top, input$by_sort), ignoreNULL = FALSE, ignoreInit = FALSE, {
        set.seed(42)
        
        # Top 10 rules
        rules_subset_top <- head(rules_subset, n = input$top, by = input$by_sort)
        output$plot_matrix <- renderPlotly(plot(rules_subset_top, method = "matrix", measure = "lift", engine = "plotly"))
        output$plot_graph <- renderPlot(plot(rules_subset_top, method = "graph"))
        output$plot_paracord <- renderPlot(plot(rules_subset_top, method = "paracoord"))
        output$plot_graph_html <- renderVisNetwork(plot(rules_subset_top, method = "graph",  engine = "htmlwidget"))
    })
    
    #Summaries
    output$summary_tr <- renderPrint(summary_)
    output$summary_rules <- renderPrint(summary(rules_subset))

}

# Run the application 
shinyApp(ui = ui, server = server)
