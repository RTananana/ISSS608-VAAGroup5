pacman:::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts
                , ggforce, tidytext, tidyverse, ggplot2, plotly, skimr
                , DT, igraph, scales, viridis, colorspace, stringr
                , knitr, wordcloud)
# Code Chuck----------------------------------------------------------------------------------------------------
#------ Read Data
mc3 <-fromJSON("data/MC3.json")

#------ Extraction of nodes and Edges
#--- Nodes ---

mc3_nodes <- as_tibble(mc3$nodes) %>%
  distinct() %>%
  mutate(country = as.character(country),
         id = as.character(id),
         product_services = as.character(product_services),
         revenue_omu = as.numeric(as.character(revenue_omu)),
         type = as.character(type)) %>%
  select (id, country, type, revenue_omu, product_services)

#--- Edges ---
mc3_edges <- as_tibble(mc3$links) %>%
  distinct() %>%
  mutate(source = as.character(source),
         target = as.character(target),
         type = as.character(type)) %>%
  group_by(source, target, type) %>% 
  summarise(weights=n()) %>%
  filter(source!=target) %>%
  ungroup()

# --- Data Analysis ----
nodecount_df <- mc3_nodes %>%
  group_by(country) %>%
  summarise(nodes_count = n_distinct(id)) 




# Shiny App UI--------------------------------------------------------------------------------------------------- 
# Define UI for application that draws a histogram

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Revenue OMU over Countries"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable_cty",
                  label = "Please select Countries",
                  choices = unique(mc3_nodes$country))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  nodecount_df <- reactive({
    input_country <- input$variable_cty
    sub_df <- mc3_nodes %>%
      filter(country == input_country) %>%
      group_by(country) %>%
      summarise(nodes_count = n_distinct(id)) 
  })
  
  output$barPlot <- renderPlot({
    
    ggplot(nodecount_df(), aes(x = country, y = nodes_count)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(x = "Country", y = "Revenue OMU") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
