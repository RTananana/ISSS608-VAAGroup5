pacman:::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts
                , ggforce, tidytext, tidyverse, ggplot2, plotly, skimr
                , DT, igraph, scales, viridis, colorspace, stringr
                , knitr, wordcloud, bslib, thematic)
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



#------------------------ Network Graph Analysis --------------------------------# PY
#---------- Graph Creation-------------#

id1 <- mc3_edges %>%
  select(source) %>%
  rename(id = source)
id2 <- mc3_edges %>%
  select(target) %>%
  rename(id = target)
mc3_nodes1 <- rbind(id1, id2) %>%
  distinct() %>%
  left_join(mc3_nodes,
            unmatched = "drop")


mc3_graph <- tbl_graph(nodes = mc3_nodes1,
                       edges = mc3_edges,
                       directed = FALSE)%>%
  mutate(betweenness_centrality = centrality_betweenness(),
         closeness_centrality = centrality_closeness(),
         eigen_centrality = centrality_eigen())

#---------- Betweenness centrality-------------#
max_betweenness <- max(as.data.frame(mc3_graph)$betweenness_centrality)
min_betweenness <- min(as.data.frame(mc3_graph)$betweenness_centrality)
mean_betweenness <-mean(as.data.frame(mc3_graph)$betweenness_centrality)


# Shiny App UI--------------------------------------------------------------------------------------------------- 

# Define UI for application that draws a histogram
ui <- navbarPage("Illegal Fishing Network Analysis",
                 theme = "https://bootswatch.com/3/cosmo/bootstrap.min.css",
                 
  #------------------------------------ Application 1 ---------------------------------------------# PY
                  # Application title
                  tabPanel("Nodes Distribution",
                    titlePanel("Nodes Types Distribution"),
                  
                  # Sidebar with a select input for country 
                    sidebarLayout(
                      sidebarPanel(width =3,
                        selectInput(inputId = "variable_cty",
                                    label = "Please select Country",
                                    choices = c("All",unique(mc3_nodes$country)))
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotlyOutput("barPlot")
                      )
                    )
                  ),
  #------------------------------------ Application 2 ---------------------------------------------# PY
                 # Application title
                 tabPanel("Network Graph",
                    titlePanel("Betweenness Centrality"),
                    
                    # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                      sidebarPanel(width = 3,
                        sliderInput("var_btw", "Score:", min = min_betweenness, max = max_betweenness, value = mean_betweenness, sep = "", width = '100%')
                      ),
                      
                      # Show a plot of the generated distribution
                      mainPanel(
                        plotOutput("betweenPlot")
                      )
                    )
                 )
                )

#---------------------------------------------------------------------------------------------------------------#

server <- function(input, output) {
  
#--------------------------------- Node Distribution Code ------------------------------------# PY
  
  node_bar_df <- reactive({
    if (input$variable_cty != 'All') {
      input_country <- input$variable_cty
      df_type <- mc3_nodes %>%
        filter(country == input_country) 
    } else{
      df_type <- mc3_nodes
    }
    
    typecount <-table(df_type$type)
    percentage_type <- typecount/ sum(typecount)
    percentage_type_labels <- paste0(round(percentage_type*100),"%")
    sub_df <- data.frame(typecount,percentage_type,percentage_type_labels)
    
    #Change column names
    colnames(sub_df)[1] <- "type"
    colnames(sub_df)[2] <- "Count"
    colnames(sub_df)[3] <- "type2"
    colnames(sub_df)[4] <- "percentage_type"
    
     sub_df |> 
      plot_ly() |> 
      add_trace(
        labels = ~type,
        values = ~Count, 
        type = "pie") %>%
      layout(title = 'Nodes Distribution', plot_bgcolor = "#e5ecf6")
    
  })
  
  output$barPlot <- renderPlotly({
    node_bar_df()
  })
  
  #--------------------------------- Network Betweenness Code ------------------------------------# PY
  
  btw_ggraph <- reactive({
    
    filtered_graph <- mc3_graph %>%
      filter(betweenness_centrality >= input$var_btw)
    
    single_nodes <- V(filtered_graph)[degree(filtered_graph) == 0]
    
    # Remove single nodes from the filtered graph
    filtered_graph <- delete.vertices(filtered_graph, single_nodes)
    
    
    GNC <- cluster_louvain(filtered_graph, weights = NULL)
    
    set.seed(1234)
    # Get the unique social groups in the filtered graph
    unique_groups <- unique(membership(GNC))
    
    # Set the node colors using the rainbow_hcl palette from the colorspace package
    node_colors <- rainbow_hcl(length(unique_groups))
    
    # Add the node colors to the filtered graph
    V(filtered_graph)$color <- node_colors[membership(GNC)]
    
    # Create a data frame with the membership numbers and corresponding colors
    # Create a data frame with the membership numbers, colors, and labels
    legend_data <- data.frame(Membership = unique_groups, Color = node_colors)
    
    
    # Plot the filtered graph
    ggraph(filtered_graph, layout = "fr") +
      geom_edge_link(aes(alpha = 0.5)) +
      geom_node_point(aes(size = betweenness_centrality, color = as.factor(membership(GNC))), alpha = 0.5) +
      geom_node_text(aes(label = membership(GNC)), vjust = -1) +
      scale_size_continuous(range = c(1, 10)) +
      scale_color_manual(values = node_colors) +  # Set the node colors manually
      guides(color = FALSE) +  # Remove the color legend
      theme_graph()
    
  })
  
  output$betweenPlot <- renderPlot({
    btw_ggraph()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
