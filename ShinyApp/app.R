pacman:::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts
                , ggforce, tidytext, tidyverse, ggplot2, plotly, skimr
                , DT, igraph, scales, viridis, colorspace, stringr
                , knitr, wordcloud, bslib, thematic, shiny, colourpicker
                , devtools, wordcloud2, tm, quanteda)
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

#---------- Closeness centrality-------------#
mc3_graph <- mc3_graph %>%
  mutate(closeness_centrality = replace(closeness_centrality, is.nan(closeness_centrality), 0))

max_close <- max(as.data.frame(mc3_graph)$closeness_centrality)
min_close <- min(as.data.frame(mc3_graph)$closeness_centrality)
mean_close <-mean(as.data.frame(mc3_graph)$closeness_centrality)

#---------- Eigen centrality-------------#
mc3_graph <- mc3_graph %>%
  mutate(eigen_centrality = replace(eigen_centrality, is.nan(eigen_centrality), 0))

max_eigen <- max(as.data.frame(mc3_graph)$eigen_centrality)
min_eigen <- min(as.data.frame(mc3_graph)$eigen_centrality)
mean_eigen <-mean(as.data.frame(mc3_graph)$eigen_centrality)


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
                 navbarMenu("Network Graph",
                            tabPanel("Betweenness Centrality",
                                     titlePanel("Betweenness Centrality"),
                                     
                                     # Sidebar with a slider input for number of bins 
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput(inputId = "var_cluster_btw",
                                                                label = "Cluster Option",
                                                                choices = c("Louvain", "Fast Greedy", "Edge Betweenness",
                                                                            "Walktrap", "Infomap")),
                                                    sliderInput("var_btw", "Score:"
                                                                , min = mean_betweenness
                                                                , max = max_betweenness
                                                                , value = mean_betweenness
                                                                , step = 10000 
                                                                , sep = ""
                                                                , width = '100%'
                                                                , animate =
                                                                  animationOptions(interval = 2000, loop = TRUE))
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotOutput("betweenPlot")
                                       )
                                     )
                            ),
                            
                            #--------------------------------Tab 2 ------------------------------------                 
                            tabPanel("Closeness Centrality",
                                     titlePanel("Closeness Centrality"),
                                     
                                     # Sidebar with a slider input for number of bins
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput(inputId = "var_cluster_close",
                                                                label = "Cluster Option",
                                                                choices = c("Louvain", "Fast Greedy", "Edge Betweenness",
                                                                            "Walktrap", "Infomap")),
                                                    sliderInput("var_close", "Score:"
                                                                , min = min_close
                                                                , max = max_close
                                                                , value = mean_close
                                                                , step = 0.1
                                                                , sep = ""
                                                                , width = '100%'
                                                                , animate =
                                                                  animationOptions(interval = 3000, loop = TRUE))
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotOutput("closePlot")
                                       )
                                     )
                            ),
                            
                            #--------------------------------Tab 3 ------------------------------------                 
                            tabPanel("Eigenvector Centrality",
                                     titlePanel("Eigenvector Centrality"),
                                     
                                     # Sidebar with a slider input for number of bins
                                     sidebarLayout(
                                       sidebarPanel(width = 3,
                                                    selectInput(inputId = "var_cluster_eigen",
                                                                label = "Cluster Option",
                                                                choices = c("Louvain", "Fast Greedy", "Edge Betweenness",
                                                                            "Walktrap", "Infomap")),
                                                    sliderInput("var_eigen", "Score:"
                                                                , min = min_eigen
                                                                , max = max_eigen
                                                                , value = mean_eigen
                                                                #, step = 0.1
                                                                , sep = ""
                                                                , width = '100%'
                                                                , animate =
                                                                  animationOptions(interval = 3000, loop = TRUE))
                                       ),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         plotOutput("eigenPlot")
                                       )
                                     )
                            )
                            
                 ),
                 
                 #------------------------------------ Application 3 ---------------------------------------------# RT
                 navbarMenu("Text Analysis",
                            
                            #--------------------------------Tab 1 ------------------------------------   
                            tabPanel("Unigram Analysis",
                                     titlePanel("Text Analysis with tidytext for Product Services"),
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    # Allow Customized stop words inclusion
                                                    checkboxInput("remove_words", "Remove specific words?", FALSE
                                                                  ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1",
                                                      textAreaInput("words_to_remove1", "Words to remove (one per line)", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove1.length > 0",
                                                      textAreaInput("words_to_remove2", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove2.length > 0",
                                                      textAreaInput("words_to_remove3", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove3.length > 0",
                                                      textAreaInput("words_to_remove4", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove4.length > 0",
                                                      textAreaInput("words_to_remove5", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove5.length > 0",
                                                      textAreaInput("words_to_remove6", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove6.length > 0",
                                                      textAreaInput("words_to_remove7", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove7.length > 0",
                                                      textAreaInput("words_to_remove8", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove8.length > 0",
                                                      textAreaInput("words_to_remove9", "", rows = 1)
                                                      ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words == 1 && input.words_to_remove9.length > 0",
                                                      textAreaInput("words_to_remove10", "", rows = 1)
                                                      ),
                                                    numericInput("num", "Maximum number of words",
                                                                 value = 100, min = 5
                                                                 ),
                                                    colourInput("col", "Background color", value = "white")
                                                    ),
                                       mainPanel(
                                         wordcloud2Output("cloud")
                                         )
                                       )
                                     ),
                            #--------------------------------Tab 2 ------------------------------------   
                            tabPanel("Bigram Analysis",
                                     titlePanel("Text Analysis with tidytext for Product Services"),
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    # Allow Customized stop words inclusion
                                                    checkboxInput("remove_words_bi", "Remove specific words?", FALSE
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1",
                                                      textAreaInput("words_to_remove_bi_1", "Words to remove (one per line)", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_1.length > 0",
                                                      textAreaInput("words_to_remove_bi_2", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_2.length > 0",
                                                      textAreaInput("words_to_remove_bi_3", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_3.length > 0",
                                                      textAreaInput("words_to_remove_bi_4", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_4.length > 0",
                                                      textAreaInput("words_to_remove_bi_5", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_5.length > 0",
                                                      textAreaInput("words_to_remove_bi_6", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_6.length > 0",
                                                      textAreaInput("words_to_remove_bi_7", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_7.length > 0",
                                                      textAreaInput("words_to_remove_bi_8", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_8.length > 0",
                                                      textAreaInput("words_to_remove_bi_9", "", rows = 1)
                                                    ),
                                                    conditionalPanel(
                                                      condition = "input.remove_words_bi == 1 && input.words_to_remove_bi_9.length > 0",
                                                      textAreaInput("words_to_remove_bi_10", "", rows = 1)
                                                    )
                                       ),
                                       mainPanel(
                                         DTOutput("filtered_tbl")
                                       )
                                     )
                            ),
                            
                            
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
    
    #--- To change after obtaining filter of words ---#
    mc3_graph_filtered <- mc3_graph
    
    #-------------------------------------------------#
    filtered_graph <- mc3_graph_filtered %>%
      filter(betweenness_centrality >= input$var_btw)
    
    single_nodes <- V(filtered_graph)[degree(filtered_graph) == 0]
    
    # Remove single nodes from the filtered graph
    filtered_graph <- delete.vertices(filtered_graph, single_nodes)
    
    if (input$var_cluster_btw == "Louvain") {
      GNC <- cluster_louvain(filtered_graph, weights = NULL)
    } else if (input$var_cluster_btw == "Fast Greedy") {
      GNC <- cluster_fast_greedy(filtered_graph, weights = NULL)
    } else if (input$var_cluster_btw == "Edge Betweenness") {
      GNC <- cluster_edge_betweenness(filtered_graph, weights = NULL)
    } else if (input$var_cluster_btw == "Walktrap") {
      GNC <- cluster_walktrap(filtered_graph, weights = NULL)
    } else {
      GNC <- cluster_infomap(filtered_graph)
    }
    
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
    
    community <- as.factor(membership(GNC))
    # Plot the filtered graph
    ggraph(filtered_graph, layout = "fr") +
      geom_edge_link(aes(alpha = 0.5)) +
      geom_node_point(aes(size = betweenness_centrality, color = community), alpha = 0.5) +
      #geom_node_text(aes(label = membership(GNC)), vjust = -1) + ##Label community number on graph
      scale_size_continuous(range = c(1, 10)) +
      scale_color_manual(values = node_colors) +  # Set the node colors manually
      #guides(color = FALSE) +  # Remove the color legend
      theme_graph()
    
  })
  
  output$betweenPlot <- renderPlot({
    btw_ggraph()
  })
  
  #--------------------------------- Network Closeness Code ------------------------------------# PY
  
  close_ggraph <- reactive({
    #--- To change after obtaining filter of words ---#
    mc3_graph_filtered <- mc3_graph
    
    #-------------------------------------------------#
    
    filtered_graph <- mc3_graph_filtered %>%
      filter(closeness_centrality >= input$var_close)
    
    # Identify single nodes (degree 0)
    single_nodes <- V(filtered_graph)[degree(filtered_graph) == 0]
    
    # Remove single nodes from the filtered graph
    filtered_graph <- delete.vertices(filtered_graph, single_nodes)
    
    
    if (input$var_cluster_close == "Louvain") {
      GNC <- cluster_louvain(filtered_graph, weights = NULL)
    } else if (input$var_cluster_close == "Fast Greedy") {
      GNC <- cluster_fast_greedy(filtered_graph, weights = NULL)
    } else if (input$var_cluster_close == "Edge Betweenness") {
      GNC <- cluster_edge_betweenness(filtered_graph, weights = NULL)
    } else if (input$var_cluster_close == "Walktrap") {
      GNC <- cluster_walktrap(filtered_graph, weights = NULL)
    } else {
      GNC <- cluster_infomap(filtered_graph)
    }
    
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
    
    
    community <- as.factor(membership(GNC))
    # Plot the filtered graph
    ggraph(filtered_graph, layout = "fr") +
      geom_edge_link(aes(alpha = 0.5)) +
      geom_node_point(aes(size = closeness_centrality, color = community), alpha = 0.5) +
      #geom_node_text(aes(label = membership(GNC)), vjust = -1) + ##Label community number on graph
      scale_size_continuous(range = c(1, 10)) +
      scale_color_manual(values = node_colors) +  # Set the node colors manually
      guides(color = FALSE) +  # Remove the color legend
      theme_graph()
    
  })
  
  output$closePlot <- renderPlot({
    close_ggraph()
  })
  
  #--------------------------------- Eigenvector Code ------------------------------------# PY
  
  eigen_ggraph <- reactive({
    #--- To change after obtaining filter of words ---#
    mc3_graph_filtered <- mc3_graph
    
    #-------------------------------------------------#
    
    filtered_graph <- mc3_graph_filtered %>%
      filter(eigen_centrality >= input$var_eigen)
    
    
    # Identify single nodes (degree 0)
    single_nodes <- V(filtered_graph)[degree(filtered_graph) == 0]
    
    # Remove single nodes from the filtered graph
    filtered_graph <- delete.vertices(filtered_graph, single_nodes)
    
    if (input$var_cluster_eigen == "Louvain") {
      GNC <- cluster_louvain(filtered_graph, weights = NULL)
    } else if (input$var_cluster_eigen == "Fast Greedy") {
      GNC <- cluster_fast_greedy(filtered_graph, weights = NULL)
    } else if (input$var_cluster_eigen == "Edge Betweenness") {
      GNC <- cluster_edge_betweenness(filtered_graph, weights = NULL)
    } else if (input$var_cluster_eigen == "Walktrap") {
      GNC <- cluster_walktrap(filtered_graph, weights = NULL)
    } else {
      GNC <- cluster_infomap(filtered_graph)
    }
    
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
    
    community <- as.factor(membership(GNC))
    # Plot the filtered graph
    ggraph(filtered_graph, layout = "fr") +
      geom_edge_link(aes(alpha = 0.5)) +
      geom_node_point(aes(size = eigen_centrality, color = community), alpha = 0.5) +
      #geom_node_text(aes(label = membership(GNC)), vjust = -1) + ##Label community number on graph
      scale_size_continuous(range = c(1, 10)) +
      scale_color_manual(values = node_colors) +  # Set the node colors manually
      #guides(color = FALSE) +  # Remove the color legend
      theme_graph()
    
  })
  
  output$eigenPlot <- renderPlot({
    eigen_ggraph()
  })
 
  #--------------------------------- Text Analysis Code ------------------------------------# RT
  
  #--------------------------------- Unigram Analysis - wordcloud --------------------------# RT
  
  
  wrdcloud <- reactive({
    token_nodes <- mc3_nodes %>%
      unnest_tokens(word, 
                    product_services)
  
  new_stop_words <- stop_words %>% 
    add_row(word=NA,lexicon="SMART") %>% 
    add_row(word="products",lexicon="SMART") %>% 
    add_row(word="services",lexicon="SMART") %>% 
    add_row(word="related",lexicon="SMART") %>% 
    add_row(word="unknown",lexicon="SMART") %>% 
    add_row(word="character",lexicon="SMART") %>% 
    add_row(word="including",lexicon="SMART")
  
  token_nodes %>% 
    anti_join(new_stop_words) 
  
  })
  
  create_wordcloud <- function(data, num_words = 100, background = "white") {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data$word)) {
      corpus <- Corpus(VectorSource(data$word))
      corpus <- tm_map(corpus, tolower)
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeNumbers)
      corpus <- tm_map(corpus, removeWords, stopwords(tolower("English")))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove1))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove2))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove3))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove4))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove5))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove6))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove7))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove8))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove9))
      corpus <- tm_map(corpus, removeWords, c(input$words_to_remove10))
      tdm <- as.matrix(TermDocumentMatrix(corpus))
      data <- sort(rowSums(tdm), decreasing = TRUE)
      data <- data.frame(word = names(data), freq = as.numeric(data))
    }
    
    # Make sure a proper num_words is provided
    if (!is.numeric(num_words) || num_words < 3) {
      num_words <- 3
    }
    
    # Grab the top n most common words
    data <- head(data, n = num_words)
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    set.seed(1234)
    wordcloud2(data, backgroundColor = background)
  }
  
  output$cloud <- renderWordcloud2({
    create_wordcloud(wrdcloud(),
                     num_words = input$num,
                     background = input$col
    )
  })

  #--------------------------------- Bigram Analysis - wordcloud --------------------------# RT
  
  
  bigram <- reactive({
    token_nodes2 <- mc3_nodes %>%
      unnest_tokens(
        input = product_services, 
        output = bigram, 
        token = 'ngrams', 
        n = 2) %>%
      filter(! is.na(bigram))
    
    new_stop_words_bi <- stop_words %>% 
      add_row(word=NA,lexicon="SMART") %>%
      add_row(word="related",lexicon="SMART") %>%
      add_row(word="including",lexicon="SMART") %>%
      add_row(word="wide",lexicon="SMART") %>%
      add_row(word="range",lexicon="SMART") %>%
      add_row(word="freelance",lexicon="SMART") %>%
      add_row(word="researcher",lexicon="SMART") %>%
      add_row(word="products",lexicon="SMART") %>%
      add_row(word="processing",lexicon="SMART") %>%
      add_row(word="preparations",lexicon="SMART") %>%
      add_row(word="food",lexicon="SMART") %>%
      add_row(word="items",lexicon="SMART") %>%
      add_row(word="character",lexicon="SMART") %>%
      add_row(word="0",lexicon="SMART") %>%
      add_row(word="unknown",lexicon="SMART") %>%
      add_row(word="services",lexicon="SMART")
    
    token_nodes2 %>%  
      separate(col = bigram, into = c('word1', 'word2'), sep = ' ') %>% 
      filter(! word1 %in% new_stop_words_bi$word) %>% 
      filter(! word2 %in% new_stop_words_bi$word) %>% 
      filter(! is.na(word1)) %>% 
      filter(! is.na(word2))%>% 
      select(word1, word2)
    
  })
  
  create_datatable <- function(data) {
    if (is.character(data$word1) & is.character(data$word2)) {
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_1) %>% 
        filter(! word2 %in% input$words_to_remove_bi_1)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_2) %>% 
        filter(! word2 %in% input$words_to_remove_bi_2)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_3) %>% 
        filter(! word2 %in% input$words_to_remove_bi_3)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_4) %>% 
        filter(! word2 %in% input$words_to_remove_bi_4)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_5) %>% 
        filter(! word2 %in% input$words_to_remove_bi_5)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_6) %>% 
        filter(! word2 %in% input$words_to_remove_bi_6)
      data <- data %>%
        filter(! word1 %in% input$words_to_remove_bi_7) %>% 
        filter(! word2 %in% input$words_to_remove_bi_7)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_8) %>% 
        filter(! word2 %in% input$words_to_remove_bi_8)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_9) %>% 
        filter(! word2 %in% input$words_to_remove_bi_9)
      data <- data %>% 
        filter(! word1 %in% input$words_to_remove_bi_10) %>% 
        filter(! word2 %in% input$words_to_remove_bi_10)
      
    }

    data <- data %>%
      count(word1, word2, sort = TRUE) %>%
      # We rename the weight column so that the 
      # associated network gets the weights (see below).
      rename(weight = n)
    
    datatable(data)
  }
  
  
  output$filtered_tbl = renderDT(create_datatable(bigram()))
  
  
}
  
   


# Run the application 
shinyApp(ui = ui, server = server)
