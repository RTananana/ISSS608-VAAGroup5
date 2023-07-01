pacman:::p_load(jsonlite, tidygraph, ggraph, visNetwork, graphlayouts
                , ggforce, tidytext, tidyverse, ggplot2, plotly, skimr
                , DT, igraph, scales, viridis, colorspace, stringr
                , knitr, wordcloud, bslib, thematic, shiny, colourpicker
                , devtools, wordcloud2, tm, quanteda, networkD3, topicmodels
                , ldatuning, shinycssloaders, ggwordcloud, hrbrthemes, treemapify
                , treemap, RColorBrewer)

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=1)

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
  select (id, country, type, revenue_omu, product_services) %>% 
  mutate(Type = recode(type, "Company" = 'Company Contacts')) %>% 
  select(id,country,Type,product_services, revenue_omu)

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

# Extract onwers with more than 3 companies
owners<-mc3_edges %>% 
  filter(type=='Beneficial Owner') %>% 
  group_by(target) %>% 
  summarise(company_count=n()) %>% 
  filter(company_count>3)

# Extract company information 
cp_oh<-mc3_edges %>%
  filter(type=='Beneficial Owner') %>% 
  filter(target %in% owners$target) %>% 
  select(source)

# Extract edge information
oh_edges <- mc3_edges %>%
  filter(type=='Beneficial Owner') %>% 
  filter(target %in% owners$target | source %in% cp_oh)

# Extract node information  
oh_id1<-oh_edges %>% 
  select(source) %>%
  rename(id = source) %>% 
  mutate(type='Company') 

oh_id2 <- oh_edges %>%
  select(target, type) %>%
  rename(id = target) 

oh_nodes <- rbind(oh_id1, oh_id2) %>% 
  distinct()

# Create owner_graph
oh_graph <- as_tbl_graph(oh_edges, directed = FALSE)

oh_graph<-oh_graph %>% 
  activate(nodes) %>% 
  left_join(oh_nodes, by=c("name"="id")) %>% 
  mutate(betweenness_centrality = centrality_betweenness(),
         closeness_centrality = centrality_closeness(),
         eigen_centrality = centrality_eigen(),
         degree_centrality = centrality_degree(),
         pagerank_centrality = centrality_pagerank(),
         authority_centrality = centrality_authority())

#-------------------------- Individual's companies count--------------------#

#Filter individual owners from edge table, and assign target to be source and vice versa
mc3_edges_indi <- mc3_edges %>%
  select (target, source, type, weights) %>%
  group_by(target) %>%
  filter(type == "Beneficial Owner") %>%
  rename (src = target) %>%
  rename (tgt = source) %>%
  distinct() %>%
  ungroup()


# Shiny App UI--------------------------------------------------------------------------------------------------- 

# Define UI for application that draws a histogram
ui <- navbarPage("Illegal Fishing Network Analysis",
                 theme = "https://bootswatch.com/3/cosmo/bootstrap.min.css",
                 
                 #------------------------------------ Application 1 ---------------------------------------------# RT
                 # Application title
                 tabPanel("Data Table Search",
                          mainPanel(
                            fluidRow(
                              column(width=12,
                                     fluidRow(withSpinner(DTOutput("data_tbl",width = "100%", height = "800px"), type = 3))
                                     )
                              )
                            )
                          ),
                 
                 #------------------------------------ Application 2 ---------------------------------------------# RT
                 # Application title
                 tabPanel("EDA",
                          # Sidebar with a select input for country
                          sidebarLayout(
                            sidebarPanel(width =2,
                                         h4("For Violin PLot and Pie Chart"),
                                         selectInput(inputId = "violin_country",
                                                     label = "Please select Country",
                                                     choices = c("All",unique(mc3_nodes$country))),
                                         sliderInput("revenue_range",
                                                     "Revenue Range:",
                                                     min = 3652.227,
                                                     max = 310612303,
                                                     value = c(3652.227,310612303)),
                                         h4("For Tree Map and Histogram"),
                                         sliderInput(inputId = "top_n",
                                                     label = "Select Number of Top Countries",
                                                     min = 5,  max = 40,  value = 5, step = 5)
                            ),
                            # Show a plot of the generated distribution
                            mainPanel(
                              fluidRow(
                                column(width=6,
                                       fluidRow(withSpinner(plotlyOutput("violinPlot"), type = 3),
                                                style = "height:400px"),
                                       fluidRow(withSpinner(plotlyOutput("TreeMap"), type = 3),
                                                style = "height:400px")
                                ),
                                column(width=6,
                                       fluidRow(withSpinner(plotlyOutput("barPlot"), type = 3),
                                                style = "height:400px"),
                                       fluidRow(withSpinner(plotlyOutput("hist"), type = 3),
                                                style = "height:400px")
                                )
                              )
                            )
                          )
                 ),
                 
                 #------------------------------------ Application 3 ---------------------------------------------# PY
                 # Application title
                 navbarMenu("Network Graph",
                            
                            #--------------------------------Tab 1 ------------------------------------
                            tabPanel("Centrality Graph",
                                     titlePanel("Centrality Graph"),
                                     
                                     # Sidebar with a slider input for number of bins
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    selectInput(inputId = "var_centrality",
                                                                label = "Centrality Option",
                                                                choices = c("Betweenness", "Closeness",
                                                                            "Eigenvector", "Degree",
                                                                            "PageRank", 
                                                                            "Authority"))),
                                       
                                       # Show a plot of the generated distribution
                                       mainPanel(
                                         column(width = 12,
                                                height = 12,
                                                fluidRow(withSpinner(plotOutput("centralityPlot", width = "100%"), type = 3))
                                         )
                                       )
                                     )
                            ),
                            
                            #--------------------------------Tab 2 ------------------------------------                 
                            tabPanel("Network Connection",
                                     titlePanel("Network Connection"),
                                     mainPanel(width = 12,
                                               height = 12,
                                               fluidRow(withSpinner(visNetworkOutput("ConnectionPlot", width = "100%", height = "800px"), type = 3))
                                     )
                            ),
                            
                            #--------------------------------Tab 3 ------------------------------------                 
                            tabPanel("Company Count",
                                     titlePanel("Individual's Company Count"),
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
                                                    sliderInput("slideCount", "Companies' Count:", 
                                                                min = 3, 
                                                                max = 10, 
                                                                value = 5, 
                                                                width = '100%')),
                                       mainPanel(width = 10,
                                                 height = 10,
                                                 fluidRow(withSpinner(visNetworkOutput("IndvComPlot", width = "100%", height = "800px"), type = 3))
                                       )
                                     )
                            )
                 ),
                 
                 #------------------------------------ Application 4 ---------------------------------------------# RT
                 navbarMenu("Text Analysis",
                            
                            #--------------------------------Tab 1 ------------------------------------   
                            tabPanel("Unigram Analysis",
                                     titlePanel("Text Analysis with tidytext for Product Services"),
                                     sidebarLayout(
                                       sidebarPanel(width = 2,
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
                                                    sliderInput("num.topics",
                                                                "Number of Topics:",
                                                                min = 2,  max = 15,  value = 5, step = 1
                                                    ),
                                                    selectInput(inputId = "ldamethod",
                                                                label = "LDA Methods",
                                                                choices = c("Gibbs", "VEM")
                                                    ),
                                                    sliderInput("num", "Maximum number of words",
                                                                min = 5, max = 150, value = 100, step = 1
                                                    ),
                                                    colourInput("col", "Background color", value = "white")
                                       ),
                                       mainPanel(
                                         fluidRow(
                                           column(width=4,
                                                  fluidRow(withSpinner(plotOutput("tm_optimizer"), type = 3), 
                                                           style = "height:200px")
                                           ),
                                           column(width=8,
                                                  fluidRow(withSpinner(plotOutput("cloud2",width = "100%", height = "800px"), type = 3))
                                           )
                                         )
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
                                                    ),
                                                    sliderInput("threshold", 
                                                                "Threshold Level:",
                                                                min = 3,
                                                                max = 30,
                                                                value = c(3,10),
                                                                step = 1
                                                    ),
                                                    sliderInput("opacity",
                                                                "Opacity of Graph Network",
                                                                min = 0.1,
                                                                max = 1,
                                                                value = 0.8
                                                    ),
                                                    selectInput(inputId = "bigramcluster",
                                                                label = "Cluster Option",
                                                                choices = c("Louvain", "Edge Betweenness", "Walktrap", "Infomap")
                                                    ),
                                                    selectInput(inputId = "centralityindication",
                                                                label = "Centrality Option",
                                                                choices = c("Degree", "Betweenness", "Closeness"))
                                       ),
                                       mainPanel(
                                         fluidRow(
                                           column(width=8,
                                                  fluidRow(withSpinner(forceNetworkOutput(outputId = "net",width = "100%", height = "800px"), type = 3))
                                           ),
                                           column(width = 4,
                                                  fluidRow(withSpinner(DTOutput("filtered_tbl",width = "100%", height = "400px"), type = 3)),
                                                  fluidRow(withSpinner(DTOutput("centrality_tbl",width = "100%", height = "400px"), type = 3))
                                           )
                                         )
                                       )
                                     )
                            )
                 )
)



#---------------------------------------------------------------------------------------------------------------#

server <- function(input, output) {
  
  #--------------------------------- data search ------------------------------------# RT
  
  output$data_tbl = renderDT(
    mc3_nodes, options = list(pageLength = 20)
    )
  
  #--------------------------------- Violin Plot ------------------------------------# RT
  
  mc3_nodes_violin<- reactive({
    
    violin <- mc3_nodes %>%
      select(id, country, Type, revenue_omu)
    
    violin$revenue_omu <- as.numeric(violin$revenue_omu)
    
    if (input$violin_country != 'All') {
      input_violin_country <- input$violin_country
      violin <- violin %>%
        filter(country == input_violin_country) %>% 
        filter(`revenue_omu` >= input$revenue_range[1]) %>%
        filter(`revenue_omu` <= input$revenue_range[2])
    } else{
      violin <- violin  %>%
        filter(`revenue_omu` >= input$revenue_range[1]) %>%
        filter(`revenue_omu` <= input$revenue_range[2])
    }
    
    
    violin_plot <- violin %>%
      plot_ly(x = ~Type, y = ~revenue_omu, split = ~Type, 
              type = 'violin', text=~id,
              box = list(visible = T),
              meanline = list(visible = T),
              color=~Type,
              colors = c("darkorange", "steelblue"))  %>%
      layout(title="Violin Plot of Operating Revenue by Type")
    
    
    
    violin_plot %>%
      layout(
        xaxis = list(
          title = "Type of Node"
        ),
        yaxis = list(
          title = "Operating Revenue",
          zeroline = F
        )
      ) 
  })
  
  output$violinPlot <- renderPlotly({
    mc3_nodes_violin()
  })
  
  #--------------------------------- Node Distribution Code ------------------------------------# PY
  
  node_bar_df <- reactive({
    if (input$violin_country != 'All') {
      input_country <- input$violin_country
      df_type <- mc3_nodes %>%
        filter(country == input_country) 
    } else{
      df_type <- mc3_nodes
    }
    
    typecount <-table(df_type$Type)
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
  
  #-------------------------------------- Tree Map Code ---------------------------------------# Abhi
  
  
  
  treemap_prep <- reactive({
    
    top_countries <- mc3_nodes %>%
      group_by(country) %>%
      summarise(total_revenue = sum(revenue_omu, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_revenue)) %>% 
      head(input$top_n) %>% 
      pull(country)
    
    
    if (input$violin_country != 'All') {
      input_violin_country <- input$violin_country
      treemap <- mc3_nodes %>%
        filter(country == input_violin_country)  %>%
        filter(Type=="Company Contacts") %>% 
        group_by(country,id) %>%
        summarise(company_revenue = sum(revenue_omu, na.rm = TRUE), .groups = "drop") %>%
        arrange(country, desc(company_revenue)) %>%
        group_by(country) %>%
        slice_max(order_by = company_revenue, n = input$top_n)
    } else{
      treemap <- mc3_nodes %>%
        filter(country == top_countries) %>%
        filter(Type=="Company Contacts") %>% 
        group_by(country,id) %>%
        summarise(company_revenue = sum(revenue_omu, na.rm = TRUE), .groups = "drop") %>%
        arrange(country, desc(company_revenue)) 
    }
    
    
    treemap$test<-"world"
    
    plot_ly(
      data = treemap,
      type="treemap",
      values=~company_revenue,
      labels=~country,
      ids=~id,
      parents=~test,
      domain=list(column=0),
      textinfo="label+ids",
      marker = list(
        colors = setNames(viridisLite::viridis(length(unique(treemap$country))),
                          unique(treemap$country))),
      hovertemplate = "<b>%{label} </b> <br>%{id} <br> Revenue: %{value}<extra></extra>") %>%
      layout(title="Top Companies by Revenue in Selected Country", font = t,
             annotations =
               list(x = 0, y = -0.1,
                    title = "",
                    text = " ",
                    showarrow = F,
                    xref='paper',
                    yref='paper'))
    
  })
  
  
  output$TreeMap <- renderPlotly({
    treemap_prep()
  })
  
  
  
  
  
  #--------------------------------- Country vs Type Hist ------------------------------------# Abhi
  
  histo_prep <- reactive({
    
    country_count <- mc3_nodes %>%
      count(country, Type) %>%
      group_by(country) %>%
      mutate(total = sum(n)) %>%
      ungroup()
    
    top_n_countries <- country_count %>%
      arrange(desc(total)) %>%
      mutate(rank = dense_rank(-total)) %>%
      filter(rank <= input$top_n)
    
    ggplot(top_n_countries, aes(x = reorder(country, -total), y = n, fill = Type)) +
      geom_col(show.legend = FALSE) +
      scale_fill_brewer(palette = "Paired") +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Country", y = "Count", title = paste("Top", input$top_n, "Countries by Type"))
    
  })
  
  
  
  output$hist <- renderPlotly({
    ggplotly(histo_prep())
  })
  
  
  #--------------------------------- Centrality Graph Code ------------------------------------# PY
  
  centrality_ggraph <- reactive({
    if (input$var_centrality == "Betweenness") {
      oh_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(colour=type,
                            alpha=0.5,
                            size=betweenness_centrality)) +
        geom_node_label(aes(label = name,
                            size=betweenness_centrality),
                        repel = TRUE, show.legend = FALSE) +
        scale_size_continuous(range=c(1,10))+
        theme_graph() 
    } else if (input$var_centrality == "Closeness") {
      oh_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(colour=type,
                            alpha=0.5,
                            size=closeness_centrality)) +
        geom_node_label(aes(label = name,
                            size=closeness_centrality),
                        repel = TRUE, show.legend = FALSE) +
        scale_size_continuous(range=c(1,10))+
        theme_graph()
    } else if (input$var_centrality == "Eigenvector") {
      oh_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(colour=type,
                            alpha=0.5,
                            size=eigen_centrality)) +
        geom_node_label(aes(label = name,
                            size=eigen_centrality),
                        repel = TRUE, show.legend = FALSE) +
        scale_size_continuous(range=c(1,10))+
        theme_graph()
    } else if (input$var_centrality == "Degree") {
      oh_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(colour=type,
                            alpha=0.5,
                            size=degree_centrality)) +
        geom_node_label(aes(label = name,
                            size=degree_centrality),
                        repel = TRUE, show.legend = FALSE) +
        scale_size_continuous(range=c(1,10))+
        theme_graph()
    } else if (input$var_centrality == "PageRank") {
      oh_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(colour=type,
                            alpha=0.5,
                            size=pagerank_centrality)) +
        geom_node_label(aes(label = name,
                            size=pagerank_centrality),
                        repel = TRUE, show.legend = FALSE) +
        scale_size_continuous(range=c(1,10))+
        theme_graph()
    } else {
      oh_graph %>%
        ggraph(layout = "fr") +
        geom_edge_link() +
        geom_node_point(aes(colour=type,
                            alpha=0.5,
                            size=authority_centrality)) +
        geom_node_label(aes(label = name,
                            size=authority_centrality),
                        repel = TRUE, show.legend = FALSE) +
        scale_size_continuous(range=c(1,10))+
        theme_graph()
    }
    
    
  })
  
  output$centralityPlot <- renderPlot({
    centrality_ggraph()
  }, height = 800, width = 1000)
  
  #--------------------------------- Network Graph Code ------------------------------------# PY
  
  network_ggraph <- reactive({
    edges_df<-oh_graph%>%
      activate(edges) %>% 
      as_tibble() 
    
    nodes_df<-oh_graph%>%
      activate(nodes) %>% 
      as_tibble() %>%
      rename(label=name) %>%
      rename(group=type) %>% 
      mutate(id=row_number()) 
    
    visNetwork(nodes=nodes_df,edges=edges_df)%>%    
      visIgraphLayout(layout = "layout_with_fr") %>%  
      visLegend() %>%
      visEdges(arrows = "to", 
               smooth = list(enabled = TRUE,              
                             type = "curvedCW")) %>%
      visNodes(font = list(size=30)) %>% 
      visLayout(randomSeed=123) %>%    
      visOptions(highlightNearest = TRUE, 
                 nodesIdSelection = TRUE)
    
    
  })
  
  output$ConnectionPlot <- renderVisNetwork({
    network_ggraph()
  })  
  
  #--------------------------------- Individual Company Count Graph Code ------------------------------------# PY
  
  indicom_ggraph <- reactive({
    
    mc3_edges_indi_filtered <- mc3_edges_indi %>%
      group_by(src) %>%
      filter(n() >= input$slideCount) %>%
      ungroup()
    
    #Create node
    id1_inv <- mc3_edges_indi_filtered %>%
      select(src) %>%
      rename(id = src)
    id2_inv <- mc3_edges_indi_filtered %>%
      select(tgt) %>%
      rename(id = tgt)
    mc3_nodes_indi_filtered <- rbind(id1_inv, id2_inv) %>%
      distinct()
    
    #prep format for plotting
    mc3_edges_indi_filtered <- mc3_edges_indi_filtered %>%
      rename(from = src) %>%
      rename(to = tgt) %>%
      filter(from!=to) %>%
      ungroup()
    
    visNetwork(mc3_nodes_indi_filtered, mc3_edges_indi_filtered) %>%
      visNodes(color = list(background = "blue", border = "red")) %>%
      visEdges(arrows = "to") %>%
      visIgraphLayout(layout = "layout_with_gem") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend() %>%
      visLayout(randomSeed = 123)
    
  })
  
  output$IndvComPlot <- renderVisNetwork({
    indicom_ggraph()
  })  
  
  #--------------------------------- Text Analysis Code ------------------------------------# RT
  
  #--------------------------------- Unigram Analysis - data prep --------------------------# RT
  
  
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
      add_row(word="0",lexicon="SMART") %>% 
      add_row(word="including",lexicon="SMART")
    
    token_nodes %>% 
      anti_join(new_stop_words) 
    
  })
  
  #------------------------------- Unigram Analysis - topic modelling ------------------------# RT
  
  topicmodelling <- function(data) {
    
    # If text is provided, convert it to a dataframe of word frequencies
    if (is.character(data$word)) {
      data <- data %>% 
        filter(! word %in% input$words_to_remove1) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove2) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove3) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove4) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove5) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove6) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove7) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove8) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove9) 
      data <- data %>% 
        filter(! word %in% input$words_to_remove10) 
      data <- data %>%
        count(id, word) %>% 
        cast_dtm(id, word, n) %>% 
        as.matrix()
    }
    return(data)
  }
  
  doctm <- reactive({
    
    doc.term.matrix <- topicmodelling(wrdcloud())
    
    result <- FindTopicsNumber(
      doc.term.matrix,
      topics = seq(from = 2, to = 15, by = 1),
      metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
      method = input$ldamethod,
      control = list(seed = 77),
      mc.cores = 2L,
      verbose = TRUE)
    
    FindTopicsNumber_plot(result)
    
  })
  
  
  output$tm_optimizer <- renderPlot({
    doctm()
  })
  
  #------------------------------- Unigram Analysis - topics ------------------------# RT
  
  topics <- reactive({
    
    doc.term.matrix <- topicmodelling(wrdcloud())
    
    lda_topics <- LDA(
      doc.term.matrix,
      k = input$num.topics,
      method = input$ldamethod,
      control = list(seed=42)
    ) %>%
      tidy(matrix = "beta")
    
    lda_topics %>%
      group_by(topic) %>%
      top_n(15, beta) %>%
      ungroup() %>%
      mutate(term2 = fct_reorder(term, beta))
    
  })
  
  output$topic_prob <- renderPlot(
    
    ggplot(
      topics(),
      aes(term2, beta, fill=as.factor(topic))
    ) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      geom_text_wordcloud_area() +
      scale_size_area(max_size = 40) )
  
  
  
  #------------------------------- Unigram Analysis - wordcloud ------------------------# RT
  
  topic_cloud <- reactive({
    
    doc.term.matrix <- topicmodelling(wrdcloud())
    
    lda_topics <- LDA(
      doc.term.matrix,
      k = input$num.topics,
      method = input$ldamethod,
      control = list(seed=42)) %>%
      tidy(matrix = "beta")
    
    lda_topics %>%
      group_by(topic) %>%
      top_n(input$num, beta) %>%
      ungroup() 
    
  })
  
  output$cloud2 <- renderPlot(
    
    topic_cloud() %>%
      ggplot(aes(label = term, size = beta, color = topic)) +
      geom_text_wordcloud(seed = 123) +
      facet_wrap(~topic, scales = "free") +
      theme_minimal() +
      theme(strip.background = element_rect(fill = "firebrick"),
            strip.text.x = element_text(colour = "white"))
    
  )
  
  
  
  #--------------------------------- Bigram Analysis - datatable --------------------------# RT
  
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
    
    return(data)
  }
  
  output$filtered_tbl = renderDT(create_datatable(bigram()),options = list(pageLength = 5))
  
  #--------------------------------- Bigram Analysis - Network Graph --------------------------# RT
  
  bigramcommunity <- reactive({
    
    bi.gram.count <- create_datatable(bigram())
    
    bi.gram.count <- bi.gram.count %>% 
      filter(`weight` >= input$threshold[1]) %>%
      filter(`weight`<= input$threshold[2]) %>% 
      graph_from_data_frame(directed = FALSE)
    
    V(bi.gram.count)$cluster <- clusters(graph = bi.gram.count)$membership
    
    cc.bi.gram.count <- induced_subgraph(
      graph = bi.gram.count,
      vids = which(V(bi.gram.count)$cluster == which.max(clusters(graph = bi.gram.count)$csize))
    )
    
    V(cc.bi.gram.count)$degree <- strength(graph = cc.bi.gram.count)
    V(cc.bi.gram.count)$closeness <- closeness(graph = cc.bi.gram.count)
    V(cc.bi.gram.count)$betweenness <- betweenness(graph = cc.bi.gram.count)
    E(cc.bi.gram.count)$width <- E(cc.bi.gram.count)$weight/max(E(cc.bi.gram.count)$weight)
    
    if (input$bigramcluster == "Louvain") {
      bigramcluster <- cluster_louvain(cc.bi.gram.count, weights = E(cc.bi.gram.count)$weight)
    }  else if (input$bigramcluster == "Edge Betweenness") {
      bigramcluster <- cluster_edge_betweenness(cc.bi.gram.count, weights = E(cc.bi.gram.count)$weight)
    } else if (input$bigramcluster == "Walktrap") {
      bigramcluster <- cluster_walktrap(cc.bi.gram.count, weights = E(cc.bi.gram.count)$weight)
    } else {
      bigramcluster <- cluster_infomap(cc.bi.gram.count)
    }
    
    V(cc.bi.gram.count)$membership <- membership(bigramcluster)
    
    network.D3 <- igraph_to_networkD3(g = cc.bi.gram.count)
    network.D3$nodes <- network.D3$nodes %>%
      mutate(Degree = V(cc.bi.gram.count)$degree) %>% 
      mutate(Betweenness = (V(cc.bi.gram.count)$betweenness/100)) %>% 
      mutate(Closeness = (V(cc.bi.gram.count)$closeness*1000000)) %>% 
      mutate(Group = 1)
    
    network.D3$links$Width <- 8*E(cc.bi.gram.count)$width
    
    network.D3$nodes$Group <- V(cc.bi.gram.count)$membership
    
    network.D3$nodes$Group <- as.numeric(network.D3$nodes$Group)
    
    forceNetwork(
      Links = network.D3$links,
      Nodes = network.D3$nodes,
      Source = 'source',
      Target = 'target',
      NodeID = 'name',
      Group = 'Group',
      opacity = input$opacity,
      Value = 'Width',
      Nodesize = input$centralityindication,
      linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
      fontSize = 12,
      zoom = TRUE,
      opacityNoHover = 1
    )
    
  })
  
  output$net <- renderForceNetwork(bigramcommunity())
  
  
  centrality <- reactive({
    
    bi.gram.count <- create_datatable(bigram())
    
    bi.gram.count <- bi.gram.count %>% 
      filter(`weight` >= input$threshold[1]) %>%
      filter(`weight`<= input$threshold[2]) %>%
      graph_from_data_frame(directed = FALSE)
    
    V(bi.gram.count)$cluster <- clusters(graph = bi.gram.count)$membership
    
    cc.bi.gram.count <- induced_subgraph(
      graph = bi.gram.count,
      vids = which(V(bi.gram.count)$cluster == which.max(clusters(graph = bi.gram.count)$csize))
    )
    
    nodecentrality <- tibble(
      word = V(cc.bi.gram.count)$name,  
      degree = strength(graph = cc.bi.gram.count),
      closeness = closeness(graph = cc.bi.gram.count), 
      betweenness = betweenness(graph = cc.bi.gram.count)
    )
    
    nodecentrality
    
  })
  
  output$centrality_tbl = renderDT(centrality(),options = list(pageLength = 5))
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

