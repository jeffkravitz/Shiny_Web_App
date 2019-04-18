# Average impact factor of citations
# Citations by journal

library(shiny)
library(markdown)
library(wordcloud2)
library(ggplot2)
library(stringr)

# Define User Interface
ui <- fluidPage( 
  
  list(
    tags$style(HTML("
      .navbar-nav li a:hover, .navbar-nav > .active > a {
      color: #fff !important;
      background-color:#32CD32 !important;
      background-image: none !important;
      }
  "))),
  
  navbarPage("Citation Statistics",

  # Tab for citation year statistics
  tabPanel("Year",
    h2("Citation Statistics"),
    p("Not all publications are created equal. Older publications are often foundational but outdated, and articles published in journals with higher impact factors are more influential. Bibliographies that more heavily cite a small subset of authors may reflect bias."),
    div("This Shiny Application, created by Jeff Kravitz, graphically displays citation statistics by analyzing bibtex files."),
    p(tags$i("Note: in order for this application to function properly, bibtex files must begin with a single @")),
  
   # Bibliography selection
   selectInput("bib", label = h3("Select Bibliography"), 
               choices = list("Jeff's Moral Psychology Proposal" = 1,
                              "Example bib from CrumpLab" = 2,
                              "Crump's Entropy Typing" = 3)),
  hr(),
    
   sidebarLayout(
      sidebarPanel(
         # Slider input for number of bins of "year histogram"
         sliderInput(inputId = "bins",
                     label = "Number of bins",
                     min = 1,
                     max = 50,
                     value = 25),
         # Slider input for bounds of "years included" 
         sliderInput(inputId = "bounds",
                     label = "Min and Max Years Included",
                     min = 1900,
                     max = 2019,
                     value = c(1950,2019))
      ),

      mainPanel(
         # Histogram of citation years
         plotOutput("year_plot"),
         
         # Tables giving descriptive stats about citation years
         fluidRow(
           column(4, offset = 1, tableOutput("year_table_1")),
           column(4, tableOutput("year_table_2"))
         )
      )
   )
  ),

  # Tab for citation title statistics
  tabPanel("Title",
    h2("Citation Statistics"),
    p("Not all publications are created equal. Older publications are often foundational but outdated. Articles published in journals with higher impact factors are more influential. Bibliographies that more heavily cite a small subset of authors may reflect bias."),
    div("This Shiny Application analyzes and graphically displays citation statistics from bibtex files."),
    p(tags$i("Note: in order for this application to function properly, bibtex files must begin with a single @")),
   
   # Bibliography selection
   selectInput("bib2", label = h3("Select Bibliography"), 
               choices = list("Jeff's Moral Psychology Proposal" = 1,
                              "Example bib from CrumpLab" = 2,
                              "Crump's Entropy Typing" = 3)),
   hr(),
   
   # Print wordcloud of citation title words
   mainPanel(
     wordcloud2Output("word_cloud")
   )
  ),

  # Tab for citation author statistics
  tabPanel("Author",
    h2("Citation Statistics"),
    p("Not all publications are created equal. Older publications are often foundational but outdated. Articles published in journals with higher impact factors are more influential. Bibliographies that more heavily cite a small subset of authors may reflect bias."),
    div("This Shiny Application analyzes and graphically displays citation statistics from bibtex files."),
    p(tags$i("Note: in order for this application to function properly, bibtex files must begin with a single @")),
         
  # Bibliography selection
  selectInput("bib3", label = h3("Select Bibliography"), 
  choices = list("Jeff's Moral Psychology Proposal" = 1,
                 "Example bib from CrumpLab" = 2,
                 "Crump's Entropy Typing" = 3)),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "num_authors",
                  label = "Include authors with minimum number of citations:",
                  min = 1,
                  max = 50,
                  value = 1)
    ),
    # Print bar chart of citation authors
    mainPanel(
      plotOutput("author_plot")
    )
  )
  ),
  

  
  tabPanel("Journal",
    h2("Citation Statistics"),
    p("Not all publications are created equal. Older publications are often foundational but outdated. Articles published in journals with higher impact factors are more influential. Bibliographies that more heavily cite a small subset of authors may reflect bias."),
    div("This Shiny Application analyzes and graphically displays citation statistics from bibtex files."),
    p(tags$i("Note: in order for this application to function properly, bibtex files must begin with a single @")),

    
    # Bibliography selection
    selectInput("bib4", label = h3("Select Bibliography"), 
                choices = list("Jeff's Moral Psychology Proposal" = 1,
                               "Example bib from CrumpLab" = 2,
                               "Crump's Entropy Typing" = 3)),
    hr(),
    

    
    navlistPanel(
      "Select Graph Type",
      tabPanel("Bar Graph",
               plotOutput("journal_plot")
      ),
      tabPanel("Pie Chart",
               plotOutput("journal_pie_chart")
      )
    )
    
    
    
  )

))

# Define Server
server <- function(input, output) {
  
   # Produce histogram of citation years
   output$year_plot <- renderPlot({
      bins <- seq(input$bounds[1], input$bounds[2], length.out = input$bins + 1)
      year_of_citations <- list(unlist(year_of_citations_a), unlist(year_of_citations_b), unlist(year_of_citations_c))
      hist(year_of_citations[[as.numeric(input$bib)]][unlist(year_of_citations[[as.numeric(input$bib)]]) > input$bounds[1] & unlist(year_of_citations[[1]]) < input$bounds[2]], 
           breaks = bins, col = 'limegreen', 
           border = 'white', 
           main = "Frequency of Cited Publications by Year",
           xlab = "Year")
   })
   
   # Produce table of citation statistics (mean, sd, se)
   output$year_table_1 <- renderTable({
     year_of_citations <- list(unlist(year_of_citations_a), unlist(year_of_citations_b), unlist(year_of_citations_c))
     year_mean <- mean(year_of_citations[[as.numeric(input$bib)]])
     year_sd <- sd((year_of_citations[[as.numeric(input$bib)]]))
     year_se <- sd((year_of_citations[[as.numeric(input$bib)]]))/sqrt(length(year_of_citations[[as.numeric(input$bib)]]))
     year_stats <- c(year_mean, year_sd, year_se)
     year_names <- c("Mean", "SD", "SE")
     year_df <- data.frame(Statistic = year_names, Value = year_stats)
   })
   
   # Produce table of citation statistics (median, max, min)
   output$year_table_2 <- renderTable({
     year_of_citations <- list(unlist(year_of_citations_a), unlist(year_of_citations_b), unlist(year_of_citations_c))
     year_median <- round(median(year_of_citations[[as.numeric(input$bib)]]))
     year_max <- round(max(year_of_citations[[as.numeric(input$bib)]]))
     year_min <- round(min((year_of_citations[[as.numeric(input$bib)]])))
     year_stats <- c(year_median, year_max, year_min)
     year_names <- c("Median", "Max", "Min")
     year_df <- data.frame(Statistic = year_names, Value = year_stats)
   })
   
   # Produce wordcloud of citation title words
   output$word_cloud <- renderWordcloud2({
     word_cloud <- list(word_cloud_a, word_cloud_b, word_cloud_c)
     word_cloud[[as.numeric(input$bib2)]]
   })
   
   # Produce barchart of citation authors
   output$author_plot <- renderPlot({
     author_list <- list(unlist(author_list_a), unlist(author_list_b), unlist(author_list_c))
     author <- data.frame(table(unlist(author_list[[as.numeric(input$bib3)]])))
     author <- author[author$Freq > input$num_authors-1,]
     ggplot(data=author, aes(x=Var1, y= Freq))+
       geom_bar(stat = "identity", fill = "limegreen")+
       labs(x = " ", title = "Frequency of Citations by Author")+
       coord_flip()+
       theme_classic(base_size = 12)
   })
   
   # Produce barchart of citation journals
   output$journal_plot <- renderPlot({
     journal_list <- list(unlist(journal_list_a), unlist(journal_list_b), unlist(journal_list_c))
     ggplot(data=data.frame(table(unlist(journal_list[[as.numeric(input$bib4)]]))), aes(x=Var1, y= Freq))+
       geom_bar(stat = "identity",position = position_dodge(width=1), fill = "limegreen")+
       labs(x = " ", title = "Frequency of Citations by Journal")+
       coord_flip()+
       theme_classic(base_size = 12)
   })
   
   # Produce pie chart of citation journals
   output$journal_pie_chart <- renderPlot({
     journal_list <- list(unlist(journal_list_a), unlist(journal_list_b), unlist(journal_list_c))
     df_journal_a <- data.frame(table(unlist(journal_list_a)))
     percent_a = rep(0,length(unique(journal_list_a)))
     percent_b = rep(0,length(unique(journal_list_c)))
     percent_c = rep(0,length(unique(journal_list_c)))
     percent_list <- list(percent_a,percent_b,percent_c)
     for (i in 1:3) {
       for (j in 1:length(journal_list[[i]])) {
         percent_list[[i]][j] <- data.frame(table(unlist(journal_list_a)))[j,2]/sum(data.frame(table(unlist(journal_list_a)))[,2])
       }
     }
     ggplot(data = data.frame(journal = journal_list[[as.numeric(input$bib4)]], percent = percent_list[[as.numeric(input$bib4)]]), aes(x="", y=percent, fill=str_wrap(journal_list[[as.numeric(input$bib4)]],25)))+
       geom_bar(width = 1, stat = "identity")+
       coord_polar("y", start=0)+
       labs(x="", y="")+
       theme(axis.text = element_blank(),
             axis.ticks = element_blank(),
             panel.grid  = element_blank(),
             legend.title = element_blank(),
             panel.grid.minor = element_blank(), 
             panel.grid.major = element_blank(),
             panel.background = element_blank(),
             plot.background = element_blank()
      )
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

