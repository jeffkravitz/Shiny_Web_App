# This is a Shiny web application developed by Jeff Kravitz for PSYC7709 "R for Reproducible Research".
# This app gnereates and graphically displays citation statistics.

# In order for this app to function properly, use a normal .bib file beginning with "@".

# Average impact factor of citations
# Citations by journal
# Citations by year
# Citations by page length
# Citations by author
# Citations by most used title word


library(shiny)
library(markdown)
library(wordcloud2)

# Define UI for application that draws a histogram
ui <- navbarPage("Citation Statistics",

tabPanel("Year",
  h2("Citation Statistics"),
  p("Not all publications are created equal. Older publications are often foundational but outdated. Articles published in journals with higher impact factors are more influential. Bibliographies that more heavily cite a small subset of authors may reflect bias."),
  div("This Shiny Application analyzes and graphically displays citation statistics from bibtex files."),
  p(tags$i("Note: in order for this application to function properly, bibtex files must begin with a single @")),
  hr(),
  
   # Bibliography selection
   selectInput("bib", label = h3("Select Bibliography"), 
               choices = list("Jeff's Moral Psychology Proposal" = 1,
                              "Example bib from CrumpLab" = 2, 
                              selected = 1)),
  hr(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId = "bins",
                     label = "Number of bins",
                     min = 1,
                     max = 50,
                     value = 25),
         sliderInput(inputId = "bounds",
                     label = "Min and Max Years Included",
                     min = 1900,
                     max = 2019,
                     value = c(1950,2019))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("year_plot"),
         tableOutput("year_table")
      )
   )
   
),
tabPanel("Title",
   h2("Citation Statistics"),
   p("Not all publications are created equal. Older publications are often foundational but outdated. Articles published in journals with higher impact factors are more influential. Bibliographies that more heavily cite a small subset of authors may reflect bias."),
   div("This Shiny Application analyzes and graphically displays citation statistics from bibtex files."),
   p(tags$i("Note: in order for this application to function properly, bibtex files must begin with a single @")),
   hr(),
   
   # Bibliography selection
   selectInput("bib2", label = h3("Select Bibliography"), 
               choices = list("Jeff's Moral Psychology Proposal" = 1,
                              "Example bib from CrumpLab" = 2, 
                              selected = 1)),
   hr(),
   
   mainPanel(
     wordcloud2Output("word_cloud")
   )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   output$year_plot <- renderPlot({
      bins <- seq(input$bounds[1], input$bounds[2], length.out = input$bins + 1)
      hist(year_of_citations[[as.numeric(input$bib)]][unlist(year_of_citations[[as.numeric(input$bib)]]) > input$bounds[1] & unlist(year_of_citations[[1]]) < input$bounds[2]], 
           breaks = bins, col = 'limegreen', 
           border = 'white', 
           main = "Frequency of Cited Publications by Year",
           xlab = "Year")
   })
   
   output$year_table <- renderTable({
     year_median <- median(year_of_citations[[as.numeric(input$bib)]])
     year_mean <- mean(year_of_citations[[as.numeric(input$bib)]])
     year_sd <- sd((year_of_citations[[as.numeric(input$bib)]]))
     year_stats <- c(year_median, year_mean, year_sd)
     year_names <- c("Median", "Mean", "SD")
     year_df <- data.frame(Statistic = year_names, Value = year_stats)
   })
   
   output$word_cloud <- renderWordcloud2({
     word_cloud <- list(word_cloud_a, word_cloud_b)
     word_cloud[[as.numeric(input$bib2)]]
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

