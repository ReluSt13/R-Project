
library(shiny)

if(interactive()){
ui <- fluidPage(
  sidebarLayout(position = "left", 
                sidebarPanel(
                         selectInput(inputId = "repartitie",
                                     label = "Alegeti o repartitie: ",
                                     choices = c("Bernoulli" = "bern",
                                                 "Binomiala" = "bin")),
                         conditionalPanel(
                           condition = "input.repartitie == 'bern'",
                           sliderInput(inputId = "bern", 
                                       label = "Probability of success",
                                       0, 1, 0.01),
                         ),
                         conditionalPanel(
                           condition = "input.repartitie == 'bin'",
                           sliderInput(inputId = "bin", 
                                       label = "Probability of success ",
                                       0, 1, 0.01)
                         )
                ),
                mainPanel(
                  fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                                plotOutput("densityGraph"),
                                plotOutput("distGraph"))
                  ),
                  fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                                column(style='border: 1px solid black',
                                       width = 11,
                                       fluidRow(
                                         splitLayout(cellWidths = c("50%", "50%"),
                                                     h3("Mean:", style = "text-align: center"),
                                                     h3(textOutput("mean"), style = "text-align: center")
                                                     )
                                       )
                                       
                                ),
                                column(
                                  style='border: 1px solid black',
                                  width = 11,
                                  fluidRow(
                                    splitLayout(cellWidths = c("50%", "50%"),
                                                h3("Variance:", style = "text-align: center"),
                                                h3(textOutput("var"), style = "text-align: center")
                                    )
                                  )
                                )
                                )
                  )
                ))
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$densityGraph <- renderPlot({
      prob <- input$bern
      x <- seq(0, 5);
      density <- dbern(x, prob);
      plot(density, type = "o");
    });
    output$distGraph <- renderPlot({
      prob <- input$bern
      x <- seq(0, 5);
      distribution <- pbern(x, prob);
      plot(distribution, type = "o");
    });
    output$mean <- renderText({input$bern});
    output$var <- renderText({input$bern * (1- input$bern)});

}

# Run the application 
shinyApp(ui = ui, server = server)
}