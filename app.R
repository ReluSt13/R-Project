
library(shiny)

if(interactive()){
ui <- fluidPage(
  sidebarLayout(position = "left", 
                sidebarPanel(
                         selectInput(inputId = "repartitie",
                                     label = "Alegeti o repartitie: ",
                                     choices = c("Bernoulli" = "bern",
                                                 "Binomiala" = "bin",
                                                 "Geometrica" = "geo")),
                         conditionalPanel(
                           condition = "input.repartitie == 'bern'",
                           sliderInput(inputId = "bern", 
                                       label = "Probability of success",
                                       0, 1, 0.01),
                           h5("The Bernoulli distribution is the discrete probability distribution
                              of a random variable which takes the value 1 with probability
                              p and the value 0 with probability q = 1 - p.The Bernoulli distribution
                              is a special case of the binomial distribution where a single trial is
                              conducted (so n would be 1 for such a binomial distribution).", style = "text-align: justify")
                  
                         ),
                         conditionalPanel(
                           condition = "input.repartitie == 'bin'",
                           sliderInput(inputId = "binProb", 
                                       label = "Probability of success",
                                       0, 1, 0.01),
                           numericInput(inputId = "binSize", 
                                        label = "Nr. of events",
                                        value = 10),
                           h5("The binomial distribution with parameters n and p is the discrete probabilty distribution of the number of successes in a sequence of n independent experiments, each asking a yes-no question, and each with its own Boolean-valued outcome: success(with probability p) or failure(with probability q = 1 - p).",
                              style = "text-align: justify")
                           
                         ),
                         conditionalPanel(
                           condition = "input.repartitie == 'geo'",
                           sliderInput(inputId = "geoProb", 
                                       label = "Probability of success",
                                       0, 1, 0.01),
                           h5("The geometric distribution is either one of two discrete probability distributions:",
                              style = "text-align: justify; font-weight: bold;"),
                           h5("    The probability distribution of the number X of Bernoulli trials needed to get one success, supported on the set {1,2,3,...}",
                              style = "text-align: justify"),
                           h5("The probability distribution of the number Y = X - 1 of failures before the first success, supported on the set {0,1,2,...}",
                              style = "text-align: justify")
                           
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

server <- function(input, output) {
  
  output$densityGraph <- renderPlot({
    if (input$repartitie == "bern")
    {
      x <- seq(0, 3);
      prob <- input$bern
      density <- dbern(x, prob);
    }
    
    if (input$repartitie == "bin")
    {
      prob <- input$binProb;
      size <- input$binSize;
      density <- dbinom(0:size, size, prob);
    }
    
    if(input$repartitie == "geo") {
      x <- seq(0, 10);
      prob <- input$geoProb
      density <- dgeom(x, prob);
    }
    
    plot(density, type = "o");
  });
    output$distGraph <- renderPlot({
      if (input$repartitie == "bern")
      {
        prob <- input$bern
        x <- seq(0, 3);
        distribution <- pbern(x, prob);
      }
      if (input$repartitie == "bin")
      {
        prob <- input$binProb;
        size <- input$binSize;
        distribution <- pbinom(0:size, size, prob);
      }
      if (input$repartitie == "geo") {
        prob <- input$geoProb
        x <- seq(0, 10);
        distribution <- pgeom(x, prob);
      }
      plot(distribution, type = "o");
    });
    output$mean <- renderText({
      if (input$repartitie == "bern") { input$bern }
      else if (input$repartitie == "bin") { input$binProb * input$binSize }
      else if (input$repartitie == "geo") {(1 - input$geoProb) / input$geoProb}
    });
    output$var <- renderText({
      if (input$repartitie == "bern") { input$bern * (1 - input$bern) }
      else if (input$repartitie == "bin") { input$binProb * input$binSize * (1 - input$binProb) }
      else if (input$repartitie == "geo") {(1 - input$geoProb) / input$geoProb ** 2}
    });

}

# Run the application 
shinyApp(ui = ui, server = server)
}