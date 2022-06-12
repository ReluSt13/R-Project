
library(shiny)
library(Rlab)

if(interactive()){
ui <- fluidPage(
  navbarPage("Navbar :)",
    tabPanel("Repartitii",
             sidebarLayout(position = "left", 
                           sidebarPanel(
                             selectInput(inputId = "repartitie",
                                         label = "Alegeti o repartitie: ",
                                         choices = c("Bernoulli" = "bern",
                                                     "Binomiala" = "bin",
                                                     "Geometrica" = "geo",
                                                     "Hypergeometrica" = "hgeo",
                                                     "Normala" = "norm",
                                                     "Uniforma continua" = "cuni",
                                                     "Poisson" = "poi",
                                                     "Exponentiala" = "exp")),
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
                                            value = 10,
                                            min = 0),
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
                               
                             ),
                             conditionalPanel(
                               condition = "input.repartitie == 'hgeo'",
                               numericInput(inputId = "hgeoSize", 
                                            label = "Size of population",
                                            value = 10,
                                            min = 0),
                               numericInput(inputId = "hgeoNrS", 
                                            label = "Nr. of samples drawn",
                                            value = 1,
                                            min = 0),
                               numericInput(inputId = "hgeoNrI", 
                                            label = "Nr. of items in population",
                                            value = 1,
                                            min = 0),
                               h5("The hypergeometric distribution is a discrete probability distribution that describes the probability of k successes(random draws for which the object drawn has a specified feature) in n draws, without replacement, from a finite population of size N that contains exactly K objects with that feature, wherein each draw is either a success or a failure. In contrast, the binomial distribution describes the probability of k successes in n draws with replacement.",
                                  style = "text-align: justify")
                               
                             ),
                             conditionalPanel(
                               condition = "input.repartitie == 'poi'",
                               numericInput(inputId = "events",
                                            label = "Nr. of successful events",
                                            value = 10),
                               numericInput(inputId = "mean1",
                                            label = "Mean",
                                            value = 10),
                               h5("The Poisson distribution is a discrete probability distribution that expresses
                         the probability of a given number of events occurring in a fixed interval of time
                         or space if these events occur with a known constant mean rate and independently
                         of the time since the last event.", style = "text-align: justify")
                             ),
                             conditionalPanel(
                               condition = "input.repartitie == 'norm'",
                               numericInput(inputId = "quantiles1",
                                            label = "Vector of quantiles",
                                            value = 10),
                               numericInput(inputId = "mean2",
                                            label = "Mean",
                                            value = 10),
                               numericInput(inputId = "deviation",
                                            label = "Standard deviation",
                                            value = 1),
                               h5("A normal distribution (also known as Gaussian, Gauss, or Laplace-Gauss distribution) is a type of continuous probability distribution for a real-valued random variable.",
                                  style = "text-align: justify")
                             ),
                             conditionalPanel(
                               condition = "input.repartitie == 'cuni'",
                               numericInput(inputId = "quantiles2",
                                            label = "Vector of quantiles",
                                            value = 10),
                               numericInput(inputId = "min2",
                                            label = "Minimum value of the distribution",
                                            value = 3),
                               numericInput(inputId = "max2",
                                            label = "Maximum value of the distribution",
                                            value = 8),
                               h5("The continuous uniform distribution or rectangular distribution is a family of
                         symmetric probability distributions. The distribution describes an experiment
                         where there is an arbitrary outcome that lies between certain bounds.The
                         bounds are defined by the parameters, a and b, which are the minimum and maximum
                         values.", style = "text-align: justify")
                             ),
                             conditionalPanel(
                               condition = "input.repartitie == 'exp'",
                               numericInput(inputId = "quantiles3",
                                            label = "Vector of quantiles",
                                            value = 10),
                               numericInput(inputId = "rate",
                                            label = "Rate",
                                            value = 0.5,
                                            step = 0.5),
                               h5("The exponential distribution is the probability distribution of the time between
                         events in a Poisson point process, i.e., a process in which events occur continuously
                         and independently at a constant average rate. It is a particular case of the gamma
                         distribution. It is the continuous analogue of the geometric distribution, and it has
                         the key property of being memoryless.", style = "text-align: justify")
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
             
    ),
    tabPanel("Evenimente",
             sidebarLayout(position = "left",
                           sidebarPanel(
                             radioButtons("tipEven",
                                          "Alegeti tipul evenimentelor:",
                                          c("Independente" = "indep",
                                            "Incompatibile" = "incomp",
                                            "Nu se cunoaste nimic" = "nimic")),
                             sliderInput("probA",
                                          "P(A) = ",
                                          value = 0.1,
                                          min = 0,
                                          max = 1,
                                          step = 0.01),
                             sliderInput("probB",
                                          "P(B) = ",
                                          value = 0.1,
                                          min = 0,
                                          max = 1,
                                          step = 0.01),
                             conditionalPanel(condition = "input.tipEven == 'indep'",
                                              fluidRow(
                                                splitLayout(cellWidths = c("65%", "35%"),
                                                            h4(HTML("P(A&cap;B) = P(A) * P(B) = "), style = "text-align: center; float: right"),
                                                            h4(textOutput("probAiBIndep"), style = "text-align: center; float: left") 
                                                           )
                                              )
                                                  
                             ),
                             conditionalPanel(condition = "input.tipEven == 'incomp'",
                                              fluidRow(
                                                h4(HTML("P(A&cap;B) = 0"), style = "text-align: center")
                                              )
                             ),
                             conditionalPanel(condition = "input.tipEven == 'nimic'",
                                              sliderInput("probAiB",
                                                          HTML("P(A&cap;B) = "),
                                                          value = 0.1,
                                                          min = 0,
                                                          max = 1
                                              )
      
                             )
                             #sliderInput("probAandB",
                              #            HTML("P(A&cap;B) = "),
                               #           value = 0,
                                #          min = 0,
                                 #         max = 1,
                                  #        step = 0.01)
                           ),
                           mainPanel(
                             fluidRow(
                               splitLayout(cellWidths = c("33.33%", "33.33%", "33.33%"),
                                           column(
                                             style='border: 1px solid black',
                                             width = 11,
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           h3(HTML("P(A&Union;B):"), style = "text-align: center"),
                                                           h3(textOutput("probAuB"), style = "text-align: center")
                                               )
                                             )
                                           ),
                                           column(
                                             style='border: 1px solid black',
                                             width = 11,
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           h3("P(A|B):", style = "text-align: center"),
                                                           h3(textOutput("probAcB"), style = "text-align: center")
                                               )
                                             )
                                           ),
                                           column(
                                             style='border: 1px solid black',
                                             width = 11,
                                             fluidRow(
                                               splitLayout(cellWidths = c("50%", "50%"),
                                                           h3("P(B|A):", style = "text-align: center"),
                                                           h3(textOutput("probBcA"), style = "text-align: center")
                                               )
                                             )
                                           )
                               )
                             )
                           )
             )
             
             
    )
  )
  
  
)

server <- function(input, output) {
  
  output$densityGraph <- renderPlot({
    if (input$repartitie == "bern")
    {
      x <- seq(0, 3);
      prob <- input$bern
      density <- dbern(x, prob);
    }
    
    else if (input$repartitie == "bin")
    {
      prob <- input$binProb;
      size <- input$binSize;
      density <- dbinom(0:size, size, prob);
    }
    
    else if(input$repartitie == "geo") {
      x <- seq(0, 10);
      prob <- input$geoProb
      density <- dgeom(x, prob);
    }
    
    else if (input$repartitie == "hgeo") {
      size <- input$hgeoSize;
      nrS <- input$hgeoNrS;
      nrI <- input$hgeoNrI;
      density <- dhyper(0:nrI, size, nrS, nrI);
    }
    
    else if(input$repartitie == "norm"){
      x <- input$quantiles1
      mean <- input$mean2
      sd <- input$deviation
      density <- dnorm(1:x, mean, sd)
    }
    
    else if(input$repartitie == "cuni"){
      x <- input$quantiles2
      min <- input$min2
      max <- input$max2
      density <- dunif(1:x,min,max);
    }
    
    else if(input$repartitie == "poi"){
      k <- input$events;
      lambda <- input$mean1;
      density <- dpois(1:k, lambda);
    }
    
    else{
      x <- input$quantiles3
      rate <- input$rate
      density <- dexp(1:x, rate);
    }
    
    if(input$repartitie == "duni")
    {plot(density);}
    else 
    {plot(density, type = "o");}
  });
  
  
  output$distGraph <- renderPlot({
    if (input$repartitie == "bern")
    {
      prob <- input$bern
      x <- seq(0, 3);
      distribution <- pbern(x, prob);
    }
    else if (input$repartitie == "bin")
    {
      prob <- input$binProb;
      size <- input$binSize;
      distribution <- pbinom(0:size, size, prob);
    }
    else if (input$repartitie == "geo") {
      prob <- input$geoProb
      x <- seq(0, 10);
      distribution <- pgeom(x, prob);
    }
    else if (input$repartitie == "hgeo") {
      size <- input$hgeoSize;
      nrS <- input$hgeoNrS;
      nrI <- input$hgeoNrI;
      distribution <- phyper(0:nrI, size, nrS, nrI);
    }
    else if(input$repartitie == "norm"){
      x <- input$quantiles1
      mean <- input$mean2
      sd <- input$deviation
      distribution <- pnorm(1:x, mean, sd)
    }
    else if (input$repartitie == "cuni") {
      q <- input$quantiles2
      min <- input$min2
      max <- input$max2
      distribution <- punif(1:q, min, max);
    }
    else if (input$repartitie == "poi"){
      q <- input$events;
      lambda <- input$mean1;
      distribution <- ppois(1:q,lambda);
    }
    else {
      x <- input$quantiles3
      rate <- input$rate
      distribution <- pexp(1:x, rate)
    }
    
    if(input$repartitie == "duni")
    {plot(distribution);}
    else
    {plot(distribution, type = "o");}
  });
  output$mean <- renderText({
    if (input$repartitie == "bern") { input$bern }
    else if (input$repartitie == "bin") { input$binProb * input$binSize }
    else if (input$repartitie == "geo") {(1 - input$geoProb) / input$geoProb}
    else if (input$repartitie == "hgeo") {input$hgeoNrS * input$hgeoNrI / input$hgeoSize}
    else if (input$repartitie == "norm") {input$mean2}
    else if (input$repartitie == "cuni") { (input$min + input$max) / 2}
    else if (input$repartitie == "poi") { input$mean1 }
    else {1 / input$rate}
  });
  output$var <- renderText({
    if (input$repartitie == "bern") { input$bern * (1 - input$bern) }
    else if (input$repartitie == "bin") { input$binProb * input$binSize * (1 - input$binProb) }
    else if (input$repartitie == "geo") {(1 - input$geoProb) / input$geoProb ** 2}
    else if (input$repartitie == "hgeo") {(input$hgeoNrS * input$hgeoNrI / input$hgeoSize) * ((input$hgeoSize - input$hgeoNrI) / input$hgeoSize) * ((input$hgeoSize - input$hgeoNrS) / (input$hgeoSize - 1))  }
    else if (input$repartitie == "norm") {input$deviation ** 2}
    else if (input$repartitie == "cuni") {((input$max - input$min) ** 2) / 12}
    else if (input$repartitie == "poi") { input$mean1 }
    else {1 / (input$rate ** 2)}
  });
  
  output$probAuB <- renderText({
    if(input$tipEven == "indep") {
      input$probA + input$probB - (input$probA * input$probB)
    } else if(input$tipEven == "incomp") {
      if(input$probA + input$probB <= 1) {
        input$probA + input$probB
      } else {
        "Error"
      }
      
    } else if(input$tipEven == "nimic") {
      if(input$probA + input$probB - input$probAiB <= 1) {
        input$probA + input$probB - input$probAiB
      } else {
        "Error"
      }
      
    }
  });
  output$probAiBIndep <- renderText({
      input$probA * input$probB
  })
  output$probAcB <- renderText({
    if(input$tipEven == "indep") {
      (input$probA * input$probB) / input$probB
    } else if(input$tipEven == "incomp") {
      0
    } else if(input$tipEven == "nimic") {
      if(input$probAiB / input$probB <= 1) {
        input$probAiB / input$probB
      }else {
        "Error"
      }
    }
    
  })
  output$probBcA <- renderText({
    if(input$tipEven == "indep") {
      (input$probA * input$probB) / input$probA
    } else if(input$tipEven == "incomp") {
      0
    } else if(input$tipEven == "nimic") {
      if(input$probAiB / input$probA <= 1) {
        input$probAiB / input$probA
      }else {
        "Error"
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
}