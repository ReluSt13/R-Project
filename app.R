
library(shiny)
library(Rlab)
library(discreteRV)

if(interactive()){
ui <- fluidPage(
  navbarPage("RV Project",
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
             
             
    ),
    tabPanel("Var. Aleatoare",
             sidebarLayout(position = "left",
                           sidebarPanel(
                             selectInput("vaSelect", "Alegeti ce actiune doriti sa efectuati: ",
                                         c("Afisati o variabila aleatoare" = "vaAfis",
                                           "Operatii cu variabile aleatoare" = "vaOp")
                             ),
                             conditionalPanel(condition = "input.vaSelect == 'vaOp'",
                                              h3("V.A. 1:", style = "margin: 0px; margin-bottom: 5px"),
                                              fluidRow(
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va1v1",
                                                                 "Val 1: ",
                                                                 value = 0,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va1v2",
                                                                 "Val 2: ",
                                                                 value = 0,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va1v3",
                                                                 "Val 3: ",
                                                                 value = 0,
                                                                 width = "75px")
                                                )
                                              ),
                                              fluidRow(
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va1p1",
                                                                 "Prob 1: ",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 0.1,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va1p2",
                                                                 "Prob 2: ",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 0.1,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va1p3",
                                                                 "Prob 3: ",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 0.1,
                                                                 width = "75px")
                                                )
                                              ),
                                              fluidRow(
                                                column(12, align = "center",
                                                       radioButtons("operatie",
                                                                    "Alegeti operatia dorita:",
                                                                    c("+" = "add",
                                                                      "-" = "sub",
                                                                      "*" = "mul",
                                                                      "/" = "div"),
                                                                    inline = TRUE
                                                       )
                                                )
                                              ),
                                              h3("V.A. 2:", style = "margin: 0px; margin-bottom: 5px"),
                                              fluidRow(
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va2v1",
                                                                 "Val 1: ",
                                                                 value = 0,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va2v2",
                                                                 "Val 2: ",
                                                                 value = 0,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va2v3",
                                                                 "Val 3: ",
                                                                 value = 0,
                                                                 width = "75px")
                                                )
                                              ),
                                              fluidRow(
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va2p1",
                                                                 "Prob 1: ",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 0.1,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va2p2",
                                                                 "Prob 2: ",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 0.1,
                                                                 width = "75px")
                                                ),
                                                div(style = "display: inline-block; margin-left: 40px",
                                                    numericInput("va2p3",
                                                                 "Prob 3: ",
                                                                 value = 0,
                                                                 min = 0,
                                                                 step = 0.1,
                                                                 width = "75px")
                                                )
                                              )
                                             ),
                             conditionalPanel(condition = "input.vaSelect == 'vaAfis'",
                                              numericInput("vaLambda", "Lambda",
                                                           value = 2,
                                                           min = 0),
                                              numericInput("vaFirst", "First value:", value = 0, min = 0)
                                             )

                           ),
                           mainPanel(
                             conditionalPanel(condition = "input.vaSelect == 'vaOp'",
                                              fluidRow(
                                                splitLayout(cellWidths = c("45%", "10%", "45%"),
                                                            h3("V.A. 1", style = "float: right"),
                                                            h3(textOutput("renderOp"), style = "text-align: center"),
                                                            h3("V.A. 2 = ")
                                                )
                                              ),
                                              fluidRow(
                                                column(12, align = "center", 
                                                       h3(htmlOutput("outputVA"))
                                                )
                                              )
                                             ),
                             conditionalPanel(condition = "input.vaSelect == 'vaAfis'",
                                              htmlOutput("vaPoisAfis"),
                                              plotOutput("vaPoisPlot")
                                             )
                             
                             
                           )
                          )
            ),
    tabPanel("Set date",
             sidebarLayout(position = "left",
                           sidebarPanel(
                             fileInput("file", "Choose CSV File",accept = c(csv=','))
                           ),
                           mainPanel(
                             fluidRow(
                               splitLayout(cellWidths = c("50%", "50%"),
                                           h3("Mediana: ", style = "float: right"),
                                           h3(textOutput("median"))
                                          )
                             ),
                             fluidRow(
                               splitLayout(cellWidths = c("50%", "50%"),
                                           h3("Cuartile: ", style = "float: right"),
                                           h3(textOutput("quantile"))
                               )
                             ),
                             fluidRow(
                               splitLayout(cellWidths = c("10%", "40%", "10%", "40%"),
                                           h4("Box Plot: ", style = "float: right; margin-top: 75%"),
                                           plotOutput("boxplot"),
                                           h4("Histogram: ", style = "float: right; margin-top: 75%"),
                                           plotOutput("hist")
                               )
                             )
                           )
             )
            ),
    tabPanel("Repartitii comune",
             sidebarLayout(position = "left",
                           sidebarPanel(
                             fluidRow(
                               splitLayout(cellWidths = c("33.33%", "33.33%", "33.33%"),
                                           column(12, align = "center", h4("A/B")),
                                           numericInput("valB1", "VB1", value = 0, width = "75px"),
                                           numericInput("valB2", "VB2", value = 0, width = "75px"),
                                          )
                             ),
                             fluidRow(
                               splitLayout(cellWidths = c("33.33%", "33.33%", "33.33%"),
                                           numericInput("valA1", "VA1", value = 0, width = "75px"),
                                           numericInput("probA1B1", "PA1B1", value = 0, min = 0, step = "0.1", width = "75px"),
                                           numericInput("probA1B2", "PA1B2", value = 0, min = 0, step = "0.1", width = "75px"),
                               )
                             ),
                             fluidRow(
                               splitLayout(cellWidths = c("33.33%", "33.33%", "33.33%"),
                                           numericInput("valA2", "VA2", value = 0, width = "75px"),
                                           numericInput("probA2B1", "PA2B1", value = 0, min = 0, step = "0.1", width = "75px"),
                                           numericInput("probA2B2", "PA2B2", value = 0, min = 0, step = "0.1", width = "75px"),
                               )
                             ),
                           ),
                           mainPanel(
                             tableOutput("commonTable"),
                             htmlOutput("informatii")
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
    else if (input$repartitie == "cuni") { (input$min2 + input$max2) / 2}
    else if (input$repartitie == "poi") { input$mean1 }
    else {1 / input$rate}
  });
  output$var <- renderText({
    if (input$repartitie == "bern") { input$bern * (1 - input$bern) }
    else if (input$repartitie == "bin") { input$binProb * input$binSize * (1 - input$binProb) }
    else if (input$repartitie == "geo") {(1 - input$geoProb) / input$geoProb ** 2}
    else if (input$repartitie == "hgeo") {(input$hgeoNrS * input$hgeoNrI / input$hgeoSize) * ((input$hgeoSize - input$hgeoNrI) / input$hgeoSize) * ((input$hgeoSize - input$hgeoNrS) / (input$hgeoSize - 1))  }
    else if (input$repartitie == "norm") {input$deviation ** 2}
    else if (input$repartitie == "cuni") {((input$max2 - input$min2) ** 2) / 12}
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
  output$outputVA <- renderUI({
    #probs
    va1p1 <- input$va1p1;
    va1p2 <- input$va1p2;
    va1p3 <- input$va1p3;
    va2p1 <- input$va2p1;
    va2p2 <- input$va2p2;
    va2p3 <- input$va2p3;
    #vals
    va1v1 <- input$va1v1;
    va1v2 <- input$va1v2;
    va1v3 <- input$va1v3;
    va2v1 <- input$va2v1;
    va2v2 <- input$va2v2;
    va2v3 <- input$va2v3;
    #operatie
    op <- input$operatie;
    #rv's
    rv1 <- RV(c(va1v1, va1v2, va1v3), c(va1p1, va1p2, va1p3));
    rv2 <- RV(c(va2v1, va2v2, va2v3), c(va2p1, va2p2, va2p3));
    if((va1p1 + va1p2 + va1p3 != 1) | (va2p1 + va2p2 + va2p3 != 1)) {"Suma probabilitatilor trebuie sa fie = 1 pentru fiecare v.a."}
    else {
      if(op == "add") {
        rvAdd <- rv1 + rv2;
        HTML(paste(paste(outcomes(rvAdd), collapse = " || "), paste(probs(rvAdd), collapse = " "), sep = "<br/>"))
      }
      else if(op == "sub") {
        rvSub <- rv1 - rv2;
        HTML(paste(paste(outcomes(rvSub), collapse = " || "), paste(probs(rvSub), collapse = " "), sep = "<br/>"))
      }
      else if(op == "mul") {
        
        product.matrix <- t(outer(c(va1v1, va1v2, va1v3), c(va2v1, va2v2, va2v3),"*")) ## find all possible products
        probability.matrix <- t(outer(c(va1p1, va1p2, va1p3), c(va2p1, va2p2, va2p3)))
        unique.products <- unique(as.vector(product.matrix))  ## find the unique products
        probability.vector <- rep(0, length(unique.products))
        
        for(i in 1:length(probability.vector)){
          
          z <- unique.products[i]
          
          indices <- which(as.vector(product.matrix) == z) ## find which elements of product.matrix match up to z
          
          probability.vector[i] <- sum(as.vector(probability.matrix)[indices]) ## sum their probabilities
          
        }
        
        rvMul <- RV(unique.products, probability.vector);
        HTML(paste(paste(outcomes(rvMul), collapse = " || "), paste(probs(rvMul), collapse = " "), sep = "<br/>"))
      }
      else if(op == "div") {
        
        division.matrix <- t(outer(c(va1v1, va1v2, va1v3), c(va2v1, va2v2, va2v3),"/")) ## find all possible divisions
        probability.matrix <- t(outer(c(va1p1, va1p2, va1p3), c(va2p1, va2p2, va2p3)))
        unique.divisions <- unique(as.vector(division.matrix))  ## find the unique divisions
        probability.vector <- rep(0, length(unique.divisions))
        
        for(i in 1:length(probability.vector)){
          
          z <- unique.divisions[i]
          
          indices <- which(as.vector(division.matrix) == z) ## find which elements of division.matrix match up to z
          
          probability.vector[i] <- sum(as.vector(probability.matrix)[indices]) ## sum their probabilities
          
        }
        
        rvSub <- RV(unique.divisions, probability.vector);
        HTML(paste(paste(outcomes(rvSub), collapse = " || "), paste(probs(rvSub), collapse = " "), sep = "<br/>"))
      }
    }
    
  })
  
  output$renderOp <- renderText({
    if(input$operatie == "add") {" + "}
    else if(input$operatie == "sub") {" - "}
    else if(input$operatie == "mul") {" * "}
    else if(input$operatie == "div") {" / "}
    
  })
  
  output$median <- renderText({
    req(input$file);
    data <- read.csv(input$file$datapath,header = FALSE,sep = ",");
    v <- c(sort(unlist(data)));
    
    if (length(v) %% 2 == 0)
    {
      median <- v[length(v)/2];
    }
    else
    {
      median <- (v[length(v)/2] + v[length(v)/2 + 1]) / 2;
    }
  })
  output$quantile <- renderText({
    req(input$file);
    data <- read.csv(input$file$datapath,header = FALSE,sep = ",");
    v <- c(sort(unlist(data)));
    quantile(v);
  })
  
  output$boxplot <- renderPlot({
    req(input$file);
    data <- read.csv(input$file$datapath,header = FALSE,sep = ",");
    v <- c(sort(unlist(data)));
    boxplot(v);
  })
  output$hist <- renderPlot({
    req(input$file);
    data <- read.csv(input$file$datapath,header = FALSE,sep = ",");
    v <- c(sort(unlist(data)));
    hist(v);
  })
  output$vaPoisAfis <- renderUI({
    pois.func <- function(x, lambda) { lambda^x * exp(-lambda) / factorial(x) }
    rv <- RV(c(input$vaFirst, Inf), pois.func, lambda = input$vaLambda);
    HTML(paste(paste(outcomes(rv), collapse = " "), paste(probs(rv), collapse = " "), sep = "<br/>"))
  })
  output$vaPoisPlot <- renderPlot({
    pois.func <- function(x, lambda) { lambda^x * exp(-lambda) / factorial(x) }
    rv <- RV(c(input$vaFirst, Inf), pois.func, lambda = input$vaLambda);
    plot(rv);
  })
  output$commonTable <- renderTable({
    data.frame(c1 = c("A/B", input$valA1, input$valA2, "-"),
               c2 = c(input$valB1, input$probA1B1, input$probA2B1, input$probA1B1 + input$probA2B1),
               c3 = c(input$valB2, input$probA1B2, input$probA2B2, input$probA1B2 + input$probA2B2),
               c4 = c("-", input$probA1B1 + input$probA1B2, input$probA2B1 + input$probA2B2, input$probA1B1 + input$probA2B1 + input$probA1B2 + input$probA2B2)
              )
  })
  output$informatii <- renderUI({
    if(input$probA1B1 + input$probA1B2 + input$probA2B1 + input$probA2B2 != 1) {"Suma probabilitatilor trebuie sa fie 1!"}
    else{
      vA1 = input$valA1;
      vA2 = input$valA2;
      vB1 = input$valB1;
      vB2 = input$valB2;
      pA1B1 = input$probA1B1;
      pA1B2 = input$probA1B2;
      pA2B1 = input$probA2B1;
      pA2B2 = input$probA2B2;
      jrv <- jointRV(outcomes = list(c(vA1, vA2), c(vB1, vB2)), probs = c(pA1B1, pA1B2, pA2B1, pA2B2));
      A <- marginal(jrv, 1);
      B <- marginal(jrv, 2);
      
      HTML(
        "<div style='width: 100%'>",
        "<div style='width:50%; float:left;'>",
        "<h3>A:</h3>",
        "<h3>",
        paste(paste(outcomes(A), collapse = " "), paste(probs(A), collapse = " "), sep = "<br/>"),
        "</h3>",
        "<h3>B:</h3>",
        "<h3>",
        paste(paste(outcomes(B), collapse = " "), paste(probs(B), collapse = " "), sep = "<br/>"),
        "</h3>",
        "</div>",
        "<div style='width:50%; float: right'>",
        "<h3>",
        paste("Medie A: ", E(A)),
        "</h3>",
        "<h3>",
        paste("Varianta A: ", V(A)),
        "</h3>",
        "<h3>",
        paste("Medie B: ", E(B)),
        "</h3>",
        "<h3>",
        paste("Varianta B: ", V(B)),
        "</h3>",
        "<h3>",
        paste("Covarianta(A, B): ", E(A * B) - (E(A) * E(B))),
        "</h3>",
        "<h3>",
        paste("Coeficientul de corelatie (A, B): ", (E(A * B) - (E(A) * E(B))) / (sqrt(V(A)) * sqrt(V(B))) ),
        "</h3>",
        "</div>",
        "</div>"
        
        
        
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
}