library(shiny)

ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "vapor"),

                titlePanel("Distributions"),

                sidebarLayout(
                  
                  sidebarPanel(
                    
                    radioButtons("dist", "Distribution type:",
                                 c("Normal" = "norm",
                                   "Uniform" = "unif",
                                   "Log-normal" = "lnorm",
                                   "Exponential" = "exp")),
                    
                    br(),
                    
                    sliderInput("n",
                                "Number of observations:",
                                value = 500,
                                min = 1,
                                max = 1000)
                    
                  ),

                  mainPanel(

                    tabsetPanel(type = "tabs",
                                tabPanel("Plot", plotOutput("plot")),
                                tabPanel("Summary", verbatimTextOutput("summary")),
                                tabPanel("Description", verbatimTextOutput("description")),
                                tabPanel("Table", tableOutput("table"))
                    )
                    
                  )
                )
)

server <- function(input, output,session) {
  
  
 showNotification("WELCOME!!!",type="warning",duration = 3)
 Sys.sleep(4)
 showNotification("R-PROGRAMMING PROJECT",type="warning",duration=3)
 Sys.sleep(3)
  
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  
  # Generate a plot of the data 

  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    par(bg="white")
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "green", border = "black")
  })
  
  # Generate a summary of the data 
  
  output$summary <- renderPrint({
    summary(d())
    
  })
  
  output$description <- renderPrint({
    x= input$n
    type=input$dist
    
    cat("\nno. of observations is ")
    cat(x,"\n")
    
    if (type == "norm"){
      cat("\n\nNormal distribution is used.\n\nA Normal distribution is an arrangememnt of a data set in which\nmost values cluster in the middle of the range and the rest taper \noff symmetrically toward either extreme. ")
    }
    if (type=="unif"){
      cat("\n\nUniform distribution is used.\n\nIn probability theory and statistics, the uniform distribution is a symmetric probability \ndistribution wherein a finite number of values are equally likely to be observed; \nevery one of n values has equal probability 1/n")
    }
    if (type=="lnorm"){
      cat("\n\nLog normal distribution is used.\n\nA set of data in which the logarithm of the variate is distributed \naccording to a normal distribution.")
    }
    if (type=="exp"){
      cat("\n\nExponential distribution is used.\n\nThe exponential distribution is defined as the probability distribution of \ntime between events in the Poisson point process.")
    }
  })
  
  output$table <- renderTable({
    d()
  })

}

shinyApp(ui, server)
