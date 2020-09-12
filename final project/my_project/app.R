
library(shiny)
library(ggplot2)

#' This application will result in a graphic of simulated probability of roll
#' a fair ten-faced die n times
#' n is an slide bar input parameter and is the times the dice will be rolled
#' the output is a graph with the cumulative probability.
#' 
ui <- fluidPage(

    # Application title
    titlePanel("Simulating the Probability Distribution of a ten-faced die rolled n times"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n",
                        "Number of rows:",
                        min = 1,
                        max = 10000,
                        value = 5000)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    roll <- function(){ # function to simulate the six-faced die roll
        die <- 1:10
        result.roll <- sample(die, size = 1, replace = TRUE)
        return(result.roll)
    }
    
    #computing the mean of the sum of rolls
    r <- 1
    
    output$distPlot <- renderPlot({
        set.seed(1986)
        num.roll <- numeric(input$n)
        num.roll.prop <- numeric(input$n)
        for(i in 1:length(num.roll)){
            num.roll[i] <- roll()
            num.roll.prop[i] <- sum(num.roll[1:i] == r)/i 
        }
        
        number <- 1: input$n
        df <- cbind.data.frame(number, num.roll.prop)
        ggplot(df, aes(x = number, y = num.roll.prop)) + 
            geom_point(color = "dodgerblue4") +
            geom_hline(yintercept = 1/10, color = "red")
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
