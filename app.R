#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weight Loss Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId = "SelectAge", 
                      label = h4("Age"), 
                      choices = c(16:80), 
                      selected = 30),
          radioButtons(inputId = "SelectGender", 
                       label = h4("Gender"),
                       choices = list("Male" = 1, 
                                      "Female" = 2), 
                       selected = 1),
          selectInput(inputId = "SelectHeightFeet", 
                      label = h4("Height (feet, inches)"), 
                      choices = list("4 feet" = 4,
                                     "5 feet" = 5,
                                     "6 feet" = 6), 
                      selected = 5),
          selectInput(inputId = "SelectHeightInches", 
                      label = "",
                      choices = list("0 inches" = 0,
                                     "1 inches" = 1,
                                     "2 inches" = 2,
                                     "3 inches" = 3,
                                     "4 inches" = 4,
                                     "5 inches" = 5,
                                     "6 inches" = 6,
                                     "7 inches" = 7,
                                     "8 inches" = 8,
                                     "9 inches" = 9,
                                     "10 inches" = 10,
                                     "11 inches" = 11), 
                      selected = 9),
          numericInput(inputId = "SelectWeight", 
                       label = h4("Weight (lbs)"),
                       value = 190),
          sliderInput(inputId = "SelectBF",
                      "Estimated bodyfat %",
                      min = 8,
                      max = 50,
                      step = .5,
                      value = 30),
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
