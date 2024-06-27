#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



ComputeBMR = function(weight = 140, height_feet = 5,height_inches = 9, age = 30, gender = 1) {
  
  #Convert height to inches
  
  height = height_feet*12 + height_inches
  
  # Convert from imperial to metric
  
  weight = weight*0.45359237
  height = height*2.54
  
  if (gender == 1) {
    bmr = 10*weight + 6.25*height - 5*age + 5
  } else if (gender == 2) {
    bmr = 10*weight + 6.25*height - 5*age - 161
  }
  
  return(bmr)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weight Loss Calculator"),

    fluidRow(
  
          column(
            width = 2,
            selectInput(inputId = "SelectAge", 
                      label = h4("Age"), 
                      choices = c(16:80),
                      selected = 30),
            radioButtons(inputId = "SelectGender", 
                         label = h4("Gender"),
                         choices = list("Male" = 1, 
                                      "Female" = 2), 
                         selected = 1),
            ),
          column(width = 2,
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
                       value = 190)
          ),
          column(width = 3,
                 sliderInput(inputId = "SelectBF",
                      "Current Estimated bodyfat %:",
                      min = 8,
                      max = 50,
                      step = .5,
                      value = 30),
                 sliderInput(inputId = "SelectBFGoal",
                      "Bodyfat % Goal:",
                      min = 8,
                      max = 50,
                      step = .5,
                      value = 20),
                 sliderInput(inputId = "SelectWeightLossRate",
                      "Amount of Weight Loss per week (lbs/week):",
                      min = .25,
                      max = 1.5,
                      step = .25,
                      value = .50)
          ),
          column(width = 2,
                 selectInput(inputId = "SelectActivityLevel", 
                      label = "Activity Level",
                      choices = list("Sedentary" = .20,
                                     "Light exercise" = .375,
                                     "Moderate exercise" = .5,
                                     "Heavy exercise" = .7,
                                     "Athlete" = .9),
                      selected = .375),
                 dateInput(inputId = "StartingDate",
                           label = "Start Date",
                           value = "2024-01-01"),
                 
          ),
          
        ),
    
    hr(),
    
    h4("Maintenance Calories:"),
    
    textOutput(outputId = "maintenanceCalories"),
    
    h4("Calories to Conusume for Weight Loss:"),
    
    textOutput(outputId = "targetCalories"),
    
    h4("Bodyweight Goal:"),
    
    textOutput(outputId = "bodyweightGoal"),
    
    h4("Weeks to reach goal:"),
    
    textOutput(outputId = "timeToReachGoal"),
    
    h4("Date to reach goal:"),
    
    verbatimTextOutput(outputId = "dateToReachGoal"),
    
    h3("Data Table"),
    
    hr(),
    
    h3("Important Information")

    
)

# Define server logic 

server <- function(input, output) {
  
  activity = reactive({as.numeric(input$SelectActivityLevel)})
  
  bmr = reactive({ComputeBMR(weight = input$SelectWeight,
                             height_feet = as.numeric(input$SelectHeightFeet),
                             height_inches = as.numeric(input$SelectHeightInches), 
                             age = as.numeric(input$SelectAge),
                             gender = input$SelectGender)})
  
  
  maintenance_calories = reactive({bmr() + bmr()*activity()})
  
  weight_loss_rate = reactive({input$SelectWeightLossRate})
  
  my_weight = reactive({input$SelectWeight})
  bf_percentage = reactive({input$SelectBF})
  bf_percentage_goal = reactive({input$SelectBFGoal})
  
  lean_mass = reactive({my_weight() - my_weight()*(bf_percentage()/100)})
  
  bodyweight_goal = reactive({(lean_mass()/(1 - (bf_percentage_goal()/100)))})
  

    output$maintenanceCalories = renderText({
      
      round(maintenance_calories())
    })
    
    output$targetCalories = renderText({
      

      target_calories = maintenance_calories() - 500*weight_loss_rate()
      
      round(target_calories)
    })
    
    output$bodyweightGoal = renderText({
      
      
      
      round(bodyweight_goal(),1)
    })
    
    output$timeToReachGoal = renderText({
      
      weeks_to_reach_goal = (my_weight() - bodyweight_goal())/(weight_loss_rate())
      
      round(weeks_to_reach_goal,1)
      
    })
    
    output$dateToReachGoal = renderPrint({
      
      starting_date = reactive({input$StartingDate})
      
      ###
      
      #my_weight = 135
      #current_bf_percentage = .15
      #goal_bf_percentage = .11
      #lean_mass = my_weight - my_weight*current_bf_percentage

      #bodyweight_goal = (lean_mass/(1 - goal_bf_percentage))
      #bodyweight_goal
      
      
      #my_weight - bodyweight_goal
      
      weeks_to_reach_goal = (my_weight() - bodyweight_goal())/(weight_loss_rate())


      #todays_date = as.Date("2024-6-27")
      #todays_date
      
      goal_date = starting_date() + weeks_to_reach_goal*7
      ###
      
      goal_date
      
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
