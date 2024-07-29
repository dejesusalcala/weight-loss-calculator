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
    
    h4("Current Maintenance Calories:"),
    
    textOutput(outputId = "maintenanceCalories"),
    
    h4("Current Caloric Deficit:"),
    
    textOutput(outputId = "targetCalories"),
    
    h4("Current Weight"),
    
    textOutput(outputId = "currentWeight"),
    
    h4("Bodyweight Goal:"),
    
    textOutput(outputId = "bodyweightGoal"),
    
    h4("Total Weight to Lose:"),
    
    textOutput(outputId = "weightToLose"),
    
    h4("Weeks to reach goal:"),
    
    textOutput(outputId = "timeToReachGoal"),
    
    h4("Date to reach goal:"),
    
    verbatimTextOutput(outputId = "dateToReachGoal"),
    
    h3("Data Table"),
    
    tableOutput(outputId = "dataTable"),
    
    hr(),
    
    h3("Important Information"),
    
    textOutput(outputId = "Paragraph1"),
    
    hr()
    
    
)

# Define server logic 

server <- function(input, output) {
  
  activity = reactive({as.numeric(input$SelectActivityLevel)})
  
  age = reactive({as.numeric(input$SelectAge)})
  
  height_feet = reactive({as.numeric(input$SelectHeightFeet)})
  
  height_inches = reactive({as.numeric(input$SelectHeightInches)})
  
  gender = reactive({input$SelectGender})
  
  bmr = reactive({ComputeBMR(weight = input$SelectWeight,
                             height_feet = height_feet(),
                             height_inches = height_inches(), 
                             age = age(),
                             gender = gender())
    })
  
  
  maintenance_calories = reactive({bmr() + bmr()*activity()})
  
  weight_loss_rate = reactive({input$SelectWeightLossRate})
  
  my_weight = reactive({input$SelectWeight})
  bf_percentage = reactive({input$SelectBF})
  bf_percentage_goal = reactive({input$SelectBFGoal})
  
  lean_mass = reactive({my_weight() - my_weight()*(bf_percentage()/100)})
  
  bodyweight_goal = reactive({(lean_mass()/(1 - (bf_percentage_goal()/100)))})
  
  weeks_to_reach_goal = reactive({(my_weight() - bodyweight_goal())/(weight_loss_rate())})
  
  starting_date = reactive({input$StartingDate})
  
  # Create data table
  
  number_of_weeks = reactive({ceiling(weeks_to_reach_goal())})
  
  
  DATE = reactive({seq(starting_date(), starting_date() + number_of_weeks()*7, by = "week")})
  
  WEIGHT = reactive({seq(my_weight(), bodyweight_goal() -weight_loss_rate(),by = -weight_loss_rate())})
  
  BF = reactive({(1 - lean_mass()/WEIGHT())})
   
   output$maintenanceCalories = renderText({
      
      round(maintenance_calories())
    })
    
    output$targetCalories = renderText({
      

      target_calories = maintenance_calories() - 500*weight_loss_rate()
      
      round(target_calories)
    })
    
    output$currentWeight = renderText({
      
      paste0(my_weight()," lbs")
      
    })
    
    output$bodyweightGoal = renderText({
      
      paste0(round(bodyweight_goal(),1)," lbs")
      
    })
    
    output$weightToLose = renderText({
      
      paste0(round(my_weight() - bodyweight_goal(), digits = 1), " lbs")
    
      })
    
    output$timeToReachGoal = renderText({
      
      weeks_to_reach_goal = (my_weight() - bodyweight_goal())/(weight_loss_rate())
      
      paste0(round(weeks_to_reach_goal,1), " weeks")
      
    })
    
    output$dateToReachGoal = renderPrint({

      goal_date = starting_date() + weeks_to_reach_goal()*7
      
      goal_date
      
    })
    
    
    output$dataTable = renderTable({
      
      height = height_feet() + height_inches()/12
      
      height_cm = height*30.48
      weight_kg = WEIGHT()/2.205
      
      if(input$SelectGender == 1)
      {
        table_bmr = 10*weight_kg + 6.25*height_cm - 5*age() + 5
        
      }else{
        table_bmr = 10*weight_kg + 6.25*height_cm - 5*age() - 161
        
      }
      
      table_maintenance_calories = table_bmr + table_bmr*activity()
      
      DataTable = data.frame(as.character(DATE(), origin = "1970-01-01"), 
                             as.character(WEIGHT()),
                             as.character(round(BF()*100, digits = 1)),
                             as.character(round(table_maintenance_calories, digits = 0)))
      
      colnames(DataTable) = c("Date",
                              "Weight (lbs)",
                              "Bodyfat (%)",
                              "Maintenance Calories")
      
      DataTable
      
      })
    
    output$Paragraph1 = renderText({
      
      text1 = "
      Maintenance calories are computed using the Mifflin-St Jeor equation. Therefore the calories given
      are dependent age, weight, height and activity level. In this calculator we do have bodyfat percentage
      as an addtional input, but this is used for the purpose of estimating a desired bodyweight goal along 
      with a timeline of when that bodyweight goal can be reached.
      "
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
