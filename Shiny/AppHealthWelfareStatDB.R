library(shiny)
library(ggplot2)

# Data pre-processing

vars <- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
var_list <- vars[,1]

values_list <- c("1,3",
                 "10",
                 "1,2",
                 "1",
                 "2012,2013",
                 "J13")


df_input_vars <- as.data.frame(cbind(var_list,values_list))
data <- return_data(lang="en",topic="diagnoserislutenvard", df_input_vars, addText=TRUE)

# Define UI
ui <- fluidPage(

  # App title ----
  titlePanel("Inpatient care diagnosis data"),

  # Sidebar layout with input and output
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(


      # Input: Selector for variable to plot
      selectInput("var1", "X_axis_variable",
                  c("Year" = "ar",
                    "Sex" = "konText",
                    "Region" = "regionText")),

      # Input: Selector for variable to plot
      selectInput("var2", "Stack variable:",
                  c("Sex" = "konText",
                    "Year" = "ar",
                    "Region" = "regionText")),

      # Input: Selector for variable to plot
      selectInput("var3", "Trellis variable:",
                  c("Region" = "regionText",
                    "Sex" = "konText",
                    "Year" = "ar")),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption
      h3(textOutput("caption")),

      # Output: Plot of the requested variable
      plotOutput("plot")

    )
  )
)

# Define server logic to plot various variables
server <- function(input, output) {

 #Reactive part of the program
  formulaText <- reactive({

    measure <- unique(data$mattText)
    paste(measure)
  })

  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })

#Plotting
  output$plot <- renderPlot({
    df <- data


    var1 <- data[,input$var1]
    var2 <- data[,input$var2]
    var3 <- data[,input$var3]
    varde <- data[,"varde"]
    data$var3 <- var3



    if(input$var1 == "ar"){namevar1 <- "Year"}
    else if(input$var1 == "konText"){namevar1 <- "Sex"}
    else if(input$var1 == "regionText"){namevar1 <- "Region"}

    if(input$var2 == "ar"){namevar2 <- "Year"}
    else if(input$var2 == "konText"){namevar2 <- "Sex"}
    else if(input$var2 == "regionText"){namevar2 <- "Region"}

    ggplot(data, aes(fill=var2, x=var1, y=as.numeric(varde))) +
      geom_bar(position="stack", stat="identity") + facet_wrap(~ var3)+
      xlab(namevar1) + ylab("Length of stay") + labs(fill = namevar2)


  })

}

# Create Shiny app ----
shinyApp(ui, server)
