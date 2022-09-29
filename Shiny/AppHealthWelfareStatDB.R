library(shiny)
library(ggplot2)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app
vars <- return_meta(type="var", lang="en", topic="diagnoserislutenvard")
var_list <- vars[,1]
# values_list <- c("1,3",
#                  "10,11",
#                  "1,2",
#                  "1",
#                  "2012,2013",
#                  "J13")

values_list <- c("1,3",
                 "10",
                 "1,2",
                 "1",
                 "2012,2013",
                 "J13")


df_input_vars <- as.data.frame(cbind(var_list,values_list))
data <- return_data(lang="en",topic="diagnoserislutenvard", df_input_vars, addText=TRUE)

#data_all <- contentToDataframe_data(data_obj)
#data <- data_all[,2:6]

#data <- contentToDataframe_data(data_obj, addText=TRUE)



# Define UI for miles per gallon app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Inpatient care diagnosis data"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      ##############
      #radioButtons("radio", label = ("Radio buttons"),
      #             choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
      #             selected = 1),

      #hr(),
      #fluidRow(column(3, verbatimTextOutput("value"))),

      ##############

      # Input: Selector for variable to plot against mpg ----
      selectInput("var1", "X_axis_variable",
                  c("Year" = "ar",
                    "Sex" = "konText",
                    "Region" = "regionText")),

      # Input: Selector for variable to plot against mpg ----
      selectInput("var2", "Stack variable:",
                  c("Sex" = "konText",
                    "Year" = "ar",
                    "Region" = "regionText")),

      # Input: Selector for variable to plot against mpg ----
      selectInput("var3", "Trellis variable:",
                  c("Region" = "regionText",
                    "Sex" = "konText",
                    "Year" = "ar")),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Formatted text for caption ----
      h3(textOutput("caption")),

      # Output: Plot of the requested variable against mpg ----
      plotOutput("plot")

    )
  )
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    #paste("mattId ~", input$var1, input$var2, input$var3)
    measure <- unique(data$mattText)
    paste(measure)
  })

  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })

  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$plot <- renderPlot({
    df <- data
    #df$in1 <- input$var1
    #df$in2 <- input$var2
    #df$in3 <- input$var3

    var1 <- data[,input$var1]
    var2 <- data[,input$var2]
    var3 <- data[,input$var3]
    varde <- data[,"varde"]
    data$var3 <- var3
    #ggplot(data, aes(fill=var2, x=var1, y=as.numeric(varde))) +
    #  geom_bar(position="stack", stat="identity") + facet_wrap(~ var3)



    if(input$var1 == "ar"){namevar1 <- "Year"}
    else if(input$var1 == "konText"){namevar1 <- "Sex"}
    else if(input$var1 == "regionText"){namevar1 <- "Region"}

    if(input$var2 == "ar"){namevar2 <- "Year"}
    else if(input$var2 == "konText"){namevar2 <- "Sex"}
    else if(input$var2 == "regionText"){namevar2 <- "Region"}

    ggplot(data, aes(fill=var2, x=var1, y=as.numeric(varde))) +
      geom_bar(position="stack", stat="identity") + facet_wrap(~ var3)+
      xlab(namevar1) + ylab("Length of stay") + labs(fill = namevar2)

    #ggplot(df, aes(fill=in2, x=in1, y=as.numeric(varde))) +
    #  geom_bar(position="stack", stat="identity") + facet_wrap(~ in3)

    #ggplot(data, aes(fill=konText, x=ar, y=as.numeric(varde))) +
    #  geom_bar(position="stack", stat="identity") + facet_wrap(~ regionText)

  })

}

# Create Shiny app ----
shinyApp(ui, server)
