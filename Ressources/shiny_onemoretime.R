library(shiny)
library(MonPackage)
reactlog::reactlog_enable()

ui <- fluidPage(
  selectInput("distribution2",
              label = "Choose a random distribution",
              choices = list("Normal distribution",
                             "Poisson distribution",
                             "Bernoulli distribution",
                             "Student distribution"),
              selected = "Normal distribution"),
  ###### NORMAL DISTRIBUTION CASE #################
  conditionalPanel(
    condition = "input.distribution2 == 'Normal distribution'",
    numericInput(inputId = "mean2",
                 label = "Choose the mean:",
                 min = -20, max = 20, value = 0, step = 0.5),
    numericInput(inputId = "variance2",
                 label = "Choose the variance:",
                 min = 1, max = 20, value = 1, step = 0.5)),
  numericInput("lengthX",
               label = "Choose the length of X axis",
               value = 10),
  
  numericInput("lengthY",
               label = "Choose the length of Y axis",
               min = 1, max = 200, value = 10),
  
  sliderInput("rayon",
              label = "Choose the radius r of the window used for the moving average:",
              min = 0, max = 100, value = 1),
  
  plotOutput("plot")
)

server <- function(input, output, session) {
  outputOptions(output, "plot", suspendWhenHidden=FALSE)

  output$plot <- renderPlot({
    if (input$distribution2 == "Normal distribution"){
  
  MNormale <- reactive({
    matrix(rnorm((input$lengthX)*(input$lengthY), input$mean2, sqrt(input$variance2)), nrow = input$lengthX)
  }) 
  r <- reactive(input$rayon)
  Moy <- reactive(moving_average(MNormale(), r()))
  p1 <- reactive(plot_matrix(MNormale(), titre = "Matrice"))
  p2 <- reactive(plot_matrix(Moy(), titre = "Matrice", r()) + coord_cartesian(xlim = c(0, dim(MNormale())[1]), ylim = c(0, dim(MNormale())[2])))
  

    cowplot::plot_grid(p1(),p2(), ncol=1, nrow=2)
    }
    })
  

  

}

shinyApp(ui, server)


######## END OF THE FIRST APP ########



library(shinydashboard)


ui <- dashboardPage(
  dashboardHeader(title = "Test réactivité"),
  dashboardSidebar(
    sidebarMenu(
        menuItem("Moving average", tabName = "Moyenne_glissante")
      )
  ),
  dashboardBody(
    tabItem(tabName = "Moyenne_glissante",
            fluidRow(
                box(
                  title = "Distribution",
                  selectInput("distribution2",
                              label = "Choose a random distribution",
                              choices = list("Normal distribution",
                                             "Poisson distribution",
                                             "Bernoulli distribution",
                                             "Student distribution"),
                              selected = "Normal distribution"),
                  ###### NORMAL DISTRIBUTION CASE #################
                  conditionalPanel(
                    condition = "input.distribution2 == 'Normal distribution'",
                    numericInput(inputId = "mean2",
                                 label = "Choose the mean:",
                                 min = -20, max = 20, value = 0, step = 0.5),
                    numericInput(inputId = "variance2",
                                 label = "Choose the variance:",
                                 min = 1, max = 20, value = 1, step = 0.5)),
              numericInput("lengthX",
                           label = "Choose the length of X axis",
                           value = 10),
              
              numericInput("lengthY",
                           label = "Choose the length of Y axis",
                           min = 1, max = 200, value = 10),
              
              sliderInput("rayon",
                          label = "Choose the radius r of the window used for the moving average:",
                          min = 0, max = 100, value = 1),
              
              plotOutput("plot")
              )
              
            ))
    
    
  )
)

server <- function(input, output) {   

  
  output$plot <- renderPlot({
    
    #distribution2 <- reactive(input$distribution2)
  
  #if (distribution2() == "Normal distribution"){
    #if (input$distribution2 == "Normal distribution"){
    
    MNormale <- reactive({
      matrix(rnorm((input$lengthX)*(input$lengthY), input$mean2, sqrt(input$variance2)), nrow = input$lengthX)
    }) 
    r <- reactive(input$rayon)
    Moy <- reactive(moving_average(MNormale(), r()))
    p1 <- reactive(plot_matrix(MNormale(), titre = "Matrice"))
    p2 <- reactive(plot_matrix(Moy(), titre = "Matrice", r()) + coord_cartesian(xlim = c(0, dim(MNormale())[1]), ylim = c(0, dim(MNormale())[2])))

    cowplot::plot_grid(p1(),p2(), ncol=1, nrow=2)#}
  
  
})
  outputOptions(output, "plot", suspendWhenHidden=FALSE)
  
  
}

shinyApp(ui, server)

