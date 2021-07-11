library(shiny)
library(shinydashboard)
library(MonPackage)
reactlog::reactlog_enable()

source('/Users/juliette/Desktop/Stage/Shiny_app/Distributions.R')
source('/Users/juliette/Desktop/Stage/Shiny_app/Randomly_generated_fields.R')
source('/Users/juliette/Desktop/Stage/Shiny_app/Moving_average.R')


ui <- dashboardPage(
  dashboardHeader(title = "Geostatistic App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bonjour hi !", tabName = "Summary"),
     menuItem("Moving average", tabName = "Moyenne_glissante")
    )
  ),
  dashboardBody(
    tabItems(
      
      # Onglet Bonjour hi !
      tabItem(tabName = "Summary",
              h2("Summary")
      ),
      
      # On glet moving average
      tabItem(tabName = "Moyenne_glissante",
              fluidRow(
                
                box(h3("Moving average on a matrix"), plotOutput("matrice", height = 500)), #plotOutput("mean_of_matrix", height = 250)),
                
                box(
                  title = "Distribution",
                  selectInput("distribution2",
                              label = "Choose a random distribution",
                              choices = distribution_choices,
                              selected = "Normal distribution"),
                  
                  #CONDITIONAL PANELS 
                  # #conditionalPanel_normal_distribution(
                  #   condition = "input.distribution2 == 'Normal distribution'",
                  #   meanId = "mean2", varianceId = "variance2", setxId = ""),
                  
                  numericInput(inputId = "mean2",
                               label = "Choose the mean:",
                               min = -20, max = 20, value = 0, step = 0.5),
                  numericInput(inputId = "variance2",
                               label = "Choose the variance:",
                               min = 1, max = 20, value = 1, step = 0.5),
                  
                
                  ###################################################################
                  
                  numericInput("lengthX",
                               label = "Choose the length of X axis",
                               value = 10),
                  
                  numericInput("lengthY",
                               label = "Choose the length of Y axis",
                               min = 1, max = 200, value = 10),
                  
                  sliderInput("rayon",
                              label = "Choose the radius r of the window used for the moving average:",
                              min = 0, max = 100, value = 1)
                )
                
                
              )
              
      )
    )
  )
)



server <- function(input, output) {

  output$matrice <- renderPlot({
    #if (input$distribution2 == "Normal distribution"){
      MNormale <- reactive({
        matrix(rnorm((input$lengthX)*(input$lengthY), input$mean2, sqrt(input$variance2)), nrow = input$lengthX)
      }) 
      r <- reactive(input$rayon)
      Moy <- reactive(moving_average(MNormale(), r()))
      p1 <- reactive(plot_matrix(MNormale(), titre = "Matrice"))
      p2 <- reactive(plot_matrix(Moy(), titre = "Matrice",r()) + coord_cartesian(xlim = c(0, dim(MNormale())[1]), ylim = c(0, dim(MNormale())[2])))
      cowplot::plot_grid(p1(),p2(), ncol=1, nrow=2)#}     
  })
    #plot_matrix(matrix(c(1,2,3,4), nrow = 2) ) }
  #   else if (input$distribution2 == "Poisson distribution"){
  #     MPoisson <- matrix(rpois((input$lengthX)*(input$lengthY), input$lambda2), nrow = input$lengthX)
  #     Moy <- moving_average(MPoisson, r = input$rayon)
  #     p1 <- plot_matrix(MPoisson, titre = "Matrice")
  #     p2 <- plot_matrix(Moy, titre = "Matrice", r = input$rayon) + coord_cartesian(xlim = c(0, dim(MPoisson)[1]), ylim = c(0, dim(MPoisson)[2]))
  #     cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
  #   else if (input$distribution2 == "Bernoulli distribution"){
  #     MBernoulli <- matrix(rbinom((input$lengthX)*(input$lengthY), 1, input$p2), nrow = input$lengthX)
  #     Moy <- moving_average(MBernoulli, r = input$rayon)
  #     p1 <- plot_matrix(MBernoulli, titre = "Matrice")
  #     p2 <- plot_matrix(Moy, titre = "Matrice", r = input$rayon) + coord_cartesian(xlim = c(0, dim(MBernoulli)[1]), ylim = c(0, dim(MBernoulli)[2]))
  #     cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
  #   else if (input$distribution2 == "Student distribution"){
  #     MStudent <- matrix(rt((input$lengthX)*(input$lengthY), input$k2), nrow = input$lengthX)
  #     Moy <- moving_average(MStudent, r = input$rayon)
  #     p1 <- plot_matrix(MStudent, titre = "Matrice")
  #     p2 <- plot_matrix(Moy, titre = "Matrice", r = input$rayon) + coord_cartesian(xlim = c(0, dim(MStudent)[1]), ylim = c(0, dim(MStudent)[2]))
  #     cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}


 # })
  
  
}

shinyApp(ui, server)
