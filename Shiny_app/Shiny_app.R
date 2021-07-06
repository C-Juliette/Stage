library(shiny)
library(shinydashboard)
library(MonPackage)
reactlog::reactlog_enable()

source('Distributions.R')
source('Randomly_generated_fields.R')
source('Moving_average.R')


ui <- dashboardPage(
  dashboardHeader(title = "Geostatistic App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Bonjour hi !", tabName = "Summary"),
      menuItem("Distributions", tabName = "Distributions"),
      menuItem("Randomly generated fields", tabName = "Couple_distribution_matrice"),#, icon = icon("dashboard")),
      menuItem("Moving average", tabName = "Moyenne_glissante"),
      menuItem("Block averages", tabName = "Moyenne_par_blocs"),#, icon = icon("th")),
      menuItem("I wanna use my own matrix", tabName = "Insertion_dimages")#, icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(

      # Onglet Bonjour hi !
      tabItem(tabName = "Summary",
              h2("Summary")
      ),
      
      # Onglet Distributions
      tabItem(tabName = "Distributions",
              fluidRow(
                 
                 box(plotOutput("superposition_densite", height = 250)),
                 
                 box(title = "Distributions",
                     checkboxGroupInput_distributions(inputId = "distrib")
                     )
              )
      ),
      
      # Onglet randomly genrated fields
      tabItem(tabName = "Couple_distribution_matrice",
              fluidRow(
                 
                box(
                    h3("Theoritical density and a realization on a matrix"),
                    plotOutput("plot1_densite", height = 250), 
                    plotOutput("plot2_matrice", height = 250)
                    ),

                box(
                    title = "Distribution",
                    selectInput_distributions(inputId = "distribution"),

                  #CONDITIONAL PANELS 
                  conditionalPanel_normal_distribution(
                     condition = "input.distribution == 'Normal distribution'",
                     meanId = "mean", varianceId = "variance", setxId = "xminmax"),
                  conditionalPanel_poisson_distribution(
                     condition = "input.distribution == 'Poisson distribution'",
                     lambdaId = "lambda", setxId = "xminmaxP"
                  ),
                  conditionalPanel_bernoulli_distribution(
                     condition = "input.distribution == 'Bernoulli distribution'",
                     pId = "p", setxId = "xminmaxB"
                   ),
                  conditionalPanel_student_distribution(
                     condition = "input.distribution == 'Student distribution'",
                     kId = "k", setxId = "xminmaxS"
                  )

                )
                )


              #)
      ),

      # Second tab content
      tabItem(tabName = "Moyenne_glissante",
              fluidRow(

                box(h3("Moving average on a matrix \n \n"), plotOutput("matrice", height = 500)), #plotOutput("mean_of_matrix", height = 250)),

                box(
                  title = "Distribution",
                  selectInput("distribution2",
                              label = "Choose a random distribution",
                              choices = distribution_choices,
                              selected = "Normal distribution"),
                  
                  #CONDITIONAL PANELS 
                  conditionalPanel_normal_distribution(
                     condition = "input.distribution2 == 'Normal distribution'",
                     meanId = "mean2", varianceId = "variance2", setxId = ""),
                  conditionalPanel_poisson_distribution(
                     condition = "input.distribution2 == 'Poisson distribution'",
                     lambdaId = "lambda2", setxId = ""
                  ),
                  conditionalPanel_bernoulli_distribution(
                     condition = "input.distribution2 == 'Bernoulli distribution'",
                     pId = "p2", setxId = ""
                  ),
                  conditionalPanel_student_distribution(
                     condition = "input.distribution2 == 'Student distribution'",
                     kId = "k2", setxId = ""
                  ),
                  
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

      ),
      # Third tab content
      tabItem(tabName = "Moyenne_par_blocs",
              h2("Average by blocks")
      ),

      # Fourthtab content
      tabItem(tabName = "Insertion_dimages",
              h2("My own matrix")
      )
    )
  )
)



server <- function(input, output) {
#  distr <- reactive({
#    input$distr
#  })


# 
 output$plot1_densite <- renderPlot({

   if (input$distribution == "Normal distribution"){
     #x = seq(input$mean - 4*input$variance, input$mean + 4*input$variance, 0.01)
     x = seq(input$xminmax[1] , input$xminmax[2], length = 100)
     d1= c(dnorm(x, input$mean, sqrt(input$variance)))
     df = data.frame(x, d1)
       p <- ggplot(df, aes(x)) +
         geom_area(aes(y = d1), color = "orange",lwd = 0.9, fill = "orange", alpha=0.2) +
         ylab("Normal theoritical distribution")
       p
  }
   else if (input$distribution == "Poisson distribution"){
     #x = seq(input$mean - 4*input$variance, input$mean + 4*input$variance, 0.01)
     x = seq(input$xminmaxP[1] , input$xminmaxP[2])
     d1= c(dpois(x, input$lambda))
     df = data.frame(x, d1)
     p <- ggplot(df, aes(x)) +
       geom_point(aes(y = d1), color = "magenta3",lwd = 0.9) +
       geom_segment(data = df, aes(x = x, y = d1, xend = x, yend = 0), color = "magenta3") +
       ylab("Poisson theoritical distribution")

     p
   }

else if (input$distribution == "Bernoulli distribution"){
  #x = seq(input$mean - 4*input$variance, input$mean + 4*input$variance, 0.01)
  x = seq(input$xminmaxB[1] , input$xminmaxB[2])
  d1= c(dbinom(x, 1, input$p))
  df = data.frame(x, d1)
  p <- ggplot(df, aes(x)) +
    geom_point(aes(y = d1), color = "green",lwd = 0.9) +
    geom_segment(data = df, aes(x = x, y = d1, xend = x, yend = 0), color = "green") +
    ylab("Bernoulli theoritical distribution")
  p
}

   else if (input$distribution == "Student distribution"){
     #x = seq(input$mean - 4*input$variance, input$mean + 4*input$variance, 0.01)
     x = seq(input$xminmaxS[1] , input$xminmaxS[2], length = 100)
     d1= c(dt(x, input$k))
     df = data.frame(x, d1)
     p <- ggplot(df, aes(x)) +
       geom_area(aes(y = d1), color = "red",lwd = 0.9, fill = "red", alpha=0.2) +
       ylab("Student theoritical distribution")
     p
   }

# 
# 
    }
    )

 output$plot2_matrice <- renderPlot({
   n = 20
   if (input$distribution == "Normal distribution"){
     plot_matrix( matrix(rnorm(n*n, input$mean, sqrt(input$variance)), nrow = n), titre = "Matrice")}
   else if (input$distribution == "Poisson distribution"){
     plot_matrix( matrix(rpois(n*n, input$lambda), nrow = n), titre = "Matrice")}
   else if (input$distribution == "Bernoulli distribution"){
     plot_matrix( matrix(rbinom(n*n, 1, input$p), nrow = n), titre = "Matrice")}
   else if (input$distribution == "Student distribution"){
     plot_matrix(matrix(rt(n*n, input$k), nrow = n), titre = "Matrice")}
 })




 output$matrice <- renderPlot({
   if (input$distribution2 == "Normal distribution"){
     MNormale <- reactive({
        matrix(rnorm((input$lengthX)*(input$lengthY), input$mean2, sqrt(input$variance2)), nrow = input$lengthX)
     }) 
     r <- reactive(input$rayon)
     Moy <- reactive(moving_average(MNormale(), r()))
     p1 <- reactive(plot_matrix(MNormale(), titre = "Matrice"))
     p2 <- reactive(plot_matrix(Moy(), titre = "Matrice", r()) + coord_cartesian(xlim = c(0, dim(MNormale())[1]), ylim = c(0, dim(MNormale())[2])))
     cowplot::plot_grid(p1(),p2(), ncol=1, nrow=2)}
   #plot_matrix(matrix(c(1,2,3,4), nrow = 2) ) }
   else if (input$distribution2 == "Poisson distribution"){
     MPoisson <- matrix(rpois((input$lengthX)*(input$lengthY), input$lambda2), nrow = input$lengthX)
     Moy <- moving_average(MPoisson, r = input$rayon)
     p1 <- plot_matrix(MPoisson, titre = "Matrice")
     p2 <- plot_matrix(Moy, titre = "Matrice", r = input$rayon) + coord_cartesian(xlim = c(0, dim(MPoisson)[1]), ylim = c(0, dim(MPoisson)[2]))
    cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
   else if (input$distribution2 == "Bernoulli distribution"){
     MBernoulli <- matrix(rbinom((input$lengthX)*(input$lengthY), 1, input$p2), nrow = input$lengthX)
     Moy <- moving_average(MBernoulli, r = input$rayon)
     p1 <- plot_matrix(MBernoulli, titre = "Matrice")
     p2 <- plot_matrix(Moy, titre = "Matrice", r = input$rayon) + coord_cartesian(xlim = c(0, dim(MBernoulli)[1]), ylim = c(0, dim(MBernoulli)[2]))
     cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
   else if (input$distribution2 == "Student distribution"){
     MStudent <- matrix(rt((input$lengthX)*(input$lengthY), input$k2), nrow = input$lengthX)
     Moy <- moving_average(MStudent, r = input$rayon)
     p1 <- plot_matrix(MStudent, titre = "Matrice")
     p2 <- plot_matrix(Moy, titre = "Matrice", r = input$rayon) + coord_cartesian(xlim = c(0, dim(MStudent)[1]), ylim = c(0, dim(MStudent)[2]))
     cowplot::plot_grid(p1,p2, ncol=1, nrow=2)}
 })

 output$superposition_densite <- renderPlot({
    d <- input$distrib
    
 })
 
 

}

shinyApp(ui, server)
