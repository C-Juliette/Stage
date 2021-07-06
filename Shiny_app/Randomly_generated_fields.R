source('Distributions.R')

selectInput_distributions <- function(inputId){
return (selectInput(inputId = inputId,
            label = "Choose a random distribution",
            choices = distribution_choices,
            selected = "Normal distribution"))
}

conditionalPanel_normal_distribution <- function(condition, meanId, varianceId, setxId){
return(conditionalPanel(
  condition = condition,
  numericInput(inputId = meanId,
               label = "Choose the mean:",
               min = -20, max = 20, value = 0, step = 0.5),
  numericInput(inputId = varianceId,
               label = "Choose the variance:",
               min = 1, max = 20, value = 1, step = 0.5),
  if(setxId != ""){
  sliderInput(inputId = setxId,
              label = "Set x axis",
              min = -30, max = 30, value = c(-10, 10))}
    )
  )
  
}

conditionalPanel_poisson_distribution <- function(condition, lambdaId, setxId){
  return(conditionalPanel(
  condition = condition,
  numericInput(inputId = lambdaId,
               label = "Choose lambda:",
               min = 0, max = 20, value = 1, step = 0.5),
  if(setxId != ""){
  sliderInput(inputId = setxId,
              label = "Set x axis",
              value = c(0, 10),
              min = -10,
              max = 50)}
    ))
}


conditionalPanel_bernoulli_distribution <- function(condition, pId, setxId){
  return(conditionalPanel(
    condition = condition,
    numericInput(inputId = pId,
                 label = "Choose the probability of success p:",
                 value = 0.5,
                 min = 0,
                 max = 1,
                 step = 0.05),
    if(setxId != ""){
    sliderInput(inputId = setxId,
                label = "Set x axis",
                value = c(0,1),
                min = 0,
                max = 1,
    )})
    )
  
}

conditionalPanel_student_distribution <- function(condition, kId, setxId){
  return(conditionalPanel(
    condition = condition,
    numericInput(inputId = kId,
                 label = "Choose the degree of freedom k:",
                 min = 0, max = 20, value = 4,
                 step = 0.5),
    if(setxId != ""){
    sliderInput(inputId = setxId,
                label = "Choose the xmax value",
                value = c(-10
                          , 10),
                min = -30,
                max = 30)}
    )
    )
  
}




