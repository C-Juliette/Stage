###### Variables


distribution_choices <- c("Normal distribution",
                          "Poisson distribution",
                          "Bernoulli distribution",
                          "Student distribution")

checkboxGroupInput_distributions <- function(inputId){
return (checkboxGroupInput(inputId = inputId, 
                   label = "Choose your distribution(s)", 
                   choices = distribution_choices,
                   selected = "Normal distribution"))
}