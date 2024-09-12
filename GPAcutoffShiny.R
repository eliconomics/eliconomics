rm(list = ls())
library(ggplot2)
library(tidyverse)
{
genGrades <- function(nClass) {
  sum(
    sample(
      seq(0,4,by=0.5), prob = sqrt(1:9), nClass, replace = T
    )
  )/nClass
}
gradeR <- function(g1, h1, h2, score) {
  (g1*h1 + score*h2)/(h1+h2)
}
newGrades <- function(g1, h1, g2, h2) {
  gradeR(g1,h1,h2,g2)
}
maxGrade <- function(g1, h1, h2) {
  gradeR(g1,h1,h2,4)
}
minGrade <- function(g1, h1, h2) {
  gradeR(g1,h1,h2,0)
}
bGrade <- function(g1, h1, h2) {
  gradeR(g1,h1,h2,3)
}
}
cGrade <- function(g1, h1, h2) {
  gradeR(g1,h1,h2,2)
}

students <- data.frame(GPA = genGrades(5),
                       maxGPA = 4,
                       minGPA = 0,
                       t = 1)

ggplot(data = students) +
  ggtitle("Danger of Losing a Scholarship with a 2.0 GPA Cutoff") +
  geom_ribbon(mapping = aes(x = t, ymax = maxGPA, ymin = minGPA, fill = !(minGPA > 2))) +
  geom_line(mapping = aes(x = t, y = GPA)) +
  scale_x_continuous(breaks = 1:17) +
  geom_abline(slope = 0, intercept = 2.0) +
  guides(fill = guide_legend(title = "Danger?")) +
  theme_classic()


ui <- page_sidebar(
  title = "Hello Shiny!",
  sidebar = sidebar(
    sliderInput(
      inputId = "ubound",
      label = "Maximum score:",
      min = 0,
      max = 4,
      value = 0,
      step = 0.5
    ),
    sliderInput(
      inputId = "mbound",
      label = "Minimum score:",
      min = 0,
      max = 4,
      value = 0,
      step = 0.5
    ),
    sliderInput(
      inputId = "seed",
      label = "set seed:",
      min = 0,
      max = 100,
      value = 0
    )
),
plotOutput(outputId = "distPlot")
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    set.seed(input$seed)
    nxtPeriod <- function(studf) {
      studf <- studf[length(studf[,1]),]
      t <- studf$t
      GPA <- newGrades(studf$GPA,t*5,genGrades(5),5)
      return(data.frame(GPA = GPA,
                        maxGPA = gradeR(GPA,t*5,5,input$ubound),
                        minGPA = gradeR(GPA,t*5,5,input$mbound),
                        t = t+1))
    }
    for (i in 1:15) {
      students <- rbind(students,nxtPeriod(students))
    }
    ggplot(data = students) +
      ggtitle("Danger of Losing a Scholarship with a 2.0 GPA Cutoff") +
      geom_ribbon(mapping = aes(x = t, ymax = maxGPA, ymin = minGPA, fill = !(minGPA > 2))) +
      geom_line(mapping = aes(x = t, y = GPA)) +
      scale_x_continuous(breaks = 1:17) +
      geom_abline(slope = 0, intercept = 2.0) +
      guides(fill = guide_legend(title = "Danger?")) +
      theme_classic()
  })
}


shinyApp(ui = ui, server = server)