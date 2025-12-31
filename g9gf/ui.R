# Packages + Setup
library(shiny)
library(tidyverse)

g9gf_2024 <- read_csv("G9GF_2024.csv")

g9gf_2024 <- g9gf_2024 %>%
  mutate(POC = case_when(
    Race == 1 ~ "White",
    Race > 1 ~ "POC"
  )) %>%
  mutate(`PreFall_Avg` = (`PreFall_Skills_1`+`PreFall_Skills_2`+`PreFall_Skills_3`+`PreFall_Skills_4`+`PreFall_Skills_5`)/5,
         `PostFall_Avg` = (`PostFall_Skills_1`+`PostFall_Skills_2`+`PostFall_Skills_3`+`PostFall_Skills_4`+`PostFall_Skills_5`)/5,
         `Current_Avg` = (`Current_Skills_1`+`Current_Skills_2`+`Current_Skills_3`+`Current_Skills_4`+`Current_Skills_5`)/5,
         `FallDiff_Skills_1` = `PostFall_Skills_1` - `PreFall_Skills_1`,
         `FallDiff_Skills_2` = `PostFall_Skills_2` - `PreFall_Skills_2`,
         `FallDiff_Skills_3` = `PostFall_Skills_3` - `PreFall_Skills_3`,
         `FallDiff_Skills_4` = `PostFall_Skills_4` - `PreFall_Skills_4`,
         `FallDiff_Skills_5` = `PostFall_Skills_5` - `PreFall_Skills_5`,
         `FallDiff_Avg` = (`FallDiff_Skills_1`+`FallDiff_Skills_2`+`FallDiff_Skills_3`+`FallDiff_Skills_4`+`FallDiff_Skills_5`)/5)

bar_values <- g9gf_2024 %>%
  select(`PreFall_Skills_1`,
         `PreFall_Skills_2`,
         `PreFall_Skills_3`,
         `PreFall_Skills_4`,
         `PreFall_Skills_5`,
         `PreFall_Avg`,
         `PostFall_Skills_1`,
         `PostFall_Skills_2`,
         `PostFall_Skills_3`,
         `PostFall_Skills_4`,
         `PostFall_Skills_5`,
         `PostFall_Avg`,
         `FallDiff_Skills_1`,
         `FallDiff_Skills_2`,
         `FallDiff_Skills_3`,
         `FallDiff_Skills_4`,
         `FallDiff_Skills_5`,
         `FallDiff_Avg`,
         `Current_Skills_1`,
         `Current_Skills_2`,
         `Current_Skills_3`,
         `Current_Skills_4`,
         `Current_Skills_5`,
         `Current_Avg`,
         Rubrics,
         Grades,
         Expectations,
         Grades_SoFar,
         Confidence,
         Habits,
         Relationships,
         Feedback_Strengths,
         Feedback_Improve,
         Motivation_Fall,
         Motivation_Spring,
         Homework_Fall,
         Homework_Spring)

histogram_values <- g9gf_2024 %>%
  select(Stress_Fall,
         Stress_Spring)

# Panels
## 1: Visualization
sidebar_content <- sidebarPanel(
  selectInput(
    "var",
    label = "Variable",
    choices = c(colnames(cbind(bar_values, histogram_values))),
    selected = "Pre-Fall_Skills_1"
    
  ),
  selectInput(
    "group1",
    label = "Group By #1",
    choices = c("NA",
                "School_Type",
                "Gender",
                "POC",
                "TA")
  ),
  selectInput(
    "group2",
    label = "Group By #2",
    choices = c("NA",
                "School_Type",
                "Gender",
                "POC",
                "TA")
  ),
  selectInput(
    "display",
    label = "Display",
    choices = c("Count",
                "Proportion"),
    selected = "Proportion"
  )
)

main_content <- mainPanel(
  htmlOutput("text"),  
  plotOutput("plot"), 
  tableOutput("table"),
  HTML("<b>Q1:</b> the 25th percentile <br>
       <b>mean:</b> the average score of all the responses <br>
       <b>Q3:</b> the 75th percentile <br>
       <b>sd:</b> the standard deviation – the higher the number, the more varied the responses <br>
       <b>count:</b> the number of responses represented in that row <br>
       <b>prop:</b> the percentage/proportion of responses represented in that row")
)

second_panel <- tabPanel(
  "Visualization",
  titlePanel("Gradeless 9th Grade Fall - 2024"),
  p("Use the selector input below to choose which variable you would like to see."),
  sidebarLayout(
    sidebar_content, main_content
  )
)

# UI
ui <- navbarPage(
  "Gradeless Fall 2024, by Chris Meng",
  second_panel
)
