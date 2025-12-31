# Packages + Setup
library(shiny)
library(tidyverse)

g9gf_2024 <- read_csv("G9GF_2024.csv")

g9gf_2024 <- g9gf_2024 %>%
  mutate(POC = case_when(
    Race == "white" ~ "White",
    Race != "white" ~ "POC"
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
         `PostFall_Skills_1`,
         `PostFall_Skills_2`,
         `PostFall_Skills_3`,
         `PostFall_Skills_4`,
         `PostFall_Skills_5`,
         `Current_Skills_1`,
         `Current_Skills_2`,
         `Current_Skills_3`,
         `Current_Skills_4`,
         `Current_Skills_5`,
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
         Stress_Spring,
         `PreFall_Avg`,
         `PostFall_Avg`,
         `Current_Avg`,
         `FallDiff_Skills_1`,
         `FallDiff_Skills_2`,
         `FallDiff_Skills_3`,
         `FallDiff_Skills_4`,
         `FallDiff_Skills_5`,
         `FallDiff_Avg`)

# Create server
server <- function(input, output) {
    output$plot <- renderPlot({
      req(input$var, input$group1, input$display)
      if (input$display == "Count") {
        if (input$group2 == "NA") {
          if (input$group1 == "NA") {
            if (input$var %in% colnames(bar_values)) {
              ggplot(data = g9gf_2024, aes_string(input$var)) +
                geom_bar(stat = "count") +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else if (input$var %in% colnames(histogram_values)) {
              ggplot(data = g9gf_2024, aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } 
          }
          else if (input$group1 == "Gender") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes_string(input$var)) +
                geom_bar(stat = "count") +
                facet_wrap(. ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(. ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            }
          }
          else {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar(stat = "count") +
                facet_wrap(. ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else{
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(. ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } 
          }
        }
        else if (input$group2 == "Gender") {
          if (input$group1 == "NA" | input$group1 == "Gender") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes_string(input$var)) +
                geom_bar(stat = "count") +
                facet_wrap(eval(as.name(input$group2)) ~ .) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                geom_bar(stat = "count") +
                facet_wrap(eval(as.name(input$group2)) ~ .) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } 
          }
          else {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]]), Gender == "boy" | Gender == "girl") %>%
                ggplot(aes_string(input$var)) +
                geom_bar(stat = "count") +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]]), Gender == "boy" | Gender == "girl") %>%
                ggplot(aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } 
          }
        }
        else {
          if (input$group1 == "NA") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group2]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar(stat = "count") +
                facet_wrap(eval(as.name(input$group2)) ~ .) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group2]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ .) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } 
          }
          else if (input$group1 == "Gender") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group2]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar(stat = "count") +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group2]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } 
          }
          else {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]]), !is.na(.data[[input$group2]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar(stat = "count") +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } else {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]]), !is.na(.data[[input$group2]])) %>%
                ggplot(aes_string(input$var)) +
                geom_bar() +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1))) +
                geom_text(stat = "count", aes(label = after_stat(count)), color = "white", size = 10, vjust = 1.5)
            } 
          }  
        }
      }
      else if (input$display == "Proportion") {
        if (input$group2 == "NA") {
          if (input$group1 == "NA") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                ggplot(aes(.data[[input$var]], y = prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
                geom_bar(stat = "count") +
                geom_text(stat = "count", color = "white", size = 8, vjust = 1.5) +
                scale_y_continuous(labels = scales::percent)
            } else {
              g9gf_2024 %>%
                ggplot(aes(.data[[input$var]], y = prop.table(stat(count)), label = scales::percent(prop.table(stat(count))))) +
                geom_bar(stat = "count") +
                geom_text(stat = "count", color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1))
            } 
          }
          else if (input$group1 == "Gender") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl") %>%
                            group_by(Gender, .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 8, vjust = 1.5) +
                facet_wrap(. ~ eval(as.name(input$group1)))
            } else{
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl") %>%
                            group_by(Gender, .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(. ~ eval(as.name(input$group1)))
            } 
          }
          else {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]])) %>%
                ggplot(aes(.data[[input$var]], y = ..prop.., group = 1, label = scales::percent(..prop..))) +
                geom_bar(stat = "count") +
                geom_text(stat = "count", color = "white", size = 8, vjust = 1.5) +
                facet_wrap(. ~ eval(as.name(input$group1)))
            } else {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group1]])) %>%
                ggplot(aes(.data[[input$var]], y = ..prop.., group = 1, label = scales::percent(..prop..))) +
                geom_bar(stat = "count") +
                geom_text(stat = "count", color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(. ~ eval(as.name(input$group1)))
            } 
          }
        }
        else if (input$group2 == "Gender") {
          if (input$group1 == "NA" | input$group1 == "Gender") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl") %>%
                            group_by(Gender, .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 8, vjust = 1.5) +
                facet_wrap(eval(as.name(input$group2)) ~ .)
            } else {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl") %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl") %>%
                            group_by(Gender, .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ .)
            } 
          }
          else {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group1]])) %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group1]])) %>%
                            group_by(Gender, .data[[input$group1]], .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 8, vjust = 1.5) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1)))
            } else {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group1]])) %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group1]])) %>%
                            group_by(Gender, .data[[input$group1]], .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1)))
            } 
          }
        }
        else {
          if (input$group1 == "NA") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group2]])) %>%
                ggplot(aes(.data[[input$var]], y = ..prop.., group = 1, label = scales::percent(..prop..))) +
                geom_bar(stat = "count") +
                geom_text(stat = "count", color = "white", size = 8, vjust = 1.5) +
                facet_wrap(eval(as.name(input$group2)) ~ .)
            } else {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group2]])) %>%
                ggplot(aes(.data[[input$var]], y = ..prop.., group = 1, label = scales::percent(..prop..))) +
                geom_bar(stat = "count") +
                geom_text(stat = "count", color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ .)
            } 
          }
          else if (input$group1 == "Gender") {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group2]])) %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group2]])) %>%
                            group_by(Gender, .data[[input$group2]], .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 8, vjust = 1.5) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1)))
            } else {
              g9gf_2024 %>%
                filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group2]])) %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(Gender == "boy" | Gender == "girl", !is.na(.data[[input$group2]])) %>%
                            group_by(Gender, .data[[input$group2]], .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1)))
            }
          }
          else {
            if (input$var %in% colnames(bar_values)) {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group2]]), !is.na(.data[[input$group1]])) %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(!is.na(.data[[input$group2]]), !is.na(.data[[input$group1]])) %>%
                            group_by(.data[[input$group2]], .data[[input$group1]], .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 8, vjust = 1.5) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1)))
            } else {
              g9gf_2024 %>%
                filter(!is.na(.data[[input$group2]]), !is.na(.data[[input$group1]])) %>%
                ggplot(aes(.data[[input$var]])) +
                geom_bar(aes_string(y = "..prop..", group = 1), stat = "count") +
                geom_text(data = . %>%
                            filter(!is.na(.data[[input$group2]]), !is.na(.data[[input$group1]])) %>%
                            group_by(.data[[input$group2]], .data[[input$group1]], .data[[input$var]]) %>%
                            tally() %>%
                            mutate(pct = n/sum(n)),
                          aes(y = pct, label = scales::percent(pct)), color = "white", size = 3.5, vjust = 1.5) +
                scale_x_continuous(breaks = seq(0,10,1)) +
                facet_wrap(eval(as.name(input$group2)) ~ eval(as.name(input$group1)))
            } 
          }
        }
      }
    })
    
    output$table <- renderTable({
      req(input$var, input$group1, input$display)
      if (input$group1 == "NA" & input$group2 == "NA") {
        g9gf_2024 %>%
          select(.data[[input$var]]) %>%
          summarize(Q1 = quantile(.data[[input$var]], 0.25), mean = mean(.data[[input$var]]), Q3 = quantile(.data[[input$var]], 0.75), sd = sd(.data[[input$var]]), count = n()) %>%
          mutate(prop = count/sum(count))
      }
      else if (input$group1 != "NA" & input$group2 == "NA") {
        g9gf_2024 %>%
          select(.data[[input$var]], .data[[input$group1]]) %>%
          group_by(.data[[input$group1]]) %>%
          summarize(Q1 = quantile(.data[[input$var]], 0.25), mean = mean(.data[[input$var]]), Q3 = quantile(.data[[input$var]], 0.75), sd = sd(.data[[input$var]]), count = n()) %>%
          ungroup() %>%
          mutate(prop = count/sum(count))
      }
      else if (input$group1 == "NA" & input$group2 != "NA") {
        g9gf_2024 %>%
          select(.data[[input$var]], .data[[input$group2]]) %>%
          group_by(.data[[input$group2]]) %>%
          summarize(Q1 = quantile(.data[[input$var]], 0.25), mean = mean(.data[[input$var]]), Q3 = quantile(.data[[input$var]], 0.75), sd = sd(.data[[input$var]]), count = n()) %>%
          ungroup() %>%
          mutate(prop = count/sum(count))
      }
      else {
        g9gf_2024 %>%
          select(.data[[input$var]], .data[[input$group1]], .data[[input$group2]]) %>%
          group_by(.data[[input$group1]], .data[[input$group2]]) %>%
          summarize(Q1 = quantile(.data[[input$var]], 0.25), mean = mean(.data[[input$var]]), Q3 = quantile(.data[[input$var]], 0.75), sd = sd(.data[[input$var]]), count = n()) %>%
          filter(count > 2) %>%
          ungroup() %>%
          mutate(prop = count/sum(count))
      }
    })
    output$text <- renderUI({
      if (input$var == "PreFall_Skills_1") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 8th grade</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>reaching out to teachers <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PreFall_Skills_2") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 8th grade</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>managing your time <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PreFall_Skills_3") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 8th grade</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>collaborating with peers <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PreFall_Skills_4") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 8th grade</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>taking notes <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PreFall_Skills_5") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 8th grade</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>organizing your school materials <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PostFall_Skills_1") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 9th grade fall</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>reaching out to teachers <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PostFall_Skills_2") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 9th grade fall</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>managing your time <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PostFall_Skills_3") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 9th grade fall</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>collaborating with peers <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PostFall_Skills_4") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 9th grade fall</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>taking notes <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PostFall_Skills_5") {
        HTML(paste("<b>Prompt: </b>Think back to you as a student <b><u>at the end of 9th grade fall</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>organizing your school materials <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "Current_Skills_1") {
        HTML(paste("<b>Prompt: </b>Think about you as a student <b><u>at the current moment</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>reaching out to teachers <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "Current_Skills_2") {
        HTML(paste("<b>Prompt: </b>Think about you as a student <b><u>at the current moment</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>managing your time <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "Current_Skills_3") {
        HTML(paste("<b>Prompt: </b>Think about you as a student <b><u>at the current moment</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>collaborating with peers <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "Current_Skills_4") {
        HTML(paste("<b>Prompt: </b>Think about you as a student <b><u>at the current moment</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>taking notes <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "Current_Skills_5") {
        HTML(paste("<b>Prompt: </b>Think about you as a student <b><u>at the current moment</b></u>. Self-assess your proficiency in each of the skills below at that point in time. <br> 
                   <b>Skill: </b>organizing your school materials <br> 
                   <b>Scale: </b>1 - Not at all skilled, 2 - Slightly skilled, 3 - Moderately skilled, 4 - Very skilled, 5 - Extremely skilled"))
      }
      else if (input$var == "PreFall_Avg") {
        "This variable was created by finding an average: summing all the self-assessment scores for the 5 skills at the end of 8th grade and dividing by 5."
      }
      else if (input$var == "PostFall_Avg") {
        "This variable was created by finding an average: summing all the self-assessment scores for the 5 skills at the end of 9th grade fall and dividing by 5."
      }
      else if (input$var == "Current_Avg") {
        "This variable was created by finding an average: summing all the self-assessment scores for the 5 skills at the current moment and dividing by 5."
      }
      else if (input$var == "FallDiff_Avg") {
        "This variable was created by finding an average: summing all the self-assessment scores for the 5 differences between post- and pre-fall skills and dividing by 5."
      }
      else if (input$var == "FallDiff_Skills_1") {
        "This variable (reaching out to teachers) was created by subtracting (PostFall-PreFall) to find the difference between self-assessed skill level before and after 9th grade fall semester."
      }
      else if (input$var == "FallDiff_Skills_2") {
        "This variable (managing your time) was created by subtracting (PostFall-PreFall) to find the difference between self-assessed skill level before and after 9th grade fall semester."
      }
      else if (input$var == "FallDiff_Skills_3") {
        "This variable (collaborating with peers) was created by subtracting (PostFall-PreFall) to find the difference between self-assessed skill level before and after 9th grade fall semester."
      }
      else if (input$var == "FallDiff_Skills_4") {
        "This variable (taking notes) was created by subtracting (PostFall-PreFall) to find the difference between self-assessed skill level before and after 9th grade fall semester."
      }
      else if (input$var == "FallDiff_Skills_5") {
        "This variable (organizing your materials) was created by subtracting (PostFall-PreFall) to find the difference between self-assessed skill level before and after 9th grade fall semester."
      }
      else if (input$var == "Rubrics") {
        HTML(paste("<b>Prompt: </b>By the end of 9th grade fall, I understood how <b><u>rubrics</b></u> were used in my classes to assess my performance. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Grades") {
        HTML(paste("<b>Prompt: </b>By the end of 9th grade fall, I understood how my <b><u>letter grades</b></u> would be calculated in my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Expectations") {
        HTML(paste("<b>Prompt: </b>By the end of 9th grade fall, I understood how my <b><u>teachers' expectations for me</b></u> in my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Feedback_Strengths") {
        HTML(paste("<b>Prompt: </b>Throughout the fall semester, the feedback that I received helped me to understand <b><u>what I was doing well</b></u> in each of my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Feedback_Improve") {
        HTML(paste("<b>Prompt: </b>Throughout the fall semester, the feedback that I received helped me to understand <b><u>my areas of improvement</b></u> in each of my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Grades_SoFar") {
        HTML(paste("<b>Prompt: </b>Based on your experience in the fall semester, the grades that I am receiving so far in this spring semester are... <br>
                   <b>Scale: </b>1 - Much lower than I expected, 2 - A little lower than I expected, 3 - About what I expected, 4 - A little higher than I expected, 5 - Much higher than I expected"))
      }
      else if (input$var == "Confidence") {
        HTML(paste("<b>Prompt: </b>By the end of 9th grade fall, I felt <b><u>more confident</b></u> in my ability to be academically successful in my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Habits") {
        HTML(paste("<b>Prompt: </b>By the end of 9th grade fall, I <b><u>developed or maintained habits</b></u> that have helped me to be academically successful in my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Relationships") {
        HTML(paste("<b>Prompt: </b>By the end of 9th grade fall, I built <b><u>relationships with teachers</b></u> that have helped me to be academically successful in my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Motivation_Fall") {
        HTML(paste("<b>Prompt: </b>Throughout the fall semester, I was motivated to do my best in my classes (on average). <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Motivation_Spring") {
        HTML(paste("<b>Prompt: </b>So far this spring semester, I am motivated to do my best in my classes (on average). <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Homework_Fall") {
        HTML(paste("<b>Prompt: </b>Throughout the fall semester, I consistently completed my homework for my classes (on average). <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Homework_Spring") {
        HTML(paste("<b>Prompt: </b>So far this spring semester, I am consistently completing my homework for my classes. <br>
                   <b>Scale: </b>1 - Strongly disagree, 2 - Disagree, 3 - Neither agree nor disagree, 4 - Agree, 5 - Strongly agree"))
      }
      else if (input$var == "Stress_Fall") {
        HTML(paste("<b>Prompt: </b>How stressful did you find the fall semester, on average? <br>
                   <b>Scale: </b>See handout."))
      }
      else if (input$var == "Stress_Spring") {
        HTML(paste("<b>Prompt: </b>How stressful did you find the fall semester, on average? <br>
                   <b>Scale: </b>See handout."))
      }
    })

}