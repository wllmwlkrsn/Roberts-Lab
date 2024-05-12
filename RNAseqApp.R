library(shiny)
library(survival)
library(ggplot2)
library(survminer)
library(tidyverse)
library(ggfortify)

 ui <- fluidPage(
   titlePanel("Transcriptional Status and Acute Myeloid Leukemia Survival Inspired by OncoLnc"),
   
   sidebarLayout(
     sidebarPanel(
       textInput(inputId =  "Hugo_Symbol",
                 label = "Search by Gene of Interest", 
                 value = "", 
                 placeholder = "Hugo Symbol of Gene"),
       
       numericInput(inputId = "Upper_Percentile", 
                    value = 50,
                    min = 50,
                    max = 99,
                    label = "Upper Percentile"),
       
       numericInput(inputId = "Lower_Percentile", 
                    value = 50,
                    min = 1,
                    max = 50,
                    label = "Lower Percentile")
     ),
     
     mainPanel(
       plotOutput(outputId = "kmplot", width = "100%")
     )
   ),
   
   #DT::DTOutput("subset"),
   tabPanel(
     "2 columns",
     fluidRow(
       column(width = 6,
              h2("Upper Quantile"),
              DT::DTOutput("table_upper")),
       column(width = 6,
              h2("Lower Quantile"),
              DT::DTOutput("table_lower"))
     ))
   
   #tableOutput("km")
 ) 
 
 server <- function(input, output) {
   subset <- reactive({
     req(input$Hugo_Symbol)
     Kaplan_Meier_Dataset[ ,c("PATIENT_ID", "OS_MONTHS", "OS_STATUS", "NRAS")][order(Kaplan_Meier_Dataset[[input$Hugo_Symbol]], decreasing = FALSE),]
   })
   
  lower <- reactive({
    subset()[1:(nrow(subset())*input$Lower_Percentile/100),]
   })
   
   upper <- reactive({
     subset()[(nrow(subset())*input$Upper_Percentile/100):nrow(subset()),]
   })
   
   subset_km <- reactive({
     dplyr::mutate(subset(), 
                   Strata = case_when(PATIENT_ID %in% upper()[,1] ~ "Upper Quantile", 
                                      PATIENT_ID %in% lower()[,1] ~ "Lower Quantile",
                                      .default = "Out of range")) %>%
       filter(Strata != "Out of range")
   })
   
   output$kmplot <- renderPlot({
     fit <- survfit(Surv(OS_MONTHS, OS_STATUS) ~ Strata, data = subset_km())
     p <- ggsurvplot(fit, data = subset_km(), 
                     xlab = "Time in Months",
                     pval = TRUE, 
                     title = paste("Kaplan-Meier of AML Patient Survival Based on", input$Hugo_Symbol, "Expression" ))
     p
   })
   #output$subset <- DT::renderDT(subset())
   output$table_lower <- DT::renderDT(lower()[,1:3])
   output$table_upper <- DT::renderDT(upper()[,1:3])
   #output$km <- renderTable(subset_km())
}
   
 shinyApp(ui = ui, server = server)

