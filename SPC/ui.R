#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            #tags$hr(),
            
            p("Check the box if the file have a Header:"),
            
            checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Normality Test", 
                         p(""),
                         p("A Shapiro-Wilk statistic is performed to test the normality of data. If p-value > 0.5 Data is Normal Distributed"),
                         plotOutput("densityplot"), 
                         verbatimTextOutput("shapirotest")),
                
                tabPanel("SPC for OOT", 
                         p(""),
                         p("A set of 8 rules are applyed to the data in order to find Out Of Trend (OOT) results:"),
                         tags$ol(
                             tags$li("Beyond Limits: any point +/- 3sigma"), 
                             tags$li("Zone A: 2 out of 3 points above 2sigma"), 
                             tags$li("Zone B: 4 out of 5 points above sigma"),
                             tags$li("Zone C: 8 consecutive points on the same side of center"), 
                             tags$li("Trend: 6 points in a row trending up or down"), 
                             tags$li("Overcontrol: 14 consecutive points alternating up and down"),
                             tags$li("Stratification: 15 consecutive points within sigma"), 
                             tags$li("Mixture: 8 consecutive points out of +/- sigma  ")
                         ),
                         p(""),
                         p("The output is an interactive Shewhart control chart, where 0 represents data value in agreement with all rules"),
                         p(""),
                         plotlyOutput("rulesplot")),
                tabPanel("Data input", tableOutput("contents"))
            )
        )
    )
)
