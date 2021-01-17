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
            tags$hr(),
            checkboxInput("header", "Header", TRUE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Normality Test", plotOutput("densityplot"), verbatimTextOutput("shapirotest")),
                tabPanel("SPC for OOT", plotlyOutput("rulesplot")),
                tabPanel("Data input", tableOutput("contents"))
            )
        )
    )
)
