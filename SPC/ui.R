source("libs.R")
source("functions.R")

ui <- fluidPage(
    titlePanel("Statistical Process Control"),
    hr(style="border-color: grey;"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            
            p("CSV File must comply with this specifications:"),
            tags$ol(
                tags$li("CSV must have sep = ;"), 
                tags$li("First column is the batch (numeric incremental value)"), 
                tags$li("Second column is the value (numerical value)")
            ),
            
            p("Check the box if the file have a Header:"),
            
            checkboxInput("header", "Header", TRUE),
            p(""),
            p("Created by"),
            a("Vasco Pereira", href = "https://www.linkedin.com/in/vascoribeirosintra/"),
            p(""),
            p("Did you like this?"),
            a("Buy me a coffee!", href = "https://paypal.me/theBiochemist?locale.x=en_US")
            
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
                # tabPanel("Process capability",
                #          numericInput("USL","Upper spec limit", value = NA),
                #          numericInput("LSL","Lower spec limit", value = NA),
                #          submitButton(text = "Apply Changes", icon = NULL, width = NULL)),
                tabPanel("Data input", tableOutput("contents"))
            )
        )
    )
)
