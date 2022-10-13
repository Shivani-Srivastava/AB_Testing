#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
    #tags$head(includeScript("google_analytics.js")),
    title = "A / B Testing",

    # Application title
    titlePanel(title=div(img(src="logo.png",align='right'),"A/B Testing")),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # File upload for dataset
            fileInput("file", "Upload input data (CSV file with header)"),
            # DropDown Input for Variant Variable
            htmlOutput("varselect_variant"),
            # DropDown Input for Outcome Variable
            htmlOutput("varselect_outcome"),
            htmlOutput("varselect_outcome_positive"),
            # Checkbox to see if variant is non-metric or numerical
            checkboxInput("nonmetric_checkbox", "Check if outcome variable is non-metric"),
            # Based on Variant Column levels, select Baseline
            
            # Select variable to group visual plots by
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Overview",
                                 p('This page contains information
                                   about how the application functions.'),
                                 verbatimTextOutput('checker')),
                        tabPanel("Data",
                          p('This tab contains a header view of the dataset with
                            statistical descriptors summary.',
                            DT::dataTableOutput('dataOverview')),
                        ),
                        tabPanel("Outputs",
                                 p('This page
                                   contains the results.'),
                                 verbatimTextOutput('dataframe')
                                 ),
                        
            )
        )
    )
)
)
