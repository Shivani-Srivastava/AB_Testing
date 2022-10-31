########################################################################
#               A/B Testing - Digital Marketing                        #
#               Back-end by Prof. Sudhir Voleti                        #
#                 Shiny by Shivani Srivastava                          #
########################################################################


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
            # Based on Variant Column levels, select Baseline
            htmlOutput("varselect_baseline"),
            
            # DropDown Input for Outcome Variable
            htmlOutput("varselect_outcome"),
            htmlOutput("varselect_outcome_positive")
            # Checkbox to see if variant is non-metric or numerical
            #checkboxInput("nonmetric_checkbox", "Check if outcome variable is non-metric")

            
            # Select variable to group visual plots by
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Overview",
                                 h3('About the Application'),
                                 p('A/B testing (also known as split testing or bucket testing) is a method of comparing two (or more) 
                                 versions of a webpage / app / product against each other to determine which one performs better. 
                                 A/B testing is essentially an experiment where two or more variants are shown
                                 to users at random, and statistical analysis is used to determine which variation performs better
                                 for a given conversion goal.
                                   '), br(),
                                 p('This application shows the following metrics:'),br(),
                                   p('Conversion rate '),
                                  p('Estimated Difference'),
                                  p('Relative Uplift(%)'),
                                  p('Pooled Sample Proportion'),
                                   p('Standard Error of Difference'),
                                   p('Z - score'),
                                   p('p-value'),
                                   p('Margin of Error'),
                                   p('CI-lower CI-upper'),
                                 h4(p("Download Sample text file")),
                                 downloadButton('downloadData1', 'Download sample input file'),br(),br()
                                 
                                 ),
                        tabPanel("Data",
                          
                            DT::dataTableOutput('dataOverview')
                        ),
                        tabPanel("Outputs",
                                 plotOutput('ABTest_Plots'),
                                 DT::dataTableOutput('dataframe')
                                 ),
                        
            )
        )
    )
)
)
