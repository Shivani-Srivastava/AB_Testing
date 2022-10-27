########################################################################
#               A/B Testing - Digital Marketing                        #
#               Back-end by Prof. Sudhir Voleti                        #
#                 Shiny by Shivani Srivastava                          #
########################################################################

library(DT)
library(shiny)
require(tidyverse)


shinyServer(function(input, output) {

  mydata <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ",", stringsAsFactors = TRUE))
      rownames(Dataset) = Dataset[,1]
      Dataset1 = Dataset[,2:ncol(Dataset)]
      #Dataset = t(Dataset)
      return(Dataset1)
    }
  })  
    
  
  output$dataOverview <- DT::renderDataTable({ mydata() },options = list(pageLength=25))
  
  
  output$varselect_variant <- renderUI({
    if (identical(mydata(), '') || identical(mydata(),data.frame())) return(NULL)
    # Variable selection:
    selectInput('variant0',"Select variable defining A/B.", colnames(mydata()), colnames(mydata()))
  })
  
  variant0 = reactive({as.data.frame(mydata()[input$variant0])})

  output$varselect_baseline <- renderUI({
    if (identical(mydata(), '') || identical(mydata(),data.frame())) return(NULL)
    # Variable selection:
    selectInput('baseline0',"Select baseline or control variant.", unique(variant0()), unique(variant0()))
    
  })

  baseline = reactive({input$baseline0})
  
      
  output$varselect_outcome <- renderUI({
    if (identical(mydata(), '') || identical(mydata(),data.frame())) return(NULL)
    # Variable selection:
    selectInput('outcome0',"Select outcome variable.", colnames(mydata()), colnames(mydata()))
    
    })
  
  #This is a list of elements
  ioutcome0 = reactive({mydata()[input$outcome0]})
  
  
  output$varselect_outcome_positive <- renderUI({
    if (identical(mydata(), '') || identical(mydata(),data.frame())) return(NULL)
    # Variable selection:
    selectInput('outcome0_pos',"Select positive outcome level.", unique(ioutcome0()), unique(ioutcome0()))
    
  })
  
  
  ioutpositive = reactive({input$outcome0_pos})
  
  
  output$checking <- renderPrint({
    variants = unique(unlist(variant0()))
    p_pool = sum(ioutcome0() == rep(ioutpositive(), nrow(ioutcome0())))/nrow(ioutcome0())
    #print(head(variant0()))
    outcome0 = ioutcome0()
    a00 = NULL
    
    #for (a0 in variants){a1 = which(variants %in% a0); cat(a1,'\n')}
      #a01 = sum(variant0() == a0); a00 = c(a00,a01)} #rep(a0,length(unique(unlist(variant0()))))
    
    #a00 = c(a00, a01)} 
    for (a0 in variants){ # a0 = variants[1] 
      #a1 = which(variants %in% a0); a1
      #a0_1 = mydata |> filter(variant0 == a0 & outcome0 == "TRUE") |> nrow()
      a0_1 = nrow(filter(mydata(), a0 == variant0() & outcome0 == ioutpositive()))
      #a0_2 = mydata |> filter(variant0 %in% a0) |> nrow()
      a0_2 = nrow(filter(mydata(),a0 == variant0()))
      #conv_rate_a0 = a0_1/a0_2
      cat(a0_2,'\n',a0_1,'\n')
      #print(a0 %in% as.data.frame(variant0()))
      #print(filter(mydata(), a0 %in% variant0()))
      }
      
  })
  
  
    # Output defining function which should ideally be stored in a Global.R file
    
    discrete_outcome_an <- function(mydata, variant0, baseline0, outcome0, outcome0_pos){
      
      #variants = variant0 %>% as.factor(.) %>% levels(.); variants
      variants = unique(unlist(variant0))
      
      baseline0_1 = nrow(filter(mydata,variant0 == baseline0 & outcome0 == outcome0_pos))
      
      baseline0_2 = nrow(filter(mydata,variant0 == baseline0))
      
      conv_rate_baseline0 = baseline0_1/baseline0_2; conv_rate_baseline0
      
      # build empty DF to populate: outp_df
      metric = c('Conversion rate','Estimated Difference', 'Relative Uplift(%)', 'Pooled Sample Proportion',  'Standard Error of Difference', 'Z - score', 'p-value', 'Margin of Error',  'CI-lower', 'CI-upper')
      
      outcomes = unique(outcome0); outcomes
      outp_df = data.frame(matrix(0, nrow=length(metric), ncol=length(unique(unlist(variant0)))))
      rownames(outp_df) = metric
      colnames(outp_df) = variants
      
      
      # p_pool = sum(ioutcome0() == rep(ioutpositive(), length(ioutcome0())))/length(ioutcome0())
      
      p_pool = sum(outcome0 == rep(outcome0_pos, nrow(outcome0)))/nrow(outcome0) # "TRUE"
      outp_df[4, 1:ncol(outp_df)] = round(p_pool, 4)
      
      a00 = NULL
      for (a0 in variants){
        a01 = sum(variant0 == a0)
        a00 = c(a00, a01)} #ERROR : Comparison of only atomic data types
      
      se_denom = sum(1/a00) # needed for next step
      SE_pool = sqrt(p_pool * (1-p_pool) * se_denom) # Depends on p_pool
      #outp_df[4,] = SE_pool
      
      MOE_pool = SE_pool * qnorm(0.975) # Depends on p_pool
      outp_df[8, 1:ncol(outp_df)] <- round(MOE_pool, 4)
      
      # populate the DF now for each variant as colm
      for (a0 in variants){ # a0 = variants[1] 
        a1 = which(variants %in% a0); a1
        #a0_1 = mydata |> filter(variant0 == a0 & outcome0 == "TRUE") |> nrow()
        a0_1 = nrow(filter(mydata, a0 == variant0 & outcome0 == outcome0_pos))
        #a0_2 = mydata |> filter(variant0 %in% a0) |> nrow()
        a0_2 = nrow(filter(mydata,a0 == variant0))
        conv_rate_a0 = a0_1/a0_2
        
        outp_df[1, a1] = round(conv_rate_a0, 4)
        
        est_diff_a0 <- (conv_rate_a0 - conv_rate_baseline0)
        
        outp_df[2,a1] <- round(est_diff_a0, 4)
        
        uplift_a0 = (conv_rate_a0 - conv_rate_baseline0)*100/conv_rate_baseline0
        outp_df[3,a1] <- round(uplift_a0, 4)
        
        zscore_a0 = est_diff_a0/SE_pool # Issue -- depends on p_pool
        outp_df[6,a1] <- round(zscore_a0, 4)
        
        pval_a0 = pnorm(q = -abs(zscore_a0), mean=0, sd=1)*2 # Issue -- depends on p_pool
        outp_df[7,a1] <- round(pval_a0, 4)
        
        se_a0 = sqrt(conv_rate_a0 * (1 - conv_rate_a0)/a0_2)
        outp_df[5, a1] <- round(se_a0, 4)
        
        ci_low_a0 = conv_rate_a0 - qnorm(0.975) * se_a0
        ci_hi_a0 = conv_rate_a0 + qnorm(0.975) * se_a0
        outp_df[9,a1] <- round(ci_low_a0, 4)
        outp_df[10,a1] <- round(ci_hi_a0, 4)
      } # for loop ends
      
      outp_df } # func ends
    
    
    
    
    output$dataframe <- DT::renderDataTable({
      req(input$file)
      
      outputdf = reactive({
        discrete_outcome_an(mydata(), variant0(),
                                              baseline0 = baseline(),
                                              outcome0 = ioutcome0(),
                                              outcome0_pos = ioutpositive())
        
        
        })
      
      as.data.frame(outputdf())
      
      })
    
    
    output$downloadData1 <- downloadHandler(
      filename = function() {"abc_testdata.csv"},
      content = function(file) {
        write.csv("data/abCtestdta.csv", file)
      }
    )
    
})
