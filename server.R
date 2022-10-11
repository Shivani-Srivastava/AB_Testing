#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DT)
library(shiny)
require(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  mydata <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
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
  
  output$varselect_outcome <- renderUI({
    if (identical(mydata(), '') || identical(mydata(),data.frame())) return(NULL)
    # Variable selection:
    selectInput('outcome0',"Select outcome variable.", colnames(mydata()), colnames(mydata()))
    
    })
  
  
  output$varselect_outcome_positive <- renderUI({
    if (identical(mydata(), '') || identical(mydata(),data.frame())) return(NULL)
    # Variable selection:
    selectInput('outcome0_pos',"Select positive outcome level.", levels(as.factor(input$outcome0)), levels(as.factor(input$outcome0)))
    
  })
  
  
  
    # Output defining function which should ideally be stored in a Global.R file
    
    discrete_outcome_an <- function(mydata, variant0, baseline0=NA, outcome0, outcome0_pos){
      
      variants = variant0 %>% as.factor(.) %>% levels(.); variants
      if (is.na(baseline0)){baseline0 = variants[1]}
      
      baseline0_1 = mydata %>% 
        filter(variant0 == baseline0 & outcome0 == outcome0_pos) %>% nrow()
      
      baseline0_2 = mydata %>% filter(variant0 == baseline0) %>% nrow()
      
      conv_rate_baseline0 = baseline0_1/baseline0_2; conv_rate_baseline0
      
      # build empty DF to populate: outp_df
      metric = c('Conv rate','Estimated Difference', 'Relative Uplift(%)', 'pooled sample proportion',  'Standard Error of Difference', 'z_score', 'p-value', 'Margin of Error',  'CI-lower', 'CI-upper')
      
      outcomes = levels(as.factor(outcome0)); outcomes
      outp_df = matrix(NA, nrow=length(metric), ncol=length(variants))
      rownames(outp_df) = metric
      colnames(outp_df) = variants
      
      p_pool = sum(outcome0 == outcome0_pos)/length(outcome0) # "TRUE"
      outp_df[4,] = p_pool %>% round(., 4)
      
      a00 = NULL
      for (a0 in variants){a01 = sum(variant0 == a0); a00 = c(a00, a01)}
      se_denom = sum(1/a00) # needed for next step
      SE_pool = sqrt(p_pool * (1-p_pool) * se_denom)
      # outp_df[4,] = SE_pool
      
      MOE_pool = SE_pool * qnorm(0.975)
      outp_df[8,] = MOE_pool %>% round(., 4)
      
      # populate the DF now for each variant as colm
      for (a0 in variants){ # a0 = variants[1]
        a1 = which(variants == a0); a1
        a0_1 = mydata %>% filter(variant0 == a0 & outcome0 == "TRUE") %>% nrow()
        a0_2 = mydata %>% filter(variant0 == a0) %>% nrow()
        conv_rate_a0 = a0_1/a0_2
        outp_df[1, a1] = conv_rate_a0 %>% round(., 4)
        
        est_diff_a0 = (conv_rate_a0 - conv_rate_baseline0)
        outp_df[2,a1] = est_diff_a0 %>% round(., 4)
        
        uplift_a0 = (conv_rate_a0 - conv_rate_baseline0)*100/conv_rate_baseline0
        outp_df[3,a1] = uplift_a0 %>% round(., 4)
        
        zscore_a0 = est_diff_a0/SE_pool
        outp_df[6,a1] = zscore_a0 %>% round(., 4)
        
        pval_a0 = pnorm(q = -abs(zscore_a0), mean=0, sd=1)*2
        outp_df[7,a1] = pval_a0 %>% round(., 4)
        
        se_a0 = sqrt(conv_rate_a0 * (1 - conv_rate_a0)/a0_2)
        outp_df[5, a1] = se_a0 %>% round(., 4)
        
        ci_low_a0 = conv_rate_a0 - qnorm(0.975) * se_a0
        ci_hi_a0 = conv_rate_a0 + qnorm(0.975) * se_a0
        outp_df[9,a1] = ci_low_a0 %>% round(., 4)
        outp_df[10,a1] = ci_hi_a0 %>% round(., 4)
      } # for loop ends
      
      outp_df } # func ends
    
    
    conti_outcome_an <- function(mydata, variant0, baseline0=NA, outcome0){
      
      variants = levels(as.factor(variant0)); variants
      if (is.na(baseline0)){baseline0 = levels(as.factor(variant0))[1]}; baseline0
      
      filt_ind0 = (variant0 == baseline0); filt_ind0[1:5]
      avg_rate_baseline0 = outcome0[filt_ind0] %>% mean(.); avg_rate_baseline0
      
      # build empty DF to populate: outp_df
      metric = c('Outcome_mean','Estimated Difference', 'Relative Uplift(%)', 'pooled sample mean',  'Standard Error of Difference', 'z_score', 'p-value', 'Margin of Error',  'CI-lower', 'CI-upper')
      
      # outcomes = levels(outcome0)
      outp_df = matrix(NA, nrow=length(metric), ncol=length(variants))
      rownames(outp_df) = metric
      colnames(outp_df) = variants
      
      mean_pool = mean(outcome0); mean_pool 
      outp_df[4,] = mean_pool %>% round(., 4)
      
      sd_pool = sd(outcome0); sd_pool
      MOE_pool = sd_pool * qnorm(0.975); MOE_pool
      outp_df[8,] = MOE_pool %>% round(., 4)
      
      # populate the DF now for each variant as colm
      for (a0 in variants){ 
        
        a1 = which(variants == a0); a1
        filt_ind0 = (variant0 == a0); filt_ind0[1:5]
        avg_rate_a0 = outcome0[filt_ind0] %>% mean(.); avg_rate_a0
        outp_df[1, a1] = avg_rate_a0 %>% round(., 4)
        
        se_a0 = outcome0[filt_ind0] %>% sd(.)
        outp_df[5, a1] = se_a0 %>% round(., 4)
        
        est_diff_a0 = (avg_rate_a0 - avg_rate_baseline0)
        outp_df[2,a1] = est_diff_a0 %>% round(., 4)
        
        uplift_a0 = (avg_rate_a0 - avg_rate_baseline0)*100/avg_rate_baseline0
        outp_df[3,a1] = uplift_a0 %>% round(., 4)
        
        zscore_a0 = est_diff_a0/sd_pool
        outp_df[6,a1] = zscore_a0 %>% round(., 4)
        
        pval_a0 = pnorm(q = -abs(zscore_a0), mean=0, sd=1)*2
        outp_df[7,a1] = pval_a0 %>% round(., 4)
        
        ci_low_a0 = avg_rate_a0 - qnorm(0.975) * se_a0
        ci_hi_a0 = avg_rate_a0 + qnorm(0.975) * se_a0
        
        outp_df[9,a1] = ci_low_a0 %>% round(., 4)
        outp_df[10,a1] = ci_hi_a0 %>% round(., 4)
      } # for loop ends
      
      outp_df } # func ends
      
    
    #outp_df2 = conti_outcome_an(abctest0, variant0=abctest0$variant, 
     #                           baseline0=NA, outcome0=abctest0$length_of_stay)
   
    #outp_df = discrete_outcome_an(mydata(), variant0=mydata()$variant0, baseline0=levels(as.factors(mydata()$variant0))[0], 
    #                             outcome0 = mydata()$outcome0, outcome0_pos=levels(as.factors(mydata()$outcome0))[0])
     
    #if(nonmetric_checkbox == TRUE){outputdf = discrete_outcome_an(mydata(), 
    #                                                              variant0 = mydata()$variant0,
    #                                                              baseline0 = levels(as.factor(mydata()$variant0))[0],
    #                                                              outcome0 = mydata()$outcome0,
    #                                                              outcome0_pos = levels(as.factor(mydata()$outcome0))[0])}
    #else {outputdf = conti_outcome_an(mydata(),
    #                       variant0 = mydata()$variant0,
    #                       baseline0 = levels(as.factor(mydata()$variant0))[0],
    #                       outcome0 = mydata()$outcome0
    #                       )}
    
    
    #output$dataframe <- DT::renderDataTable(outputdf)
    
    
})
