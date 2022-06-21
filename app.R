library(shiny)
library(shinyalert)
library(knitr)
library(tidyverse)
library(kableExtra)
library(cutpointr)
library(shinyBS)

source("utils.R")

cutpoint_methods <- c(
  "Cost-effective", 
  "The Closest to (0, 1) Criteria", 
  "Youden", 
  "Sens-Spec product", 
  "Index of Union"
)

cutpoints_colours <- RColorBrewer::brewer.pal(length(cutpoint_methods), "Set2")
names(cutpoints_colours) <- cutpoint_methods

dropdown_width <- NULL


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("cutpoint",
                  "Select cutpoint:",
                  min = 0,
                  max = 1,
                  value = 0.5),
      tags$br(),
      dropdownButton(
        checkboxInput('cutpoints_all', 'Select All/None', value=FALSE),
        label="Add cutpoints", status="default", width=dropdown_width,
        checkboxGroupInput(
          inputId="cutpoint_methods", label="Cutpoint methods:", width=dropdown_width,
          choices=cutpoint_methods,
          selected=NULL)
        ),
      tags$br(),
      bsCollapsePanel(
        "Inputs values of Net Monetary Benefit (NMB):",
        splitLayout(
          numericInput("tp_nmb", "True Positive", value=NULL),
          numericInput("fp_nmb", "False Positive", value=NULL)
        ),
        splitLayout(
          numericInput("fn_nmb", "False Negative", value=NULL),
          numericInput("tn_nmb", "True Negative", value=NULL)
        )
      ),
      tags$br(),
      bsCollapsePanel(
        "Settings for sampling",
        sliderInput("event_rate",
                    "Rate of the outcome:",
                    min = 0,
                    max = 0.5,
                    value = 0.1),
        sliderInput("auc",
                    "Model discrimination (AUC):",
                    min = 0.5,
                    max = 1,
                    value = 0.75),
        sliderInput("n_samples",
                    "Sample size:",
                    min = 0,
                    max = 1e4,
                    value = 1e3)
      )
    ),
    mainPanel(
      plotOutput("histogram", height=320),
      tags$br(),
      tableOutput("confusion_matrix") 
    )
  )
)


server <- function(input, output, session) {
  
  observe({
    updateCheckboxGroupInput(
      inputId='cutpoint_methods', 
      choices=cutpoint_methods,
      selected=if(input$cutpoints_all) cutpoint_methods else c()
    )
  })
  
  observe({
    if(all(!is.na(input$tp_nmb), !is.na(input$fp_nmb), !is.na(input$fn_nmb), !is.na(input$tn_nmb))){
      return() # do nothing if all inputs are there
    } else if("Cost-effective" %in% input$cutpoint_methods){
      updateCheckboxGroupInput( # update checkbox to uncheck the cost-effective box
        inputId='cutpoint_methods', 
        choices=cutpoint_methods,
        selected=input$cutpoint_methods[input$cutpoint_methods != "Cost-effective"]
      )
      shinyalert( # show alert to user
        "Oops!", 
        paste(
          "The Cost-effective threshold only works when there are valid",
          "inputs for the Net Monetary Benefit 2x2 matrix below.",
          " You can expand it by clicking on the box below."
        ), 
        type = "error")
    }
  })
  
  df_preds <- reactive({
    set.seed(42)
    data <- get_sample(
      auc=input$auc, 
      n_samples=input$n_samples, 
      prevalence=input$event_rate
    )
    
    mod <- glm(actual ~ predicted, data=data, family=binomial())
    data$actual <- as.factor(data$actual)
    data$proba <- predict(mod, type="response")
    df_out <<- data
    data
  })
  
  get_cutpoints <- reactive({
    thresholds <- get_thresholds(
      predicted=df_preds()$proba, 
      actual=df_preds()$actual,
      NMB = c(
        "TP"=input$tp_nmb,
        "TN"=input$tn_nmb,
        "FP"=input$fp_nmb,
        "FN"=input$fn_nmb
      )
    )
  })
  
  output$histogram <- renderPlot({
    
    p_hist <- 
      df_preds() %>%
      ggplot(aes(x=proba, fill=actual)) + 
      geom_histogram() +
      geom_vline(xintercept=input$cutpoint, size=2, alpha=0.7) +
      theme_bw() +
      labs(
        x="Predicted probabilities",
        y="Number of observations",
        fill="Event"
      ) +
      scale_x_continuous(limits=c(0,1)) +
      theme(text = element_text(size = 13))
    
    cutpoints_df <- data.frame(
      cp_name = input$cutpoint_methods,
      cp_value = get_cutpoints()[input$cutpoint_methods],
      cp_size = seq(from=4, to=3, length.out=length(input$cutpoint_methods))
    )
    
    if(nrow(cutpoints_df) > 0) {
      p_hist <- p_hist +
        geom_vline(
          data=cutpoints_df, 
          aes(xintercept=cp_value, col=cp_name),
          size=cutpoints_df$cp_size,
          alpha=0.7, 
          linetype="dashed"
        )
    }
    p_hist

  })
  
  output$confusion_matrix <- renderText({
    
    df_preds() %>%
      group_by(actual, predicted=proba > input$cutpoint) %>%
      summarize(n=n()) %>% 
      ungroup() %>%
      mutate(predicted = factor(predicted, levels=c(FALSE, TRUE))) %>%
      pivot_wider(
        names_from=predicted, values_from=n, 
        names_expand=T, values_fill=0
      ) %>%
      mutate(actual=as.logical(as.numeric(actual)-1)) %>%
      rename(" "=actual) %>%
      kable(
        "html", 
        caption=glue::glue(
          "Confusion matrix using visualised data above",
          "and the selected cutpoint of {input$cutpoint}"
        )
      ) %>%
      kable_paper(full_width=FALSE, font_size=18) %>%
      column_spec(2:3, width = "6em") %>%
      column_spec(1, bold=T) %>%
      add_header_above(c(" "=1, "Predicted"=2)) %>%
      group_rows("Actual", 1, 2)
  })
}


shinyApp(ui = ui, server = server)
