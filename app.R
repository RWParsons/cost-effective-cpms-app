library(shiny)
library(shinyalert)
library(knitr)
library(tidyverse)
library(ggrepel)
library(kableExtra)
library(cutpointr)
library(shinyBS)
library(cowplot)
library(shinyWidgets)
library(shinyjs)

source("utils.R")

cutpoint_methods <- c(
  "Cost-effective",
  "The Closest to (0, 1) Criteria",
  "Youden",
  "Sens-Spec product",
  "Index of Union"
)

dropdown_width <- NULL


ui <- fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      sliderInput("cutpoint",
        "Select cutpoint:",
        min = 0,
        max = 1,
        value = 0.5
      ),
      tags$br(),
      dropdownButton(
        checkboxInput("cutpoints_all", "Select All/None", value = FALSE),
        label = "Add cutpoints", status = "default", width = dropdown_width,
        checkboxGroupInput(
          inputId = "cutpoint_methods", label = "Cutpoint methods:", width = dropdown_width,
          choices = cutpoint_methods,
          selected = NULL
        )
      ),
      tags$br(),
      bsCollapsePanel(
        "Inputs values of Net Monetary Benefit (NMB):",
        splitLayout(
          numericInput("tp_nmb", "True Positive", value = NULL),
          numericInput("fp_nmb", "False Positive", value = NULL)
        ),
        splitLayout(
          numericInput("fn_nmb", "False Negative", value = NULL),
          numericInput("tn_nmb", "True Negative", value = NULL)
        )
      ),
      tags$br(),
      radioButtons(
        "data_source",
        label = "Use simulated or your own data:",
        choices = c("simulated", "upload"),
        selected = "simulated"
      ),
      bsCollapse(
        id = "sampling_controls",
        bsCollapsePanel(
          "Settings for sampling",
          sliderInput("event_rate",
            "Rate of the outcome:",
            min = 0,
            max = 0.5,
            value = 0.1
          ),
          sliderInput("auc",
            "Model discrimination (AUC):",
            min = 0.5,
            max = 1,
            value = 0.75
          ),
          sliderInput("n_samples",
            "Sample size:",
            min = 0,
            max = 1e4,
            value = 1e3
          )
        )
      ),
      fileInput(
        "datafile", "Upload CSV file",
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      uiOutput("proba_selector"),
      uiOutput("actual_selector")
    ),
    mainPanel(
      plotOutput("plots", height = 320),
      tags$br(),
      tableOutput("confusion_matrix")
    )
  )
)


server <- function(input, output, session) {
  observe({
    if (input$data_source == "simulated") {
      show("sampling_controls")
      hide("datafile")
    } else {
      hide("sampling_controls")
      show("datafile")
    }
  })

  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })

  cols <- reactive({
    df <- filedata()
    if (is.null(df)) {
      return(NULL)
    } else {
      return(names(df))
    }
  })

  output$proba_selector <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("col_proba", "Select the predicted probability variable from:", cols())
  })

  output$actual_selector <- renderUI({
    if (is.null(cols())) {
      return(NULL)
    }
    selectInput("col_actual", "Select the (binary) outcome variable from:", cols())
  })

  observe({
    updateCheckboxGroupInput(
      inputId = "cutpoint_methods",
      choices = cutpoint_methods,
      selected = if (input$cutpoints_all) cutpoint_methods else c()
    )
  })

  observe({
    if (all(!is.na(input$tp_nmb), !is.na(input$fp_nmb), !is.na(input$fn_nmb), !is.na(input$tn_nmb))) {
      return() # do nothing if all inputs are there
    } else if ("Cost-effective" %in% input$cutpoint_methods) {
      updateCheckboxGroupInput( # update checkbox to uncheck the cost-effective box
        inputId = "cutpoint_methods",
        choices = cutpoint_methods,
        selected = input$cutpoint_methods[input$cutpoint_methods != "Cost-effective"]
      )
      shinyalert( # show alert to user
        "Oops!",
        paste(
          "The Cost-effective threshold only works when there are valid",
          "inputs for the Net Monetary Benefit 2x2 matrix below.",
          " You can expand it by clicking on the box below."
        ),
        type = "error"
      )
    }
  })

  df_preds <- reactive({
    if (input$data_source == "simulated") {
      set.seed(42)
      data <- get_sample(
        auc = input$auc,
        n_samples = input$n_samples,
        prevalence = input$event_rate
      )

      mod <- glm(actual ~ predicted, data = data, family = binomial())
      data$actual <- as.factor(data$actual)
      data$proba <- predict(mod, type = "response")
    } else {
      if (is.null(filedata())) {
        return()
      }
      if (length(unique(c(input$col_proba, input$col_actual))) != 2) {
        return()
      }
      data <-
        filedata() %>%
        select(proba = input$col_proba, actual = input$col_actual) %>%
        mutate(actual = as.factor(actual))

      if (length(unique(data$actual)) != 2) {
        return()
      }
    }
    if (is.null(data)) {
      return()
    }
    if (nrow(data) == 0) {
      return()
    }
    data
  })

  get_cutpoints <- reactive({
    thresholds <- get_thresholds(
      predicted = df_preds()$proba,
      actual = df_preds()$actual,
      NMB = c(
        "TP" = input$tp_nmb,
        "TN" = input$tn_nmb,
        "FP" = input$fp_nmb,
        "FN" = input$fn_nmb
      ),
      get_what = c("optimal_cutpoint", "sensitivity", "specificity")
    )
  })

  output$plots <- renderPlot({
    if (is.null(df_preds())) {
      return()
    }

    theme_list <- list(
      theme_bw(),
      theme(text = element_text(size = 13))
    )

    pal <- "Dark2"

    p_hist <-
      df_preds() %>%
      ggplot(aes(x = proba, fill = actual)) +
      geom_histogram() +
      labs(
        x = "Predicted probabilities",
        y = "Number of observations",
        fill = "Event"
      ) +
      scale_x_continuous(limits = c(0, 1)) +
      theme_list

    cutpoints_df <- get_cutpoints()
    cutpoints_df <- filter(cutpoints_df, cutpoint_method %in% input$cutpoint_methods)
    cutpoints_df <- rbind(
      cutpoints_df,
      data.frame(
        optimal_cutpoint = input$cutpoint,
        sensitivity = get_selected_cp_metrics()$sensitivity,
        specificity = get_selected_cp_metrics()$specificity,
        cutpoint_method = c("Selected cutpoint")
      )
    ) %>%
      mutate(cutpoint_method = factor(cutpoint_method, levels = c("Selected cutpoint", cutpoint_methods)))

    cutpoints_df$cp_size <- seq(from = 4, to = 3, length.out = nrow(cutpoints_df))


    rocobj <- pROC::roc(df_preds()$actual, df_preds()$proba)
    p_roc <- pROC::ggroc(rocobj) +
      labs(
        col = "",
        y = "Specificity",
        x = "Sensitivity"
      ) +
      theme_list

    # add cutpoints to graphs
    p_hist <-
      p_hist +
      geom_vline(
        data = cutpoints_df,
        aes(xintercept = optimal_cutpoint, col = cutpoint_method),
        size = cutpoints_df$cp_size,
        alpha = 0.7,
        linetype = "dashed"
      ) +
      scale_color_brewer(palette = pal)

    p_roc <-
      p_roc +
      geom_point(
        data = cutpoints_df,
        aes(x = sensitivity, y = specificity, col = cutpoint_method),
        alpha = 0.7
      ) +
      geom_label_repel(
        data = cutpoints_df,
        aes(x = sensitivity, y = specificity, label = cutpoint_method)
      ) +
      scale_color_brewer(palette = pal) +
      guides(col = "none")

    # get legends out of plots and put at bottom with cowplot
    leg1 <- get_legend(
      p_hist +
        guides(
          col = "none",
          fill = guide_legend(nrow = 1)
        ) +
        theme(legend.position = "bottom")
    )

    leg2 <- get_legend(
      p_roc +
        guides(col = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
    )

    p_hist <- p_hist + guides(col = "none", fill = "none")
    p_roc <- p_roc + guides(col = "none")

    plots <- plot_grid(p_hist, p_roc)
    legends <- plot_grid(leg1, leg2, ncol = 1)
    plot_grid(plots, legends, ncol = 1, rel_heights = c(0.8, 0.2))
  })

  get_confusion <- reactive({
    cm <-
      df_preds() %>%
      group_by(actual, predicted = proba > input$cutpoint) %>%
      summarize(n = n()) %>%
      ungroup()
    
    # if the cutpoint is above or below ALL predicted probs, add the empty rows to confusion matrix
    if (nrow(cm) != 4) { 
      cm <- rbind(
        cm,
        data.frame(
          actual = c(0, 1),
          predicted = !unique(cm$predicted),
          n = 0
        )
      )
    }
    cm
  })

  output$confusion_matrix <- renderText({
    if (is.null(df_preds())) {
      return()
    }
    get_confusion() %>%
      mutate(predicted = factor(predicted, levels = c(FALSE, TRUE))) %>%
      pivot_wider(
        names_from = predicted, values_from = n,
        names_expand = T, values_fill = 0
      ) %>%
      mutate(actual = as.logical(as.numeric(actual) - 1)) %>%
      rename(" " = actual) %>%
      kable(
        "html",
        caption = glue::glue(
          "Confusion matrix using visualised data above ",
          "and the selected cutpoint ({input$cutpoint})"
        )
      ) %>%
      kable_paper(full_width = FALSE, font_size = 18) %>%
      column_spec(2:3, width = "6em") %>%
      column_spec(1, bold = T) %>%
      add_header_above(c(" " = 1, "Predicted" = 2)) %>%
      group_rows("Actual", 1, 2)
  })

  get_selected_cp_metrics <- reactive({
    cp_fp <-
      get_confusion() %>%
      filter(predicted == 1, actual == 0) %>%
      pull(n)

    cp_fn <-
      get_confusion() %>%
      filter(predicted == 0, actual == 1) %>%
      pull(n)

    cp_tp <-
      get_confusion() %>%
      filter(predicted == 1, actual == 1) %>%
      pull(n)

    cp_tn <-
      get_confusion() %>%
      filter(predicted == 0, actual == 0) %>%
      pull(n)

    list(
      fp = cp_fp,
      fn = cp_fn,
      tp = cp_tp,
      tn = cp_tn,
      sensitivity = sensitivity(tp = cp_tp, fn = cp_fn),
      specificity = specificity(fp = cp_fp, tn = cp_tn)
    )
  })
}


shinyApp(ui = ui, server = server)
