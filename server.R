library(shiny)
library(ggplot2)
library(ggcorrplot)
library(shinythemes)

server <- function(input, output, session) {
  
  # Reactive value for the dataset
  data <- reactiveVal() # Initialize as a reactive value
  
  # Observe sample dataset selection
  observeEvent(input$sample_data, {
    if (input$sample_data == "mtcars") {
      data(mtcars)
    } else if (input$sample_data == "faithful") {
      data(faithful)
    } else {
      data(NULL) # Reset if "None" is selected
    }
  })
  
  # Load user-uploaded CSV
  observeEvent(input$file, {
    req(input$file)
    data(read.csv(input$file$datapath))
  })
  
  # Update variable dropdowns dynamically
  observeEvent(data(), {
    req(data())
    updateSelectInput(session, "xvar", choices = names(data()))
    updateSelectInput(session, "yvar", choices = names(data()))
  })
  
  # Perform regression
  regression_model <- eventReactive(input$run_regression, {
    req(input$xvar, input$yvar)
    lm(as.formula(paste(input$yvar, "~", input$xvar)), data = data())
  })
  
  # Display dataset summary
  output$data_summary <- renderTable({
    req(data())
    dataset_summary <- as.data.frame(t(sapply(data(), summary)))
    dataset_summary
  }, rownames = TRUE)
  
  output$dataset_summary_text <- renderUI({
    req(data())
    num_rows <- nrow(data())
    num_cols <- ncol(data())
    
    HTML(paste0(
      "<h4>Dataset Summary</h4>",
      "<p>This dataset contains <b>", num_rows, "</b> rows and <b>", num_cols, "</b> columns. ",
      "The table below provides a summary of each column, including measures such as the mean, median, and range for numeric variables, ",
      "as well as the distribution of values for categorical variables. This overview helps in understanding the structure and content of the data.</p>"
    ))
  })
  
  
  # Correlation matrix
  output$correlation_matrix <- renderPlot({
    req(data())
    numeric_data <- data()[, sapply(data(), is.numeric)]
    corr <- cor(numeric_data, use = "complete.obs")
    ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE) +
      labs(title = "Correlation Matrix") +
      theme_minimal()
  })
  
  # Scatterplot with regression line and summary below
  output$scatter_plot <- renderPlot({
    req(data(), input$xvar, input$yvar)
    ggplot(data(), aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "darkblue") +
      geom_smooth(method = "lm", col = "red") +
      labs(
        title = "Scatterplot with Regression Line",
        x = input$xvar,
        y = input$yvar
      ) +
      theme_minimal()
  })
  
  # QQ Plot of Residuals
  output$qq_plot <- renderPlot({
    req(regression_model())
    residuals <- residuals(regression_model())
    qqnorm(residuals, main = "QQ Plot of Residuals")
    qqline(residuals, col = "red", lwd = 2)
  })
  
  output$residuals_table <- renderTable({
    req(data(), regression_model())
    model <- regression_model()
    
    residuals_data <- data.frame(
      Actual_Y = data()[[input$yvar]],       # Actual y-values
      Fitted_Y = fitted(model),             # Fitted y-values
      Residuals = residuals(model)          # Residuals
    )
    residuals_data
  }, rownames = TRUE)
  
  output$residual_analysis <- renderUI({
    req(regression_model())
    residuals <- residuals(regression_model())
    
    # Calculate mean and standard deviation of residuals
    residual_mean <- mean(residuals)
    residual_sd <- sd(residuals)
    
    # Generate insights
    insight <- if (abs(residual_mean) < 1e-5) {
      "The residuals have a mean close to zero, indicating no significant bias in predictions."
    } else {
      "The residuals have a non-zero mean, suggesting potential bias in predictions."
    }
    
    distribution <- if (residual_sd < 1) {
      "The residuals have a low standard deviation, indicating tightly clustered errors."
    } else {
      "The residuals have a high standard deviation, suggesting larger variability in errors."
    }
    
    # Render the analysis
    HTML(paste0(
      "<h4>Residual Analysis</h4>",
      "<p><b>Mean of Residuals:</b> ", round(residual_mean, 5), "</p>",
      "<p><b>Standard Deviation of Residuals:</b> ", round(residual_sd, 5), "</p>",
      "<p>", insight, "</p>",
      "<p>", distribution, "</p>"
    ))
  })
  
  
  # Model summary and essential information
  output$model_details <- renderUI({
    req(regression_model())
    model <- regression_model()
    summary <- summary(model)
    conf_int <- confint(model)
    f_stat <- summary$fstatistic
    f_p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)
    coefficients <- coef(summary)
    
    # Extract t-statistics and p-values for coefficients
    t_intercept <- coefficients[1, "t value"]
    p_intercept <- coefficients[1, "Pr(>|t|)"]
    t_slope <- coefficients[2, "t value"]
    p_slope <- coefficients[2, "Pr(>|t|)"]
    
    intercept <- coef(model)[1]
    slope <- coef(model)[2]
    
    # Analyze R^2
    r2_analysis <- if (summary$r.squared >= 0.7) {
      "The model explains a high proportion of the variability in the response variable, indicating strong predictive power."
    } else if (summary$r.squared >= 0.4) {
      "The model explains a moderate proportion of the variability in the response variable, indicating acceptable predictive power."
    } else {
      "The model explains a low proportion of the variability in the response variable, indicating weak predictive power."
    }
    
    utility_test <- if (f_p_value < 0.05) {
      "The F-test indicates that the model is statistically significant overall, meaning that the predictor variable is useful for explaining the response variable."
    } else {
      "The F-test indicates that the model is not statistically significant overall, meaning that the predictor variable may not explain the response variable well."
    }
    
    # Analyze intercept and slope utility
    intercept_utility <- if (p_intercept < 0.05) {
      "The intercept is statistically significant, indicating that it contributes meaningfully to the model."
    } else {
      "The intercept is not statistically significant, suggesting it may not meaningfully contribute to the model."
    }
    
    slope_utility <- if (p_slope < 0.05) {
      "The slope is statistically significant, indicating a meaningful relationship between the predictor and response variable."
    } else {
      "The slope is not statistically significant, suggesting there may not be a strong relationship between the predictor and response variable."
    }
    
    # Render essential model details in HTML
    HTML(paste0(
      "<h4>Model Summary</h4>",
      "<ul>",
      "<li><b>Regression Line Equation:</b> ", 
      "y = ", round(intercept, 3), " + ", round(slope, 3), "x</li>",
      "<li><b>R-squared:</b> ", round(summary$r.squared, 3), "</li>",
      "<li><b>Adjusted R-squared:</b> ", round(summary$adj.r.squared, 3), "</li>",
      "<li><b>Residual Standard Error:</b> ", round(summary$sigma, 3), "</li>",
      "<li><b>F-statistic:</b> ", round(f_stat[1], 3), "</li>",
      "<li><b>P-value for F-test:</b> ", format(f_p_value, scientific = TRUE), "</li>",
      "<li><b>Intercept:</b> Estimate = ", round(intercept, 3), 
      ", t-statistic = ", round(t_intercept, 3), 
      ", p-value = ", format(p_intercept, scientific = TRUE), "</li>",
      "<li><b>Slope:</b> Estimate = ", round(slope, 3), 
      ", t-statistic = ", round(t_slope, 3), 
      ", p-value = ", format(p_slope, scientific = TRUE), "</li>",
      "</ul>",
      "<h4>Analysis</h4>",
      "<p>", r2_analysis, "</p>",
      "<p>", utility_test, "</p>",
      "<p><b>Intercept Utility:</b> ", intercept_utility, "</p>",
      "<p><b>Slope Utility:</b> ", slope_utility, "</p>"
    ))
  })
  
  
  # Predict output
  predicted_value <- eventReactive(input$predict_btn, {
    req(regression_model(), input$predict_input)
    input_value <- as.numeric(input$predict_input)
    new_data <- data.frame(input_value)
    colnames(new_data) <- input$xvar
    predict(regression_model(), newdata = new_data)
  })
  
  output$dynamic_input <- renderUI({
    req(input$xvar, input$yvar)
    textInput(
      "predict_input",
      label = paste("Prediction for", input$yvar, "given a", input$xvar, "value:"),
      value = ""
    )
  })
  
  output$predicted_output <- renderText({
    req(predicted_value())
    paste("Predicted Value:", round(predicted_value(), 2))
  })
  
  # Download regression summary
  output$download_summary <- downloadHandler(
    filename = "regression_summary.txt",
    content = function(file) {
      req(regression_model()) # Ensure the model exists
      model <- regression_model()
      summary_obj <- summary(model)
      conf_int <- confint(model)
      
      # Generate the summary content
      summary_content <- paste0(
        "Regression Summary\n",
        "=================\n",
        "Formula: ", as.character(summary_obj$call), "\n\n",
        "Coefficients:\n",
        "Intercept: ", round(coef(model)[1], 3), 
        " (95% CI: ", round(conf_int[1, 1], 3), " - ", round(conf_int[1, 2], 3), ")\n",
        "Slope: ", round(coef(model)[2], 3), 
        " (95% CI: ", round(conf_int[2, 1], 3), " - ", round(conf_int[2, 2], 3), ")\n\n",
        "Model Fit Statistics:\n",
        "R-squared: ", round(summary_obj$r.squared, 3), "\n",
        "Adjusted R-squared: ", round(summary_obj$adj.r.squared, 3), "\n",
        "Residual Standard Error: ", round(summary_obj$sigma, 3), "\n",
        "Degrees of Freedom: ", summary_obj$df[2], "\n\n",
        "F-statistic: ", round(summary_obj$fstatistic[1], 3), "\n",
        "P-value for F-test: ", format(pf(summary_obj$fstatistic[1], 
                                          summary_obj$fstatistic[2], 
                                          summary_obj$fstatistic[3], 
                                          lower.tail = FALSE), scientific = TRUE), "\n"
      )
      
      # Write to file
      writeLines(summary_content, con = file)
    }
  )
}
