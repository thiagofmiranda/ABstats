#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Test Duration Calculator
  output$sample_size <- renderInfoBox({
    baseline_rate <- input$baseline_rate / 100
    expected_rate <- baseline_rate * (1 + input$expected_lift / 100)
    power <- input$power / 100
    alpha <- 1 - input$confidence_level / 100
    hypothesis_type <- input$hypothesis_type

    result <- calculate_sample_size(baseline_rate, expected_rate, power, alpha, hypothesis_type)

    infoBox(
      "Required Sample Size", h4(ceiling(result)), icon = icon("person", lib = "font-awesome") ,color = "green"
    )
  })

  output$test_duration <- renderInfoBox({
    baseline_rate <- input$baseline_rate / 100
    expected_rate <- baseline_rate * (1 + input$expected_lift / 100)
    power <- input$power / 100
    alpha <- 1 - input$confidence_level / 100
    hypothesis_type <- input$hypothesis_type
    daily_users <- input$daily_users

    result <- calculate_sample_size(baseline_rate, expected_rate, power, alpha, hypothesis_type)


    days <- ceiling(result / daily_users)

    infoBox(
      "Estimated Test Duration", h4(paste(days, "days")), icon = icon("calendar") ,color = "green"
    )

  })

  output$rate_a <- renderInfoBox({
    conv_a <- input$conv_a
    users_a <- input$users_a

    rate_a <- round(conv_a/users_a, 4) * 100
    infoBox(
      "Conversion Rate (Variant A)", h4(paste0(rate_a,"%")), icon = icon("a", lib = "font-awesome") ,color = "green"
    )

  })

  output$rate_b <- renderInfoBox({
    conv_b <- input$conv_b
    users_b <- input$users_b

    rate_b <- round(conv_b/users_b, 4) * 100
    infoBox(
      "Conversion Rate (Variant B)", h4(paste0(rate_b,"%")), icon = icon("b", lib = "font-awesome") ,color = "green"
    )

  })

  output$uplift <- renderInfoBox({
    conv_a <- input$conv_a
    users_a <- input$users_a
    rate_a <- conv_a/users_a

    conv_b <- input$conv_b
    users_b <- input$users_b
    rate_b <- conv_b/users_b

    uplift <- ((rate_b - rate_a) / rate_a) * 100

    infoBox(
      "Uplift", h4(paste0(uplift,"%")), icon = icon("chart-line", lib = "font-awesome") ,color = "green"
    )

  })

  # Hypothesis Test Results
  output$p_value <- renderInfoBox({
    conv_a <- input$conv_a
    users_a <- input$users_a
    rate_a <- conv_a/users_a

    conv_b <- input$conv_b
    users_b <- input$users_b
    rate_b <- conv_b/users_b

    hypothesis_type <- input$test_hypothesis_type
    alpha <- 1 - input$test_confidence_level / 100
    yates_correction <- input$yates_correction

    diff <- rate_b - rate_a

    # changing the hypothesis type side if it is necessary
    hypothesis_type <- ifelse(hypothesis_type == "less" & diff < 0, "greater", hypothesis_type)

    prop_test <- prop.test(c(conv_a, conv_b), c(users_a, users_b), conf.level = alpha, alternative = hypothesis_type, correct = yates_correction)

    infoBox(
      "P-value", h4(round(prop_test$p.value, 4)), icon = icon("star", lib = "font-awesome") ,color = "green"
    )

  })

  output$confidence_interval <- renderInfoBox({
    conv_a <- input$conv_a
    users_a <- input$users_a
    rate_a <- conv_a/users_a

    conv_b <- input$conv_b
    users_b <- input$users_b
    rate_b <- conv_b/users_b

    hypothesis_type <- input$test_hypothesis_type
    alpha <- 1 - input$test_confidence_level / 100
    yates_correction <- input$yates_correction

    diff <- rate_b - rate_a

    # changing the hypothesis type side if it is necessary
    hypothesis_type <- ifelse(hypothesis_type == "less" & diff < 0, "greater", hypothesis_type)

    prop_test <- prop.test(c(conv_a, conv_b), c(users_a, users_b), conf.level = alpha, alternative = hypothesis_type, correct = yates_correction)

    infoBox(
      "Confidence Interval", h4(paste0("(", round(prop_test$conf.int[1], 4), ", ", round(prop_test$conf.int[2], 4), ")")),
      icon = icon("arrows-left-right", lib = "font-awesome"), color = "green"
    )
  })

  output$result <- renderInfoBox({
    conv_a <- input$conv_a
    users_a <- input$users_a
    rate_a <- conv_a/users_a

    conv_b <- input$conv_b
    users_b <- input$users_b
    rate_b <- conv_b/users_b

    hypothesis_type <- input$test_hypothesis_type
    alpha <- 1 - input$test_confidence_level / 100
    yates_correction <- input$yates_correction

    diff <- rate_b - rate_a

    # changing the hypothesis type side if it is necessary
    hypothesis_type <- ifelse(hypothesis_type == "less" & diff < 0, "greater", hypothesis_type)

    prop_test <- prop.test(c(conv_a, conv_b), c(users_a, users_b), conf.level = alpha, alternative = hypothesis_type, correct = yates_correction)

    if (prop_test$p.value < 0.05) {
      infoBox(
        "Test Result", h4("Significant!!! \n The two rates are different"), icon =  icon("thumbs-up", lib = "glyphicon") ,color = "green"
      )
    } else {
      infoBox(
        "Test Result", h4("Not significant... \n The two rates are not different"), icon =  icon("thumbs-down", lib = "glyphicon") ,color = "green"
      )
    }
  })

  output$power <- renderInfoBox({
    conv_a <- input$conv_a
    users_a <- input$users_a
    rate_a <- conv_a/users_a

    conv_b <- input$conv_b
    users_b <- input$users_b
    rate_b <- conv_b/users_b

    hypothesis_type <- input$test_hypothesis_type
    alpha <- 1 - input$test_confidence_level / 100
    yates_correction <- input$yates_correction

    alpha <- 1 - input$confidence_level / 100
    hypothesis_type <- input$hypothesis_type

    result <- calculate_test_power(users_a, users_b, rate_a, rate_b, alpha, hypothesis_type)
    result <- round(result, 4) * 100

    infoBox(
      "Observed Power", h4(paste0(result,"%")), icon = icon("dumbbell", lib = "font-awesome") ,color = "green"
    )
  })


}
