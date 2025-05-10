#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      freshTheme = setTheme(),
      skin = "blue",
      dashboardHeader(title = "A/B Stats"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Home", tabName = "main", icon = icon("home")),
          menuItem("Test Duration", tabName = "duration", icon = icon("calculator")),
          menuItem("Test Results", tabName = "results", icon = icon("chart-bar")),
          menuItem("Help", tabName = "explanation", icon = icon("book")),
          menuItem("About Me", tabName = "about", icon = icon("person"))
        )
      ),
      dashboardBody(
        tabItems(
          # Main Page
          tabItem(tabName = "main",
                  h2("Welcome to A/B Stats!"),
                  fluidRow(
                    box(
                      title = "What is A/B Stats?",
                      status = "success",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      HTML("
              <p><strong>A/B Stats</strong> is your go-to tool for conducting A/B testing with ease and precision. Whether you're optimizing a webpage, app, or marketing campaign, this dashboard provides the tools you need to make data-driven decisions.</p>
              <p>With A/B Stats, you can:</p>
              <ul>
                <li>Estimate the required sample size and test duration for your experiments.</li>
                <li>Analyze test results to determine statistical significance and uplift.</li>
                <li>Understand the methodology behind hypothesis testing.</li>
              </ul>
              <p>Navigate through the tabs on the left to explore the features and start optimizing your experiments today!</p>
            ")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Why A/B Testing?",
                      status = "success",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      HTML("
              <p>A/B testing is a powerful method to compare two versions of a product or feature to determine which performs better. It helps you:</p>
              <ul>
                <li>Make informed decisions backed by data.</li>
                <li>Optimize user experience and conversion rates.</li>
                <li>Reduce guesswork and improve ROI.</li>
              </ul>
            ")
                    ),
                    box(
                      title = "How to Use This Dashboard",
                      status = "success",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      HTML("
              <p>Using A/B Stats is simple:</p>
              <ol>
                <li>Go to the <strong>Test Duration</strong> tab to calculate the required sample size and test duration.</li>
                <li>Visit the <strong>Test Results</strong> tab to analyze your experiment's outcomes.</li>
                <li>Check the <strong>Help</strong> tab for a detailed explanation of A/B testing and hypothesis testing.</li>
                <li>Learn more about the creator in the <strong>About Me</strong> tab.</li>
              </ol>
            ")
                    )
                  )
          ),

          # Test Duration Calculator
          tabItem(tabName = "duration",
                  h2("Test Duration Calculator"),
                  fluidRow(
                    box(title = "Inputs", status = "primary", solidHeader = TRUE, width = 6,
                        fluidRow(
                          column(
                            width = 6,
                            numericInput("baseline_rate", "Baseline Conversion Rate Expected (%)", value = 10, min = 0, max = 100)
                          ),
                          column(
                            width = 6,
                            numericInput("expected_lift", "Expected Uplift (%)", value = 5, min = 1, max = 100)
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            numericInput("daily_users", "Average of Daily Unique Users Expected", value = 10000, min = 1)
                          ),
                          column(
                            width = 6,
                            numericInput("confidence_level", "Confidence Level (%)", value = 95, min = 90, max = 99)
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            radioButtons("hypothesis_type", "Hypothesis Type", choiceValues = c("less", "two.sided"), choiceNames = c("One-sided", "Two-sided"))
                          ),
                          column(
                            width = 6,
                            numericInput("power", "Power Level (%)", value = 80, min = 50, max = 99)
                          )
                        )
                    ),
                    box(title = "Results", status = "success", solidHeader = TRUE, width = 6,
                        fluidRow(
                          infoBoxOutput("sample_size",width = 6),
                          infoBoxOutput("test_duration",width = 6)
                        )
                    )
                  )
          ),

          # Hypothesis Test Results
          tabItem(tabName = "results",
                  h2("Hypothesis Test Results"),
                  fluidRow(
                    box(title = "Inputs", status = "primary", solidHeader = TRUE, width = 6,
                        fluidRow(
                          column(width = 6, numericInput("conv_a", "Conversions (Variant A)", value = 50, min = 0)),
                          column(width = 6, numericInput("conv_b", "Conversions (Variant B)", value = 60, min = 0))
                        ),
                        fluidRow(
                          column(width = 6, numericInput("users_a", "Total Users (Variant A)", value = 1000, min = 1)),
                          column(width = 6, numericInput("users_b", "Total Users (Variant B)", value = 1000, min = 1))
                        ),
                        fluidRow(
                          column(width = 6,
                                 radioButtons("test_hypothesis_type", "Hypothesis Type",
                                              choiceValues = c("less", "two.sided"), choiceNames = c("One-sided", "Two-sided"))
                          ),
                          column(
                            width = 6,
                            numericInput("test_confidence_level", "Confidence Level (%)", value = 95, min = 90, max = 99)
                          )
                        ),
                        fluidRow(
                          column(
                            width = 6,
                            radioButtons("yates_correction", "Yates Correction", choiceValues = c(TRUE, FALSE), choiceNames = c("Yes", "No"), selected = FALSE)
                          )
                        )


                    ),
                    box(title = "Results", status = "success", solidHeader = TRUE, width = 6,
                        infoBoxOutput("rate_a", width = 6),
                        infoBoxOutput("rate_b", width = 6),
                        infoBoxOutput("uplift", width = 6),
                        infoBoxOutput("power", width = 6),
                        infoBoxOutput("p_value", width = 6),
                        infoBoxOutput("confidence_interval", width = 6),
                        infoBoxOutput("result", width = 12)
                    )
                  )
          ),

          # Hypothesis Testing Explained
          tabItem(tabName = "explanation",
                  h2("Understanding A/B Testing and Hypothesis Testing"),
                  fluidRow(
                    box(
                      title = "What is A/B Testing?",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      HTML("
              <p>An <strong>A/B test</strong> is a controlled experiment used to compare two versions of a product, feature, or webpage — typically called <strong>A (control)</strong> and <strong>B (variation)</strong>. The goal is to determine which version performs better based on a specific metric, such as conversion rate or click-through rate.</p>
              <p>For example, you might test two different button designs to see which one gets more clicks.</p>
            ")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Why Use A/B Testing?",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      HTML("
              <p>A/B testing helps you:</p>
              <ul>
                <li>Make data-driven decisions.</li>
                <li>Optimize user experience and engagement.</li>
                <li>Reduce guesswork and improve ROI.</li>
                <li>Validate ideas before full-scale implementation.</li>
              </ul>
            ")
                    ),
                    box(
                      title = "When to Use A/B Testing?",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      HTML("
              <p>A/B testing is ideal when you want to:</p>
              <ul>
                <li>Test changes to a webpage, app, or marketing campaign.</li>
                <li>Compare two versions of a feature or design.</li>
                <li>Measure the impact of changes on user behavior.</li>
              </ul>
            ")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Hypothesis Testing in A/B Testing",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      collapsible = TRUE,
                      withMathJax(HTML("
              <h4>Key Concepts:</h4>
              <ul>
                <li><strong>Null Hypothesis (H<sub>0</sub>):</strong> Assumes no difference between the two versions (e.g., \\( p_1 = p_2 \\)).</li>
                <li><strong>Alternative Hypothesis (H<sub>1</sub>):</strong> Assumes a difference exists (e.g., \\( p_1 \\ne p_2 \\)).</li>
              </ul>
              <h4>Steps in Hypothesis Testing:</h4>
              <ol>
                <li>Define the hypotheses (H<sub>0</sub> and H<sub>1</sub>).</li>
                <li>Choose a significance level (\\( \\alpha \\), typically 0.05).</li>
                <li>Calculate the test statistic (e.g., Z-score).</li>
                <li>Find the p-value and compare it to \\( \\alpha \\).</li>
                <li>Make a decision: reject or fail to reject H<sub>0</sub>.</li>
              </ol>
              <h4>Formula for Z-Score:</h4>
              <p style='margin-left:20px'>\\[
              Z = \\frac{\\hat{p}_1 - \\hat{p}_2}{\\sqrt{\\hat{p}(1 - \\hat{p})\\left(\\frac{1}{n_1} + \\frac{1}{n_2}\\right)}}
              \\]</p>
              <p>Where:</p>
              <ul>
                <li>\\( \\hat{p}_1 \\) and \\( \\hat{p}_2 \\): Observed proportions for groups A and B.</li>
                <li>\\( \\hat{p} \\): Pooled proportion.</li>
                <li>\\( n_1 \\) and \\( n_2 \\): Sample sizes for groups A and B.</li>
              </ul>
            "))
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Common Errors in Hypothesis Testing",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      HTML("
              <ul>
                <li><strong>Type I Error (False Positive):</strong> Rejecting H<sub>0</sub> when it is true.</li>
                <li><strong>Type II Error (False Negative):</strong> Failing to reject H<sub>0</sub> when H<sub>1</sub> is true.</li>
              </ul>
              <p>Example:</p>
              <ul>
                <li>Type I: Concluding version B is better, but there is no real difference.</li>
                <li>Type II: Concluding there is no difference, but version B is actually better.</li>
              </ul>
            ")
                    ),
                    box(
                      title = "Improving Test Accuracy",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 6,
                      collapsible = TRUE,
                      HTML("
              <p>To improve the accuracy of your tests:</p>
              <ul>
                <li>Increase the sample size.</li>
                <li>Use a higher significance level (e.g., \\( \\alpha = 0.01 \\)).</li>
                <li>Ensure proper randomization of test groups.</li>
                <li>Run tests for an adequate duration to capture meaningful data.</li>
              </ul>
            ")
                    )
                  )
          ),
          tabItem(
            tabName = "about",
            h2("About Me"),
            fluidRow(
              userBox(
                title = userDescription(
                  title = "Thiago F. Miranda",
                  subtitle = "Data Scientist | Data Analyst | Statistician",
                  type = 1,
                  image = "https://avatars.githubusercontent.com/u/33705632?v=4"
                ),
                status = "success",
                width = 12,
                HTML("
        <p>
          I’m a data scientist with over 6 years of experience, specializing in statistics and data-driven decision-making.
        </p>
        <p>
          My expertise spans <strong>data analysis</strong>, <strong>predictive modeling</strong>, and the development of scalable solutions that translate complex data into clear insights and business impact. I’m passionate about using statistical techniques to solve real-world problems and optimize strategies.
        </p>
        <p>
          Committed to continuous learning, I stay current with the latest advancements in data science and statistical computing to ensure that the solutions I deliver are both modern and effective.
        </p>
        <p>
          <strong>Let’s connect data with purpose.</strong>
        </p>
        <hr>
        <p>
          <a href='https://www.thiagofmiranda.com' target='_blank' class='btn btn-primary'>
            🌐 Visit my website
          </a>
          <a href='https://www.linkedin.com/in/thiiagofmiranda/' target='_blank' class='btn btn-primary'>
            🔗 LinkedIn
          </a>
          <a href='https://github.com/thiagofmiranda' target='_blank' class='btn btn-primary'>
            💻 GitHub
          </a>
        </p>
      ")
              )
            )
          )


        )
      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ABstats"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
