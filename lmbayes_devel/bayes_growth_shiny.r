library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(doBy)

ui <- fluidPage(
  titlePanel("Empirical Bayes Prediction with Growth Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("early_time", "Max age used in early fit:", min = 1, max = 20, value = 7, step = 1),
      sliderInput("alpha", "Shrinkage factor (alpha):", min = 0, max = 1, value = 0.4, step = 0.05),
      textInput("train_idx", "Training subject indices (e.g. 1:10 or c(1,3,5))", value = "1:10"),
      numericInput("test_idx", "Index of test subject:", value = 11, min = 1, max = 50),
      actionButton("stop_app", "Exit App", class = "btn-danger")
    ),
    mainPanel(
      plotOutput("growthPlot")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$stop_app, {
    stopApp()
  })

  output$growthPlot <- renderPlot({
    dat <- doBy::child_growth
    bb  <- dat |> filter(gender == "boy")
    sub <- unique(bb$subject)

    # Evaluate train indices safely
    idx.train <- tryCatch(eval(parse(text = input$train_idx)), error = function(e) NULL)
    if (is.null(idx.train) || any(idx.train < 1 | idx.train > length(sub))) return(NULL)

    idx.test <- input$test_idx
    if (idx.test < 1 || idx.test > length(sub)) return(NULL)

    train <- bb |> filter(subject %in% sub[idx.train])
    this <- bb |> filter(subject %in% sub[idx.test])
    this.early <- this |> filter(age <= input$early_time)

    fit.list <- tryCatch(train |> lm_by(height ~ age + I(age^2) | subject, data = _), error = function(e) NULL)
    if (is.null(fit.list)) return(NULL)

    coef.df <- fit.list |> coef()
    sig <- fit.list |> sigma()
    m <- colMeans(coef.df)
    C <- cov(coef.df)
    sd <- mean(sig)
    pr0 <- list(m = m, C = C, sd = sd)

    bench <- lmb(height ~ age + I(age^2), data = this)
    early <- lmb(height ~ age + I(age^2), data = this.early)
    lmb1 <- lmb(height ~ age + I(age^2), data = this.early, prior = pr0)
    lmb2 <- lmb(height ~ age + I(age^2), data = this.early, prior = list(m = m), alpha = input$alpha)

    this <- this |> add_pred(bench, "bench")
    this <- this |> add_pred(early, "early")
    this <- this |> add_pred(lmb1, "lmb1")
    this <- this |> add_pred(lmb2, "lmb2")

    this.long <- this |> pivot_longer(-c(gender, age, subject, height))
    y_limits <- range(this$height, na.rm = TRUE)

    ggplot(this.long, aes(x = age)) +
      geom_vline(xintercept = input$early_time) + 
      geom_point(aes(y = height)) +
      geom_line(aes(y = value, color = name, linetype = name), linewidth = 1) +
      coord_cartesian(ylim = y_limits) +
      theme_minimal()
  })
}

shinyApp(ui, server)
