library(shiny)
library(ggplot2)

fluidPage(
    titlePanel("Genetic Drift Simulator"),
    sidebarLayout(
        sidebarPanel(
            # Input: select from menu
            numericInput(inputId = "population_size",
            label = "Population size:",
            value = 10,
            step = 10),
            sliderInput(inputId = "initial_frequency",
            label = "Initial frequency of allele 1:",
            min = 0,
            max = 1,
            step = 0.1,
            value = 0.5),
            actionButton(inputId = "run",
            label = "Run simulation"),
            actionButton(inputId = "reset",
            label = "Reset values")
        ),
        mainPanel(
            fluidRow(
                column(4,
                    verbatimTextOutput("text")
                )
            ),
            fluidRow(
                column(8,
                    plotOutput("pop_plot")
                )
            ),
            fluidRow(
                column(8,
                    plotOutput("freq_plot")
                )
            )
        )
    )
)
