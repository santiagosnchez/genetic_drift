library(shiny)
library(ggplot2)

# function to make a circle
# https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
circleFun <- function(center=c(0,0), diameter=10, npoints=100){
    r = diameter / 2
    tt = seq(0,2*pi,length.out = npoints)
    xx = center[1] + r * cos(tt)
    yy = center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
}


function(input, output, session) {

    # Anything that calls autoInvalidate will automatically invalidate.
    autoInvalidate <- reactiveValues(timer=NULL)

    returns <- reactiveValues(
        z=NULL,
        x=NULL,
        y=NULL,
        freq=NULL,
        circle=NULL,
        i=NULL
    )

    frequencies <- reactiveValues(df=NULL)

    observeEvent(list(input$population_size,input$initial_frequency), {
        returns$z=rbinom(input$population_size, 1, input$initial_frequency)
        returns$x=rnorm(input$population_size)
        returns$y=rnorm(input$population_size)
        returns$freq=input$initial_frequency
        returns$circle=circleFun()
        returns$i=0

        frequencies$df = data.frame(x=returns$i, y=returns$freq)

        autoInvalidate$timer = reactiveTimer(Inf)

    })

    population <- reactive({
        data.frame(x=returns$x, y=returns$y, z=returns$z)
    })

    grow_freq <- function(df, x, y){
        rbind(df, c(x,y))
    }

    grow <- reactive({
        frequencies$df <- grow_freq(frequencies$df, returns$i, returns$freq)
    })

    drift <- reactive({
        returns$z = sample(returns$z, replace=T)
        # random locations
        returns$x = rnorm(input$population_size)
        returns$y = rnorm(input$population_size)
        # calculate frequency
        returns$freq = sum(returns$z == 1)/input$population_size
        # increase to next generation
        returns$i = returns$i+1
    })

    observeEvent(input$run, {
        autoInvalidate$timer = reactiveTimer(100) # changed to 1 second
        drift()
        grow()
    })

    observeEvent(autoInvalidate$timer(), {
        if (returns$freq < 1 & returns$freq > 0 & returns$i != 0){
            autoInvalidate$timer()
            drift()
            grow()
        }
        # else if (returns$freq == 0 | returns$freq == 1) {
        #     autoInvalidate$timer = reactiveTimer(Inf)
        # }
    })

    observeEvent(input$reset, {
        returns$z = rbinom(input$population_size, 1, input$initial_frequency)
        returns$x = rnorm(input$population_size)
        returns$y = rnorm(input$population_size)
        returns$freq = input$initial_frequency
        returns$circle=circleFun(center=c(0,0), diameter=10, npoints=100)
        returns$i = 0

        frequencies$df = data.frame(x=returns$i, y=returns$freq)

        autoInvalidate$timer = reactiveTimer(Inf)
    })

    output$text <- renderText({
        #autoInvalidate$timer()
        text = paste("Population size: ",input$population_size,"\n",
        "Frequency allele 1: ",returns$freq,"\n",
        "Generation: ",returns$i, sep="")
        print(text)
    })

    output$pop_plot <- renderPlot({
        #autoInvalidate$timer()
        ggplot(data=population(), aes(x, y)) +
        geom_point(aes(color=factor(z)), size=5, alpha=0.7) +
        geom_path(data=returns$circle, color="black", size=2) +
        scale_color_brewer(type="qual", palette=1, name="allele") +
        theme(axis.title=element_blank(), axis.text=element_blank()) +
        theme(legend.title=element_text(size=16), legend.text=element_text(size=14))
    },
    height = 400, width = 450)

    output$freq_plot <- renderPlot({
        #autoInvalidate$timer()
        if (dim(frequencies$df)[1] == 1){
            ggplot(data=frequencies$df, aes(x, y)) +
            geom_hline(yintercept=0.5) +
            geom_point() +
            labs(x="generation",y="frequency") +
            ylim(0,1)
        } else {
            ggplot(data=frequencies$df, aes(x, y)) +
            geom_hline(yintercept=0.5) +
            geom_point() +
            geom_line() +
            labs(x="generation",y="frequency") +
            ylim(0,1)
        }
    },
    height = 300, width = 500)

}
