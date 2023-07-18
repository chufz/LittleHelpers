library(shiny)
library(plotly)
library(scales)
#######################################################################
#' Function for generating a plotly headtail plot for one query in mtch object
plotly_headtail <- function(top, bottom){
    # get relative intensities
    top$V2 <- rescale(top$V2, to=c(0,100))
    bottom$V2  <- rescale(bottom$V2, to=c(0,100))
    # create layout
    layout <- list(
        title = "",
        xaxis = list(title = "m/z",
                     zeroline=TRUE,
                     range=c(0, max(top$V1)),
                     nticks=8,
                     autorange = TRUE),
        yaxis = list(title = "Signal Intensity [%]",
                     zeroline=TRUE,
                     tickmode='array',
                     tickvals=c(-100, -50, 0, 50, 100),
                     ticktext=c('100','50', '0', '50', '100'))
    )
    
    # create plot
    p <- plot_ly(
        top,
        x =  ~ V1,
        y =  ~ V2,
        showlegend = F,
        type = 'bar',
        marker = list(size = 3, color = 'red'),
        hoverinfo = 'none'
    )
    
    p <- add_markers(
            p,
            type = "scatter",
            x = top$V1,
            y = top$V2,
            hovertemplate = paste('<br>mz:', '%{x}', '<br>int: %{y}<br>'),
            hoverlabel = list(namelength = 0)
        )

    p <- add_trace(
                p,
                type = "bar",
                x = bottom$V1,
                y = -bottom$V2,
                marker = list(color = 'blue'),
                hoverinfo = 'none'
            )
        
    p <- add_markers(
                p,
                x = bottom$V1,
                y = -bottom$V2,
                type = 'scatter',
                marker = list(color = 'blue'),
                hovertemplate = paste('<br>mz:', '%{x}', '<br>int: %{y}<br>'),
                hoverlabel = list(namelength = 0)
            )
    
    p <- layout(
            p,
            title = layout$title,
            xaxis = layout$xaxis,
            yaxis = layout$yaxis
        )
    
    p <- add_annotations(
            p,
            type = 'text',
            x = c(15, 15),
            y = c(100, -100),
            text = c("", ""),
            textfont = list(color = c('red', 'blue')),
            showarrow = F
        )
    
    p <- p %>% layout(hovermode = "x", hoverdistance = 1)
    
    # return plot
    p
}
#######################################################################
ui <- fluidPage(titlePanel(shiny::div("MS2plotteR")),
                sidebarLayout(
                    sidebarPanel(
                        textAreaInput("box1", "Upper Spectra", value = "", width = '100%', rows = 15, resize = "both"),
                        textAreaInput("box2", "Lower Spectra", value = "", width = '100%', rows = 15, resize = "both"),
                        actionButton("b_plot", "Plot")
                     ),
                    mainPanel(
                        plotlyOutput("plot")
                    ))
                )
                     
server <- function(input, output, session) {
    
    observeEvent(input$b_plot,{
        B1 <- read.table(text=input$box1, sep=" ")
        B2 <- read.table(text=input$box2, sep=" ")

        output$plot <- plotly::renderPlotly(
            plotly_headtail(B1,B2))
    })
}
#######################################################################
# Run the application
shinyApp(ui, server)
