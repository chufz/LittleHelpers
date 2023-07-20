library(shiny)
library(plotly)
library(scales)
library(MsCoreUtils)
#######################################################################
#' Function for generating a plotly headtail plot for one query in mtch object
plotly_headtail <- function(x_peaks, y_peaks, xLabel = "query", xColor = "#E41A1C",
                                 yLabel = "target", yColor =  "#377EB8", matchSize = 5,
                                 ppm = 20, tolerance = 0) {
    x_peaks$intensity <- rescale(x_peaks$intensity, to=c(0,100))
    y_peaks$intensity  <- rescale(y_peaks$intensity, to=c(0,100))
    p <- plotly::plot_ly()
    x_range <- range(x_peaks$mz, y_peaks$mz, na.rm = TRUE) + c(-1, 1)
    y_max <- max(x_peaks$intensity, y_peaks$intensity, na.rm = TRUE)
    y_peaks$intensity <- -y_peaks$intensity
    
    ht <- "<b>%{text}</b><br>mz: %{x}<br>int: %{y}"
    if (nrow(x_peaks)) {
        x_peaks$zero <- 0.0
        x_peaks$match <- ""
        x_peaks$color <- xColor[1L]
        idx <- which(common(x_peaks$mz, y_peaks$mz, tolerance, ppm))
        if (length(idx))
            x_peaks$match[idx] <- "matched"
        p <- add_segments(p, data = x_peaks, x = ~mz, y = ~zero, xend = ~mz,
                     yend = ~intensity, line = list(color = xColor[1L]),
                     name = xLabel, hovertemplate = ht)
    }
    if (nrow(y_peaks)) {
        y_peaks$zero <- 0.0
        y_peaks$match <- ""
        y_peaks$color <- yColor[1L]
        idx <- which(common(y_peaks$mz, x_peaks$mz, tolerance, ppm))
        if (length(idx))
            y_peaks$match[idx] <- "matched"
        p <- add_segments(p, data = y_peaks, x = ~mz, y = ~zero, xend = ~mz,
                     yend = ~intensity, line = list(color = yColor[1L]),
                     name = yLabel, hovertemplate = ht)
    }
    pks <- rbind(x_peaks, y_peaks)
    pks <- pks[pks$match != "", , drop = FALSE]
    if (nrow(pks))
        p <- plotly::add_trace(p, data = pks, x = ~mz, y = ~intensity,
                               type = "scatter", mode = "markers",
                               hoverinfo = "none", name = "matched",
                               marker = list(size = matchSize[1L],
                                             color = ~color))
    plotly::layout(p, xaxis = list(title = "m/z", zeroline = FALSE),
                   yaxis = list(title = "intensity", zeroline = TRUE),
                   hovermode = "x", hoverdistance = 1)
}
#######################################################################
ui <- fluidPage(titlePanel(shiny::div("MS2plotteR")),
                sidebarLayout(
                    sidebarPanel(
                        textAreaInput("box1", "Upper Spectra", value = "", width = '100%', rows = 15, resize = "both"),
                        textAreaInput("box2", "Lower Spectra", value = "", width = '100%', rows = 15, resize = "both"),
                        radioButtons("format","Lower Spectra format:", choices = c("Text"="a", "MassBank"="b")),
                        actionButton("b_plot", "Plot")
                     ),
                    mainPanel(
                        plotlyOutput("plot")
                    ))
                )
                     
server <- function(input, output, session) {

    observeEvent(input$b_plot,{
        B1 <- read.table(text=input$box1, sep=" ", col.names=c("mz","intensity"))
        if(input$format == "a"){
            B2 <- read.table(text=input$box2, sep=" ", col.names=c("mz","intensity"))
        }
        if(input$format == "b"){
            B2 <- read.table(text=input$box2, sep=" ")[,3:4]
            colnames(B2) <- c("mz","intensity")
        }
        
        output$plot <- plotly::renderPlotly(
            plotly_headtail(B1,B2, ppm = 20, tolerance = 0))
    })
}
#######################################################################
# Run the application
shinyApp(ui, server)
