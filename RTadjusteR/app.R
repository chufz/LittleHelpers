library(shiny)
library(shinyWidgets)
library(plotly)
library(RaMS)
library(shinyFiles)
library(shinyjs)
library(data.table)
library(DT)
library(readr)
#######################################################################
#starter values
file_selection <- ""
#function defining mz range
pmppm <- function(mass, ppm=4)c(mass*(1-ppm/1000000), mass*(1+ppm/1000000))
#function getting MS1 from mzml
get_data <- function(file){
    if(length(file)==1){
        data <- grabMzmlData(file, grab_what = "MS1")$MS1
    }else{
        data <- grabMSdata(file, grab_what = "MS1")$MS1
    }
    return(data)
}
# Function for generating a plotly chromatogram plot
plotly_chrom <- function(data, mass, ppm, rt=NULL){
    if(length(unique(data$filename))==0){
        EIC_list <- lapply(unique(mass), function(mass){
            data[mz%between%pmppm(mass = mass, ppm = ppm)]
        })
        EIC_list <- unique(rbindlist(EIC_list))
        if(length(rt)==0){
            fig <- plot_ly(EIC_list, x = ~rt, y = ~int, type = 'scatter', mode = 'lines', source = "A")
        }else{
            fig <- plot_ly(EIC_list, x = ~rt, y = ~int, type = 'scatter', mode = 'lines', source = "A")%>% 
                add_lines(x = rt, line = list(color= "grey", widthh=0.5, dash="dot"), showlegend = FALSE)%>%
                event_register("plotly_doubleclick")
        }
     
    }else{
        EIC_list <- lapply(unique(mass), function(mass){
            data[mz%between%pmppm(mass = mass, ppm = ppm)]
        })
        EIC_list <- unique(data.table::rbindlist(EIC_list))
        if(length(rt)==0){
            fig <- plot_ly(EIC_list, x = ~rt, y = ~int, color= ~filename, type = 'scatter', mode = 'lines', source = "A")
        }else{
            fig <- plot_ly(EIC_list, x = ~rt, y = ~int, color= ~filename, type = 'scatter', mode = 'lines', source = "A")%>%
                add_lines(x = rt, line = list(color= "grey", widthh=0.5, dash="dot"), showlegend = FALSE)%>%
                event_register("plotly_doubleclick")
        }
    }
}
#######################################################################
# Define the Graphics
ui <-   navbarPage(title="EIC",
        tabPanel(title="Chromatogram",
            # left side for file upload     
            sidebarPanel(
                width = 3,
                div(
                    img(src='ufz.png', width = "180px", align = "right")
                ),
                br(), br(),
                p(strong("1. Select MZML directory:")),
                shinyDirButton('newfiles', label='choose', title='Chose the MZML directory that appears in the list', viewtype = "detail"),
                br(), br(),
                selectInput('filelist', '2. Select Samples:', file_selection, multiple=TRUE, selectize=FALSE, size=15),
                actionButton(
                    inputId = "selectall", label = "select all",
                    style = "padding:12px; font-size:70%"
                ),
                br(), br(),
                textInput("pattern", label="Select by pattern:", value = "", width = NULL, placeholder = ""),
                actionButton(
                    inputId = "pattern_button", label = "select pattern",
                    style = "padding:12px; font-size:70%"
                ),
                br(), br(),
                actionButton(
                    inputId = "load", label = strong("3. Load data"),
                    style = "padding:12px; font-size:70%"
                ),
                br(), br()
            ),
            # main panel with EIC
            mainPanel(
                width = 6,
                tabsetPanel(
                    type = "tabs",
                    tabPanel("EIC",
                             id = "panel1",
                             plotlyOutput("chrom"),
                             verbatimTextOutput("selection"),
                             dataTableOutput("dynamic")
                    ),
                )
            ),
            # right panel for Targets
            sidebarPanel(
                width = 3,
                sliderInput("trans", "ppm",
                            min = 0, max = 80, value = 15
                ),
                textInput("mzlist", label="m/z value:", value = "", width = NULL, placeholder = NULL),
                actionButton("plot", "plot value"),
                br(), br(), 
                fileInput("csvfile", '4. Upload csv file with m/z values:', accept=".csv"),
                textInput("mzcol", label="m/z Col Nr.", value = "4", width = 40, placeholder = NULL),
                textInput("rtcol", label="RT Col Nr.", value = "5", width = 40, placeholder = NULL),
                downloadButton("d_store", "5. Save csv with new RT values")
            )
        )
)
#######################################################################
# Define the server reaction:
server <- function(input, output) {
    q <- reactiveValues(mz=142.0448, ppm=20, data=list(), table=data.frame(), mzcol=1, rtcol=4, rt=0) #dummy variables added
    # chose from local file system 
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    shinyDirChoose(input, 'newfiles', root=volumes)
    # if new directory is selected, generate a new filelist
    observeEvent(input$newfiles,{
        if (is.integer(input$newfiles)) {
            cat("Choose directory: ")
        } else {
        q$filepath <- parseDirPath(volumes, input$newfiles)
        print(q$filepath)
        file_selection <- list.files(path = q$filepath, pattern=".mzML$")
        updateSelectInput(inputId="filelist", choices=file_selection)
        }
    })
    # if a file is selected
    observeEvent(input$filelist,{
        q$file <- paste0(parseDirPath(volumes, input$newfiles), "/", input$filelist)
    })
    # load in data by button
    observeEvent(input$load,{
        q$data <- get_data(q$file)    
    })
    # if a new ppm value is entered, change the ppm
    observeEvent(input$trans,{
        q$ppm <- as.numeric(input$trans)
    })
    # if button "plot new"
    observeEvent(input$plot,{
        q$mz <- as.numeric(input$mzlist)
        output$chrom <-  renderPlotly(plotly_chrom(data=q$data, mass=q$mz, ppm=q$ppm))
    })
    # if select all
    observeEvent(input$selectall,{
    q$file <- list.files(path = q$filepath, pattern=".mzML$", full.names = TRUE)
    updateSelectInput(inputId="filelist", selected=basename(q$file))
    })
    # if a pattern is added
    observeEvent(input$pattern_button,{
    q$file <- list.files(path = q$filepath, pattern=".mzML$", full.names = TRUE)[grep(input$pattern, list.files(path = q$filepath, pattern=".mzML$", full.names = TRUE))]
    updateSelectInput(inputId="filelist", selected=basename(q$file))
    })
    # if csv file uploaded
    observeEvent(input$csvfile,{
    q$table <- readr::read_csv(input$csvfile$datapath)
    output$dynamic <- renderDT(DT::datatable(q$table,
                                             selection = "single"),server = TRUE)
    })
    
    # preserve the column names
    
    q$column_names <- colnames(q$table)
    
    # if mz col value is changed
    observeEvent(input$mzcol,{
        q$mzcol <- input$mzcol
    })
    # if rt col value is changed
    observeEvent(input$rtcol,{
        q$rtcol <- input$rtcol
    })
    # if clicked on value in table
    observeEvent(input$dynamic_rows_selected, {
        if(!is.na(as.numeric(q$table[input$dynamic_rows_selected,as.numeric(q$mzcol)]))){
            q$mz <- as.numeric(q$table[input$dynamic_rows_selected,as.numeric(q$mzcol)])
        }
        if(!is.na(as.numeric(q$table[input$dynamic_rows_selected,as.numeric(q$rtcol)]))){
            q$rt <- as.numeric(q$table[input$dynamic_rows_selected,as.numeric(q$rtcol)])
        }
        output$chrom <-  renderPlotly(plotly_chrom(data=q$data, mass=q$mz, ppm=q$ppm, rt=q$rt))
    })
    # Get Rt in panel
    output$selection <- renderPrint({ if(q$rt==0){"Select RT value"}else{paste("RT=", round(q$rt,4), "min")} })
    # if double click on graph, get rt value
    p1 <- reactive({event_data("plotly_click", source = "A")})
    observeEvent(p1(),{ 
        q$rt <- as.list(p1())$x
        #new table
        q$table[input$dynamic_rows_selected,as.numeric(q$rtcol)] <- round(q$rt,2)
        #output$dynamic <- renderDataTable({})
        })
    # Store Result for uploaded files
    output$d_store <- downloadHandler(
        filename = function() {
            paste0("new_RT.csv")
        },
        content = function(file) {
            # replace the column names by the preserved
            colnames(q$table) <- q$column_names
            
            # save depending on the name given from the commandline input
            readr::write_csv(q$table, file)
            message("New csv file with updated RT has beeen stored.")
        }
    )
}
#######################################################################
# Run the application
shinyApp(ui, server)
