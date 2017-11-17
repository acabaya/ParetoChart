library(shiny)
library(dplyr)
library(ggplot2)
library(markdown)

ui <- fluidPage(
navbarPage("Pareto Chart", theme = "cosmo",
    
    tabPanel(p(icon("database"), "App"),
            
        sidebarLayout(
            
            sidebarPanel(
                tabsetPanel(
                    tabPanel("Inputs",
                        fileInput("datafile", "Choose csv file to upload", 
                              multiple = FALSE, accept = c("csv")),
                        radioButtons("dataType", "Action: ",
                            c("None" = "None", 
                              "Summarize" = "Aggregate",
                              "Pivot" = "Pivot"),
                            selected="None"),
                        uiOutput("xcol"),
                        conditionalPanel("input.dataType=='None'",
                            uiOutput("ycol")),
                        conditionalPanel("input.dataType=='Pivot'",
                            uiOutput("var"),
                            radioButtons("pivotType", "Pivot: ",
                                c("Sum" = "Sum", "Mean" = "Mean"),
                                selected="Sum")
                        )
                    ),
                    
                    tabPanel("Graph Options",
                        textInput('main', 'Title', value = ""),
                        textInput('ylab', 'Y axis label', value = ""),
                        textInput('xlab', 'X axis label', value = ""),
                        
                        selectInput('barcol', label ='Bar Color',
                            choices=c("White"="white", 
                                      "Pale Green" ="palegreen",
                                      "Powder Blue"="powderblue", 
                                      "Indian Red"="indianred", 
                                      "Dark Gray"="grey90",
                                      "Slate Gray"="slategray3"),
                            multiple=FALSE, selectize=TRUE,selected="gray97"),
                        
                        selectInput('linecol', label ='Line Color',
                            choices=c("Gray" ="gray97",
                                      "White"="white",
                                      "Dark Gray"="grey90"),
                            multiple=FALSE, selectize=TRUE,selected="gray97")
                    ),
                    
                    tabPanel("Download Options",
                        h5(HTML("Downloading Plots")),
                        selectInput(
                            inputId = "downloadPlotType",
                            label = h5("Select download file type"),
                            choices = list("PNG"="png", "PDF"="pdf","BMP"="bmp","JPEG"="jpeg")
                        ),
                        # Allow the user to set the height and width of the plot download.
                        h5(HTML("Set download image dimensions<br>(units are inches for PDF, pixels for all other formats)")),
                        numericInput(
                            inputId = "downloadPlotHeight",
                            label = "Height (inches/pixels)",
                            value = 600,min = 1,max = 1000),
                        numericInput(
                            inputId = "downloadPlotWidth",
                            label = "Width (inches/pixels)",
                            value = 800,min = 1,max = 1000),
                        # Choose download filename.
                        textInput(
                            inputId = "downloadPlotFileName",
                            label = h5("Enter file name for download")),
                        # File downloads when this button is clicked.
                        downloadButton(outputId="downloadPlot", label="Download Plot"),
                        hr(),
                        h5(HTML("Donloading Tables")),
                        # File downloads when this button is clicked.
                        downloadButton(outputId="downloadTable", label="Download Table"),
                        h5(HTML("Tables are saved in csv format.")),
                        textInput(
                            inputId = "downloadTableFileName",
                            label = h5("Enter file name for download"))
                    )
                ) 
            ), # sidebarPanel
            
            
            mainPanel(
                tabsetPanel(
                    tabPanel("Plot",
                        plotOutput('plot', width="100%", click="plot_click",
                            hover=hoverOpts(id="plot_hover", delayType="throttle"),
                            brush=brushOpts(id="plot_brush"))
                    ), # tabPanel Plot
                    
                    tabPanel('Numeric Summary',
                        tableOutput("result")     
                    ), # tabPanel Numeric Summary
                    
                    tabPanel("Uploaded Data",
                        tableOutput("data")   
                    ) # tabPanel Uploaded Data
                ) # tabsetPanel
            ) # mainPanel
        ) # sidebarLayout
    ), # tabPanel App
    tabPanel(p(icon("question"), "How-To"), includeMarkdown("howto.md")),
    tabPanel(p(icon("info"), "About"), includeMarkdown("about.md"))
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    filedata <- reactive({
        infile <- input$datafile
        if (is.null(infile)) {
            return(NULL)
        }
        read.csv(infile$datapath,na.strings = c("NA","."))
    })
    
    output$xcol <- renderUI({
        df <-filedata()
        if (is.null(df)) return(NULL)
    
        items=names(df)
        names(items)=items
        selectInput("x", "Category:",items)
    })
  
    output$ycol <- renderUI({
        df <-filedata()
        if (is.null(df)) return(NULL)
        items=names(df)
        names(items)=items
        selectInput("y", "Frequency:",items)
    })

    output$var <- renderUI({
        df <-filedata()
        if (is.null(df)) return(NULL)
        items=names(df)
        names(items)=items
        selectInput("v", "Variable:",items)
    })
        
    output$summary <- renderPrint({
        dfl <- filedata()
        if(is.null(dfl)) return(NULL)
        r <- recodedata()
        r
    })

    recodedata  <- reactive({
        df <- filedata()
        if (is.null(df)) return(NULL)
        
        if (dataType() == "Aggregate") {
            xvar <- df[,input$x]
            x <- data.frame(category=xvar)
            colnames(x) <- "category"
            
            df <- x %>% 
                group_by(category) %>% 
                summarise(count=n()) %>% 
                arrange(desc(count))
            colnames(df) <- c("category", "count")
        } else if (dataType() == "Pivot") {
            xvar <- df[,input$x]
            yvar <- df[,input$v]
            df <- data.frame(category=xvar, count=yvar)
            
            if (pivotType() == "Sum") {
                df <- df %>% 
                    group_by(category) %>% 
                    summarise(count=sum(count)) %>% 
                    arrange(desc(count))
                
            } else if(pivotType() == "Mean") {
                df <- df %>% 
                    group_by(category) %>% 
                    summarise(count=mean(count)) %>% 
                    arrange(desc(count))
            } else {

                 df <- df %>% 
                    group_by(category) %>% 
                    summarise(count=sum(count)) %>% 
                    arrange(desc(count)) 
                 
                 colnames(df) <- c("category", "count")
            }
        } else {
            xvar <- df[,input$x]
            yvar <- df[,input$y]
            df <- data.frame(category=xvar, count=yvar)
            #colnames(df) <- c("category", "count")
        }
    
        #colnames(df) <- c("category", "count")

        df <- df[!is.na(df$count),]  
        title = input$xcol
        
        df <- arrange(df, desc(count))
        
        df$category <- ordered(df$category, 
            levels=unlist(df$category, use.names=FALSE))
        
        if(!is.numeric(df$count)) return(NULL)
        df <- df %>% 
            mutate(rank = as.integer(category), 
            cum.freq = cumsum(count), 
            cum.perc=round(cum.freq/sum(count)*100,1))
    })
        
    plotObject <- reactive({
        plotData <- recodedata()
        
        nr <- nrow(plotData)
        if (!is.numeric(plotData$count)) return(NULL)
        N <- max(plotData$count, na.rm = TRUE)
        px <- plotData$cum.perc/100 * max(plotData$count, na.rm = TRUE)
        
        Df_ticks <- data.frame(xtick0 = rep(nr +.55, nr), 
            xtick1 = rep(nr +.59, nr), ytick = px)
        rect1 <- data.frame(xmin=nr+0.55, xmax=nr+1, ymin=-0.1*N, ymax=1.1*N)
        
        g <- ggplot(plotData, aes(x=category, y=count), fill="white")

        if (input$main!="") {
            g <- g + labs(title = input$main)
        } else {
            g <- g + labs(title = paste("Pareto Chart of ", input$x))
        }
        
        if (input$xlab!="") {
            g <- g + xlab(input$xlab)
        } else {
            g <- g + xlab(input$x)
        }
        
        if (input$ylab!="") {
                g <- g + ylab(input$ylab)
        } else {
            if (dataType() == "Pivot"){
                yl <- paste(pivotType(), "of", input$v)
                g <- g + ylab(yl)
            } else {
                g <- g + ylab("Absolute Count")
            }
        }

        g <- g + geom_bar(stat = "identity", aes(fill=rank), 
                          fill = input$barcol, colour = "black")
        g <- g + geom_line(aes(x=rank, y=px, color=rank))
        g <- g + geom_point(aes(x = rank, y = px,color = rank), pch = 19)
        g <- g + scale_y_continuous(breaks=c(0, plotData$count), limits=c(-0.1*N, 1.1*N))
        g <- g + scale_x_discrete(breaks = plotData$category)
        g <- g + guides(fill = FALSE, color = FALSE)
        g <- g + theme(panel.background = element_rect(fill = 'white', colour = 'white'),
                  panel.grid.major = element_line(colour = "grey95"))
        
        g <- g + geom_rect(data=rect1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                fill="white", inherit.aes = F)
        g <- g + annotate("text", x=nr+0.8, y=px,label=plotData$cum.perc,size=4)
        g <- g + geom_segment(x = nr+0.55, xend=nr+0.55, y = -0.1 * N, 
                yend = N * 1.1, color = "grey50")
        g <- g + geom_segment(x = .4, xend=.4, y = -.1 * N, 
                yend = N * 1.1, color = "grey50")
        g <- g + geom_segment(x=0.4, xend=nr+0.55, y = 0.8 * N, 
                yend = 0.8* N, color = "red")
        g <- g + geom_segment(x = .4, xend=nr+0.55, y = -0.1 * N, 
                yend = -0.1 * N, color = "grey50")
        g <- g + geom_segment(x = .4, xend=nr+0.55, y = N * 1.1, 
                yend = 1.1 * N, color = "grey50")
        g <- g + geom_segment(data = Df_ticks, aes(x = xtick0, y = ytick, 
                xend = xtick1, yend = ytick))
        g <- g + theme(axis.title=element_text(family="Trebuchet MS", 
                color="#666666", face="bold", size=18)) +
                theme(plot.title = element_text(family = "Trebuchet MS", 
                color="#666666", face="bold", size=22, hjust=0))
        
        g <- g + theme(axis.text.x=element_text(angle=45, hjust=1))
            
        return(g)
        #return(list(graph = g, Df = Df[, c(3, 1, 2, 4, 5)]))   
    })

    downloadPlotType <- reactive({
      input$downloadPlotType  
    })
    
    dataType <- reactive({
      input$dataType  
    })
    
    pivotType <- reactive({
      input$pivotType  
    })
        
    # Get the download dimensions.
    downloadPlotHeight <- reactive({
        input$downloadPlotHeight
    })
    
    downloadPlotWidth <- reactive({
        input$downloadPlotWidth
    })
    
    # Get the download file name.
    downloadPlotFileName <- reactive({
        input$downloadPlotFileName
    })

    # Get the download file name.
    downloadTableFileName <- reactive({
        input$downloadTableFileName
    })
      
    output$plot <- renderPlot({
        dfl <-filedata()
        if (is.null(df)) return(NULL)
        
        plotObject()
    })
    
    output$result <- renderTable({
        recodedata()
    })
    
    output$data <- renderTable({
        df <-filedata()
        df
    })
    
    # Include a downloadable file of the plot in the output list.
    output$downloadPlot <- downloadHandler(
        filename = function() {
            paste(downloadPlotFileName(), downloadPlotType(), sep=".")   
        },
        content = function(con) {
            plotFunction <- match.fun(downloadPlotType())
            plotFunction(con, width = downloadPlotWidth(), height = downloadPlotHeight())
            print(plotObject())
            dev.off(which=dev.cur())
            
        }
    )
    
    output$downloadTable <- downloadHandler(
        filename = function() {
            paste(downloadTableFileName(), "csv", sep=".")   
        },
        content = function(file) {
            write.csv(recodedata(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)