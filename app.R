# ADNANE Marouane

library(shiny)
library("ggplot2")
library("corrplot")
library("PerformanceAnalytics")
library("FactoMineR")


  ui <- shinyUI(fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        
        radioButtons("radio", h3("choose the type of exploring data"), 
                     choices = list("ploting data"=1,
                                    "Kmeans"=2,
                                    "PCA"=3)),
        uiOutput('type_plot'),
        uiOutput('analyse'),
        uiOutput("obs"),
        uiOutput("xcol"),
        uiOutput("ycol"),
        uiOutput("count_cl")
      ),
      mainPanel(
        tableOutput("contents"),
        textOutput('test'),
        plotOutput('plot')

      )
    ) 
    
    
    
  ))
  
  server <- function(input, output) {
   
    datafile <- reactive({
      df <- input$file1
      
      if(is.null(df)){return(NULL)}
      
      tmp <- read.csv(df$datapath, header = input$header, sep = ",")
      return(tmp)
    })
    
    output$contents <- renderTable({
      df <- datafile()
      if (is.null(df))
        return(NULL)
      
      head(df)
      
    })
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
      df <- datafile()
      if (is.null(df))
        return(NULL)
      df[, c(input$xcol, input$ycol)]
    })
    
    clusters <- reactive({
      kmeans(selectedData(),input$clusters)
    })
    
     output$type_plot <- renderUI({
       df <- datafile()
       if(is.null(df)) {return(NULL)}
       if(input$radio == 1){
         selectInput('your_plot','select a plot',choices = list("histograme" = 1,
                                                              "scatterplot" =2,
                                                              "boxplot" =3))
       
       }else{return(NULL)}
       
     })
     
     
     output$analyse <- renderUI({
       if(input$radio == 3){
         selectInput('anal','choices',list("corrplot" = 1,
                                           "char_corr" =  2,
                                           "var_fac_map" = 3,
                                           "box_plot_var" = 4))
       } 
     })
     

     output$xcol<-renderUI({
       df <- datafile()
       if(is.null(df)) {return(NULL)}
      if(input$radio ==1|input$radio ==2){
        selectInput('xcol', 'X Variable', names(df[,2:6]), selected = names(df)[[3]])        
      } 
       
    })
     
    output$ycol<-renderUI({
      df <- datafile()
      
      if(is.null(df)) {return(NULL)}
      if(input$radio == 1|input$radio == 2){
        selectInput('ycol', 'Y Variable', names(df[,2:6]), selected = names(df)[[2]])
      }
    })
    
    output$count_cl<-renderUI({
      if(input$radio ==2){
        numericInput('clusters', 'count cluster', 2, min = 1, max = 9)
      }
    })
    
observeEvent(input$radio,{
  if(input$radio == 1 ){
    output$plot <- renderPlot({
      
      df <- datafile()
      if (is.null(df)){return(NULL)}
      
      if(input$your_plot == 1){
        hist(df[[input$xcol]],xlab = input$xcol)
        
      }
      else if(input$your_plot == 2){
        ggplot(df,aes_string(x = input$xcol, y = input$ycol)) + geom_point()
      }
      
      else if(input$your_plot == 3){
        ggplot(df,aes_string(x = input$xcol, y = input$ycol))+
          geom_boxplot()+
          ylab("grade")
      }
      
    })
    

  }else if(input$radio == 2 ){
    output$plot <- renderPlot({
      df <- datafile()
      if (is.null(df))
        return(NULL)
      
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
      par(mar = c(5.1, 4.1, 0, 1))
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
  }else{
    
    
    output$plot <- renderPlot({
      
      df <- datafile()
      if(is.null(df)){return(NULL)}
      res.pca <- PCA(df[,2:5], graph = FALSE)
      eigenvalues <- res.pca$eig
      cor.mat <- round(cor(df[,2:5]),2)
      if(input$anal == 1){
        corrplot(cor.mat, type="upper", order="hclust",
                 tl.col="black", tl.srt=45)

      }else if(input$anal == 2){
        chart.Correlation(df[, 2:5], histogram=TRUE, pch=19)
        
      }else if(input$anal == 3){

        plot(res.pca, choix = "var")
        
      }else if(input$anal == 4){
        barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues),
                main = "Variances",
                xlab = "Principal Components",
                ylab = "Percentage of variances",
                col ="steelblue")
        # Add connected line segments to the plot
        lines(x = 1:nrow(eigenvalues), eigenvalues[, 2],
              type="b", pch=19, col = "red")
      }

    })
    
  }
  
})

  }
  
  shinyApp(ui, server)
