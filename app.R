library(shiny)
#devtools::install_github("jrowen/rhandsontable", dependencies = T, upgrade_dependencies = T)
library(rhandsontable)
library(shiny)
library(dplyr)
library(ggplot2)
library(boot)
library(shinythemes)

enableBookmarking(store = "url")

DF = data.frame("Evidence"=LETTERS[1:5], "Relevance" = rep(0,5), "Source reliability" = rep(0,5), "Information reliability" = rep(0,5), "Support" = rep(NA,5), stringsAsFactors = FALSE)
DF_template = data.frame("Evidence"=LETTERS[1:5], "Relevance" = rep("",5), "Source reliability" = rep("",5), "Information reliability" = rep("",5), "Support" = rep("",5), stringsAsFactors = FALSE)

ui <- function(request){shinyUI(fluidPage(
  theme = shinythemes::shinytheme("paper"),  # <--- Specify theme here
  
  titlePanel("Ziggurat Plot"),
  sidebarLayout(
    sidebarPanel(
      helpText("Use this page to make a Ziggurat plot of your evidence."),
      
      h4(helpText("1.a Define Support Levels")),
      helpText("First, enter your levels of support below, in ascending order. For example, 'Refutes', 'Mixed', 'Some', 'Strong'. Use the buttons to add or remove levels as you please; you can have a minimum of 2 and a maximum of 4 levels"),
      uiOutput("textbox_ui"),
      actionButton("add_btn", "Add Support Level", style='padding:4px; font-size:80%'),
      br(),
      actionButton("rm_btn", "Remove Support Level", style='padding:4px; font-size:80%'),
      br(),
      h4(helpText("1.b Define minimum and maximum scores")),
      helpText("Provide the range of values below for which you wish to score evidence in terms of its relevance, source reliability, and information reliability, as well as the breaks between values (e.g., 0 to 3 in steps of 1)"),
      numericInput("minvalues","Minimum score:", 0),
      numericInput("maxvalues","Maximum score:", 3),
      numericInput("minbreaks","Steps of:", 1),
      br(),
      h4(helpText("Bookmarking")),
      helpText("On bookmarking your data, the URL will update. Copy this and you will be able to return to the app. On returning to the app, make sure to click add graph again to show your ziggurat plot again."),
      bookmarkButton(),
      br(),
      width=5
      
    ),
    
    mainPanel(
      h4(helpText("2. Add Data")),
      helpText("Then, add data to the table below (right-click to delete/insert rows). Remove any empty rows before clicking 'add graph'. You can bookmark your session at any time (scroll down to bottom left of screen)."),
      helpText("Alternatively, you can upload your data in a .csv file using the template below."),
      downloadButton("downloadDataTemplate", "Download Data Upload Template"), fileInput('file1', 'Choose CSV File'),
      br(),
      rHandsontableOutput("hot"),
      #textOutput('debug1'),
      br(),
      h4(helpText("3. Create Plot")),
      helpText("Once you're happy, generate a Ziggurat plot. You'll need to have at least two levels of support defined, and at least three rows of data. If you get an error, press this button again when you've fixed it."),
      actionButton("add_graph", "Add Graph"),
      br(),
      
      
      h3(textOutput("zigerror")),
      h3(textOutput("zigerror2")),
      plotOutput("ziggy"),
      br(),
      
      h4(helpText("4. Data Download")),
      helpText("If you want to download the data you've entered, hit here."),
      
      downloadButton("downloadData", "Download Data"),
      
      br(),
      br(),
      
      h4(helpText("5. Plot Download")),
      helpText("If you want to download an image of the plot, hit here."),
      
      downloadButton("downloadPlot", "Download Plot")
      
      , width=7)
  )
))
}

server <- function(input, output, session){
  values <- reactiveValues()
  
  cache_tbl = NULL
  
  onRestore(function(state) {
    tmp = state$input$hot
    tmp$data = jsonlite::fromJSON(
      jsonlite::toJSON(tmp$data), simplifyVector = FALSE)
    cache_tbl <<- tmp
  })
  
  # Handsontable
  observe({
    if (!is.null(input$hot)) {
      DF = hot_to_r(input$hot)
    } else if (!is.null(cache_tbl)) {
      DF = hot_to_r(cache_tbl)
      cache_tbl <<- NULL
    } else {
      if (is.null(values[["DF"]]))
        DF <- DF
      else
        DF <- values[["DF"]]
    }
    values[["DF"]] <- DF
  })
  
  observe({
    inFile = input$file1
    if (is.null(inFile))
      return(NULL)
    data1 = read.csv(inFile$datapath)
    DF <- data1
    values[["DF"]] <- DF
  })
  
  
  # defining dropdown options
  output$hot <- renderRHandsontable({
    dropdownOptions <- {
      n <- counter$n
      levs <- c(input$textin1, input$textin2)
      if(n == 3){
        levs <- append(levs, input$textin3)
      }
      if(n == 4){
        levs <- append(levs, input$textin3)
        levs <- append(levs, input$textin4)
      }
      levs
    }
    dropdownOptionsother <- {
      levsother <- seq(input[["minvalues"]],input[["maxvalues"]],by=input[["minbreaks"]])
      levsother
    }
    
    rhandsontable(values[["DF"]], stretchH = "all", overflow = 'visible') %>% #width="300px", 
      hot_col("Support", type = "dropdown", source = dropdownOptions) %>%
      hot_col("Relevance", type = "dropdown", source = dropdownOptionsother) %>%
      hot_col("Source.reliability", type = "dropdown", source = dropdownOptionsother) %>%
      hot_col("Information.reliability", type = "dropdown", source = dropdownOptionsother)
    
  })
  
  ## Download 
  output$downloadData <- downloadHandler(
    filename = "ZigguratData.csv",
    content = function(file) {
      write.csv(values[["DF"]], file, row.names = FALSE)
    }
  )
  ## Download template
  output$downloadDataTemplate <- downloadHandler(
    filename = "ZigguratDataTemplate.csv",
    content = function(file) {
      write.csv(DF_template, file, row.names = FALSE)
    }
  )
  
  ### Getting outcomes in text boxes
  # Track the number of input boxes to render
  counter <- reactiveValues(n = 4)
  
  #Track the number of input boxes previously
  prevcount <- reactiveValues(n = 4)
  
  observeEvent(input$add_btn, {
    if (counter$n < 4){
      counter$n <- counter$n + 1
      prevcount$n <- counter$n - 1
    }
  })
  
  observeEvent(input$rm_btn, {
    if (counter$n > 2) {
      counter$n <- counter$n - 1 
      prevcount$n <- counter$n + 1
    }
    
  })
  
  output$counter <- renderPrint(print(counter$n))
  SupportList <- c()
  
  textboxes <- reactive({
    n <- counter$n
    
    if (n > 0) {
      
      # If the no. of textboxes previously were more than zero, then 
      #save the text inputs in those text boxes 
      if(prevcount$n > 0){
        
        vals = c()
        if(prevcount$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcount$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpid = paste0("textin",i)
          vals[i] = input[[inpid]] 
        }
        if(isInc){
          vals <- c(vals, "")
        }
        
        if(vals[1]==""){
          vals <- c('Refutes','Mixed','Weakly Supports', 'Strongly Supports')
        }
        
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textin", i),
                    label = paste0("Support Level ", i), value = vals[i])
        })
        
      }else{
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textin", i),
                    label = paste0("Support Level ", i), value = "")
        }) 
      }
    }
    
  })
  
  # SupportLevels <- reactive({
  #   n <- counter$n
  #   levs <- paste0(input$textin1, input$textin2)
  #   if(n == 3){
  #     levs <- paste0(levs, input$textin3)
  #   }
  #   if(n == 4){
  #     levs <- paste0(levs, input$textin3)
  #     levs <- paste0(levs, input$textin4)
  #   }
  #   levs
  # })
  
  output$textbox_ui <- renderUI({ textboxes() })
  #
  
  ### Now, let's try plotting the inputted data
  
  ziggurat2 <- eventReactive(input$add_graph,{
    zig <- values[["DF"]]
    zig$numeric.weight <- zig$Relevance * zig$Source.reliability * zig$Information.reliability
    outcomeCats <- reactive({
      n <- counter$n
      levs <- c(input$textin1, input$textin2)
      if(n == 3){
        levs <- append(levs, input$textin3)
      }
      if(n == 4){
        levs <- append(levs, input$textin3)
        levs <- append(levs, input$textin4)
      }
      levs
    })
    CatToNumeric <- data.frame(Support=outcomeCats(), numeric.support=c(1:length(outcomeCats())))
    
    if(length(setdiff(zig$Support, outcomeCats()))>0){
      "Some of the support values entered in the table don't match Support Levels"
    } else {
      zig <- merge(zig, CatToNumeric, by="Support")
      zig[,c("Weight", "Support")] <- NULL
      
      # Return y coordinates for evidence pieces
      
      zig.sort<- zig %>%
        arrange(desc(numeric.weight))%>% # arrange by decending order of weight - this ensures we plot widest blocks at the bottom of the stack
        group_by(numeric.support)%>%
        mutate(cumul=seq_along(numeric.support))%>% # index each evidence piece within each support group
        mutate(y.coord=cumul-0.5)%>% # this gives the y coordinate for that block (querk of using geom_tile being that it builds the tile from the centre point outwards)
        ungroup()
      
      
      #Prepare additional elements for plot ####
      
      # create data frame that will be used as category bars in plot
      cat.bars<- data.frame(numeric.support=factor(c(1:length(outcomeCats()))),xmin=seq(0.5, length(outcomeCats())-0.5, 1), xmax=seq(1.5, length(outcomeCats())+0.5, 1), y=(0))
      
      # Calculate total weight per outcome category
      
      zig.areas <- zig.sort%>%
        group_by(numeric.support)%>%
        summarise(sum.support=sum(numeric.weight),numeric.support=mean(numeric.support),num.ev=n())%>%
        ungroup()
      
      # calculate the weighted mean
      
      zig.w.means<- data.frame(w.mean=weighted.mean(zig$numeric.support,zig$numeric.weight))
      
      guide.points<- data.frame(numeric.support=c(-1,0,0.5,1))
      
      # calculate bootstrapped confidence interval
      
      #bootstrap confidence intervals
      df <- data.frame(x=zig$numeric.support, w=zig$numeric.weight/input[["maxvalues"]])
      wm3 <- function(d,i){
        return(weighted.mean(d[i,1], d[i,2]))
      }
      bootwm <- boot(df, wm3, R=10000)
      bci<- boot.ci(boot.out = bootwm)
      
      lower.ci<- bci$basic[4]
      upper.ci<- bci$basic[5]
      
      extra.rows <- data.frame(Evidence = letters[1:4], Relevance=c(0,0,0,0), Source.reliability=c(0,0,0,0), Information.reliability=c(0,0,0,0), cumul=c(0,0,0,0),numeric.support=c(1,2,3,4), y.coord=c(0,0,0,0), numeric.weight=c(0,0,0,0))
      zig.sort <- rbind(zig.sort,extra.rows)
      
      #output$debug1 <- renderText({str(zig.sort)})
      
      ggplot()+
        geom_tile(data=zig.sort, aes(x=numeric.support,y=y.coord,width=numeric.weight/(input[["maxvalues"]]*10),height=1, fill=factor(numeric.support),colour=factor(numeric.support)),alpha=0.4)+
        geom_errorbarh(data=cat.bars, aes(xmin=xmin,xmax=xmax,y=y,colour=factor(numeric.support)),height=0,size=1)+
        scale_x_continuous(breaks=1:length(outcomeCats()),label=outcomeCats())+
        scale_y_continuous(expand = c(0, 0.2))+
        scale_fill_manual(values = c("#990000","#FFCC00","#66CC00","darkgreen"),guide="none")+
        scale_colour_manual(values = c("#990000","#FFCC00","#66CC00","darkgreen"),guide="none")+
        scale_alpha(guide='none')+
        xlab("Degree of support")+
        ylab("Number of evidence blocks")+
        theme_classic()+
        geom_point(data=zig.w.means,aes(x=w.mean,y=-0.5),shape=16,size=5)+
        geom_errorbarh(aes(xmin=lower.ci,xmax=upper.ci,y=-0.5),size=1,height=0.5)
      
      
      
      
    }
    
  })
  
  observeEvent(input$add_graph, {
    outcomeCats <- reactive({
      n <- counter$n
      levs <- c(input$textin1, input$textin2)
      if(n == 3){
        levs <- append(levs, input$textin3)
      }
      if(n == 4){
        levs <- append(levs, input$textin3)
        levs <- append(levs, input$textin4)
      }
      levs
    })
    
    if(max(unique(values[["DF"]][,2:4]))>input[["maxvalues"]] | min(unique(values[["DF"]][,2:4]))<input[["minvalues"]]){
      output$zigerror2 <- renderText("Please enter values within the minimum and maximum specified!")
    } else{
      output$zigerror2 <- renderText("")
    }
    
    if(sum(nchar(outcomeCats()))<2){
      output$zigerror <- renderText("Please enter support levels!")
    } else {
      if (nrow(values[["DF"]]) > 2){
        if(class(ziggurat2())[[1]]=="character"){
          output$zigerror <- renderText({ziggurat2()})
        } else {
          output$ziggy <- renderPlot({ziggurat2()})
          output$zigerror <- renderText("")
        }
      } else {
        output$zigerror <- renderText("Please enter at least 3 rows of data")
      }
    }
  })
  
  ## Download 
  output$downloadPlot <- downloadHandler(
    filename = "ZigguratPlot.png",
    content = function(file) {
      ggsave(file, ziggurat2(), device="png", width = 180, height = 97.5, units = "mm", dpi=600)
    }
  )
  
  ## Bookmarking
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
}

shinyApp(ui=ui, server=server)