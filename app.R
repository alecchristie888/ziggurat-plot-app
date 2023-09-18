library(shiny)
#devtools::install_github("jrowen/rhandsontable", dependencies = T, upgrade_dependencies = T)
library(rhandsontable)
library(shiny)
library(dplyr)
library(ggplot2)
library(boot)
library(shinythemes)
library(shinydashboard)

enableBookmarking(store = "url")

DF = data.frame("Evidence"=LETTERS[1:5], "Relevance" = rep(0,5), "Source reliability" = rep(0,5), "Information reliability" = rep(0,5), "Support" = rep(NA,5), stringsAsFactors = FALSE)
DF_template = data.frame("Evidence"=LETTERS[1:5], "Relevance" = rep("",5), "Source reliability" = rep("",5), "Information reliability" = rep("",5), "Support" = rep("",5), stringsAsFactors = FALSE)

ui <- function(request){shinyUI(
  
  dashboardPage(skin = "yellow",
                dashboardHeader(title="Balance Evidence Assessment Model - Ziggurat Plot Application", titleWidth = "calc(100% - 44px)"),
                dashboardSidebar(
                  sidebarMenu(id = "tabs",
                              menuItem(
                                "Information",
                                tabName = "info_tab",
                                icon = icon(name="info",style="color: #13dd3b; padding-left: 10px; font-size: 30px; padding-right: 30px")
                              ),
                              menuItem(
                                "Define values",
                                tabName = "vals_tab",
                                icon = icon(name="keyboard", style="padding-right: 40px; font-size: 25px")
                                ),
                              menuItem(
                                "Ziggurat plot",
                                tabName = "plot_tab",
                                icon = icon(name="cubes",style="color: #e4370c; font-size: 30px; padding-right: 40px")
                                )
                              )
                  ),
      dashboardBody(
        tags$head( 
          tags$style(HTML(".main-sidebar { font-size: 16px; } .sidebar-menu li { margin-bottom: 10px; }")) #change the font size to 20
        ),
        tabItems(
          tabItem(
            tabName="info_tab",
                box(
                  h2("About this app"),
                  p("This application is to assist those assessing assumptions using the Balance Evidence Assessment Model. To read more about the Balance Evidence Assessment Model, you can read",
                  a(href="https://doi.org/10.31219/osf.io/ujk6n","this article.",target="_blank"),
                  "A published open-access article will be available soon",
                  a(href="https://doi.org/10.1111/csp2.13024","here.", target="_blank"),
                  "These articles contain guidance on its use and important considerations - such as ensuring that those assessing the evidence are representative of the diversity of the evidence itself.",style='font-size:17px;'),
                  h3("What is the Balance Evidence Assessment Model?"),
                  p("The Balance Evidence Assessment Model (BEAM) is designed to help with the issue of how to weigh several 
                  different pieces of evidence that may differ greatly in their source and relevance. A piece of evidence can be defined as:
                  'Any relevant data, information, knowledge, and wisdom used to assess an assumption' related to a question of interest.
                  BEAM is applied to assumptions, which can be identified when planning a project, to understand how strong the evidence is and our confidence
                  that the assumption is valid.",style='font-size:17px;'),
                  h3("An illustrative example"),
                  p("As an example, imagine you are part of a local NGO team on a project aiming to reduce and reverse the decline in seabird populations on an 
                  island on which rats have been introduced. Following the",
                  a(href="https://conservationstandards.org/about/","Conservation Standards,",target="_blank"),
                  ", in the planning stage of a project, you might create a Situation Analysis (to identify relevant threats, 
                  opportunities, and stakeholders) and a Theory of Change (to outline the logical steps by which a strategy will contribute to achieving set 
                  targets). This would highlight that you are in fact making several key assumptions about how your actions will help you achieve your conservation
                  targets.",style='font-size:17px;'),
                  p("In a Situation Analysis, an example of an assumption may be: the seabird population is declining rapidly and problematically due to 
                  the introduction of rats. In a Theory of Change, assumptions may include: 1) rat eradication through trapping (without poisoning) is socially 
                  acceptable to local partners and communities; and 2) rat eradication through trapping (without poisoning) will lead to the recovery of the 
                  seabird population within the project timescale. Others might include that rat eradication using trapping is feasible or cost-effective in 
                  terms of the budgets and resources available.",style='font-size:17px;'),
                  p("To check if these assumptions are valid you'll need to assess evidence from a range of sources including, but not limited to, local knowledge, 
                  experience, and wisdom of practitioners and partners, scientific studies and syntheses, expert judgements, reports, databases (e.g., citizen 
                  science), local records, and observations.",style='font-size:17px;'),
                  h2("How the BEAM works"),
                  p("BEAM helps you weigh this evidence by treating each piece of evidence as a cube. The size of each cube represents the weight of that piece of 
                  evidence, which is represented by three dimensions: the information reliability, source reliability, and relevance. The diagram below shows how you
                  can imagine weighing the evidence. Bigger cubes represent evidence with greater weight. The evidence may either refute or support the assumption you're
                  assessing. There may be situations where evidence can only refute or support an assumption (true or false), but in most situations it will be more of a gradient
                  of support (refutes, mixed support, weakly or strongly supports).",style='font-size:17px;'),
                  br(),
                  img(src="BEAMplot.png",height="50%",width="50%"),
                  br(),
                  p("You can then place the cube of evidence on the balance depending on how much it supports the assumption.
                  You can then imagine how the balance may tilt based on all the cubes of evidence you have assembled. If we're confident in the assumption, it will tilt to the right, 
                  if not it will tilt to the left, and may sit somewhere in between if we're not sure.",style='font-size:17px;'),
                  h2("Using the Ziggurat plot app"),
                  p("The Ziggurat plot app helps you to visualise the evidence you are assessing in a form similar to this diagram and understand how confident you are in the assumption you're assessing.",
                  "For example, you may produce plots like the ones below, which show different scenarios based on the available evidence.",style='font-size:17px;'),
                  br(),
                  img(src="Zigplots.png",height="60%",width="60%"),
                  br(),
                  p("In the next tab (Define Values - left of screen above), you can edit the values you can assign to the pieces of evidence. We suggest a scale of 0-3 for assessing the information 
                  reliability, source reliability, and relevance. You can also edit the names you give to different levels of support (we suggest the ones as above).
                  You can also edit the numerical values associated to each level of support, for which the defaults are:
                  -2 = Refutes, 0 = Mixed, 1 = Weakly Supports, and 2 = Strongly Supports. If you wanted to increase the required amount of supporting evidence for you to have confidence 
                  in an assumption (i.e., a higher threshold or required level of proof), you could change the value for Refutes to -4, effectively doubling the leverage of refuting evidence (see below).",style='font-size:17px;'),
                  br(),
                  img(src="Zigplotsleverage.png",height="30%",width="30%"),
                  br(),
                  h2("Copyright"),
                  "The Ziggurat plot app by Alec P. Christie and authors listed in Christie et al. (2023; https://osf.io/ujk6n/) is licensed under a", 
                  a(href="http://creativecommons.org/licenses/by-sa/4.0/","Creative Commons Attribution-ShareAlike 4.0 International License."),
                  "Based on a work at:", 
                  a(href="https://github.com/alecchristie888/ziggurat-plot-app","https://github.com/alecchristie888/ziggurat-plot-app.",target="_blank"),
                  br(),
                  img(href="http://creativecommons.org/licenses/by-sa/4.0/", style="border-width:0", src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png",target="_blank"),
                  br(),
                  h2("Acknowledgements"),
                  p("I'd like to acknowledge the help of Dr Hannah Wauchope and Dr William Morgan in coding early versions of the Ziggurat plot app, which I then refined and adapted into the app that is now published."),
                  width=12
                )),
          tabItem(
            tabName = "vals_tab",
            helpText("Use this page to define the levels of support and scores you can use to assess your evidence."),
            box(
              title="1.a Define strength of support levels",
              collapsible = TRUE,
              helpText("First, enter your levels of support below, in ascending order. For example, 'Refutes', 'Mixed', 'Weakly supports', 'Strongly supports'. Use the buttons to add or remove levels as you please; you can have a minimum of 2 and a maximum of 4 levels"),
              uiOutput("textbox_ui"),
              actionButton("add_btn", "Add Support Level", style='padding:4px; font-size:80%'),
              br(),
              actionButton("rm_btn", "Remove Support Level", style='padding:4px; font-size:80%'),
              br(),
              width=12
            ),
            box(
              title="1.b Define minimum and maximum scores",
              collapsible = TRUE,
              helpText("Provide the range of values below for which you wish to score evidence in terms of its relevance, source reliability, and information reliability, as well as the breaks between values (e.g., 0 to 3 in steps of 1)"),
              numericInput("minvalues","Minimum score:", 0),
              numericInput("maxvalues","Maximum score:", 3),
              numericInput("minbreaks","Steps of:", 1),
              width=12
            ),
            box(
              title="1.c Define numeric values for strength of support levels",
              collapsible = TRUE,
              helpText("Provide the numeric values below for each strength of support - we suggest -2, 0, 1, and 2 for refutes, mixed, weakly supports, and strongly supports, respectively. If you wanted to double the leverage of refuting evidence, 
                       to increase the required amount of supporting evidence, you could set the refutes value as -4 (see Info tab). This could be done as a sensitivity analysis to see how much this changes your assessment of the evidence as a whole."),
              uiOutput("strengthsupport_ui"),
              width=12
            )
          ),
          tabItem(
            tabName = "plot_tab",
            helpText("Use this page to make a Ziggurat plot of your evidence. If you want to bookmark your work at any time, scroll down the page and click the bookmark button."),
            box(
              title="2. Add Data",
              solidHeader = TRUE,
              helpText("You can now add data to the table below (right-click to delete/insert rows). Remove any empty rows before clicking 'add graph'."),
              helpText("Alternatively, you can upload your data in a .csv file using the template below."),
              downloadButton("downloadDataTemplate", "Download Data Upload Template"), fileInput('file1', 'Choose CSV File'),
              br(),
              rHandsontableOutput("hot",width="100%", height="100%"),
              helpText("Remember to right click on the table to delete or insert rows. Don't leave any rows blank."),
              #textOutput('debug1'),
              width=12
            ),
            box(title="Bookmarking",
                helpText("On bookmarking your data, the URL will update. Copy this and you will be able to return to the app. On returning to the app, make sure to click add graph again to show your ziggurat plot again."),
                bookmarkButton(),
                width=12
            ),
            box(
              title="3. Create Plot",
              solidHeader = TRUE,
              helpText("Once you're happy, generate a Ziggurat plot. You'll need to have at least two levels of support defined, and at least three rows of data. If you get an error, press this button again when you've fixed it."),
              actionButton("add_graph", "Add Graph"),
              br(),
              h3(textOutput("zigerror")),
              h3(textOutput("zigerror2")),
              plotOutput("ziggy"),
              width=12
              ),
            box(
              title="4. Data Download",
              collapsible = TRUE,
              helpText("If you want to download the data you've entered, click below."),
              downloadButton("downloadData", "Download Data"),
              br(),
              br(),
              width=12
            ),
            box(
              title="5. Plot Download",
              collapsible = TRUE,
              helpText("If you want to download an image of the plot, click below."),
              downloadButton("downloadPlot", "Download Plot"),
              width=12
            )
        )
      ),
      tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))))
  )
)}

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
      levs <- c(input$textin1)
      if(n == 2){
        levs <- append(levs, input$textin2)
      }
      if(n == 3){
        levs <- append(levs, input$textin2)
        levs <- append(levs, input$textin3)
      }
      if(n == 4){
        levs <- append(levs, input$textin2)
        levs <- append(levs, input$textin3)
        levs <- append(levs, input$textin4)
      }
      levs
    }
    dropdownOptionsother <- {
      levsother <- seq(input[["minvalues"]],input[["maxvalues"]],by=input[["minbreaks"]])
      levsother
    }
    
    rhandsontable(values[["DF"]], stretchH = "all") %>% #width="300px", 
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
  
  textboxes1 <- reactive({
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
  
  
  textboxes2 <- reactive({
    n <- counter$n
    
    if (n > 0) {
      
      # If the no. of textboxes previously were more than zero, then 
      #save the text inputs in those text boxes 
      if(prevcount$n > 0){
        
        vals_support = c()
        if(prevcount$n > n){
          lesscnt <- n
          isInc <- FALSE
        }else{
          lesscnt <- prevcount$n
          isInc <- TRUE
        }
        for(i in 1:lesscnt){
          inpid = paste0("textinsupport",i)
          vals_support[i] = input[[inpid]] 
        }
        if(isInc){
          vals_support <- c(vals_support, "")
        }
        
        if(vals_support[1]==""){
          vals_support <- c('-2','0','1', '2')
        }
        
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textinsupport", i),
                    label = paste0("Support Level ", i), value = vals_support[i])
        })
        
      }else{
        lapply(seq_len(n), function(i) {
          textInput(inputId = paste0("textinsupport", i),
                    label = paste0("Support Level ", i), value = "")
        }) 
      }
    }
    
  })
  
  SupportLevels <- reactive({
    n <- counter$n
    levs <- paste0(input$textin1)
    if(n == 2){
      levs <- paste0(levs, input$textin2)
    }
    if(n == 3){
      levs <- paste0(levs, input$textin2)
      levs <- paste0(levs, input$textin3)
    }
    if(n == 4){
      levs <- paste0(levs, input$textin2)
      levs <- paste0(levs, input$textin3)
      levs <- paste0(levs, input$textin4)
    }
    levs
  })
  
  NumericSupportLevels <- reactive({
    n <- counter$n
    nums <- paste0(input$textinsupport1)
    if(n == 2){
      nums <- paste0(nums, input$textinsupport2)
    }
    if(n == 3){
      nums <- paste0(nums, input$textinsupport2)
      nums <- paste0(nums, input$textinsupport3)
    }
    if(n == 4){
      nums <- paste0(nums, input$textinsupport2)
      nums <- paste0(nums, input$textinsupport3)
      nums <- paste0(nums, input$textinsupport4)
    }
    nums
  })
  
  output$textbox_ui <- renderUI({ textboxes1() })
  output$strengthsupport_ui <- renderUI({ textboxes2() })
  #
  
  ### Now, let's try plotting the inputted data
  
  ziggurat2 <- eventReactive(input$add_graph,{
    zig <- values[["DF"]]
    zig$numeric.weight <- zig$Relevance * zig$Source.reliability * zig$Information.reliability
    outcomeCats <- reactive({
      n <- counter$n
      levs <- c(input$textin1)
      if(n == 2){
        levs <- append(levs, input$textin2)
      }
      if(n == 3){
        levs <- append(levs, input$textin2)
        levs <- append(levs, input$textin3)
      }
      if(n == 4){
        levs <- append(levs, input$textin2)
        levs <- append(levs, input$textin3)
        levs <- append(levs, input$textin4)
      }
      levs
    })
    numericoutcomeCats <- reactive({
      n <- counter$n
      nums <- c(input$textinsupport1)
      if(n == 2){
        nums <- append(nums, input$textinsupport2)
      }
      if(n == 3){
        nums <- append(nums, input$textinsupport2)
        nums <- append(nums, input$textinsupport3)
      }
      if(n == 4){
        nums <- append(nums, input$textinsupport2)
        nums <- append(nums, input$textinsupport3)
        nums <- append(nums, input$textinsupport4)
      }
      as.numeric(nums)
    })
    CatToNumeric <- data.frame(Support=outcomeCats(), numeric.support=numericoutcomeCats())
    
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
      if(length(outcomeCats())>1){
        numeric.out.cats <- numericoutcomeCats()
        cat.bars<- data.frame(numeric.support=factor(numeric.out.cats),xmin=c(numeric.out.cats[1],numeric.out.cats[2],numeric.out.cats[2],numeric.out.cats[2]), xmax=c(numeric.out.cats[2],numeric.out.cats[2],numeric.out.cats[3],numeric.out.cats[4]), y=(0))
      }
      if(length(outcomeCats())==1){
        cat.bars<- data.frame(numeric.support=factor(numericoutcomeCats()),xmin=numericoutcomeCats(), xmax=numericoutcomeCats(), y=(0))
      }
      
      # calculate the weighted mean
      
      zig.w.means<- data.frame(w.mean=weighted.mean(zig$numeric.support,zig$numeric.weight/input[["maxvalues"]]**3))
      
      guide.points<- data.frame(numeric.support=numericoutcomeCats())
      
      # calculate bootstrapped confidence interval
      
      #bootstrap confidence intervals
      if(length(unique(zig$numeric.support))>1){
      df <- data.frame(x=zig$numeric.support, w=zig$numeric.weight/input[["maxvalues"]]**3)
      wm3 <- function(d,i){
        return(weighted.mean(d[i,1], d[i,2]))
      }
      bootwm <- boot(df, wm3, R=10000)
      bci<- boot.ci(boot.out = bootwm)
      
      lower.ci<- bci$basic[4]
      upper.ci<- bci$basic[5]
      
      if(lower.ci<min(zig$numeric.support)){
        lower.ci <- min(zig$numeric.support)
      }
      if(upper.ci>max(zig$numeric.support)){
        upper.ci <- max(zig$numeric.support)
      }
      
      size1 <- 1
      
      }else{
        lower.ci<-zig$numeric.support[1]
        upper.ci<-zig$numeric.support[1]
        size1<-0
      }
      
      #To ensure all categories are plotted
      extra.rows <- data.frame(Evidence = letters[1:4], Relevance=c(0,0,0,0), Source.reliability=c(0,0,0,0), Information.reliability=c(0,0,0,0), cumul=c(0,0,0,0),numeric.support=numericoutcomeCats(), y.coord=c(NA,NA,NA,NA), numeric.weight=c(0,0,0,0))
      zig.sort <- rbind(zig.sort,extra.rows)
      
      #output$debug1 <- renderText({str(zig.sort)})
      minnumoutcat <- min(numericoutcomeCats())
      adjustsizeplot <- 22+(22*0.25*(1-minnumoutcat/-2))

      ggplot()+
        geom_tile(data=zig.sort, aes(x=numeric.support,y=y.coord,width=numeric.weight/(input[["maxvalues"]]**3),height=1, fill=factor(numeric.support), colour=factor(numeric.support)),alpha=0.4, size=0.3)+
        geom_errorbarh(data=cat.bars, aes(xmin=xmin,xmax=xmax,y=y,colour=factor(numeric.support)),height=0.1,size=0.5)+
        scale_x_continuous(breaks=numericoutcomeCats(),label=gsub('\\s','\n',outcomeCats()),expand = c(0,0.5))+
        scale_y_continuous(expand = c(0, 0.2))+
        scale_fill_manual(values = c("#990000","#FFCC00","#66CC00","darkgreen"),guide="none")+
        scale_colour_manual(values = c("#990000","#FFCC00","#66CC00","darkgreen"),guide="none")+
        scale_alpha(guide='none')+
        xlab("Strength of support")+
        ylab("Number of\npieces of evidence")+
        theme_classic()+
        theme(axis.title.y = element_text(size=24, margin = unit(c(0, 5, 0, 0), "mm")), 
              axis.title.x = element_text(size=24, margin = unit(c(5, 0, 0, 0), "mm")),
              axis.text = element_text(size=adjustsizeplot))+
        geom_point(data=zig.w.means,aes(x=w.mean,y=-0.5),shape=16,size=5)+
        geom_errorbarh(aes(xmin=lower.ci,xmax=upper.ci,y=-0.5),size=size1,height=0.5)
      
    }
    
  })
  
  observeEvent(input$add_graph, {
    outcomeCats <- reactive({
      n <- counter$n
      levs <- c(input$textin1)
      if(n == 2){
        levs <- append(levs, input$textin2)
      }
      if(n == 3){
        levs <- append(levs, input$textin2)
        levs <- append(levs, input$textin3)
      }
      if(n == 4){
        levs <- append(levs, input$textin2)
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
      ggsave(file, ziggurat2(), device="png", width = 270, height = 146.25, units = "mm", dpi=600)
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