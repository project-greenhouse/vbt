#----------#
# Source global file
source("global.R")

#----------#
# Data -----
dataPath <- "www/demoData.RDS"
demoData <- readRDS(dataPath)
readme <-  "www/LandingPage.Rmd"

#----------#

# UI -----
ui <- dashboardPage(
    skin = "black",
    ## Header -----
    dashboardHeader(
        ### Title -----
        title = "Load-Velocity App", titleWidth = 250,
        ### User Icon -----
        userOutput("user"),
        ### Github Icon Link -----
        tags$li(
            class="dropdown", 
            tags$a(
                # link reference
                href="https://github.com/project-greenhouse/vbt-trial",
                # icon
                icon("github"), 
                # display text
                "Source Code", 
                target="_blank"
            )
        )
    ),
    ## Sidebar -----
    dashboardSidebar(
        width = 250,
        sidebarMenu(
            # sidebar ID
            id = "sidebarID",
            
            ### Tab Items -----
            menuItem("Welcome", tabName = "tab_home", icon = icon("home")),
            menuItem("Hisotrical Data", tabName = "tab_history", icon = icon("calendar")),
            menuItem("Session Comparison", tabName = "tab_S2S", icon = icon("people-arrows")),
            menuItem("Training Planner", tabName = "tab_training", icon = icon("chart-simple")),
            menuItem("Team Table", tabName = "tab_team", icon = icon("people-group")),
            
            ### File upload -----
            fileInput(inputId = "inputData", 
                      label = "Enter your data here:",
                      buttonLabel = "upload",
                      placeholder = ".csv files only"),
            
            ### Download button -----
            downloadBttn(
                outputId = "dwnldTemplate",
                label = "Download Template",
                size = "sm",
                style = "float",
                color = "success"
            )
        )
    ),
    ## Body -----
    dashboardBody(
        tabItems(
            ### Welcome Tab -----
            tabItem(
                tabName = "tab_home",
                fluidPage(
                    fluidRow(
                        DTOutput(outputId = "dataTest")
                    ),
                    includeMarkdown('www/LandingPage.Rmd')
                )
            ),
            ### History Tab -----
            tabItem(
                tabName = "tab_history",
                fluidPage(
                    fluidRow(
                        column(
                            width = 3,
                            #### Historical Player filter -----
                            selectInput(inputId = "HistPlayer",
                                        label = "Select Player: ",
                                        choices = unique(demoData$User),
                                        selectize = FALSE
                            )
                        ),
                        column(
                            width = 6,
                            #### Historical Exercise filter -----
                            selectInput(inputId = "HistExercise",
                                        label = "Select Exercise: ",
                                        choices = "",
                                        selectize = FALSE
                            )
                        )
                    ),
                    fluidRow(
                        #### Summary Plots Tab Box -----
                        tabBox(
                            side = "left",
                            height = "350px",
                            selected = "tabE1RM",
                            width = 12,
                            ##### Hist E1RM Plot -----
                            tabPanel(
                                title = "Est. 1RM",
                                value = "tabE1RM",
                                withLoader(
                                    echarts4rOutput(
                                        outputId = "histPlotE1RM"
                                    ),
                                    type = "html",
                                    loader = "loader4"
                                )
                            ),
                            ##### Hist AUC Plot -----
                            tabPanel(
                                title = "Power AUC",
                                value = "tabAUC",
                                withLoader(
                                    echarts4rOutput(
                                        outputId = "histPlotAUC"
                                    ),
                                    type = "html",
                                    loader = "loader4"
                                )
                            ),
                            ##### Hist Combo Plot -----
                            tabPanel(
                                title = "Combined",
                                value = "tabCombo",
                                withLoader(
                                    echarts4rOutput(
                                        outputId = "histPlotCombo"
                                    ),
                                    type = "html",
                                    loader = "loader4"
                                )
                            )
                        )
                    ),
                    fluidRow(
                        tabBox(
                            title = "Summary Tables",
                            side = "right",
                            selected = "sess",
                            height = "350px",
                            width = 12,
                            #### All Reps Tab -----
                            tabPanel(
                                title = "All Reps", 
                                value = "all", 
                                icon = icon("chart-gantt"),
                                dataTableOutput(
                                    outputId = "histAll", 
                                    height = "400px"
                                )
                            ),
                            #### Set Average Tab -----
                            tabPanel(
                                title = "Set Average", 
                                value = "avg", 
                                icon = icon("vials"),
                                dataTableOutput(
                                    outputId = "histAvg", 
                                    height = "400px"
                                )
                            ),
                            #### Best Rep Tab -----
                            tabPanel(
                                title = "Best Rep", 
                                value = "best", 
                                icon = icon("code-pull-request"),
                                dataTableOutput(
                                    outputId = "histBest", 
                                    height = "400px"
                                )
                            ),
                            #### Session Summary Tab -----
                            tabPanel(
                                title = "Session Summary", 
                                value = "sess", 
                                icon = icon("diagram-next"),
                                dataTableOutput(
                                    outputId = "histSess", 
                                    height = "400px"
                                )
                            )
                        )
                    )
                )
            ),
            ### S2S Tab -----
            tabItem(
                # Tab Name
                tabName = "tab_S2S",
                fluidPage(
                    fluidRow(
                        box(
                            width = 4,
                            title = "Session 1",
                            background = "red",
                            column(
                                width = 6,
                                ###### Player 1 Select -----
                                selectInput(
                                    inputId = "S2SselectP1",
                                    label = "Player 1:",
                                    choices = "",
                                    selectize = FALSE
                                )
                            ),
                            column(
                                width = 6,
                                ###### Date 1 Select -----
                                selectInput(
                                    inputId = "S2SdateP1",
                                    label = "Select Date: ", 
                                    choices = "",
                                    selectize = FALSE
                                )
                            )
                        ),
                        column(
                            width = 3,
                            offset = 1,
                            selectInput(
                                inputId = "exrcsS2S",
                                label = "Select Exercise",
                                choices = unique(demoData$Exercise),
                                selectize = FALSE
                            )
                        ),
                        box(
                            width = 4,
                            title = "Session 2",
                            background = "olive",
                            column(
                                width = 6,
                                ###### Player 2 Select -----
                                selectInput(
                                    inputId = "S2SselectP2",
                                    label = "Player 2:",
                                    choices = "",
                                    selectize = FALSE
                                )
                            ),
                            column(
                                width = 6,
                                ###### Date 2 Select -----
                                selectInput(
                                    inputId = "S2SdateP2",
                                    label = "Select Date: ", 
                                    choices = "",
                                    selectize = FALSE
                                )
                            )
                        )
                    ),
                    # Plot Outputs
                    fluidRow(
                        tabBox(
                            side = "left",
                            selected = "tabLV",
                            width = 12,
                            ###### LV Comp Plot -----
                            tabPanel(
                                title = "Load-Velocity",
                                value = "tabLV",
                                withLoader(
                                    echarts4rOutput(
                                        outputId = "S2SplotLV",
                                    ),
                                    type = "html",
                                    loader = "loader4"
                                )
                            ),
                            ###### LP Comp Plot -----
                            tabPanel(
                                title = "Load-Power",
                                value = "tabPOW",
                                withLoader(
                                    plotOutput(
                                        outputId = "S2SplotPOW"
                                    ),
                                    type = "html",
                                    loader = "loader4"
                                )
                            )
                        )
                    ),
                    fluidRow(
                        # Player 1 Section
                        column(
                            width = 6,
                            box(
                                width = 12,
                                title = "Session 1",
                                background = "red",
                                fluidRow(
                                    # Left Column
                                    column(
                                        width = 6,
                                        ###### Comp 1 Est1RM -----
                                        valueBoxOutput(
                                            outputId = "e1RM1",
                                            width = 6
                                        ),
                                        ##### Comp 1 AUC -----
                                        valueBoxOutput(
                                            outputId = "PowFactor1",
                                            width = 6
                                        )
                                    ),
                                    # Right Column
                                    column(
                                        width = 6,
                                        ##### Comp 1 Peak Velo -----
                                        valueBoxOutput(
                                            outputId = "PeakV1",
                                            width = 6
                                        ),
                                        ##### Comp 1 Peak Force -----
                                        valueBoxOutput(
                                            outputId = "PeakF1",
                                            width = 6
                                        )
                                    )
                                ),
                                fluidRow(
                                    ###### Comp 1 Summary -----
                                    DTOutput(
                                        outputId = "S2SSumm1"
                                    )
                                )
                            )
                        ),
                        # player 2 Section
                        column(
                            width = 6,
                            box(
                                width = 12,
                                title = "Session 2",
                                background = "olive",
                                fluidRow(
                                    # Left Column
                                    column(
                                        width = 6,
                                        ###### Comp 2 Est1RM -----
                                        valueBoxOutput(
                                            outputId = "e1RM2",
                                            width = 6
                                        ),
                                        ##### Comp 2 AUC -----
                                        valueBoxOutput(
                                            outputId = "PowFactor2",
                                            width = 6
                                        )
                                    ),
                                    column(
                                        width = 6,
                                        ##### Comp 2 Peak Velo -----
                                        valueBoxOutput(
                                            outputId = "PeakV2",
                                            width = 6
                                        ),
                                        ##### Comp 2 Peak Force -----
                                        valueBoxOutput(
                                            outputId = "PeakF2",
                                            width = 6
                                        )
                                    )
                                ),
                                fluidRow(
                                    ###### Comp 2 Summary -----
                                    DTOutput(
                                        outputId = "S2SSumm2"
                                    )
                                )
                            )
                        )
                    )
                )
            ),
            ### Training Rec Tab -----
            tabItem(
                tabName = "tab_training",
                fluidPage(
                    #### eChart Custom Theme -----
                    e_theme_register('{"color":["#3c8dbc","#dd4b39","#00c0ef","#00a65a","#f49c11"]}', name = "LV_Theme"),
                    #### Row 1 -----
                    fluidRow(
                        column(
                            width = 3,
                            ##### PP Player filter -----
                            selectInput(inputId = "playerPP",
                                        label = "Select Player: ",
                                        choices = unique(demoData$User),
                                        selectize = FALSE
                            )
                        ),
                        column(
                            width = 3,
                            ##### PP Exercise filter -----
                            selectInput(inputId = "exercisePP",
                                        label = "Select Exercise: ",
                                        choices = "",
                                        selectize = FALSE
                            )
                        ),
                        column(
                            width = 3,
                            ##### PP Date filter -----
                            selectInput(inputId = "datePP",
                                        label = "Select Date: ",
                                        choices = "",
                                        selectize = FALSE
                            )
                        )
                    ),
                    
                    #### Row 2 -----
                    fluidRow(
                        column(
                            width = 4,
                            box(
                                title = "Training Velocity Zones",
                                width = 12,
                                ##### Velocity Zone Outputs -----
                                ###### Max Strength  -----
                                withLoader(
                                    valueBoxOutput(
                                        outputId = "MaxSpdZone",
                                        width = 12
                                    ),
                                    type = "html",
                                    loader = "loader10"
                                ),
                                ###### Strength-Speed -----
                                withLoader(
                                    valueBoxOutput(
                                        outputId = "SpStZone",
                                        width = 12
                                    ),
                                    type = "html",
                                    loader = "loader10"
                                ),
                                ###### Peak Power -----
                                withLoader(
                                    valueBoxOutput(
                                        outputId = "PPZone",
                                        width = 12
                                    ),
                                    type = "html",
                                    loader = "loader10"
                                ),
                                ###### Speed-Strength -----
                                withLoader(
                                    valueBoxOutput(
                                        outputId = "StSpZone",
                                        width = 12
                                    ),
                                    type = "html",
                                    loader = "loader10"
                                ),
                                ###### Max Speed -----
                                withLoader(
                                    valueBoxOutput(
                                        outputId = "MSZone",
                                        width = 12
                                    ),
                                    type = "html",
                                    loader = "loader10"
                                )
                            )
                        ),
                        column(
                            width = 8,
                            box(
                                title = "Load-Velocity Profile",
                                width = 12,
                                ##### PP Graph -----
                                withLoader(
                                    echarts4rOutput(
                                        outputId = "LVplot"
                                    ),
                                    type="html",
                                    loader = "loader1"
                                )
                            ),
                            box(
                                title = "Intensity Training Zones",
                                width = 12,
                                ##### Intensity-Velocity Chart
                                DTOutput(
                                    outputId = "RecTable"
                                )
                            )
                        )
                    )
                )
            ),
            ### Team Training Tab -----
            tabItem(
                tabName = "tab_team",
                fluidPage(
                    fluidRow(
                        column(
                            width = 3,
                            #### Exercise Select -----
                            selectInput(
                                inputId = "exerciseTeam",
                                label = "Select Exercise: ",
                                choices = unique(demoData$Exercise),
                                selectize = FALSE
                            )
                        ),
                        column(
                            width = 3,
                            #### Date Select -----
                            selectInput(
                                inputId = "dateTeam",
                                label = "Select Date: ",
                                choices = "",
                                selectize = FALSE
                            )
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Team Programming Table",
                            width = 12,
                            #### Table Radio Buttons -----
                            radioGroupButtons(
                                inputId = "radioTeam",
                                label = "", 
                                choiceNames = c("Velocity (m/s)", "Load (kg)"),
                                choiceValues = c("velo", "load"),
                                individual = TRUE,
                                status = "success",
                                checkIcon = list("yes" = icon("check"))
                            ),
                            #### Team Table Output -----
                            withLoader(
                                DTOutput(
                                    outputId = "TeamTable"
                                ),
                                type="html",
                                loader = "loader1"
                            )
                        )
                    )
                )
            )
        )
    )
) 



# Server -----
server <- function(input, output, session) {
    
    # Packages -----
    
    ## Shiny -----
    # Install and load required packages
    if (!require("shiny")) {
        install.packages("shiny")
    }
    library(shiny)
    
    ## Tidyverse -----
    # Install and load required packages
    if (!require("tidyverse")) {
        install.packages("tidyverse")
    }
    library(tidyverse)
    
    ## Shiny Dashboard -----
    # Install and load required packages
    if (!require("shinydashboard")) {
        install.packages("shinydashboard")
    }
    library(shinydashboard)
    
    ## Shiny Widgets -----
    # Install and load required packages
    if (!require("shinyWidgets")) {
        install.packages("shinyWidgets")
    }
    library(shinyWidgets)
    
    ## Shiny Dashboard Plus -----
    # Install and load required packages
    if (!require("shinydashboardPlus")) {
        install.packages("shinydashboardPlus")
    }
    library(shinydashboardPlus)
    
    ## Shiny Custom Loader -----
    # Install and load required packages
    if (!require("shinycustomloader")) {
        install.packages("shinycustomloader")
    }
    library(shinycustomloader)
    
    ## DT -----
    # Install and load required packages
    if (!require("DT")) {
        install.packages("DT")
    }
    library(DT)
    
    ## eCharts4R -----
    # Install and load required packages
    if (!require("echarts4r")) {
        install.packages("echarts4r")
    }
    library(echarts4r)
    
    ## Rmarkdown -----
    # Install and load required packages
    if (!require("markdown")) {
        install.packages("markdown")
    }
    library(markdown)
    
    #--------------------#
    
    ## Data -----
    fileData <- reactive({
        file <- input$inputData
        
        if(!is.null(file)) {
            read.csv(file$datapath)
        }
    })
    
    dataValidate <- reactive({
        x <- if (!is.null(input$inputData)) {
            TRUE
        } else {
            FALSE
        }
    })
    
    appData <- reactive({
        
        df <- if (!is.null(input$inputData)) {
            x <- fileData() %>%
                transmute(
                    "Date" = date(mdy(Date)),
                    "User" = User,
                    "Exercise" = Exercise,
                    "Set" = Set,
                    "Rep" = Rep,
                    "Load" = Load,
                    "Velocity" = Velocity,
                    "Power" = Power
                )
            return(x)
        } else {return(demoData)}
        
        return(df)
    })
    
    output$dataTest <- renderDT({
        req(input$inputData)
        
        x <- fileData() %>% select (-X)
        
        return(x)
    })
    
    ### Data Upload handler -----
    
    
    ## Header -----
    ### User Output -----
    output$user <- renderUser({
        dashboardUser(
            name = "Greenhouse Performance", 
            image = "GSPlogo.png", 
            title = "Lauren Green",
            subtitle = "Owner", 
            footer = p("Together We Grow", class = "text-center"),
            fluidRow(
                # Website
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://www.greenhouse-ps.com",
                        icon = icon("home")
                    )
                ),
                # Github
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://github.com/project-greenhouse",
                        icon = icon("square-github")
                    )
                ),
                # Instagram
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://www.instagram.com/greenhouse_ps/",
                        icon = icon("square-instagram")
                    )
                ),
                #YouTube
                dashboardUserItem(
                    width = 3,
                    socialButton(
                        href = "https://www.youtube.com/channel/UC7pHX9ynSGyVoZAwhNQNQzw",
                        icon = icon("square-youtube")
                    )
                )
            )
        )
    })
    
    #----------#
    
    ## Sidebar -----
    ### Template Download -----
    output$dwnldTemplate <- downloadHandler(
        filename = function() {
            paste0("LV_Profile_Template_", Sys.Date(), ".csv")
        },
        content = function(con) {
            Date = c("mm-dd-yyyy")
            User = c("First Player")
            Exercise = c("Back Squat")
            Set = c("1")
            Rep = c("1")
            Load = c("kg")
            Velocity = c("Mean Velocity (m/s)")
            Power = c("Mean Power (W)")
            
            template <- data.frame(Date, User, Exercise, Set, Rep, Load, Velocity, Power)
            write.csv(template, con)
        },
        contentType = "text/csv"
    )
    
    ### New data Observe Event -----
    # Direct view to Historical Tab when new data is uploaded
    observeEvent( input$inputData,{
        
        # update selected tab when new data uploads
        updateTabItems(
            session = session,
            inputId = "sidebarID",
            selected = "tab_history")
        
    })
    
    #----------#
    
    ## History -----
    
    ### Reactive Filters -----
    histPlayer <- reactive({
        input$HistPlayer
    })
    
    histExercise <- reactive({
        input$HistExercise
    })
    
    ###  Athlete Observe Function -----
    observe({
        
        n <- appData()
        # Filter users by data
        #userFilter <-  demoData %>%
        #  select(User)
        
        # List of Athletes filtered from exercise select
        athList <- unique(n$User)
        
        # Update P1 select input
        updateSelectInput(session,"HistPlayer",choices=athList, selected = athList[1])
        
    })
    
    ###  Exercise Observe Function -----
    observeEvent(input$HistPlayer, {
        
        n <- appData()
        
        user <- histPlayer()
        
        # Filter users by data
        #exerciseFilter <-  x %>%
        #  filter(User == user) %>%
        #  select(Exercise)
        
        # List of Athletes filtered from exercise select
        exrcsList <- unique(n$Exercise[n$User == user])
        
        # Update P1 select input
        updateSelectInput(session,"HistExercise",choices=exrcsList, selected = exrcsList[1])
    })
    
    ### History Data -----
    histDF <- reactive({
        
        x <- demoData
        
        # Input filters
        user <- input$HistPlayer
        exrcs <- input$HistExercise
        
        n <- filter(x, User == user & Exercise == exrcs)
        # Filter df
        histDF <- n %>%
            transmute(
                "Date" = date(Date),
                "User" = User,
                "Exercise" = Exercise,
                "Set" = Set,
                "Rep" = Rep,
                "Load" = Load,
                "Velocity" = Velocity,
                "Power" = Power
            )
        
        return(histDF)
    })
    
    ### History Session DT output -----
    #### Session Est 1RM by date -----
    histSessE1RM <- reactive({
        
        # Hist data
        hdf <- histDF()
        
        # Reference Exercise selected
        
        exrcs <- input$HistExercise
        
        # number of sessions in Hist data
        l <- unique(hdf$Date)
        
        # Create Empty Table
        ## Date column
        Date = c()
        ## Est 1RM column
        Est1RM = c()
        # data frame to be compiled
        dframe <- data.frame(Date,Est1RM)
        
        for (i in l) {
            
            # Filter data frame for best rep by velocity, grouped by load
            df <- hdf %>%
                # filter for date selected
                filter(Date == i) %>%
                # group by load
                group_by(Load) %>%
                # filter for best rep by fastest velocity
                filter(Velocity == max(Velocity)) %>%
                # Select Date, Velocity and Load columns for calculations
                select(Date, Load, Velocity)
            
            # create value to input into Date column
            Date <- first(df$Date)
            
            # create value to input into Est1RM column
            Est1RM <- E1RMcalc(df, exrcs)
            
            # create new df to join to original data frame
            dframe2 <- data.frame(Date,Est1RM)
            
            # Join new df to original df
            dframe <- bind_rows(dframe, dframe2)
        }
        
        return(dframe)
        
    })
    
    #### Session Final Calc -----
    histSession <- reactive ({
        
        hdf <- histDF()
        df <- histSessE1RM()
        
        df2 <- hdf %>% 
            mutate("MVT" = LVmvt(Exercise)) %>%
            group_by(Date, Load) %>%
            filter(Velocity == max(Velocity)) %>%
            select(Date, Load, MVT, Velocity) %>%
            left_join(y = df, by = "Date") %>%
            mutate("relLoad" = Load/Est1RM) %>%
            group_by(Date) %>%
            summarise(
                "MVT" = max(MVT),
                "yInt" = lm(Velocity ~ Load)$coefficients[1],
                "slope" = lm(Velocity ~ Load)$coefficients[2],
                "nSlope" = lm(Velocity ~ relLoad)$coefficients[2]) %>%
            mutate("Est1RM" = round(((MVT - yInt)/slope),0),
                   "PeakF" = ((0-yInt)/slope),
                   "pFactor" = round((yInt*PeakF)/2,0),
                   "deficit" = paste0(ifelse(nSlope > -1, " Velocity", " Force"))) %>%
            transmute("Date" = Date,
                      "Power Factor" = pFactor,
                      "Est 1RM (kg)" = Est1RM,
                      "Peak Velocity (m/s)" = round(yInt,2),
                      "Peak Force (kg)" = round(PeakF,0),
                      "Slope" = round(nSlope,2),
                      "Power Deficit" = deficit) %>%
            arrange(Date)
    })
    
    #### History Session DT Output -----
    output$histSess <- renderDT({
        
        df <- histSession()
        
        ## Color and values for table color formatting
        brksPF <- seq(100, 300, 1)
        clrsPF <- colorRampPalette(c("white", "#27737b"))(length(brksPF) + 1)
        brksRM <- seq(100, 300, 1)
        clrsRM <- colorRampPalette(c("white", "#c1232a"))(length(brksRM) + 1)
        
        return(
            datatable(
                df,
                selection = "none",
                rownames = FALSE,
                extensions = c("Scroller", "Buttons"),
                options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    columnDefs = list(list(className = "dt-center", targets = "_all")),
                    deferRender = TRUE,
                    scrollY = 325,
                    scroller = TRUE
                )
            ) %>%
                formatStyle("Power Factor", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("Est 1RM (kg)", backgroundColor = styleInterval(brksRM, clrsRM))
        )
    })
    
    ### History Session Avg DT output -----
    output$histAvg <- renderDT({
        
        # Data
        hdf <- histDF()
        # Sort data
        df <- hdf %>% group_by(Date, Exercise, Load) %>%
            summarise(
                "Reps" = max(Rep),
                "Avg Mean Velocity (m/s)" = round(mean(Velocity),2),
                "SD Mean Velocity (m/s)" = round(sd(Velocity),2),
                "Avg Mean Power (W)" = round(mean(Power),2),
                "SD Mean Power (W)" = round(sd(Power),2))
        
        # Create Table
        return(
            datatable(
                df,
                rownames = FALSE,
                extensions = c("RowGroup", "Scroller", "Buttons"),
                options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    rowGroup = list(dataSrc = 0),
                    columnDefs = list(list(className = "dt-center", targets = "_all")),
                    deferRender = TRUE,
                    scrollY = 325,
                    scroller = TRUE
                )
            ) 
        )
    })
    
    ### History Session Best DT output -----
    output$histBest <- renderDT({
        
        # Data
        hdf <- histDF()
        
        # Sort data
        df <- hdf %>% group_by(Date, Exercise, Load) %>%
            filter(Velocity == max(Velocity)) %>%
            summarise(
                "Rep" = max(Rep),
                "Avg Velocity (m/s)" = round(mean(Velocity),2),
                "Avg Power (W)" = round(mean(Power),0)
            )
        
        # Create Table
        return(
            datatable(
                df,
                rownames = FALSE,
                extensions = c("RowGroup", "Scroller", "Buttons"),
                options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    rowGroup = list(dataSrc = 0),
                    columnDefs = list(list(className = "dt-center", targets = "_all")),
                    deferRender = TRUE,
                    scrollY = 325,
                    scroller = TRUE
                )
            ) 
        )
    })
    
    ### History All Reps DT output -----
    output$histAll <- renderDT({
        
        # Data
        hdf <- histDF()
        
        # Sort data
        df <- hdf %>%
            transmute("Date" = Date, 
                      "Exercise" = Exercise, 
                      "Set" = Set, 
                      "Rep" = Rep, 
                      "Load" = Load, 
                      "Avg Velocity (m/s)" = Velocity,
                      "Avg Power (W)" = Power) %>%
            group_by(Date, Exercise, Load)
        
        # Create Table
        return(
            datatable(
                df,
                rownames = FALSE,
                extensions = c("RowGroup", "Scroller", "Buttons"),
                options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                    rowGroup = list(dataSrc = c(0,2)),
                    columnDefs = list(list(className = "dt-center", targets = "_all")),
                    deferRender = TRUE,
                    scrollY = 325,
                    scroller = TRUE
                )
            ) 
        )
    })
    
    ### History Plot Data -----
    histData <- reactive({
        
        # Data
        hdf <- histDF()
        
        # Sort data
        df <- hdf %>% 
            mutate("MVT" = LVmvt(Exercise)) %>%
            group_by(Date, Load) %>%
            filter(Velocity == max(Velocity)) %>%
            select(Date, Load, MVT, Velocity) %>%
            group_by(Date) %>%
            summarise(
                "MVT" = max(MVT),
                "yInt" = lm(Velocity ~ Load)$coefficients[1],
                "slope" = lm(Velocity ~ Load)$coefficients[2]) %>%
            mutate("Est1RM" = round(((MVT - yInt)/slope),0),
                   "PeakF" = ((0-yInt)/slope),
                   "pFactor" = round((yInt*PeakF)/2,0))%>% 
            arrange(Date)
    })
    
    ### History Bar Plot -----
    output$histPlotCombo <- renderEcharts4r({
        
        avg <- list(
            type = "average",
            name = "AVG"
        )
        
        
        histData() |>
            e_charts(Date) |>
            e_bar(Est1RM, name = "Est. 1RM") |>
            e_bar(pFactor, name = "Power Factor") |>
            e_mark_line(data = avg) |>
            e_x_axis(position = "top") |>
            e_tooltip(trigger = "axis") |>
            e_toolbox_feature(feature = "saveAsImage") |>
            e_theme("infographic")
    })
    
    ### History E1RM Plot -----
    output$histPlotE1RM <- renderEcharts4r({
        
        x <- histData()
        s <- sd(x$Est1RM)
        m <- mean(x$Est1RM)
        
        L <- m-s
        U <- m+s
        
        x |>
            e_charts(Date) |>
            e_line(Est1RM, name = "Est. 1RM", legend = FALSE) |>
            e_mark_area(data = list(
                list(yAxis = L),
                list(yAxis = U)),
                itemStyle = list(color = "lightgreen")) |>
            e_tooltip(trigger = "axis") |>
            e_y_axis(
                min = round(m-(s*5),0), 
                max = round(m+(s*5),0),
                name = "Load (kg)",
                nameLocation = "middle",
                nameGap = 35) |>
            e_x_axis(position = "top") |>
            e_toolbox_feature(feature = "saveAsImage") |>
            e_theme("green")
        
    })
    
    ### History AUC Plot -----
    output$histPlotAUC <- renderEcharts4r({
        
        x <- histData()
        s <- sd(x$pFactor)
        m <- mean(x$pFactor)
        
        L <- m-s
        U <- m+s
        
        x |>
            e_charts(Date) |>
            e_line(pFactor, name = "Est. 1RM", legend = FALSE) |>
            e_mark_area(data = list(
                list(yAxis = L),
                list(yAxis = U)),
                itemStyle = list(color = "lightgreen")) |>
            e_tooltip(trigger = "axis") |>
            e_x_axis(position = "top") |>
            e_y_axis(
                min = round(m-(s*5),0), 
                max = round(m+(s*5),0),
                name = "Arbitray Units (AU)",
                nameLocation = "middle",
                nameGap = 35) |>
            e_toolbox_feature(feature = "saveAsImage") |>
            e_theme("green")
        
    })
    
    #-----#
    ### Observe History Data -----
    observe({
        if(!is.null(input$inputData)) {
            
            # History DF -
            histDF <- reactive({
                
                x <- appData()
                
                # Input filters
                user <- input$HistPlayer
                exrcs <- input$HistExercise
                
                n <- filter(x, User == user & Exercise == exrcs)
                # Filter df
                histDF <- n %>%
                    transmute(
                        "Date" = date(Date),
                        "User" = User,
                        "Exercise" = Exercise,
                        "Set" = Set,
                        "Rep" = Rep,
                        "Load" = Load,
                        "Velocity" = Velocity,
                        "Power" = Power
                    )
            })
            
            # Session First Calc -
            histSessE1RM <- reactive({
                
                #hist data
                hdf <- histDF()
                
                # Sort data
                df <- hdf %>% 
                    mutate("MVT" = ifelse(Exercise == "Back Squat", 0.3, ifelse(Exercise == "Bench Press", 0.17))) %>%
                    group_by(Date, Load) %>%
                    filter(Velocity == max(Velocity)) %>%
                    select(Date, Load, MVT, Velocity) %>%
                    group_by(Date) %>%
                    summarise(
                        "MVT" = max(MVT),
                        "yInt" = lm(Velocity ~ Load)$coefficients[1],
                        "slope" = lm(Velocity ~ Load)$coefficients[2]) %>%
                    mutate("Est1RM" = round(((MVT - yInt)/slope),0)) %>%
                    arrange(Date) %>%
                    select(Date, Est1RM)
            })
            
            # Session Final Calc -
            histSession <- reactive ({
                
                hdf <- histDF()
                df <- histSessE1RM()
                
                df2 <- hdf %>% 
                    mutate("MVT" = ifelse(Exercise == "Back Squat", 0.3, ifelse(Exercise == "Bench Press", 0.17))) %>%
                    group_by(Date, Load) %>%
                    filter(Velocity == max(Velocity)) %>%
                    select(Date, Load, MVT, Velocity) %>%
                    left_join(y = df, by = "Date") %>%
                    mutate("relLoad" = Load/Est1RM) %>%
                    group_by(Date) %>%
                    summarise(
                        "MVT" = max(MVT),
                        "yInt" = lm(Velocity ~ Load)$coefficients[1],
                        "slope" = lm(Velocity ~ Load)$coefficients[2],
                        "nSlope" = lm(Velocity ~ relLoad)$coefficients[2]) %>%
                    mutate("Est1RM" = round(((MVT - yInt)/slope),0),
                           "PeakF" = ((0-yInt)/slope),
                           "pFactor" = round((yInt*PeakF)/2,0),
                           "deficit" = paste0(ifelse(nSlope > -1, " Velocity", " Force"))) %>%
                    transmute("Date" = Date,
                              "Power Factor" = pFactor,
                              "Est 1RM (kg)" = Est1RM,
                              "Peak Velocity (m/s)" = round(yInt,2),
                              "Peak Force (kg)" = round(PeakF,0),
                              "Slope" = round(nSlope,2),
                              "Power Deficit" = deficit) %>%
                    arrange(Date)
            })
            
            # History Session DT Output -
            output$histSess <- renderDT({
                
                df <- histSession()
                
                ## Color and values for table color formatting
                brksPF <- seq(100, 300, 1)
                clrsPF <- colorRampPalette(c("white", "#27737b"))(length(brksPF) + 1)
                brksRM <- seq(100, 300, 1)
                clrsRM <- colorRampPalette(c("white", "#c1232a"))(length(brksRM) + 1)
                
                return(
                    datatable(
                        df,
                        selection = "none",
                        rownames = FALSE,
                        extensions = c("Scroller", "Buttons"),
                        options = list(
                            dom = 'Bfrtip',
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            columnDefs = list(list(className = "dt-center", targets = "_all")),
                            deferRender = TRUE,
                            scrollY = 325,
                            scroller = TRUE
                        )
                    ) %>%
                        formatStyle("Power Factor", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                        formatStyle("Est 1RM (kg)", backgroundColor = styleInterval(brksRM, clrsRM))
                )
            })
            
            # History Session Avg DT output -
            output$histAvg <- renderDT({
                
                # Data
                hdf <- histDF()
                # Sort data
                df <- hdf %>% group_by(Date, Exercise, Load) %>%
                    summarise(
                        "Reps" = max(Rep),
                        "Avg Mean Velocity (m/s)" = round(mean(Velocity),2),
                        "SD Mean Velocity (m/s)" = round(sd(Velocity),2),
                        "Avg Mean Power (W)" = round(mean(Power),2),
                        "SD Mean Power (W)" = round(sd(Power),2))
                
                # Create Table
                return(
                    datatable(
                        df,
                        rownames = FALSE,
                        extensions = c("RowGroup", "Scroller", "Buttons"),
                        options = list(
                            dom = 'Bfrtip',
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            rowGroup = list(dataSrc = 0),
                            columnDefs = list(list(className = "dt-center", targets = "_all")),
                            deferRender = TRUE,
                            scrollY = 325,
                            scroller = TRUE
                        )
                    ) 
                )
            })
            
            # History Session Best DT output -
            output$histBest <- renderDT({
                
                # Data
                hdf <- histDF()
                
                # Sort data
                df <- hdf %>% group_by(Date, Exercise, Load) %>%
                    filter(Velocity == max(Velocity)) %>%
                    summarise(
                        "Rep" = max(Rep),
                        "Avg Velocity (m/s)" = round(mean(Velocity),2),
                        "Avg Power (W)" = round(mean(Power),0)
                    )
                
                # Create Table
                return(
                    datatable(
                        df,
                        rownames = FALSE,
                        extensions = c("RowGroup", "Scroller", "Buttons"),
                        options = list(
                            dom = 'Bfrtip',
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            rowGroup = list(dataSrc = 0),
                            columnDefs = list(list(className = "dt-center", targets = "_all")),
                            deferRender = TRUE,
                            scrollY = 325,
                            scroller = TRUE
                        )
                    ) 
                )
            })
            
            # History All Reps DT output -
            output$histAll <- renderDT({
                
                # Data
                hdf <- histDF()
                
                # Sort data
                df <- hdf %>%
                    transmute("Date" = Date, 
                              "Exercise" = Exercise, 
                              "Set" = Set, 
                              "Rep" = Rep, 
                              "Load" = Load, 
                              "Avg Velocity (m/s)" = Velocity,
                              "Avg Power (W)" = Power) %>%
                    group_by(Date, Exercise, Load)
                
                # Create Table
                return(
                    datatable(
                        df,
                        rownames = FALSE,
                        extensions = c("RowGroup", "Scroller", "Buttons"),
                        options = list(
                            dom = 'Bfrtip',
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                            rowGroup = list(dataSrc = c(0,2)),
                            columnDefs = list(list(className = "dt-center", targets = "_all")),
                            deferRender = TRUE,
                            scrollY = 325,
                            scroller = TRUE
                        )
                    ) 
                )
            })
            
            # History Plot Data -
            histData <- reactive({
                
                # Data
                hdf <- histDF()
                
                # Sort data
                df <- hdf %>% 
                    mutate("MVT" = LVmvt(Exercise)) %>%
                    group_by(Date, Load) %>%
                    filter(Velocity == max(Velocity)) %>%
                    select(Date, Load, MVT, Velocity) %>%
                    group_by(Date) %>%
                    summarise(
                        "MVT" = max(MVT),
                        "yInt" = lm(Velocity ~ Load)$coefficients[1],
                        "slope" = lm(Velocity ~ Load)$coefficients[2]) %>%
                    mutate("Est1RM" = round(((MVT - yInt)/slope),0),
                           "PeakF" = ((0-yInt)/slope),
                           "pFactor" = round((yInt*PeakF)/2,0))%>% 
                    arrange(Date)
            })
            
            # History Bar Plot -
            output$histPlotCombo <- renderEcharts4r({
                
                avg <- list(
                    type = "average",
                    name = "AVG"
                )
                
                
                histData() |>
                    e_charts(Date) |>
                    e_bar(Est1RM, name = "Est. 1RM") |>
                    e_bar(pFactor, name = "Power Factor") |>
                    e_mark_line(data = avg) |>
                    e_x_axis(position = "top") |>
                    e_tooltip(trigger = "axis") |>
                    e_toolbox_feature(feature = "saveAsImage") |>
                    e_theme("infographic")
            })
            
            # History E1RM Plot -
            output$histPlotE1RM <- renderEcharts4r({
                
                x <- histData()
                s <- sd(x$Est1RM)
                m <- mean(x$Est1RM)
                
                L <- m-s
                U <- m+s
                
                x |>
                    e_charts(Date) |>
                    e_line(Est1RM, name = "Est. 1RM", legend = FALSE) |>
                    e_mark_area(data = list(
                        list(yAxis = L),
                        list(yAxis = U)),
                        itemStyle = list(color = "lightgreen")) |>
                    e_tooltip(trigger = "axis") |>
                    e_y_axis(
                        min = round(m-(s*5),0), 
                        max = round(m+(s*5),0),
                        name = "Load (kg)",
                        nameLocation = "middle",
                        nameGap = 35) |>
                    e_x_axis(position = "top") |>
                    e_toolbox_feature(feature = "saveAsImage") |>
                    e_theme("green")
                
            })
            
            # History AUC Plot -
            output$histPlotAUC <- renderEcharts4r({
                
                x <- histData()
                s <- sd(x$pFactor)
                m <- mean(x$pFactor)
                
                L <- m-s
                U <- m+s
                
                x |>
                    e_charts(Date) |>
                    e_line(pFactor, name = "Est. 1RM", legend = FALSE) |>
                    e_mark_area(data = list(
                        list(yAxis = L),
                        list(yAxis = U)),
                        itemStyle = list(color = "lightgreen")) |>
                    e_tooltip(trigger = "axis") |>
                    e_x_axis(position = "top") |>
                    e_y_axis(
                        min = round(m-(s*5),0), 
                        max = round(m+(s*5),0),
                        name = "Arbitray Units (AU)",
                        nameLocation = "middle",
                        nameGap = 35) |>
                    e_toolbox_feature(feature = "saveAsImage") |>
                    e_theme("green")
                
            })
        }
    })
    
    #----------#
    ## S2S -----
    ### Reactive Filters -----
    exerciseS2S <- reactive({
        input$exrcsS2S
    })
    
    s2sSelect1 <- reactive({
        input$S2SselectP1
    })
    
    s2sSelect2 <- reactive({
        input$S2SselectP2
    })
    
    ### Exercise Observe Function -----
    observe({
        exList <- unique(appData()$Exercise)
        
        updateSelectInput(session,"exrcsS2S",choices=exList, selected = exList[1])
    })
    
    ###  Athlete Observe Function -----
    observe({
        
        exrcs <- input$exrcsS2S
        
        n <- appData()
        
        # List of Athletes filtered from exercise select
        athList <- unique(n$User[n$Exercise == exrcs])
        
        # Update P1 select input
        updateSelectInput(session,"S2SselectP1",choices=athList, selected = athList[1])
        # Update P2 select input
        updateSelectInput(session,"S2SselectP2",choices=athList, selected = athList[1])
    })
    
    ###  Date 1 Observe Function -----
    observe({
        
        n <- appData()
        ath <- s2sSelect1()
        exrcs <- exerciseS2S()
        
        # List of dates for P1
        dateList <- unique(n$Date[n$Exercise==exrcs & n$User==ath])
        
        # Update Date1 select input
        updateSelectInput(session,"S2SdateP1",choices=dateList, selected = dateList[1])
    })
    
    ###  Date 2 Observe Function -----
    observe({
        
        n <- appData()
        ath <- s2sSelect2()
        exrcs <- exerciseS2S()
        
        # List of dates for P1
        dateList <- unique(n$Date[n$Exercise==exrcs & n$User==ath])
        
        # Update Date1 select input
        updateSelectInput(session,"S2SdateP2",choices=dateList, selected = dateList[1])
    })
    
    
    ### S2S 1 Data -----
    dfP2P1 <- reactive({
        
        ath <- input$S2SselectP1
        exrcs <- input$exrcsS2S
        date <- input$S2SdateP1
        
        P2Pdf1 <- appData() %>%
            filter(User == ath, Exercise == exrcs, Date == date) %>%
            group_by(Load) %>%
            filter(Velocity == max(Velocity)) %>%
            transmute(
                "Exercise" = exrcs,
                "Load" = Load, 
                "Velocity" = round(Velocity,2),
                "Power" = round(Power,0)
            )
        
        return(P2Pdf1)
    })
    
    #### S2S 1 Outputs -----
    P2Pout1 <- reactive({
        
        # P1 Data
        df <- dfP2P1()
        
        output1 <- LVcoef(df)
        
        return(output1)
    })
    
    ##### S2S 1 e1RM -----
    output$e1RM1 <- renderValueBox({
        val <- P2Pout1()
        valueBox(
            value = val$e1rm[1], 
            subtitle = "Est. 1RM (kg)", 
            icon = icon("dumbbell"),
            color = "red"
        )
    })
    
    ##### S2S 1 PowFactor -----
    output$PowFactor1 <- renderValueBox({
        val <- P2Pout1()
        
        # Solve for area of right triangle
        x <- round((val$yInt[1] * val$L0[1])/2,0)
        
        valueBox(
            value = x,
            subtitle = "Power Factor", 
            icon = icon("bolt"),
            color = "red"
        )
    })
    
    ##### S2S 1 PeakF -----
    output$PeakF1 <- renderValueBox({
        val <- P2Pout1()
        
        valueBox(
            value = round(val$L0[1],0),
            subtitle = "Peak Force (kg)", 
            icon = icon("arrows-up-to-line"),
            color = "red"
        )
    })
    
    ##### S2S 1 PeakVelo -----
    output$PeakV1 <- renderValueBox({
        val <- P2Pout1()
        
        valueBox(
            value = round(val$yInt[1],2),
            subtitle = "Peak Velocity (m/s)", 
            icon = icon("gauge-high"),
            color = "red"
        )
    })
    
    #### Summary Table 1 ----- 
    output$S2SSumm1 <- renderDT({
        
        df <- dfP2P1()
        
        df <- df %>%
            transmute(
                "Load" = Load,
                "Avg. Velocity (m/s)" = Velocity,
                "Avg. Power (w)" = Power
            ) %>%
            arrange(Load)
        
        datatable(
            df, 
            rownames = FALSE,
            filter = "none",
            options = list(
                dom = 't',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
            )
        )
    })
    
    ### S2S 2 Data -----
    dfP2P2 <- reactive({
        
        ath <- input$S2SselectP2
        exrcs <- input$exrcsS2S
        date <- input$S2SdateP2
        
        P2Pdf2 <- appData() %>%
            filter(User == ath, Exercise == exrcs, Date == date) %>%
            group_by(Load) %>%
            filter(Velocity == max(Velocity)) %>%
            transmute(
                "Exercise" = exrcs,
                "Load" = Load, 
                "Velocity" = round(Velocity,2),
                "Power" = round(Power,0)
            )
        
        return(P2Pdf2)
    })
    
    #### Summary Table 2 ----- 
    output$S2SSumm2 <- renderDT({
        # Column Names
        df <- dfP2P2()
        
        df <- df %>%
            transmute(
                "Load" = Load,
                "Avg. Velocity (m/s)" = Velocity,
                "Avg. Power (w)" = Power
            ) %>%
            arrange(Load)
        
        datatable(
            df, 
            rownames = FALSE,
            filter = "none",
            options = list(
                dom = 't',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
            )
        )
    })
    
    #### S2S 2 Outputs -----
    P2Pout2 <- reactive({
        
        # P2 data
        df <- dfP2P2()
        
        output2 <- LVcoef(df)
        
        return(output2)
    })
    
    ##### S2S 2 e1RM -----
    output$e1RM2 <- renderValueBox({
        val <- P2Pout2()
        valueBox(
            value = val$e1rm[1],
            subtitle = "Est. 1RM (kg)", 
            icon = icon("dumbbell"),
            color = "olive"
        )
    })
    
    ##### S2S 2 PowFactor -----
    output$PowFactor2 <- renderValueBox({
        val <- P2Pout2()
        
        x <- round((val$L0[1] * val$yInt[1])/2,0)
        
        valueBox(
            value = x, 
            subtitle = "Power Factor", 
            icon = icon("bolt"),
            color = "olive"
        )
    })
    
    ##### S2S 2 PeakF -----
    output$PeakF2 <- renderValueBox({
        val <- P2Pout2()
        
        valueBox(
            value = round(val$L0[1],0), 
            subtitle = "Peak Force (kg)", 
            icon = icon("arrows-up-to-line"),
            color = "olive"
        )
    })
    
    ##### S2S 2 PeakVelo -----
    output$PeakV2 <- renderValueBox({
        val <- P2Pout2()
        
        valueBox(
            value = round(val$yInt[1],2),
            subtitle = "Peak Velocity (m/s)", 
            icon = icon("gauge-high"),
            color = "olive"
        )
    })
    
    ### S2S Plot Data joined -----
    dfS2SplotData <- reactive({
        
        #Session 1
        sess1 <- "Session 1"
        
        # Add session 1 column
        s1df <- dfP2P1() %>%
            mutate("Session" = sess1)
        
        
        #Session 2
        sess2 <- "Session 2"
        
        # Add session 2 column
        s2df <- dfP2P2() %>%
            mutate("Session" = sess2)
        
        
        # Combined Session table
        S2Sjoined <- bind_rows(s1df, s2df)
        
        # Group by Session
        S2Sjoined <- group_by(S2Sjoined, Session)
        
        return(S2Sjoined)
    })
    
    ### S2S LV Plot Output ----- 
    output$S2SplotLV <-  renderEcharts4r({
        dfS2SplotData() |> 
            group_by(Session) |>
            e_charts(Load) |> 
            e_scatter(
                Velocity,
                bind = Load,
                symbol = "diamond",
                symbol_size = 10,
                legend = FALSE
            ) |>
            e_lm(
                Velocity ~ Load, name = c("Session 1", "Session 2")) |>
            e_title(
                text = "Load-Velocity Relationship: Session Comparison") |>
            e_tooltip(
                formatter = htmlwidgets::JS(
                    "function(params){
        return('<strong>Load: ' + params.name + 
                '</strong><br />%1RM: ' + params.value[0] + 
                '<br />MV: ' + params.value[1]) 
                }"
                )
            )|>
            #e_axis_labels(x = "kg", y = "m/s") |>
            e_legend(right = "35%", bottom = 0) |>
            e_x_axis(
                min = 0, 
                max = 200,
                name = "Load (kg)",
                nameLocation = "middle",
                nameGap = 30) |>
            e_y_axis(
                min = 0, 
                max = 2, 
                name = "Velocity (m/s)",
                nameLocation = "middle",
                nameGap = 35,
                formatter = e_axis_formatter(
                    digits = 2)) |>
            e_toolbox_feature(
                feature = "saveAsImage") |>
            e_theme("infographic")
    })
    
    ### S2S Power Plot Output ----- 
    output$S2SplotPOW <- renderPlot({
        df <-  dfS2SplotData()
        
        plot <- ggplot(df, aes(x = Load, y = Power, color = Session)) +
            geom_point(size = 5) +
            geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, 
                        level = 0.85, aes(fill = Session), alpha = 0.25) +
            scale_color_manual(values = c("#de4c39", "#3d9970"), 
                               labels = c("S1", "S2")) +
            scale_fill_manual(values = c("#de4c39", "#3d9970"), 
                              labels = c("S1", "S2")) +
            labs(x = "Load (kg)", y = "Power (W)", title = "Load-Power Relationship: Session Comparison") +
            theme_minimal() +
            theme(legend.position = "bottom")
        
        return(plot)
    })
    
    #-----#
    ### Observe S2S Function -----
    observeEvent(input$inputData, {
        
        # S2S 1 Data -
        dfP2P1 <- reactive({
            
            ath <- input$S2SselectP1
            exrcs <- input$exrcsS2S
            date <- input$S2SdateP1
            
            P2Pdf1 <- appData() %>%
                filter(User == ath, Exercise == exrcs, Date == date) %>%
                group_by(Load) %>%
                filter(Velocity == max(Velocity)) %>%
                transmute(
                    "Exercise" = exrcs,
                    "Load" = Load, 
                    "Velocity" = round(Velocity,2),
                    "Power" = round(Power,0)
                )
            
            return(P2Pdf1)
        })
        
        # S2S 1 Outputs -
        P2Pout1 <- reactive({
            
            # P1 Data
            df <- dfP2P1()
            
            output1 <- LVcoef(df)
            
            return(output1)
        })
        
        # S2S 1 e1RM -
        output$e1RM1 <- renderValueBox({
            val <- P2Pout1()
            valueBox(
                value = val$e1rm[1], 
                subtitle = "Est. 1RM (kg)", 
                icon = icon("dumbbell"),
                color = "red"
            )
        })
        
        # S2S 1 PowFactor -
        output$PowFactor1 <- renderValueBox({
            val <- P2Pout1()
            
            # Solve for area of right triangle
            x <- round((val$yInt[1] * val$L0[1])/2,0)
            
            valueBox(
                value = x,
                subtitle = "Power Factor", 
                icon = icon("bolt"),
                color = "red"
            )
        })
        
        # S2S 1 PeakF -
        output$PeakF1 <- renderValueBox({
            val <- P2Pout1()
            
            valueBox(
                value = round(val$L0[1],0),
                subtitle = "Peak Force (kg)", 
                icon = icon("arrows-up-to-line"),
                color = "red"
            )
        })
        
        # S2S 1 PeakVelo -
        output$PeakV1 <- renderValueBox({
            val <- P2Pout1()
            
            valueBox(
                value = round(val$yInt[1],2),
                subtitle = "Peak Velocity (m/s)", 
                icon = icon("gauge-high"),
                color = "red"
            )
        })
        
        # Summary Table 1 -
        output$S2SSumm1 <- renderDT({
            
            df <- dfP2P1()
            
            df <- df %>%
                transmute(
                    "Load" = Load,
                    "Avg. Velocity (m/s)" = Velocity,
                    "Avg. Power (w)" = Power
                ) %>%
                arrange(Load)
            
            datatable(
                df, 
                rownames = FALSE,
                filter = "none",
                options = list(
                    dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                )
            )
        })
        
        # S2S 2 Data -
        dfP2P2 <- reactive({
            
            ath <- input$S2SselectP2
            exrcs <- input$exrcsS2S
            date <- input$S2SdateP2
            
            P2Pdf2 <- appData() %>%
                filter(User == ath, Exercise == exrcs, Date == date) %>%
                group_by(Load) %>%
                filter(Velocity == max(Velocity)) %>%
                transmute(
                    "Exercise" = exrcs,
                    "Load" = Load, 
                    "Velocity" = round(Velocity,2),
                    "Power" = round(Power,0)
                )
            
            return(P2Pdf2)
        })
        
        # Summary Table 2 -
        output$S2SSumm2 <- renderDT({
            # Column Names
            df <- dfP2P2()
            
            df <- df %>%
                transmute(
                    "Load" = Load,
                    "Avg. Velocity (m/s)" = Velocity,
                    "Avg. Power (w)" = Power
                ) %>%
                arrange(Load)
            
            datatable(
                df, 
                rownames = FALSE,
                filter = "none",
                options = list(
                    dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                )
            )
        })
        
        # S2S 2 Outputs -
        P2Pout2 <- reactive({
            
            # P2 data
            df <- dfP2P2()
            
            output2 <- LVcoef(df)
            
            return(output2)
        })
        
        # S2S 2 e1RM -
        output$e1RM2 <- renderValueBox({
            val <- P2Pout2()
            valueBox(
                value = val$e1rm[1],
                subtitle = "Est. 1RM (kg)", 
                icon = icon("dumbbell"),
                color = "olive"
            )
        })
        
        # S2S 2 PowFactor -
        output$PowFactor2 <- renderValueBox({
            val <- P2Pout2()
            
            x <- round((val$L0[1] * val$yInt[1])/2,0)
            
            valueBox(
                value = x, 
                subtitle = "Power Factor", 
                icon = icon("bolt"),
                color = "olive"
            )
        })
        
        # S2S 2 PeakF -
        output$PeakF2 <- renderValueBox({
            val <- P2Pout2()
            
            valueBox(
                value = round(val$L0[1],0), 
                subtitle = "Peak Force (kg)", 
                icon = icon("arrows-up-to-line"),
                color = "olive"
            )
        })
        
        # S2S 2 PeakVelo -
        output$PeakV2 <- renderValueBox({
            val <- P2Pout2()
            
            valueBox(
                value = round(val$yInt[1],2),
                subtitle = "Peak Velocity (m/s)", 
                icon = icon("gauge-high"),
                color = "olive"
            )
        })
        
        # S2S Plot Data joined -
        dfS2SplotData <- reactive({
            
            #Session 1
            sess1 <- "Session 1"
            
            # Add session 1 column
            s1df <- dfP2P1() %>%
                mutate("Session" = sess1)
            
            
            #Session 2
            sess2 <- "Session 2"
            
            # Add session 2 column
            s2df <- dfP2P2() %>%
                mutate("Session" = sess2)
            
            
            # Combined Session table
            S2Sjoined <- bind_rows(s1df, s2df)
            
            # Group by Session
            S2Sjoined <- group_by(S2Sjoined, Session)
            
            return(S2Sjoined)
        })
        
        #S2S LV Plot Output -
        output$S2SplotLV <-  renderEcharts4r({
            dfS2SplotData() |> 
                group_by(Session) |>
                e_charts(Load) |> 
                e_scatter(
                    Velocity,
                    bind = Load,
                    symbol = "diamond",
                    symbol_size = 10,
                    legend = FALSE
                ) |>
                e_lm(
                    Velocity ~ Load, name = c("Session 1", "Session 2")) |>
                e_title(
                    text = "Load-Velocity Relationship: Session Comparison") |>
                e_tooltip(
                    formatter = htmlwidgets::JS(
                        "function(params){
        return('<strong>Load: ' + params.name + 
                '</strong><br />%1RM: ' + params.value[0] + 
                '<br />MV: ' + params.value[1]) 
                }"
                    )
                )|>
                #e_axis_labels(x = "kg", y = "m/s") |>
                e_legend(right = "35%", bottom = 0) |>
                e_x_axis(
                    min = 0, 
                    max = 200,
                    name = "Load (kg)",
                    nameLocation = "middle",
                    nameGap = 30) |>
                e_y_axis(
                    min = 0, 
                    max = 2, 
                    name = "Velocity (m/s)",
                    nameLocation = "middle",
                    nameGap = 35,
                    formatter = e_axis_formatter(
                        digits = 2)) |>
                e_toolbox_feature(
                    feature = "saveAsImage") |>
                e_theme("infographic")
        })
        
        # S2S Power Plot Output -
        output$S2SplotPOW <- renderPlot({
            df <-  dfS2SplotData()
            
            plot <- ggplot(df, aes(x = Load, y = Power, color = Session)) +
                geom_point(size = 5) +
                geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = TRUE, 
                            level = 0.85, aes(fill = Session), alpha = 0.25) +
                scale_color_manual(values = c("#de4c39", "#3d9970"), 
                                   labels = c("S1", "S2")) +
                scale_fill_manual(values = c("#de4c39", "#3d9970"), 
                                  labels = c("S1", "S2")) +
                labs(x = "Load (kg)", y = "Power (W)", title = "Load-Power Relationship: Session Comparison") +
                theme_minimal() +
                theme(legend.position = "bottom")
            
            
            return(plot)
        })
    })
    
    #----------#
    ## Training Recs Tab ----- 
    
    ### Reactive Filters -----
    TRplayer <- reactive({
        input$playerPP
    })
    
    TRexercise <- reactive({
        input$exercisePP
    })
    
    TRdata <- reactive({
        input$datePP
    })
    
    ###  TR Athlete Observe Function -----
    observe({
        
        df <- appData()
        
        # List of Athletes filtered from exercise select
        athList <- unique(df$User)
        
        # Update P1 select input
        updateSelectInput(session,"playerPP",choices=athList, selected = athList[1])
        
    })
    
    ###  TR Exercise Observe Function -----
    observe({
        df <- appData()
        
        ath <- input$playerPP
        
        # List of Athletes filtered from exercise select
        exList <- unique(df$Exercise[df$User==ath])
        
        # Update PP Exercise select input
        updateSelectInput(session,"exercisePP",choices=exList, selected = exList[1])
    })
    
    ###  TR Date Observe Function -----
    observe({
        df <- appData()
        
        ath <- input$playerPP
        exrcs <- input$exercisePP
        
        # List of Athletes filtered from exercise select
        dateList <- unique(df$Date[df$User==ath & df$Exercise==exrcs])
        
        # Update PP Date select input
        updateSelectInput(session,"datePP",choices=dateList, selected = dateList[1])
    })
    
    #-----#
    
    ### TR LV Filters -----
    dfTR <- reactive({
        
        ath <- TRplayer()
        exrcs <- TRexercise()
        date <- TRdata()
        
        df <- appData() %>%
            filter(User == ath, Exercise == exrcs, Date == date) %>%
            group_by(Load) %>%
            summarise(
                "Exercise" = first(Exercise),
                "Velocity" = max(Velocity)
            )
        
        return(df)
    })
    
    ### TR LV Coefficients -----
    trLVcoef <- reactive({
        
        # data from TR LV filters
        df <- dfTR()
        
        # use LVcoef function
        x <- LVcoef(df)
        
        return(x)
    })
    
    ### TR LV regression model -----
    dfLV <- reactive({
        
        # data from TR LV Coefficients
        df <- trLVcoef()
        
        # Use LV Data Frame function
        x <- LVdf(df)
        
        return(x)
    })
    
    ### TR Rec Table Data -----
    dfRecTable <- reactive({
        
        # data from TR LV Coefficients
        df <- trLVcoef()
        
        data <- IntnstyVelo(df)
        
        return(data)
    })
    
    ### TR Rec Zone Data -----
    dfRecZones <- reactive({
        
        # data from TR LV Coefficients
        df <- trLVcoef()
        
        x <- trnZones(df)
        
        return(x)
    })
    
    ### TR Summary Table ----- 
    output$PPtable <- renderDT({
        datatable(
            dfPow(), 
            rownames = FALSE,
            filter = "none",
            options = list(
                dom = 't',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
            )
        )
    })
    
    #---------#
    ### Training Zones Outputs -----
    
    ##### Max Strength Zone -----
    output$MSZone <- renderValueBox({
        val <- dfRecZones()
        
        minL <-  round(val$MaxStr[1],2)
        maxL <-  round(val$MaxStr[2],2)
        
        valueBox(
            value = paste0(maxL, "m/s", " - ", minL, "m/s"),
            subtitle = "Max Strength Zone", 
            color = "red"
        )
    })
    
    ##### Strength-Speed Zone -----
    output$StSpZone <- renderValueBox({
        val <- dfRecZones()
        
        minL <-  round(val$StrSpd[1],2)
        maxL <-  round(val$StrSpd[2],2)
        
        valueBox(
            value = paste0(maxL, "m/s", " - ", minL, "m/s"),
            subtitle = "Strength-Speed Zone",
            color = "yellow"
        )
    })
    
    ##### Peak Power Zone -----
    output$PPZone <- renderValueBox({
        val <- dfRecZones()
        
        minL <-  round(val$PeakP[1],2)
        maxL <-  round(val$PeakP[2],2)
        
        valueBox(
            value = paste0(maxL, "m/s", " - ", minL, "m/s"),
            subtitle = "Peak Power Zone", 
            color = "aqua"
        )
    })
    
    ##### Speed-Strength Zone -----
    output$SpStZone <- renderValueBox({
        val <- dfRecZones()
        
        minL <-  round(val$SpdStr[1],2)
        maxL <-  round(val$SpdStr[2],2)
        
        valueBox(
            value = paste0(maxL, "m/s", " - ", minL, "m/s"),
            subtitle = "Speed-Strength Zone", 
            color = "green"
        )
    })
    
    ##### Max Speed Zone -----
    output$MaxSpdZone <- renderValueBox({
        val <- dfRecZones()
        
        minL <-  round(val$MaxSpd[1],2)
        maxL <-  round(val$MaxSpd[2],2)
        
        valueBox(
            value = paste0(maxL, "m/s", " - ", minL, "m/s"),
            subtitle = "Max Speed Zone",
            color = "light-blue"
        )
    })
    
    #----------#
    #### Training Zones Plot -----
    
    ##### LV eChart Output -----
    output$LVplot <- renderEcharts4r({
        
        # Load-Velocity dataframe
        df <- dfLV() %>%
            filter(Zone %in% c("Max Strength", "Strength-Speed", "Power","Speed-Strength","Max Speed"))
        
        
        min <- list(
            name = "Min",
            type = "min"
        )
        
        PowPlot <- df |>
            group_by(Zone) |>
            e_charts(Loads) |>
            e_area(Velocity, legend = TRUE, symbol = "none") |>
            e_title(text ="Grouped by Estimated Loading Zones") |>
            e_tooltip(
                trigger = "item",
                formatter = e_tooltip_pointer_formatter(digits = 2),
                axisPointer = list(
                    type = "cross")) |>
            e_x_axis(
                min = min(df$Loads), 
                max = max(df$Loads),
                name = "Load (kg)",
                nameLocation = "middle",
                nameGap = 25) |>
            e_y_axis(
                min = min(df$Velocity), 
                max = max(df$Velocity),
                name = "Velocity (m/s)",
                nameLocation = "middle",
                nameGap = 30) |>
            e_toolbox_feature(
                feature = "saveAsImage") |>
            e_legend(type = "scroll", bottom = "2%")|>
            e_theme("LV_Theme")
        
        return(PowPlot)
    })
    
    #### PP Rec Table Output -----
    output$RecTable <- renderDT({
        
        # Recommendation table data
        df <- dfRecTable()
        
        min <- min(df$R1)
        max <- max(df$R8)
        
        ## Color and values for table color formatting
        brksPF <- seq(min, max , 0.1)
        clrsPF <- colorRampPalette(c("#c1232a", "white"))(length(brksPF) + 1)
        
        datatable(
            df, 
            rownames = FALSE,
            filter = "none",
            options = list(
                dom = 't',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
            )
        ) %>%
            formatStyle("R1", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
            formatStyle("R2", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
            formatStyle("R3", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
            formatStyle("R4", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
            formatStyle("R5", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
            formatStyle("R6", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
            formatStyle("R7", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
            formatStyle("R8", backgroundColor = styleInterval(brksPF, clrsPF))
    })
    
    #-----#
    ### Observe TR Tab Function -----
    
    observeEvent( input$inputData, {
        # TR LV Filters -
        dfTR <- reactive({
            
            ath <- TRplayer()
            exrcs <- TRexercise()
            date <- TRdata()
            
            df <- appData() %>%
                filter(User == ath, Exercise == exrcs, Date == date) %>%
                group_by(Load) %>%
                summarise(
                    "Exercise" = first(Exercise),
                    "Velocity" = max(Velocity)
                )
            
            return(df)
        })
        
        # TR LV Coefficients -
        trLVcoef <- reactive({
            
            # data from TR LV filters
            df <- dfTR()
            
            # use LVcoef function
            x <- LVcoef(df)
            
            return(x)
        })
        
        # TR LV regression model -
        dfLV <- reactive({
            
            # data from TR LV Coefficients
            df <- trLVcoef()
            
            # Use LV Data Frame function
            x <- LVdf(df)
            
            return(x)
        })
        
        # TR Rec Table Data -
        dfRecTable <- reactive({
            
            # data from TR LV Coefficients
            df <- trLVcoef()
            
            data <- IntnstyVelo(df)
            
            return(data)
        })
        
        # TR Rec Zone Data -
        dfRecZones <- reactive({
            
            # data from TR LV Coefficients
            df <- trLVcoef()
            
            x <- trnZones(df)
            
            return(x)
        })
        
        # TR Summary Table -
        output$PPtable <- renderDT({
            datatable(
                dfPow(), 
                rownames = FALSE,
                filter = "none",
                options = list(
                    dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                )
            )
        })
        
        #---------#
        # Training Zones Outputs -
        
        # Max Strength Zone -
        output$MSZone <- renderValueBox({
            val <- dfRecZones()
            
            minL <-  round(val$MaxStr[1],2)
            maxL <-  round(val$MaxStr[2],2)
            
            valueBox(
                value = paste0(maxL, "m/s", " - ", minL, "m/s"),
                subtitle = "Max Strength Zone", 
                #icon = icon("gauge-high"),
                color = "red"
            )
        })
        
        # Strength-Speed Zone -
        output$StSpZone <- renderValueBox({
            val <- dfRecZones()
            
            minL <-  round(val$StrSpd[1],2)
            maxL <-  round(val$StrSpd[2],2)
            
            valueBox(
                value = paste0(maxL, "m/s", " - ", minL, "m/s"),
                subtitle = "Strength-Speed Zone",
                #icon = icon("gauge-high"),
                color = "yellow"
            )
        })
        
        # Peak Power Zone -
        output$PPZone <- renderValueBox({
            val <- dfRecZones()
            
            minL <-  round(val$PeakP[1],2)
            maxL <-  round(val$PeakP[2],2)
            
            valueBox(
                value = paste0(maxL, "m/s", " - ", minL, "m/s"),
                subtitle = "Peak Power Zone", 
                #icon = icon("gauge-high"),
                color = "aqua"
            )
        })
        
        # Speed-Strength Zone -
        output$SpStZone <- renderValueBox({
            val <- dfRecZones()
            
            minL <-  round(val$SpdStr[1],2)
            maxL <-  round(val$SpdStr[2],2)
            
            valueBox(
                value = paste0(maxL, "m/s", " - ", minL, "m/s"),
                subtitle = "Speed-Strength Zone", 
                #icon = icon("gauge-high"),
                color = "green"
            )
        })
        
        # Max Speed Zone -
        output$MaxSpdZone <- renderValueBox({
            val <- dfRecZones()
            
            minL <-  round(val$MaxSpd[1],2)
            maxL <-  round(val$MaxSpd[2],2)
            
            valueBox(
                value = paste0(maxL, "m/s", " - ", minL, "m/s"),
                subtitle = "Max Speed Zone",
                #icon = icon("gauge-high"),
                color = "light-blue"
            )
        })
        
        #----------#
        # Training Zones Plot -
        
        # LV eChart Output -
        output$LVplot <- renderEcharts4r({
            
            # Load-Velocity dataframe
            df <- dfLV() %>%
                filter(Zone %in% c("Max Strength", "Strength-Speed", "Power","Speed-Strength","Max Speed"))
            
            
            min <- list(
                name = "Min",
                type = "min"
            )
            
            PowPlot <- df |>
                group_by(Zone) |>
                e_charts(Loads) |>
                e_area(Velocity, legend = TRUE, symbol = "none") |>
                e_title(text ="Grouped by Estimated Loading Zones") |>
                e_tooltip(
                    trigger = "item",
                    formatter = e_tooltip_pointer_formatter(digits = 2),
                    axisPointer = list(
                        type = "cross")) |>
                e_x_axis(
                    min = min(df$Loads), 
                    max = max(df$Loads),
                    name = "Load (kg)",
                    nameLocation = "middle",
                    nameGap = 25) |>
                e_y_axis(
                    min = min(df$Velocity), 
                    max = max(df$Velocity),
                    name = "Velocity (m/s)",
                    nameLocation = "middle",
                    nameGap = 30) |>
                e_toolbox_feature(
                    feature = "saveAsImage") |>
                e_legend(type = "scroll", bottom = "2%")|>
                e_theme("LV_Theme")
            
            return(PowPlot)
        })
        
        # PP Rec Table Output -
        output$RecTable <- renderDT({
            
            # Recommendation table data
            df <- dfRecTable()
            
            min <- min(df$R1)
            max <- max(df$R8)
            
            ## Color and values for table color formatting
            brksPF <- seq(min, max , 0.1)
            clrsPF <- colorRampPalette(c("#c1232a", "white"))(length(brksPF) + 1)
            
            datatable(
                df, 
                rownames = FALSE,
                filter = "none",
                options = list(
                    dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                )
            ) %>%
                formatStyle("R1", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("R2", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("R3", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("R4", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("R5", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("R6", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("R7", backgroundColor = styleInterval(brksPF, clrsPF)) %>%
                formatStyle("R8", backgroundColor = styleInterval(brksPF, clrsPF))
        })
    })
    
    #----------#
    ## Team Training Tab ----- 
    ### Reactive Filters -----
    TTexercise <- reactive({
        input$exerciseTeam
    })
    
    TTdate <- reactive({
        input$dateTeam
    })
    
    ###  TT Exercise Observe Function -----
    observe({
        n <- appData()
        # List of dates for P1
        exList <- unique(n$Exercise)
        
        # Update PP Date select input
        updateSelectInput(session,"exerciseTeam",choices=exList, selected = exList[1])
    })
    
    ### TT Date Observe Function -----
    observe({
        n <- appData()
        
        exrcs <- input$exerciseTeam
        
        # List of dates for P1
        dateList <- unique(n$Date[n$Exercise==exrcs])
        
        # Update PP Date select input
        updateSelectInput(session,"dateTeam",choices=dateList, selected = dateList[1])
    })
    
    ### Team Training dataframe -----
    dfTeam <- reactive({
        exrcs <- TTexercise()
        date <- TTdate()
        
        df <- appData() %>%
            filter(
                Exercise == exrcs,
                Date == date
            ) %>%
            group_by(User, Load) %>%
            summarise(
                "Exercise" = first(Exercise),
                "Velocity" = max(Velocity)
            )
        
        return(df)
    })
    
    ### Team Velocity df -----
    dfVeloTeam <- reactive({
        data <- dfTeam()
        
        df <- dfTeamVelo(data)
        
        return(df)
    })
    
    ### Team Load df -----
    dfLoadTeam <- reactive({
        data <- dfTeam()
        
        df <- dfTeamLoad(data)
        
        return(df)
    })
    
    ### Team Training Table ----- 
    output$TeamTable <- renderDT({
        
        # Recommendation table data
        df <- if(input$radioTeam == "velo") {
            dfVeloTeam()
        } else {
            dfLoadTeam()
        }
        
        datatable(
            df, 
            rownames = FALSE,
            filter = "none",
            options = list(
                dom = 't',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
            )
        )
    })
    
    #-----#
    
    ### TT Observe Function -----
    observeEvent(input$inputData, {
        # Team Training dataframe -
        dfTeam <- reactive({
            exrcs <- TTexercise()
            date <- TTdate()
            
            df <- appData() %>%
                filter(
                    Exercise == exrcs,
                    Date == date
                ) %>%
                group_by(User, Load) %>%
                summarise(
                    "Exercise" = first(Exercise),
                    "Velocity" = max(Velocity)
                )
            
            return(df)
        })
        
        # Team Velocity df -
        dfVeloTeam <- reactive({
            data <- dfTeam()
            
            df <- dfTeamVelo(data)
            
            return(df)
        })
        
        # Team Velocity df -
        dfLoadTeam <- reactive({
            data <- dfTeam()
            
            df <- dfTeamLoad(data)
            
            return(df)
        })
        
        # Team Training Tab -
        output$TeamTable <- renderDT({
            
            # Recommendation table data
            df <- if(input$radioTeam == "velo") {
                dfVeloTeam()
            } else {
                dfLoadTeam()
            }
            
            datatable(
                df, 
                rownames = FALSE,
                filter = "none",
                options = list(
                    dom = 't',
                    columnDefs = list(list(className = 'dt-center', targets = "_all"))
                )
            )
        })
        
    })
}

shinyApp(ui, server)