#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(usmap)
library(dplyr)
library(openintro)

is.valid <- function(x) {
    require(shiny)
    is.null(need(x, message = FALSE))
}

# Read in the file
ann <- read.csv(file = "annual_generation_state.csv", header = TRUE)

# *****All pre-processing*****

# Remove commas and change class to numeric
ann$GENERATION..Megawatthours. <-
    gsub(',', '', ann$GENERATION..Megawatthours.)
ann$GENERATION..Megawatthours. <-
    sapply(ann$GENERATION..Megawatthours., as.numeric)

# Remove all rows that had something empty, uppercase all states and make them categories
ann <- subset(ann, ann$STATE != "  ")
ann$STATE <- toupper(ann$STATE)
ann$STATE <- as.factor(ann$STATE)

# Delete all that had a generation of less than 0
ann <- ann[ann$GENERATION..Megawatthours. >= 0, ]

# Delete specified rows and then categorize them
ann <- subset(ann, ann$ENERGY.SOURCE != "Other Biomass")
ann <- subset(ann, ann$ENERGY.SOURCE != "Other")
ann <- subset(ann, ann$ENERGY.SOURCE != "Other Gases")
ann <- subset(ann, ann$ENERGY.SOURCE != "Pumped Storage")
ann$ENERGY.SOURCE <- as.factor(ann$ENERGY.SOURCE)


# Change names
levels(ann$ENERGY.SOURCE) <-
    sub("Hydroelectric Conventional",
        "Hydro",
        levels(ann$ENERGY.SOURCE))
levels(ann$ENERGY.SOURCE) <-
    sub("Solar Thermal and Photovoltaic",
        "Solar",
        levels(ann$ENERGY.SOURCE))
levels(ann$ENERGY.SOURCE) <-
    sub("Wood and Wood Derived Fuels", "Wood", levels(ann$ENERGY.SOURCE))

f.cols = c(
    "#F8766D",
    "#D89000",
    "#A3A500",
    "#39B600",
    "#00BF7D",
    "#00BFC4",
    "#00B0F6",
    "#9590FF",
    "#E76BF3",
    "#FF62BC"
)

# Used for various graphs
b <- ann
ann <-
    subset(ann, ann$TYPE.OF.PRODUCER == "Total Electric Power Industry")
ann <- subset(ann, ann$ENERGY.SOURCE != "Total")
ann <- droplevels(ann)
#ann$STATE <- abbr2state(ann$STATE)
UsTotal <- subset(ann, ann$STATE == "US-TOTAL")
#UsTotal <- subset(UsTotal, UsTotal$ENERGY.SOURCE != "Total")
UsTotal <-
    subset(UsTotal,
           UsTotal$TYPE.OF.PRODUCER == "Total Electric Power Industry")
#View(UsTotal)

UsTotalPercent <-
    UsTotal %>% group_by(YEAR) %>% mutate(Percent = paste0(round(
        GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.) * 100,
        2
    )))
UsTotalPercent$Percent <- sapply(UsTotalPercent$Percent, as.numeric)
#View(UsTotal)
#View(UsTotalPercent)
years <- c(1990:2019)
blank <- c("No year")
years <- c(blank, years)
energies <- c(
    "No energy",
    "Coal",
    "Geothermal",
    "Hydro",
    "Natural Gas",
    "Nuclear",
    "Petroleum",
    "Solar",
    "Wind",
    "Wood"
)
# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Application title
    dashboardHeader(title = "United States Energy Sources"),
    dashboardSidebar(
        disable = FALSE,
        collapsed = FALSE,
        checkboxGroupInput(
            "energy",
            "Energy Options",
            c(
                "All",
                "Coal",
                "Geothermal",
                "Hydro",
                "Natural Gas",
                "Nuclear",
                "Petroleum",
                "Solar",
                "Wind",
                "Wood"
            ),
            inline = TRUE,
            selected = c("All")
        ),
        "Zone 1",
        selectInput(
            "z1State",
            "Select State",
            choices = unique(ann$STATE),
            selected = "IL"
        ),
        selectInput("z1Energy",
                    "Select Energy",
                    choices = energies),
        selectInput("z1Year", "Select Year", choices = years),
        "Zone 2",
        selectInput("z2State", "Select State", unique(ann$STATE), selected = "US-TOTAL"),
        
        selectInput("z2Energy", "Select Energy", choices = energies),
        selectInput("z2Year", "Select Year", choices = years)
    ),
    # Sidebar with a slider input for number of bins
    dashboardBody(
        tabsetPanel(
            type = "tabs",
            tabPanel(
                "Part 1 Line Graphs",
                plotOutput("linePercent"),
                plotOutput("lineCTotal"),
            ),
            tabPanel(
                "Part 1 Bar Graphs",
                plotOutput("stackBar"),
                plotOutput("stackBarPercent"),
            ),
            tabPanel(
                "Part 1 Tables",
                splitLayout(
                    dataTableOutput("part1T1", height = 200),
                    dataTableOutput("part1T2", height = 200)
                )
                
            ),
            tabPanel("Part 2",
                     fluidRow(
                         column(
                             6,
                             fluidRow(
                                 "Zone 1",
                                 plotOutput("z1_BarNum", height = 200),
                                 plotOutput("z1_BarPercent", height = 200),
                                 plotOutput("z1_LineNum", height = 200),
                                 plotOutput("z1_LinePercent", height = 200),
                                 dataTableOutput("z1_part2T1", height = 200),
                                 dataTableOutput("z1_part2T2", height = 200)
                                 
                                 
                             )
                         ),
                         column(
                             6,
                             fluidRow(
                                 "Zone 2",
                                 plotOutput("z2_BarNum", height = 200),
                                 plotOutput("z2_BarPercent", height = 200),
                                 plotOutput("z2_LineNum", height = 200),
                                 plotOutput("z2_LinePercent", height = 200),
                                 dataTableOutput("z2_part2T1", height = 200),
                                 dataTableOutput("z2_part2T2", height = 200)
                                 
                             )
                             
                         )
                     )),
            tabPanel("Part 3",
                     splitLayout(
                         fluidRow(
                             fluidRow(column(6,selectInput("m1_energy", "Select Energy", choices = energies, width = '70%')),
                             column(6,selectInput("m1_year", "Select Year", choices = years, width = '70%'))),
                             fluidRow(
                                 plotOutput("Map1")
                             )
                         ),
                         fluidRow(
                             fluidRow(column(6,selectInput("m2_energy", "Select Energy", choices = energies, width = '70%')),
                                      column(6,selectInput("m2_year", "Select Year", choices = years, width = '70%'))),
                             fluidRow(
                                 plotOutput("Map2")
                             )
                         )
                     )
                     ),
            tabPanel(
                "About Page",
                mainPanel(
                    HTML(
                        paste(
                            h3("Thank you for using this app!"),'<br/>',
                            h4("This app contains the numbers of the various different types of energy sources categorized by State and US-Total.
                     It is written by Syed Raza and is Project 1 for the CS 424 class at UIC"),'<br/>',
                            h4("The data can be found through this link: https://www.eia.gov/electricity/data/state/")
                        )
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    z1_ann <- ann
    map <- ann
    map_ann <- map %>% rename(state = STATE)
    energyReactive <-
        reactive({
            subset(UsTotalPercent,
                   UsTotalPercent$ENERGY.SOURCE %in% input$energy)
        })
    tableReactive <-
        reactive({
            subset(UsTotalPercent,
                   UsTotalPercent$ENERGY.SOURCE %in% input$energy)
        })
    z1_stateReactive <-
        reactive({
            subset(z1_ann, z1_ann$STATE == input$z1State)
        })
    z1_energyReactive <-
        reactive({
            subset(z1_ann, z1_ann$ENERGY.SOURCE == input$z1Energy)
        })
    z1_yearReactive <-
        reactive({
            subset(z1_ann, z1_ann$YEAR == input$z1Year)
        })
    z2_stateReactive <-
        reactive({
            subset(ann, ann$STATE == input$z2State)
        })
    z2_energyReactive <-
        reactive({
            subset(z1_ann, z1_ann$ENERGY.SOURCE == input$z2Energy)
        })
    z2_yearReactive <-
        reactive({
            subset(z1_ann, z1_ann$YEAR == input$z2Year)
        })
    z1_completeReactive <-
        reactive({
            z1_ann <- subset(z1_ann, z1_ann$STATE == input$z1State)
            z1_ann <-
                subset(z1_ann, z1_ann$ENERGY.SOURCE == input$z1Energy)
            subset(z1_ann, z1_ann$YEAR == input$z1Year)
        })
    z2_completeReactive <-
        reactive({
            z1_ann <- subset(z1_ann, z1_ann$STATE == input$z2State)
            z1_ann <-
                subset(z1_ann, z1_ann$ENERGY.SOURCE == input$z2Energy)
            subset(z1_ann, z1_ann$YEAR == input$z2Year)
        })
    map_Reactive <-
        reactive({
            mapN_ann <- subset(map_ann, map_ann$ENERGY.SOURCE == input$m1_energy)
            mapN_ann <-
                subset(mapN_ann, mapN_ann$YEAR == input$m1_year)
            
        })
    mapPercent_Reactive <-
        reactive({
            mapP_ann <- subset(map_ann, map_ann$ENERGY.SOURCE == input$m2_energy)
            mapP_ann <-
                subset(mapP_ann, mapP_ann$YEAR == input$m2_year)
            mapP_ann <-
                subset(mapP_ann, mapP_ann$state != "US-TOTAL")
            mapPP_ann <-
                mapP_ann %>% group_by(YEAR) %>% mutate(Percent = paste0(round(
                    GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.) * 100,
                    2
                )))
            mapPP_ann$Percent <-
                sapply(mapPP_ann$Percent, as.numeric)
            mapPP_ann <-
                subset(mapPP_ann, mapPP_ann$YEAR == input$m2_year)
            
        })
    
    output$stackBar <- renderPlot({
        b <- subset(b, b$ENERGY.SOURCE != "Total")
        if ("All" %in% input$energy) {
            ggplot(b,
                   aes(
                       fill = ENERGY.SOURCE,
                       y = GENERATION..Megawatthours.,
                       x = YEAR
                   )) + geom_bar(position = "stack", stat = "identity") + labs(y= "Energy Generated (MWh)",x = "Year") + labs(fill = "Energy Source")
        }
        else{
            us <- energyReactive()
            ggplot(us,
                   aes(
                       fill = ENERGY.SOURCE,
                       y = GENERATION..Megawatthours.,
                       x = YEAR
                   )) + geom_bar(position = "stack", stat = "identity")+labs(y= "Energy Generated (MWh)",x = "Year") + labs(fill = "Energy Source")
        }
    })
    
    output$stackBarPercent <- renderPlot({
        if ("All" %in% input$energy) {
            b <- subset(b, b$ENERGY.SOURCE != "Total")
            ggplot(b,
                   aes(
                       fill = ENERGY.SOURCE,
                       y = GENERATION..Megawatthours.,
                       x = YEAR
                   )) + geom_bar(position = "fill", stat = "identity")+
                xlab("Year") + ylab("Energy Generation (Percentage)")+ labs(fill = "Energy Source")
        }
        else{
            us <- energyReactive()
            ggplot(us,
                   aes(
                       fill = ENERGY.SOURCE,
                       y = GENERATION..Megawatthours.,
                       x = YEAR
                   )) + geom_bar(position = "fill", stat = "identity")+ labs(y= "Energy Generated (MWh)",x = "Year", fill = "Energy Source")
        }
    })
    
    output$lineCTotal <- renderPlot({
        if ("All" %in% input$energy) {
            ggplot(
                UsTotalPercent,
                aes(
                    fill = ENERGY.SOURCE,
                    y = GENERATION..Megawatthours.,
                    x = YEAR,
                    color = ENERGY.SOURCE
                )
            ) + geom_line() + geom_point(show.legend = FALSE) +
                xlab(input$energy) + coord_cartesian(xlim  = c(1990, 2020),
                                                     ylim = c(0, 2100000000)) +
                scale_color_manual(values = f.cols, drop = FALSE)+ labs(y= "Energy Generated (MWh)",x = "Year")+ labs(fill = "Energy Source")
        }
        else{
            us <- energyReactive()
            ggplot(
                us,
                aes(
                    fill = ENERGY.SOURCE,
                    y = GENERATION..Megawatthours.,
                    x = YEAR,
                    color = ENERGY.SOURCE
                )
            ) + geom_line() + geom_point(show.legend = FALSE) +
                coord_cartesian(xlim  = c(1990, 2020),
                                ylim = c(0, 2100000000)) + scale_color_manual(values = f.cols, drop = FALSE)+ labs(y= "Energy Generated (MWh)",x = "Year")+ labs(fill = "Energy Source")
        }
    })
    
    output$linePercent <- renderPlot({
        if ("All" %in% input$energy) {
            ggplot(
                UsTotalPercent,
                aes(
                    fill = ENERGY.SOURCE,
                    y = Percent,
                    x = YEAR,
                    color = ENERGY.SOURCE
                )
            ) + geom_line() + geom_point(show.legend = FALSE)  + coord_cartesian(xlim  = c(1990, 2020),
                                                     ylim = c(0, 100)) +
                scale_color_manual(values = f.cols, drop = FALSE)+ labs(x = "Year", y = "Energy Generation (Percentage)",fill = "Energy Source")
        } else{
            us <- energyReactive()
            #View(us)
            ggplot(us,
                   aes(
                       fill = ENERGY.SOURCE,
                       y = us$Percent,
                       x = YEAR,
                       color = ENERGY.SOURCE
                   )) +
                geom_line() + geom_point(show.legend = FALSE) + coord_cartesian(xlim  = c(1990, 2020),
                                                                                                     ylim = c(0, 100)) +
                scale_color_manual(values = f.cols, drop = FALSE)+ labs(y= "Energy Generated (MWh)",x = "Year") + labs(fill = "Energy Source")
        }
        
    })
    
    output$z1_BarNum <- renderPlot({
        state <- z1_stateReactive()
        if (input$z1Energy != "No energy" &
            input$z1Year != "No year" & is.valid(input$z1State)) {
            print("We in the null checker for z1energy")
            state <- z1_completeReactive()
        }
        #state$STATE <- state2abbr(state$STATE)
        #View(state)
        ggplot(
            state,
            aes(
                fill = ENERGY.SOURCE,
                y = GENERATION..Megawatthours.,
                x = state$YEAR
            )
        ) + geom_bar(position = "stack", stat = "identity")+ labs(y= "Energy Generated (MWh)",x = "Year")+labs(fill = "Energy Source")
    })
    
    output$z1_BarPercent <- renderPlot({
        state <- z1_stateReactive()
        #state$STATE <- state2abbr(state$STATE)
        #View(state)
        ggplot(
            state,
            aes(
                fill = ENERGY.SOURCE,
                y = GENERATION..Megawatthours.,
                x = state$YEAR
            )
        ) + geom_bar(position = "fill", stat = "identity")+
            xlab("Year") + ylab("Energy Generation (Percentage)") +labs(fill = "Energy Source")
    })
    
    output$z1_LineNum <- renderPlot({
        state <- z1_stateReactive()
        if (input$z1Energy != "No energy" &
            input$z1Year != "No year" & is.valid(input$z1State)) {
            print("We in the null checker for z1energy")
            state <- z1_completeReactive()
        }
        #state$STATE <- state2abbr(state$STATE)
        #View(state)
        #state <- subset(state, state$ENERGY.SOURCE != "Total")
        ggplot(
            state,
            aes(
                fill = ENERGY.SOURCE,
                y = GENERATION..Megawatthours.,
                x = YEAR,
                color = ENERGY.SOURCE
            )
        ) + geom_line() + geom_point(show.legend = FALSE) +
            xlab(input$energy) +
            scale_color_manual(values = f.cols, drop = FALSE)+ labs(y= "Energy Generated (MWh)",x = "Year")+labs(fill = "Energy Source")
    })
    output$z1_LinePercent <- renderPlot({
        state <- z1_stateReactive()
        state <-
            state %>% group_by(YEAR) %>% mutate(Percent = paste0(round(
                GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.) * 100,
                2
            )))
        state$Percent <- sapply(state$Percent, as.numeric)
        ggplot(state,
               aes(
                   fill = ENERGY.SOURCE,
                   y = Percent,
                   x = YEAR,
                   color = ENERGY.SOURCE
               )) + geom_line() + geom_point(show.legend = FALSE) +
            xlab("Year") + ylab("Energy Generation (Percentage)")+coord_cartesian(xlim  = c(1990, 2020),
                                                                                  ylim = c(0, 100)) +
            scale_color_manual(values = f.cols, drop = FALSE)+labs(fill = "Energy Source")
    })
    
    output$z2_BarNum <- renderPlot({
        state <- z2_stateReactive()
        if (input$z2Energy != "No energy" &
            input$z2Year != "No year" & is.valid(input$z2State)) {
            print("We in the null checker for z1energy")
            state <- z2_completeReactive()
        }
        #state$STATE <- state2abbr(state$STATE)
        #View(state)
        ggplot(
            state,
            aes(
                fill = ENERGY.SOURCE,
                y = GENERATION..Megawatthours.,
                x = state$YEAR
            )
        ) + geom_bar(position = "stack", stat = "identity")+ labs(y= "Energy Generated (MWh)",x = "Year")+labs(fill = "Energy Source")
    })
    output$z2_BarPercent <- renderPlot({
        state <- z2_stateReactive()
        #state$STATE <- state2abbr(state$STATE)
        #View(state)
        ggplot(
            state,
            aes(
                fill = ENERGY.SOURCE,
                y = GENERATION..Megawatthours.,
                x = state$YEAR
            )
        ) + geom_bar(position = "fill", stat = "identity")+
            xlab("Year") + ylab("Energy Generation (Percentage)")+labs(fill = "Energy Source")
    })
    
    output$z2_LineNum <- renderPlot({
        state <- z2_stateReactive()
        if (input$z2Energy != "No energy" &
            input$z2Year != "No year" & is.valid(input$z2State)) {
            state <- z2_completeReactive()
        }
        ggplot(
            state,
            aes(
                fill = ENERGY.SOURCE,
                y = GENERATION..Megawatthours.,
                x = YEAR,
                color = ENERGY.SOURCE
            )
        ) + geom_line() + geom_point(show.legend = FALSE) +
            xlab(input$energy) +
            scale_color_manual(values = f.cols, drop = FALSE)+ labs(y= "Energy Generated (MWh)",x = "Year")+labs(fill = "Energy Source")
    })
    output$z2_LinePercent <- renderPlot({
        state <- z2_stateReactive()
        state <-
            state %>% group_by(YEAR) %>% mutate(Percent = paste0(round(
                GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.) * 100,
                2
            )))
        state$Percent <- sapply(state$Percent, as.numeric)
        ggplot(state,
               aes(
                   fill = ENERGY.SOURCE,
                   y = Percent,
                   x = YEAR,
                   color = ENERGY.SOURCE
               )) + geom_line() + geom_point(show.legend = FALSE) +
            xlab("Year") + ylab("Energy Generation (Percentage)")+coord_cartesian(xlim  = c(1990, 2020),
                                                 ylim = c(0, 100)) +
            scale_color_manual(values = f.cols, drop = FALSE)+labs(fill = "Energy Source")
    })
    
    
    output$part1T1 <- renderDataTable({
        if ("All" %in% input$energy) {
            i <- UsTotal
            i$STATE <- NULL
            i$TYPE.OF.PRODUCER <- NULL
            i$Percent <- NULL
            i
        } else{
            i <- tableReactive()
            i$STATE <- NULL
            i$TYPE.OF.PRODUCER <- NULL
            i$Percent <- NULL
            i
        }
        
    })
    
    output$part1T2 <- renderDataTable({
        if ("All" %in% input$energy) {
            j <- UsTotalPercent
            j$STATE <- NULL
            j$TYPE.OF.PRODUCER <- NULL
            j$GENERATION..Megawatthours. <- NULL
            j
        }
        else{
            i <- tableReactive()
            i$GENERATION..Megawatthours. <- NULL
            i$STATE <- NULL
            i$TYPE.OF.PRODUCER <- NULL
            i
        }
    })
    
    output$z1_part2T1 <- renderDataTable({
        if (input$z1Energy != "No energy" &
            input$z1Year != "No year" & is.valid(input$z1State)) {
            state <- z1_completeReactive()
            state$TYPE.OF.PRODUCER <- NULL
            state
        }
        else{
            u <- z1_stateReactive()
            u$TYPE.OF.PRODUCER <- NULL
            u
        }
        
    })
    
    output$z1_part2T2 <- renderDataTable({
        state <- z1_stateReactive()
        state <-
            state %>% group_by(YEAR) %>% mutate(Percent = paste0(round(
                GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.) * 100,
                2
            )))
        state$Percent <- sapply(state$Percent, as.numeric)
        state$TYPE.OF.PRODUCER <- NULL
        state$GENERATION..Megawatthours. <- NULL
        state
    })
    
    output$z2_part2T1 <- renderDataTable({
        if (input$z2Energy != "No energy" &
            input$z2Year != "No year" & is.valid(input$z2State)) {
            state <- z2_completeReactive()
            state$TYPE.OF.PRODUCER <- NULL
            state
        }
        else{
            u <- z2_stateReactive()
            u$TYPE.OF.PRODUCER <- NULL
            u
        }
    })
    
    output$z2_part2T2 <- renderDataTable({
        state <- z2_stateReactive()
        state <-
            state %>% group_by(YEAR) %>% mutate(Percent = paste0(round(
                GENERATION..Megawatthours. / sum(GENERATION..Megawatthours.) * 100,
                2
            )))
        state$Percent <- sapply(state$Percent, as.numeric)
        state$TYPE.OF.PRODUCER <- NULL
        state$GENERATION..Megawatthours. <- NULL
        state
    })
    
    output$Map1 <- renderPlot({
        m <- map_Reactive()
        #View(m)
        if (is.valid(input$m1_year) & is.valid(input$m1_energy)) {
            plot_usmap(data = m,
                       values = "GENERATION..Megawatthours.",
                       color = "red") +
                scale_fill_continuous(name = "Energy Generation", label = scales::comma) +
                theme(legend.position = "right")
        }
    })
    
    output$Map2 <- renderPlot({
        if (is.valid(input$m2_year) & is.valid(input$m2_energy)) {
            mp <- mapPercent_Reactive()
            plot_usmap(data = mp,
                       values = "Percent",
                       color = "red") +
                scale_fill_continuous(name = "Percentage", label = scales::comma) +
                theme(legend.position = "right")
        }
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
