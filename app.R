#Ameya Bhandari
#NFL Bust Project

# Load packages ---- 
library(shiny)
library(maps)
library(mapproj)
library(pacman)
library(rvest)
library(ggplot2)
library(plotly)
p_load(tidyverse, readxl, janitor, reshape2)


df.player <- read_excel("C:/Users/ameyab/Downloads/Bust project.xlsx", sheet = "Players", skip = 0, col_names = TRUE) 
df.player$DraftOrd <- seq.int(nrow(df.player))
df.player <- melt(df.player, id.vars = c("DraftOrd"))
colnames(df.player) <- c('DraftOrd','Year','Team')
df.player <- df.player[,c(2,1,3)]
df.player <- df.player %>% drop_na()


df.team <- read_excel("C:/Users/ameyab/Downloads/Bust project.xlsx", sheet = "Team", skip = 0, col_names = TRUE) 
df.team$DraftOrd <- seq.int(nrow(df.team))
df.team <- melt(df.team, id.vars = c("DraftOrd"))
colnames(df.team) <- c('DraftOrd','Year','Team')
df.team <- df.team[,c(2,1,3)]
df.team <- df.team %>% drop_na()


df.school <- read_excel("C:/Users/ameyab/Downloads/Bust project.xlsx", sheet = "School", skip = 0, col_names = TRUE) 
df.school$DraftOrd <- seq.int(nrow(df.school))
df.school <- melt(df.school, id.vars = c("DraftOrd"))
colnames(df.school) <- c('DraftOrd','Year','Team')
df.school <- df.school[,c(2,1,3)]
df.school <- df.school %>% drop_na()


df.position <- read_excel("C:/Users/ameyab/Downloads/Bust project.xlsx", sheet = "Position", skip = 0, col_names = TRUE) 
df.position$DraftOrd <- seq.int(nrow(df.position))
df.position <- melt(df.position, id.vars = c("DraftOrd"))
colnames(df.position) <- c('DraftOrd','Year','Team')
df.position <- df.position[,c(2,1,3)]
df.position <- df.position %>% drop_na()


df.games <- read_excel("C:/Users/ameyab/Downloads/Bust project.xlsx", sheet = "Games", skip = 0, col_names = TRUE) 
df.games$DraftOrd <- seq.int(nrow(df.games))
df.games <- melt(df.games, id.vars = c("DraftOrd"))
colnames(df.games) <- c('DraftOrd','Year','Team')
df.games <- df.games[,c(2,1,3)]
df.games <- df.games %>% drop_na()

df.college_years <- read_excel("C:/Users/ameyab/Downloads/Bust project.xlsx", sheet = "College_years", skip = 0, col_names = TRUE) 
df.college_years$DraftOrd <- seq.int(nrow(df.college_years))
df.college_years <- melt(df.college_years, id.vars = c("DraftOrd"))
colnames(df.college_years) <- c('DraftOrd','Year','Team')
df.college_years <- df.college_years[,c(2,1,3)]
df.college_years <- df.college_years %>% drop_na()

df.NFL_years <- read_excel("C:/Users/ameyab/Downloads/Bust project.xlsx", sheet = "NFL_years", skip = 0, col_names = TRUE) 
df.NFL_years$DraftOrd <- seq.int(nrow(df.NFL_years))
df.NFL_years <- melt(df.NFL_years, id.vars = c("DraftOrd"))
colnames(df.games) <- c('DraftOrd','Year','Team')
df.NFL_years <- df.NFL_years[,c(2,1,3)]
df.NFL_years <- df.NFL_years %>% drop_na()


# CREATE PANEL

df <- cbind(df.player,df.position[,3],df.school[,3],df.team[,3], df.games[,3], df.college_years[,3], df.NFL_years[,3]) 
colnames(df)<- c("Year","DraftOrd","Player","Position","School","Team", "Games_Played", "College_years", "NFL_years")
df$Year <- round(as.numeric(as.character(df$Year)),digits=0)

df.gg <- df %>% group_by(Year,Games_Played)

df.gg <- df %>% 
    group_by(Year, Games_Played, School) %>%
    summarise(number = n())

df.nn <- df %>% group_by(NFL_years, College_years)

df.nn <- df %>% 
    group_by(NFL_years, College_years, Position) %>%
    summarise(number = n())
    
df.pp <- df %>% group_by(Games_Played, Position)

df.pp <- df %>% 
    group_by(Games_Played, Position, Team) %>%
    summarise(number = n())

df.pp <- df %>% group_by(Games_Played, Position, Team, School)

# User interface ----
ui <- fluidPage(
    titlePanel("Ameya Bhandari- Bust Project"),
    
    
    sidebarLayout(
        sidebarPanel(
            helpText("Find the number of years played in the NFL based on number of years played in college."),
            
            selectInput("college_yearss", h4("Years in College"), 
                        choices = list("3" = 3, "4" = 4,
                                       "5" = 5), selected = 3,4,5),
            
            sliderInput("nfl_yearss", h4("Years in NFL"),
                        min = 0, max = 25, value = c(0, 25))
        ),
        
        mainPanel(
            h3("Years in College vs NFL"),
            fluidRow(plotOutput("map"))
        )
    ),sidebarLayout(
        sidebarPanel(
            helpText("Find the number of games played in the NFL based on position."),
            
        ),
        
        mainPanel(
            h3("Total games played per position"),
            fluidRow(plotlyOutput("gamer"))
    
        )
        
    ),
    sidebarLayout(
        sidebarPanel(
            helpText("Number of NFL games played per college"),
        ),
        
        mainPanel(
            h3("NFL games played per college"),
            fluidRow(plotlyOutput("colleger"))
        )
    )
)

# Server logic ----
server <- function(input, output) {
    output$map <- renderPlot({

        ggplot(subset(df.nn, College_years%in%input$college_yearss), aes(x=NFL_years, y=College_years)) + 
        geom_count() + 
        xlim(input$nfl_yearss[1], input$nfl_yearss[2]) +
        
        labs( 
            y="Years in College", 
            x="Years in NFL")
    })
    output$gamer <- renderPlotly({
        ggplot(df.pp, aes(x=Position, y=Games_Played, fill = Team)) + 
            geom_bar(stat = "identity", position = "stack") + 
            labs( 
                 y="Games played in NFL", 
                 x="Position") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
    
    output$colleger <- renderPlotly({
        
        ggplot(df.pp, aes(y=Games_Played, x=School)) + 
            geom_bar(stat = "identity", position = "stack")+
            labs( 
                y="Games played in NFL", 
                x="College") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
    })
    
}

# Run app ----
shinyApp(ui, server)
