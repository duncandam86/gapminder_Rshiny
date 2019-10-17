library(gapminder)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(colourpicker)
library(plotly)

shinyUI(
  tagList(
    #themeSelector(),
    navbarPage(
      theme = shinytheme('flatly'),
      'GAPMINDER',
      #============= Overview tab UI=====================
      tabPanel(
        'Overview',
        sidebarPanel(
          width = 3,
          #text input for xaxis 
          selectInput(
            inputId = 't1_xaxis', 
            label ='Select x-axis', 
            choices = c('Life Expectancy' = 'lifeExp',
                        'GDP per Capita' = 'gdpPercap',
                        'Population' = 'pop'),
            selected = 'gdpPercap'
          ),
          
          
          #text input for yaxis
          selectInput(
            inputId = 't1_yaxis', 
            label ='Select y-axis', 
            choices = c('Life Expectancy' = 'lifeExp',
                        'GDP per Capita' = 'gdpPercap',
                        'Population' = 'pop'),
            selected = 'lifeExp'
          ),
          
          #linear fit
          checkboxInput(
            inputId = 'fit', 
            label = 'Add line for best fit',
            value = FALSE
          ), 
          
          #year selection
          checkboxGroupInput(
            inputId = 't1_year',
            label = 'Choose year(s)',
            choices = c('All', unique(gapminder$year)),
            selected = c(1957,1972, 1982)
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              'Data Visualization',
              icon = icon('chart-bar'),
              fluidRow(
                #column(width =1),
                column(
                  width = 12,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 't1_plot',
                      height = 300
                    ) 
                  )
                )
              ),
              tags$hr(style = 'width : 80%'),
              fluidRow(
                #column(width =1),
                column( 
                  width= 12,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 't4_plot',
                      height = 300
                    )  
                  )
                )
              ),
              tags$hr( style = 'width : 80%'),
              fluidRow(
                #column(width =1),
                column(
                  width = 6,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 't2_plot',
                      height = 250
                    )  
                  )
                ),
                column(
                  width = 6,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 't3_plot',
                      height = 250
                    )  
                  )
                )
              )
            ),
            tabPanel(
              'Data Table',
              icon = icon('table'),
              br(),
              dataTableOutput(
                outputId = 'table_output1'
              )
            )
          )
        )
      ),
      #==================Continent tab UI========================
      tabPanel(
        'Continent',
        sidebarPanel(
          width = 3,
          #text input for xaxis 
          selectInput(
            inputId = 'cont_xaxis', 
            label ='Select x-axis', 
            choices = c('Life Expectancy' = 'lifeExp',
                        'GDP per Capita' = 'gdpPercap',
                        'Population' = 'pop'),
            selected = 'gdpPercap'
          ),
          
          
          #text input for yaxis
          selectInput(
            inputId = 'cont_yaxis', 
            label ='Select y-axis', 
            choices = c('Life Expectancy' = 'lifeExp',
                        'GDP per Capita' = 'gdpPercap',
                        'Population' = 'pop'),
            selected = 'lifeExp'
          ),
          
          #checkbox for continents
          radioButtons(
            inputId = 'cont',
            label = 'Choose a continent',
            choices = c(levels(gapminder$continent)),
            selected = 'Asia'
          ),
          
          #slider for years
          sliderInput(
            inputId = 'cont_year',
            label = 'Choose a year',
            min = min(gapminder$year),
            max = max(gapminder$year),
            value = 1957,
            step = 5
          ),
          
          colourInput(
            inputId = 'cont_col',
            label= 'Choose a color for scatter plot',
            value = '#C538CF'
          ),
          
          #linear fit box
          checkboxInput(
            inputId = 'cont_fit',
            label = 'Add line for linear fit',
            value = FALSE
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              'Data Visualization',
              icon = icon('chart-pie'),
              fluidRow(
                column(width = 1),
                column(
                  width = 10,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 'cont_plot',
                      height =  300
                    )
                  )
                ),
                column(width = 1)
              ),
              hr(),
              fluidRow(
                column(
                  width = 4,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 'pie_pop',
                      height = 350
                    ) 
                  )
                ),
                column(
                  width = 4,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 'pie_life',
                      height = 320
                    )
                  )
                ),
                column(
                  width = 4,
                  box(
                    width = NULL,
                    plotlyOutput(
                      outputId = 'pie_gdp',
                      height = 350
                    )
                  )
                )
              )
            ),
            tabPanel(
              'Data Table',
              icon = icon('table'),
              br(),
              dataTableOutput(
                outputId = 'cont_table'
              )
            )
          )
        )
      ),
      #===========================Country Tab UI =========================
      tabPanel(
        'Country',
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = 'count_axis',
            label = 'Choose a category',
            choices = c('Life Expectancy' = 'lifeExp',
                        'GDP per Capita' = 'gdpPercap',
                        'Population' = 'pop',
                        'Growth Domestic Product' = 'gdp'),
            selected = 'gdpPercap'
          ),
          
          selectInput(
            inputId = 'count',
            label = 'Select Country',
            choices = levels(gapminder$country),
            selected = c('Vietnam','Poland','Australia', 'Angola','Mexico','Nepal'),
            multiple = TRUE
          ),  
          
          checkboxGroupInput(
            inputId = 'count_year',
            label = 'Choose year(s)',
            choices = c(unique(gapminder$year)),
            selected = c(1952,1962,1972,1982,1992)
          )
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              'Data Visualization',
              icon = icon('chart-area'),
              column(
                width = 12,
                box(
                  width = NULL,
                  plotlyOutput(
                    outputId = 'count_plot_1',
                    height = 300
                  )
                )
              ),
              hr(style = 'width : 80%'),
              column(
                width = 12,
                box(
                  width = NULL,
                  plotlyOutput(
                    outputId = 'count_plot_2'
                  )
                )
              )
            ),
            tabPanel(
              'Data Table',
              icon = icon('table'),
              br(),
              dataTableOutput(
                outputId = 'count_table'
              )
            )
          )
        )
      )
    )
  )
)