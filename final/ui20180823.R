sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dados", tabName = "data", icon = icon("filter")),
    menuItem("Análises", tabName = "analises", icon = icon("bar-chart")),
    menuItem("Tabela Dinâmica", tabName = "pivotTable", icon = icon("bar-chart"))
    #actionButton("restoreds",label="Restaurar Dataframe",class="btn-danger",style="color:white;")
  )
)

body <-  dashboardBody(
  tabItems(
    # Data Clean Tab
    tabItem(tabName = "data",
            box(
              title = "Dados Coletados em São Paulo", solidHeader = TRUE, width = 12, status = "danger",
              collapsible = TRUE,
              fluidRow(
                column(
                  width = 12,
                  DT::dataTableOutput('dadosSP')
                )
              )
            ),
            box(
              title = "Dados Coletados em Mogi das Cruzes", solidHeader = TRUE, width = 12, status = "danger",
              collapsible = TRUE,
              fluidRow(
                column(
                  width = 12,
                  DT::dataTableOutput('dadosMogi')
                )
              )
            )
    ),
    # Analysis Tab
    tabItem(tabName = "analises",
      box(title = "Análise Geral" , solidHeader = TRUE, collapsible = TRUE,width = "12", status = "primary",
          fluidRow(
            tabBox(
              title = "Histograma de Concentração de CO",
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset1", height = "100%",
              tabPanel("São Paulo",
                       selectInput(
                         "histChartLibSP",
                         label = "Escolha a biblioteca de Visualização",
                         choices = c("R Basics" = "r",
                                     "Plotly" = "plotly",
                                     "ggplot2" = "ggplot",
                                     "highcharter" = "highcharter"
                         ),
                         selected = "r"
                       ),
                       uiOutput("escolherHistVarsSP"),
                       conditionalPanel("input.histChartLibSP == 'r' ",plotOutput("rBaseHistSP")),
                       conditionalPanel("input.histChartLibSP == 'plotly' ",plotlyOutput("rPlotlyHistSP")),
                       conditionalPanel("input.histChartLibSP == 'highcharter' ",highchartOutput ("rHChartHistSP")),
                       conditionalPanel("input.histChartLibSP == 'ggplot' ", plotOutput("rGgplotHistSP"))
              ),
              tabPanel("Mogi das Cruzes",
                       selectInput(
                         "histChartLibMogi",
                         label = "Escolha a biblioteca de Visualização",
                         choices = c("R Basics" = "r",
                                     "Plotly" = "plotly",
                                     "ggplot2" = "ggplot",
                                     "highcharter" = "highcharter"
                         ),
                         selected = "r"
                       ),
                       uiOutput("escolherHistVarsMogi"),
                       conditionalPanel("input.histChartLibMogi == 'r' ",plotOutput("rBaseHistMogi")),
                       conditionalPanel("input.histChartLibMogi == 'plotly' ",plotlyOutput("rPlotlyHistMogi")),
                       conditionalPanel("input.histChartLibMogi == 'highcharter' ",highchartOutput ("rHChartHistMogi")),
                       conditionalPanel("input.histChartLibMogi == 'ggplot' ", plotOutput("rGgplotHistMogi"))
              ),
              tabPanel("Ambos", 
                       plotlyOutput("histSPMogi")
              )
            ),
            tabBox(
              title = "Concentração de CO Mensal",
              id = "tabset2", height = "100%",
              tabPanel("São Paulo",
                       selectInput(
                         "boxPlotChartLibSP",
                         label = "Escolha a biblioteca de Visualização",
                         choices = c("Plotly" = "plotly",
                                     "ggplot2" = "ggplot"
                         ),
                         selected = "plotly"
                       ),
                       selectInput(
                         "spBoxPlotVars",
                         label =  "coluna",
                         choices = c("mediaHorariaPpm" = "mediaHorariaPpm", "maxHorariaPpm" = "maxHorariaPpm",
                                     "minHorariaPpm" = "minHorariaPpm", "mediaMovel" = "mediaMovel"),
                         selected = "mediaMovel"
                       )
                       ,
                       conditionalPanel("input.boxPlotChartLibSP == 'r' ",plotOutput("rBaseBoxPlotSP")),
                       conditionalPanel("input.boxPlotChartLibSP == 'plotly' ",plotlyOutput("rPlotlyBoxPlotSP")),
                       conditionalPanel("input.boxPlotChartLibSP == 'highcharter' ",highchartOutput ("rHChartBoxPlotSP")),
                       conditionalPanel("input.boxPlotChartLibSP == 'ggplot' ", plotOutput("rGgplotBoxPlotSP"))
                       
              ),
              tabPanel("Mogi das Cruzes",
                       selectInput(
                         "boxPlotChartLibMogi",
                         label = "Escolha a biblioteca de Visualização",
                         choices = c("Plotly" = "plotly",
                                     "ggplot2" = "ggplot"
                         ),
                         selected = "plotly"
                       ),
                       selectInput(
                         "mogiBoxPlotVars",
                         label =  "coluna",
                         choices = c("mediaHorariaPpm" = "mediaHorariaPpm", "maxHorariaPpm" = "maxHorariaPpm",
                                     "minHorariaPpm" = "minHorariaPpm", "mediaMovel" = "mediaMovel"),
                         selected = "mediaMovel"
                       )
                       ,
                       conditionalPanel("input.boxPlotChartLibMogi == 'r' ",plotOutput("rBaseBoxPlotMogi")),
                       conditionalPanel("input.boxPlotChartLibMogi == 'plotly' ",plotlyOutput("rPlotlyBoxPlotMogi")),
                       conditionalPanel("input.boxPlotChartLibMogi == 'highcharter' ",highchartOutput ("rHChartBoxPlotMogi")),
                       conditionalPanel("input.boxPlotChartLibMogi == 'ggplot' ", plotOutput("rGgplotBoxPlotMogi"))
              )
            )
          ),
          br(),
          fluidRow(
            tabBox(
              title = "Média Movel dia da Semana",
              id = "tabset2", height = "100%", width = "12",
              tabPanel(
                "São Paulo",
                selectInput(
                  "boxPlotDiarySP",
                  label = "Escolha a biblioteca de Visualização",
                  choices = c("Plotly" = "plotly",
                              "ggplot2" = "ggplot",
                              "highcharter" = "highcharter"
                  ),
                  selected = "plotly"
                ),
                conditionalPanel("input.boxPlotDiarySP == 'plotly'",plotlyOutput("rPlotlyBoxPlotDiarySP")),
                conditionalPanel("input.boxPlotDiarySP == 'ggplot'",plotOutput("rGgplotBoxPlotDiarySP")),
                conditionalPanel("input.boxPlotDiarySP == 'highcharter'", highchartOutput("rHCBoxPlotDiarySP"))
              ),
              tabPanel(
                "Mogi das Cruzes",
                selectInput(
                  "boxPlotDiaryMogi",
                  label = "Escolha a biblioteca de Visualização",
                  choices = c("Plotly" = "plotly",
                              "ggplot2" = "ggplot",
                              "highcharter" = "highcharter"
                  ),
                  selected = "plotly"
                ),
                conditionalPanel("input.boxPlotDiaryMogi == 'plotly'",plotlyOutput("rPlotlyBoxPlotDiaryMogi")),
                conditionalPanel("input.boxPlotDiaryMogi == 'ggplot'",plotOutput("rGgplotBoxPlotDiaryMogi")),
                conditionalPanel("input.boxPlotDiaryMogi == 'highcharter'", highchartOutput("rHCBoxPlotDiaryMogi"))
              )
              
            )
          )
      ),
      
      box(title = "Análise por Mês", status = "primary", solidHeader = TRUE,collapsible = TRUE, width = "12",
        fluidRow(
          tabBox(id = "tabset3",width = "12",
            tabPanel(
              "São Paulo",
              column(
                width = 6,
                highchartOutput("monoxido_por_dia_mes_sp")
              ),
              column(
                width = 6,
                highchartOutput("monoxido_mediacum_por_dia_mes_sp")
              )
            ),
            tabPanel(
              "Mogi das Cruzes",
              column(
                width = 6,
                highchartOutput("monoxido_por_dia_mes_mogi")
              ),
              column(
                width = 6,
                highchartOutput("monoxido_mediacum_por_dia_mes_mogi")
              )
            )
          )
        )
      )
            
    ),
    # Multivariate Analysis Tab
    tabItem(tabName = "pivotTable",
        fluidRow(
          column(width = 12,
                 rpivotTableOutput("pivotTableCO")
                 )
        )
    )
  )
)

header <- dashboardHeader()

ui <- dashboardPage(header,sidebar,body,skin = "red")

