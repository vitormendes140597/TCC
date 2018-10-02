sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Visão Geral", tabName = "general", icon = icon("home")),
    menuItem("Data Clean", tabName = "dataclean", icon = icon("filter")),
    menuItem("Análise Univariada", tabName = "univariate", icon = icon("bar-chart")),
    menuItem("Análise Multivariada", tabName = "multivariate", icon = icon("bar-chart")),
    actionButton("restoreds",label="Restaurar Dataframe",class="btn-danger",style="color:white;")
  )
)

body <-  dashboardBody(
  tabItems(
    # General Tab
    tabItem(tabName = "general",
            div(class = "page-header", h3("Bem vindo ao MackAirDashBoard",class="text-center"))
    ),

    # Data Clean Tab
    tabItem(tabName = "dataclean",
            fluidRow(
              box(
                title = "Mecanismo de Limpeza de Dados", solidHeader = TRUE,
                collapsible = TRUE, width = 12, status = "primary",
                fluidRow(
                  column(width = 6,
                    h2("MackAir Data Clean",class="text-center"),
                    selectInput(
                                  "cleanOperation",
                                  label = "Operação",
                                  choices = list("Filtrar" = 1, "Capping" = 2),
                                  selected = 1
                                ),
                    conditionalPanel(
                      condition = "input.cleanOperation == 1",
                      textInput(
                        "filterCondition",
                        h4("Condição de Filtragem"),
                        placeholder = "Ex: age >= 18 & status == 1",
                        value = ""
                      ),
                      actionButton("clean.filterds", "Filtrar")
                    ),
                    ###################
                    # Capping
                    ##################
                    conditionalPanel(
                      condition = "input.cleanOperation == 2",
                      
                      uiOutput("clean.choose_NumericVars"),
                      
                      selectInput(
                        "cappingMethod",
                        label = "Valor de Substuição",
                        choices = list("Média"= 1, "Mediana"=2, "Percentil" = 3),
                        selected = 1
                      ),
                      
                      conditionalPanel(
                        condition = "input.cappingMethod == 3",
                        numericInput(
                          "cleanPercentile",
                          label = "Escolha um Percentil",
                          min = 0,
                          max = 1,
                          step = 0.01,
                          value = 0.99
                        )
                      ),
                      
                      textInput(
                        "replaceCondition",
                        h4("Condição de Substituição"),
                        placeholder = "Ex: age >= 18 & status == 1",
                        value = ""
                      ),
                      actionButton("clean.applyCapping", "Aplicar")
                    )
                    
                  ),
                  column(width = 6,
                    h2("Dados Categóricos", class="text-center")
                  )
                )
              )
            ),
            fluidRow(
              column(width = 12,
                     DT::dataTableOutput('table')  %>% withSpinner(color="#0dc5c1")
                     )
            )

    ),
    # Univariate Analysis Tab
    tabItem(tabName = "univariate"

    ),
    # Multivariate Analysis Tab
    tabItem(tabName = "multivariate"

    )
  )
)

header <- dashboardHeader()

ui <- dashboardPage(header,sidebar,body)

