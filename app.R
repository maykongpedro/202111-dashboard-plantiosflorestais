
library(shiny)
library(shinydashboard)


# Carregar bases ----------------------------------------------------------
dados_uf <- plantiosflorestais::mapeamentos_estados
dados_muni <- plantiosflorestais::mapeamentos_municipios


# Ui ----------------------------------------------------------------------
ui <- dashboardPage(
  
    skin = "green",
    header = dashboardHeader(
        title = "Plantios florestais"
        #titleWidth = 275
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Contexto", tabName = "contexto"),
            menuItem(text = "Informações por relatório", tabName = "info_relatorio"),
            menuItem(text = "Informações estaduais", tabName = "info_uf"),
            menuItem(text = "Informações municipais", tabName = "info_muni")
        )
    ),
    body = dashboardBody(
        tabItems(
            tabItem(
                tabName = "contexto",
                mod_contexto_ui("contexto_geral")
            )
        )
    )
    
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
}

shinyApp(ui, server)