
library(shiny)
library(shinydashboard)


# Carregar bases ----------------------------------------------------------
dados <- readr::read_rds("data/dados_completos.rds")
tab_informativa <- readr::read_rds("data/tab_informativa.rds")

# dados |> dplyr::glimpse()

# Ui ----------------------------------------------------------------------
ui <- dashboardPage(

    skin = "green",
    header = dashboardHeader(
        title = "Plantios florestais"
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(
            # página 1 - contexto
            menuItem(text = "Contexto", tabName = "contexto"),
            # página 2 - informações em nível estadual
            menuItem(text = "Informações estaduais", tabName = "info_uf")
        )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "contexto",
          mod_contexto_ui("contexto_geral", tab_informativa)
        ),
        tabItem(
          tabName = "info_uf",
          mod_infos_uf_ui("informacoes_uf", dados)
        )
      )
    )
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  mod_contexto_server("contexto_geral", tab_informativa)
  mod_infos_uf_server("informacoes_uf", dados)
  
}

shinyApp(ui, server)