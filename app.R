
library(shiny)
library(shinydashboard)


# Carregar bases ----------------------------------------------------------
dados <- readr::read_rds("data/dados_completos.rds")
# dados |> dplyr::glimpse()

# shape dos estados
# shp_br <-
#   geobr::read_state() |>
#   dplyr::rename(uf = abbrev_state)


# Ui ----------------------------------------------------------------------
ui <- dashboardPage(

    skin = "green",
    header = dashboardHeader(
        title = "Plantios florestais"
        #titleWidth = 275
    ),
    sidebar = dashboardSidebar(
        sidebarMenu(
            # página 1 - contexto
            menuItem(text = "Contexto", tabName = "contexto"),
            # página 2 - informações para cada relatório
            menuItem(text = "Informações por relatório", tabName = "info_relatorio"),
            # página 3 - informações em nível estadual
            menuItem(text = "Informações estaduais", tabName = "info_uf"),
            # página 4 - informações em nível de município
            menuItem(text = "Informações municipais", tabName = "info_muni")
        )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "contexto",
          mod_contexto_ui("contexto_geral", dados)
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
  
  mod_contexto_server("contexto_geral", dados, shp_br)
  mod_infos_uf_server("informacoes_uf", dados)
  
}

shinyApp(ui, server)