
library(shiny)
library(shinydashboard)


# Carregar bases ----------------------------------------------------------

# dados sobre florestas
dados_uf <- plantiosflorestais::mapeamentos_estados
dados_muni <- plantiosflorestais::mapeamentos_municipios

# alocar as duas bases em uma lista
bases <- list(
  dados_uf = dados_uf,
  dados_muni = dados_muni
)

# shapes
# shp_brasil <- geobr::read_state() |> 
#  dplyr::rename(uf = "abbrev_state")

# abrindo no excel
# dados_uf |> viewxl::view_in_xl()
# dados_muni |> viewxl::view_in_xl()

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
                mod_contexto_ui("contexto_geral", bases)
            ),
            tabItem(
              tabName = "info_uf",
              mod_infos_uf_ui("informacoes_uf", bases)
            )
        )
    )
    
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  mod_contexto_server("contexto_geral", bases)
  mod_infos_uf_server("informacoes_uf", bases)
}

shinyApp(ui, server)