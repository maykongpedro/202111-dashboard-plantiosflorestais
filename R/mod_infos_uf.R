
# Ui ----------------------------------------------------------------------

mod_infos_uf_ui <- function(id, base){
    
    # gerar escolha dos relatórios
    relatorios_uf <- bases |> 
        purrr::pluck("dados_uf") |> 
        dplyr::distinct(mapeamento) 
    
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                width = 6
                # definir output
            ),
            # column(
            #     width = 3,
            #     shinyWidgets::radioGroupButtons(
            #         inputId = ns("tipos_relatorios"),
            #         label = "Selecione a abrangência do relatório.",
            #         choices = c("Nacional", "Estadual", "Municipal"),
            #         size = "normal"
            #         )
            # ),
            column(
                width = 3,
                # selectInput(
                #     inputId = ns("nome_mapeamento"),
                #     label = "Selecione um mapeamento",
                #     choices = escolhas_relatorios,
                #     selected = escolhas_relatorios[7]
                # )
            )
            
        ),
        fluidRow(
            column(width = 6),
            box(
                width = 6,
                # plotOutput(outputId = ns("mapa_relatorios"), height = "800px")
                # plotOutput(outputId = ns("mapa_relatorios"))
                highcharter::highchartOutput(outputId = ns("mapa_relatorios"), 
                                             height = "800px")
            )
        )
    )
    
}

# Server ------------------------------------------------------------------

mod_infos_uf_server <- function(id, bases) {
    moduleServer(id, function(input, output, session) {
        output$grafico <- highcharter::renderHighchart({
            
        })
        
    })
}
