

# Ui ----------------------------------------------------------------------

mod_contexto_ui <- function(id, dados) {

    ns <- shiny::NS(id)
    
    # tagList(
        fluidRow(
            box(
                width = 5
                # definir output textual
                # escrever texto de contexto
            ),
            box(
                width = 7,
                title = "Relatórios dos mapeamentos utilizados e suas fontes",
                status = "success",
                solidHeader = TRUE,
                reactable::reactableOutput(outputId = ns("tabela"))
            )
        )
    # )
    
}

# # Server ------------------------------------------------------------------
mod_contexto_server <- function(id, dados) {
    moduleServer(id, function(input, output, session){
        
        # dados <- tab_informativa
        # dados |> dplyr::glimpse()
        
        output$tabela <- reactable::renderReactable({
            
            dados |>
                dplyr::select(-relatorio_markdown,-link) |> 
                reactable::reactable(
                    striped = TRUE,
                    compact = TRUE,
                    highlight = TRUE,
                    columns = list(
                        
                        responsavel = reactable::colDef("Responsável pelo relatório"),
                        fonte = reactable::colDef("Fonte"),
                        nome_relatorio = reactable::colDef(
                            "Nome e link do relatório",
                            minWidth = 200,
                            cell = function(value, index){
                                # Render as a link
                                url <- paste0(dados[index, "link"])
                                htmltools::tags$a(
                                    href = url, 
                                    target = "_blank", 
                                    as.character(value)
                                )
                            }
                        ),
                        # nome_relatorio = reactable::colDef("Nome do relatório"),
                        nivel_de_abrangencia = reactable::colDef("Nível de abrangência"),
                        pais_uf = reactable::colDef("País/Unidade Federativa"),
                        ano_base = reactable::colDef("Ano-base")
                    )
                )
            
        })
        

    })
}


