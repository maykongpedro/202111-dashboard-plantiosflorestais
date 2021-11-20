

# Ui ----------------------------------------------------------------------

mod_contexto_ui <- function(id){
    
    ns <- NS(id)
    tagList(
        fluidRow(
            box(
                width = 6
                # definir output
            ),
            column(
                #offset = 6,
                width = 6,
                selectInput(
                    inputId = ns("tipos_relatorios"),
                    choices = c("Nacional", "Estadual", "Municipal"),
                    label = "Selecione a abrangência do relatório."
                )
            )

        ),
        fluidRow(
            column(width = 6),
            box(
                width = 6,
                plotOutput(outputId = ns("mapa_relatorios"), height = "800px")
            )
        )
    )
    
}

# Server ------------------------------------------------------------------
mod_contexto_server <- function(id, base){
    moduleServer(id, function(input, output, session){
        
        
        
    })
}


