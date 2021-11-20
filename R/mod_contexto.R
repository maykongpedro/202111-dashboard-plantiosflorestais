

# Ui ----------------------------------------------------------------------

mod_contexto_ui <- function(id){
    
    #ns <- NS(id)
    
    ns <- NS(id)
    tagList(
        # linha 1
        fluidRow(
            column(
                width = 12,
                # uiOutput para o título porque ele vai ser reativo segundo o tipo,
                # além de alterar a cor.
                uiOutput(outputId = ns("titulo"))
            )
        ),
        # espaço em branco
        br(),
        # linha 2
        fluidRow(
            column(
                width = 4,
                selectInput(
                    inputId = ns("pokemon"),
                    label = "Selecione um pokémon",
                    choices = c("Carregando" = "")
                ),
                # uiOutput para a imagem porque ela vai alterar também
                uiOutput(outputId = ns("img_pokemon"))
            ),
            column(
                width = 8,
                plotOutput(outputId = ns("grafico_habilidades"))
            )
        ),
        # espaço em branco
        br(),
        # linha 3
        fluidRow(
            column(
                width = 12,
                plotOutput(outputId = ns("grafico_freq"))
            )
        )
    )
    
}

# Server ------------------------------------------------------------------


