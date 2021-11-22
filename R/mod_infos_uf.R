
# Ui ----------------------------------------------------------------------

mod_infos_uf_ui <- function(id, bases){
    
    # gerar escolha dos relatórios -> transformar em um uiOutput
    escolhas_relatorios <- bases |> 
        purrr::pluck("dados_uf") |> 
        dplyr::distinct(mapeamento) |> 
        dplyr::arrange(mapeamento) |> 
        dplyr::pull(mapeamento)
    
    # gerar escolhas ano base (isso vai ser o principal)
    escolhas_ano_base <- bases |> 
        purrr::pluck("dados_uf") |>
        dplyr::mutate(ano_base = as.double(ano_base)) |> 
        dplyr::distinct(ano_base) |> 
        dplyr::arrange(ano_base) |> 
        dplyr::pull(ano_base)
    
    # transformar a função acima em uma function separada em outro script R
    
    # gerar escolhas de uf -> transformar em um uiOutput
    # retirar NA das escolhas
    escolhas_uf <- bases |> 
        purrr::pluck("dados_uf") |>
        dplyr::distinct(uf) |> 
        dplyr::arrange(uf) |> 
        dplyr::pull(uf)
    
    
    ns <- NS(id)
    tagList(
        fluidRow(
            column(
                width = 2,
                sliderInput(
                    inputId = ns("ano_base"),
                    label = "Escolha o ano base do relatório",
                    min = min(escolhas_ano_base),
                    max = max(escolhas_ano_base),
                    value = 2019,
                    step = 1,
                    sep = ""
                    # width = "300px"
                )
            ),
            column(
                width = 4,
                selectInput(
                    inputId = ns("nome_mapeamento"),
                    label = "Selecione um mapeamento",
                    choices = escolhas_relatorios,
                    selected = escolhas_relatorios[1]
                    # width = "300px"
                )
            ),
            column(
                width = 2,
                shinyWidgets::pickerInput(
                    inputId = ns("uf"),
                    label = "Selecione o estado de interesse.",
                    choices = escolhas_uf,
                    selected = escolhas_uf,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                )
            )
            
        ),
        fluidRow(
            # column(width = 6),
            box(
                width = 6,
                highcharter::highchartOutput(outputId = ns("plot_genero"))
            )
        )
    )
    
}

# Server ------------------------------------------------------------------

mod_infos_uf_server <- function(id, bases) {
    moduleServer(id, function(input, output, session) {
        
        output$plot_genero <- highcharter::renderHighchart({
            
            # quanto atualizo o estado ele não está fazendo o arrange
            
            tb_plot <- bases |> 
                purrr::pluck("dados_uf") |> 
                # dplyr::filter(
                #     ano_base == "2019",
                #     mapeamento == "IBÁ - Relatório Anual 2020",
                #     # uf %in% c("PR", "SC", "RS")
                #     uf %in% unique(bases$dados_uf$uf)
                # ) |> 
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento,
                    uf %in% input$uf
                    ) |>
                dplyr::group_by(uf, genero) |> 
                dplyr::summarise(area_tot = sum(area_ha, na.rm = TRUE)) |> 
                dplyr::arrange(dplyr::desc(area_tot))
            
            tb_plot |>
                highcharter::hchart(
                    type = "column",
                    highcharter::hcaes(
                        x = uf,
                        y = area_tot,
                        group = genero
                    ),
                    stacking = "normal"
                    ) |> 
                # highcharter::hc_colors(
                #     #definir cores
                #     colors = c('#000004', '#B63679', '#FCFDBF')
                # ) |>
                highcharter::hc_xAxis(title = list(text = "Estado")) |>
                highcharter::hc_yAxis(title = list(text = "Área em hectares")) |>
                highcharter::hc_title(text = "Área plantada por gênero e estado") |> 
                highcharter::hc_subtitle(text = "Subtítulo atual") |> 
                highcharter::hc_add_theme(highcharter::hc_theme_elementary()) 

            
        })
        
    })
}
