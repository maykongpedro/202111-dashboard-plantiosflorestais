
# Ui ----------------------------------------------------------------------

mod_infos_uf_ui <- function(id, dados){
    
    # gerar escolhas ano base
    escolhas_ano_base <- dados |> 
        dplyr::mutate(ano_base = as.double(ano_base)) |> 
        dplyr::distinct(ano_base) |> 
        dplyr::arrange(ano_base) |> 
        dplyr::pull(ano_base)
    
    # gerar escolha dos relatórios -> usar um observer
    # escolhas_relatorios <- dados |> 
    #     dplyr::distinct(mapeamento) |> 
    #     dplyr::arrange(mapeamento) |> 
    #     dplyr::pull(mapeamento)
    

    
    # transformar a função acima em uma function separada em outro script R
    
    # gerar escolhas de uf -> transformar em um uiOutput
    escolhas_uf <- dados |> 
        dplyr::distinct(uf) |> 
        dplyr::arrange(uf) |>
        tidyr::drop_na() |> 
        dplyr::pull(uf)
    
    
    ns <- shiny::NS(id)
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
                # shiny::uiOutput(outputId =ns("nome_mapeamento"))
                selectInput(
                    inputId = ns("nome_mapeamento"),
                    label = "Selecione um mapeamento",
                    choices = c("Carregando..." = "")
                    # selectize = TRUE,
                    #selected = escolhas_relatorios[1]
                    # choices = escolhas_relatorios,
                    # selected = escolhas_relatorios[1]
                    # width = "300px"
                )
            ),
            column(
                width = 2,
                shinyWidgets::pickerInput(
                    inputId = ns("uf"),
                    label = "Selecione o estado de interesse.",
                    choices = c("Carregando..." = ""),
                    # choices = escolhas_uf,
                    # selected = escolhas_uf,
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

mod_infos_uf_server <- function(id, dados) {
    moduleServer(id, function(input, output, session) {
        
        # atualizando a lista de mapeamento disponíveis de acordo com o ano
        shiny::observe({
            
            # gerar escolhas dos relatórios
            escolhas_relatorios <- dados |>
                dplyr::filter(ano_base == input$ano_base) |> 
                dplyr::distinct(mapeamento) |>
                dplyr::arrange(mapeamento) |>
                dplyr::pull(mapeamento)
            
            # atualizar select input definido na ui
            shiny::updateSelectInput(
                session = session,
                inputId = "nome_mapeamento",
                choices = escolhas_relatorios,
                selected = escolhas_relatorios[1]
            )

        })
        
        # atualizando a lista de estados disponíveis de acordo com o relatório
        shiny::observeEvent(
            eventExpr = input$nome_mapeamento, 
            handlerExpr = {
    
            # gerar escolhas dos estados
            escolhas_uf <- dados |> 
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento
                ) |> 
                dplyr::distinct(uf) |> 
                dplyr::arrange(uf) |>
                tidyr::drop_na() |> 
                dplyr::pull(uf)
            
            # atualizar select input dos estados
            shinyWidgets::updatePickerInput(
                session = session,
                inputId = "uf",
                choices = escolhas_uf,
                selected = escolhas_uf
            )
            
        })
        
        
        output$plot_genero <- highcharter::renderHighchart({
            
            # total de área por estado
            tb_total_uf <- dados |> 
                # dplyr::filter(
                #     ano_base == "2019",
                #     mapeamento == "IBÁ - Relatório Anual 2020",
                #     # uf %in% c("PR", "SC", "RS")
                #     uf %in% unique(dados$uf)
                # ) |>
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento,
                    uf %in% input$uf
                    ) |>
                dplyr::group_by(uf) |> 
                dplyr::summarise(area_uf = sum(area_ha, na.rm = TRUE),
                                 .groups = 'drop')
            
            # total de área por estado e por gênero
            tb_total_genero <- dados |> 
                # dplyr::filter(
                #     ano_base == "2019",
                #     mapeamento == "IBÁ - Relatório Anual 2020",
                #     # uf %in% c("PR", "SC", "RS")
                #     uf %in% unique(dados$uf)
                # ) |>
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento,
                    uf %in% input$uf
                    ) |>
                dplyr::group_by(uf, genero) |> 
                dplyr::summarise(area_tot = sum(area_ha, na.rm = TRUE),
                                 .groups = 'drop') 
            
            # total de área para gerar o plot
            tb_plot <- tb_total_uf |> 
                dplyr::left_join(tb_total_genero, by = "uf") |>
                dplyr::arrange(
                    dplyr::desc(area_uf),
                    dplyr::desc(area_tot),
                    ) |> 
                dplyr::select(-area_uf)
            
            
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
                highcharter::hc_colors(
                    #definir cores
                    colors = c('#000004', '#B63679', '#FCFDBF')
                    # colors = definir_cores_genero(tb_plot$genero)
                ) |>
                # "eucalipto" = "#35B779",
                # "pinus" = "#ED7953",
                # "outros" = "#31688E",
                highcharter::hc_xAxis(title = list(text = "Estado")) |>
                highcharter::hc_yAxis(title = list(text = "Área em hectares")) |>
                highcharter::hc_title(text = "Área plantada por gênero e estado") |> 
                highcharter::hc_subtitle(text = "Subtítulo atual") |> 
                highcharter::hc_add_theme(highcharter::hc_theme_elementary()) 

            
        })
        
    })
}
