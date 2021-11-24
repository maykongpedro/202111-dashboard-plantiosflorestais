
# Ui ----------------------------------------------------------------------

mod_infos_uf_ui <- function(id, dados){
    
    # gerar escolhas ano base
    escolhas_ano_base <- dados |> 
        dplyr::mutate(ano_base = as.double(ano_base)) |> 
        dplyr::distinct(ano_base) |> 
        dplyr::arrange(ano_base) |> 
        dplyr::pull(ano_base)
    
    ns <- shiny::NS(id)
    tagList(
        
        # linha 1 - título ----
        fluidRow(
            column(
                width = 12,
                h3("Informações de floresta plantada por estado.")
            )
        ),
        
        # espaço 1 -----
        br(),
        
        # linha 2 - valueBox ----
        fluidRow(
            shinydashboard::valueBoxOutput(outputId = ns("area_total_relatorio"), width = 2),
            shinydashboard::valueBoxOutput(outputId = ns("area_total_euc"), width = 2),
            shinydashboard::valueBoxOutput(outputId = ns("area_total_pin"), width = 2),
            shinydashboard::valueBoxOutput(outputId = ns("area_total_outros"), width = 2)
        ),
        
        # linha 3 - inputs ----
        fluidRow(
            box(
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
            box(
                width = 3,
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
            box(
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
            ),
            box(
                width = 3,
                title = "Dados do relatório exibidos no mapa",
                shinyWidgets::awesomeRadio(
                    inputId = ns("genero_mapa"),
                    label = "Selecione um gênero para exibir no mapa",
                    choices = c("Carregando..." = ""),
                    #choices = c("Eucalyptus", "Pinus", "Outros"),
                    # selected = "Pinus",
                    inline = TRUE
                    
                )
                # selectInput(
                #     inputId = ns("genero_mapa"),
                #     label = "Selecione um gênero para exibir no mapa",
                #     choices = c("Carregando..." = ""),
                #     multiple = FALSE
                # )
            )
        ), 
    
        # linha 4 - gráfico e mapa
        fluidRow(
            box(
                width = 4,
                highcharter::highchartOutput(outputId = ns("plot_genero"))
            ),
            box(
                width = 4,
                highcharter::highchartOutput(outputId = ns("plot_percentual"))
            ),
            box(
                width = 4,
                highcharter::highchartOutput(
                    outputId = ns("mapa_genero"),
                    height = "600px"
                    )
            )
        )
    )
    
}

# Server ------------------------------------------------------------------

mod_infos_uf_server <- function(id, dados) {
    moduleServer(id, function(input, output, session) {
        
        # atualizando a lista de mapeamento disponíveis de acordo com o ano ----
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
        
        # atualizando a lista de estados e gêneros disponíveis de acordo com o relatório
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
            
            # gerar escolhas dos gêneros
            escolhas_generos <- dados |> 
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento
                ) |> 
                dplyr::distinct(genero) |> 
                dplyr::arrange(genero) |>
                tidyr::drop_na() |> 
                dplyr::pull(genero)
     
           # atualizar select input dos gêneros
            # shiny::updateSelectInput(
            #     session = session,
            #     inputId = "genero_mapa",
            #     choices = escolhas_generos,
            #     selected = escolhas_generos[1] 
            # )
            
            shinyWidgets::updateAwesomeRadio(
                session = session,
                inputId = "genero_mapa",
                # transformar em character para retirar os fatores
                choices = as.character(escolhas_generos),
                selected = as.character(escolhas_generos)[1],
                inline = TRUE
            )
            
        })
        
        # output - valueBox: área total do relatório
        output$area_total_relatorio <- shinydashboard::renderValueBox({
            
            area_total <- dados |>
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento,
                    uf %in% input$uf
                ) |>
                dplyr::group_by(mapeamento) |>
                dplyr::summarise(area_total = sum(area_ha, na.rm = TRUE),
                                 .groups = 'drop') |>
                dplyr::pull(area_total) |> 
                scales::number(big.mark = ".", decimal.mark = ",")

            # browser()
            # area_total <- dados |> gerar_valor_para_valuebox(
            #     #base = dados,
            #     input$ano_base,
            #     input$nome_mapeamento,
            #     input$uf
            #     # genero_desejado = NULL,
            #     # todos_os_generos = TRUE
            # )

            # area_total <- gerar_valor_para_valuebox(
            #     dados,
            #     ano = 2016,
            #     "AGEFLOR - A indústria de base florestal no Rio Grande do Sul 2017",
            #     uf %in% unique(dados$uf),
            #     genero_desejado = NULL,
            #     todos_os_generos = TRUE
            # )
            
            shinydashboard::valueBox(
                value = area_total,
                subtitle = "Área total do relatório (ha)",
                icon = shiny::icon("chart-area")
            )
        })
        
        # output - valueBox: área total de Eucalipto
        output$area_total_euc <- shinydashboard::renderValueBox({
            
            area_total_euc <- dados |>
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento,
                    uf %in% input$uf,
                    genero == "Eucalyptus"
                ) |>
                dplyr::group_by(mapeamento) |>
                dplyr::summarise(area_total = sum(area_ha, na.rm = TRUE),
                                 .groups = 'drop') |>
                dplyr::pull(area_total) |> 
                scales::number(big.mark = ".", decimal.mark = ",")
            
            
            shinydashboard::valueBox(
                value = area_total_euc,
                subtitle = "Área total com Eucalyptus (ha)",
                icon = shiny::icon("pagelines"),
                color = "olive"
            )
            
        })
        
        # output - valueBox: área total de Pinus
        output$area_total_pin <- shinydashboard::renderValueBox({
            
            area_total_pin <- dados |>
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento,
                    uf %in% input$uf,
                    genero == "Pinus"
                ) |>
                dplyr::group_by(mapeamento) |>
                dplyr::summarise(area_total = sum(area_ha, na.rm = TRUE),
                                 .groups = 'drop') |>
                dplyr::pull(area_total) |> 
                scales::number(big.mark = ".", decimal.mark = ",")
            
            
            shinydashboard::valueBox(
                value = area_total_pin,
                subtitle = "Área total com Pinus (ha)",
                icon = shiny::icon("tree"),
                color = "orange"
            )
            
        })
        
        # output - valueBox: área total de Pinus
        output$area_total_outros <- shinydashboard::renderValueBox({
            
            area_total_outros <- dados |>
                dplyr::filter(
                    ano_base == input$ano_base,
                    mapeamento == input$nome_mapeamento,
                    uf %in% input$uf,
                    genero != c("Eucalyptus", "Pinus")
                ) |>
                dplyr::group_by(mapeamento) |>
                dplyr::summarise(area_total = sum(area_ha, na.rm = TRUE),
                                 .groups = 'drop') |>
                dplyr::pull(area_total) |> 
                scales::number(big.mark = ".", decimal.mark = ",")
            
            
            shinydashboard::valueBox(
                value = area_total_outros,
                subtitle = "Área total com outros gêneros (ha)",
                icon = shiny::icon("tree"),
                color = "purple"
            )
            
        })
        
        
        # output - gráfico de colunas
        output$plot_genero <- highcharter::renderHighchart({
            
            # total de área por estado
            tb_total_uf <- dados |> 
                # dplyr::filter(
                #     # ano_base == "2019",
                #     ano_base == "2012",
                #     #mapeamento == "IBÁ - Relatório Anual 2020",
                #     mapeamento == "Famato - Diagnóstico de florestas plantadas do Estado de Mato Grosso - 2013",
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
                #     # ano_base == "2019",
                #     ano_base == "2012",
                #     #mapeamento == "IBÁ - Relatório Anual 2020",
                #     mapeamento == "Famato - Diagnóstico de florestas plantadas do Estado de Mato Grosso - 2013",
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
                        
            # montando plot
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
                    #colors = c('#35B779', '#31688E')
                    # colors = c('#35B779', '#ED7953', '#31688E')
                    #colors = c('#35B779', '#ED7953', '#31688E', '#31688E', '#721F81', '233E6C' )
                    colors = definir_cores_genero(tb_plot)
                ) |>
                # "eucalipto" = "#35B779",
                # "pinus" = "#ED7953",
                # "outros" = "#31688E",
                # "corte" = "#575C6D"
                # "acacia" = "#721F81"
                # "tectona" = #233E6C"
                highcharter::hc_xAxis(title = list(text = "Estado")) |>
                highcharter::hc_yAxis(title = list(text = "Área em hectares")) |>
                highcharter::hc_title(text = "Área plantada por gênero e estado") |> 
                highcharter::hc_subtitle(text = input$nome_mapeamento) |> 
                highcharter::hc_add_theme(highcharter::hc_theme_elementary()) 

            
        })
        
        # output - mapa de área por gênero
        output$mapa_genero <- highcharter::renderHighchart({
            
            # total de área por estado
            tb_total_uf <- dados |> 
                # dplyr::filter(
                #     # ano_base == "2019",
                #     ano_base == "2012",
                #     #mapeamento == "IBÁ - Relatório Anual 2020",
                #     mapeamento == "Famato - Diagnóstico de florestas plantadas do Estado de Mato Grosso - 2013",
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
                #     # ano_base == "2019",
                #     ano_base == "2012",
                #     #mapeamento == "IBÁ - Relatório Anual 2020",
                #     mapeamento == "Famato - Diagnóstico de florestas plantadas do Estado de Mato Grosso - 2013",
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
                dplyr::select(-area_uf) |> 
                # filtrando o gênero 
                dplyr::filter(genero %in% input$genero_mapa) |>
                # transformando 0 em NA para o mapa pintar de cinza
                dplyr::mutate(area_tot = dplyr::na_if(area_tot, 0))

            
            # montando plot
            p_map <- highcharter::hcmap(
                map = "countries/br/br-all",
                nullColor = "#d3d3d3",
                data = tb_plot,
                value = "area_tot",
                joinBy = c("hc-a2", "uf"),
                borderColor = "#FAFAFA",
                borderWidth = 0.1,
                name = "Estado",
                dataLabels = list(enabled = TRUE, format = "{point.uf}"),
                tooltip = list(valueDecimals = 0, valueSuffix = " ha"),
                download_map_data = F
            ) |>
                highcharter::hc_colorAxis(
                    stops = highcharter::color_stops(
                        n = 10,
                        substring(viridis::viridis(20,
                                                   option = "viridis"
                                                   ),
                                  0,
                                  7)
                        )
                ) 
                # highcharter::hc_xAxis(title = list(text = "Estado")) |>
                # highcharter::hc_yAxis(title = list(text = "Área em hectares")) |>
                # highcharter::hc_title(text = "Área plantada por gênero e estado") |>
                # highcharter::hc_subtitle(text = input$nome_mapeamento) |>
                #highcharter::hc_add_theme(highcharter::hc_theme_elementary())

            p_map
            
        })
        
        
    
        
        
    })
}
