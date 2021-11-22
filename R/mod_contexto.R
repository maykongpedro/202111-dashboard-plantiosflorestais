

# Ui ----------------------------------------------------------------------

mod_contexto_ui <- function(id, base){
    
    # gerar escolha dos relatórios
    relatorios_uf <- bases |> 
        purrr::pluck("dados_uf") |> 
        dplyr::distinct(mapeamento) 
    
    relatorios_muni <- bases |> 
        purrr::pluck("dados_muni") |> 
        dplyr::distinct(mapeamento)
    
    escolhas_relatorios <- 
        dplyr::bind_rows(relatorios_uf, relatorios_muni) |> 
        dplyr::distinct(mapeamento) |> 
        dplyr::arrange(mapeamento) |> 
        dplyr::pull(mapeamento)
    
    
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
                selectInput(
                    inputId = ns("nome_mapeamento"),
                    label = "Selecione um mapeamento",
                    choices = escolhas_relatorios,
                    selected = escolhas_relatorios[7]
                )
            )

        ),
        fluidRow(
            column(width = 6),
            box(
                width = 6,
                highcharter::highchartOutput(outputId = ns("mapa_relatorios"), 
                                             height = "800px")
            )
        )
    )
    
}

# Server ------------------------------------------------------------------
mod_contexto_server <- function(id, bases){
    moduleServer(id, function(input, output, session){
        
        # output$mapa_relatorios <- renderPlot({
        #     # dados_uf |> dplyr::glimpse()
        #     # base |>
        #     # dados_uf |> 
        #         # dplyr::group_by(mapeamento, genero) |>
        #         # dplyr::summarise(area = sum(area_ha, na.rm = TRUE)) |> 
        #         # #dplyr::filter(mapeamento == "IBÁ - Relatório Anual 2020") |> 
        #         # # dplyr::filter(mapeamento == "IBÁ - Não identificado") 
        #         # 
        #         # dplyr::filter(mapeamento == input$nome_mapeamento)  |>       
        #         # 
        #         # ggplot2::ggplot(
        #         #     ggplot2::aes(
        #         #         x = genero,
        #         #         y = area/10^6
        #         #     )
        #         # ) +
        #         # ggplot2::geom_col() 
        #         
        #     
        #     # gerar resumo de relatório existente por estado
        #     tb_relatorio_uf <- base |>
        #         #dados_uf |>
        #         dplyr::distinct(mapeamento, uf) |>
        #         dplyr::filter(!is.na(uf)) |>
        #         dplyr::group_by(mapeamento) |>
        #         dplyr::mutate(valor = 1)
        # 
        #     # filtrar relatório
        #     tb_relatorio_selecionado <- tb_relatorio_uf |>
        #         # dplyr::filter(mapeamento == "IBÁ - Relatório Anual 2020")
        #         dplyr::filter(mapeamento == input$nome_mapeamento)
        # 
        #     # fazer join
        #     shp_plot <- shp_brasil |>
        #         dplyr::left_join(tb_relatorio_selecionado, by = "uf")
        # 
        #     # plotar
        #     shp_plot |>
        #         #sf::st_simplify(dTolerance = 10000) |>
        #         ggplot2::ggplot()+
        #         ggplot2::geom_sf(alpha = .5,
        #                          color = "white",
        #                          size = 0.2,) +
        #         ggplot2::geom_sf(
        #             ggplot2::aes(fill = valor),
        #             show.legend = FALSE
        #         ) +
        #         ggplot2::theme_minimal() +
        #         ggplot2::theme(axis.text = ggplot2::element_blank())
        #     
        # })
 
        output$mapa_relatorios <- highcharter::renderHighchart({
            
            # gerar resumo de relatório existente por estado
            tb_relatorio_uf <- bases |>
                purrr::pluck("dados_uf") |> 
                dplyr::distinct(mapeamento, uf) |>
                dplyr::filter(!is.na(uf)) |>
                dplyr::group_by(mapeamento) |>
                dplyr::mutate(valor = 1)
            
            # filtrar relatório
            tb_relatorio_selecionado <- tb_relatorio_uf |>
                # dplyr::filter(mapeamento == "IBÁ - Relatório Anual 2020")
                dplyr::filter(mapeamento == input$nome_mapeamento)

            highcharter::hcmap(
                map = "countries/br/br-all",
                nullColor = "#d3d3d3",
                data = tb_relatorio_selecionado,
                value = "valor",
                joinBy = c("hc-a2", "uf"),
                borderColor = "#FAFAFA",
                borderWidth = 0.1,
                name = "Estado",
                dataLabels = list(enabled = TRUE, format = "{point.code}"),
                tooltip = list(valueDecimals = 0),
                download_map_data = F
            ) |>
                highcharter::hc_legend(ggplot2::element_blank()) |>
                highcharter::hc_colorAxis(minColor = "#008d4c",
                                          maxColor = "#008d4c")
            
        })

 
        
    })
}


