
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
                plotOutput(outputId = ns("plot_genero"))
                # plotOutput(outputId = ns("mapa_relatorios"))
                # highcharter::highchartOutput(outputId = ns("mapa_relatorios"), 
                #                              height = "800px")
            )
        )
    )
    
}

# Server ------------------------------------------------------------------

mod_infos_uf_server <- function(id, bases) {
    moduleServer(id, function(input, output, session) {
        
        # output$plot_genero <- renderPlot({
        #     
        #     tb_plot <- bases |> 
        #         purrr::pluck("dados_uf") |> 
        #         dplyr::filter(
        #             ano_base == "2019",
        #             mapeamento == "IBÁ - Relatório Anual 2020",
        #             # uf %in% c("PR", "SC", "RS")
        #             uf %in% unique(bases$dados_uf$uf)
        #             ) |> 
        #         # dplyr::filter(
        #         #     ano_base == input$ano_base,
        #         #     mapeamento == input$nome_mapeamento,
        #         #     uf %in% input$uf
        #         #     ) |>
        #         dplyr::group_by(uf, genero) |> 
        #         dplyr::summarise(area_tot = sum(area_ha, na.rm = TRUE))
        #         
        #     tb_plot |> 
        #         ggplot2::ggplot(
        #             ggplot2::aes(
        #                 x = uf,
        #                 y = area_tot,
        #                 fill = genero
        #             )
        #         ) +
        #         ggplot2::geom_col() +
        #         ggplot2::scale_fill_viridis_d()
        #         
        #     
        # })
        
        output$plot_genero <- highcharter::renderHighchart({
            
            tb_plot <- bases |> 
                purrr::pluck("dados_uf") |> 
                dplyr::filter(
                    ano_base == "2019",
                    mapeamento == "IBÁ - Relatório Anual 2020",
                    # uf %in% c("PR", "SC", "RS")
                    uf %in% unique(bases$dados_uf$uf)
                ) |> 
                # dplyr::filter(
                #     ano_base == input$ano_base,
                #     mapeamento == input$nome_mapeamento,
                #     uf %in% input$uf
                #     ) |>
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
                
                # highcharter::hc_yAxis(title = "Área em hectares") |> 
                highcharter::hc_add_theme(highcharter::hc_theme_elementary()) 

  
            

            


            
        })
        
    })
}
