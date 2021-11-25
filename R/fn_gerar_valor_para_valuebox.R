
# nao consegui fazer a função funcionar
gerar_valor_para_valuebox <- function(database,
                                      ano,
                                      nome_mapeamento,
                                      estado
                                      # genero_desejado,
                                      # todos_os_generos = FALSE
                                      ) {

    # filtrar base - ele não encontra nada nessa base sei lá porque
    base <- database |> 
        dplyr::filter(
            ano_base == ano,
            mapeamento == nome_mapeamento,
            uf %in% estado
        )
    
    # dados |> 
    #     dplyr::filter(
    #         ano_base == 2019,
    #         mapeamento == "AGEFLOR - O setor de base florestal no Rio Grande do Sul 2020",
    #         uf %in% "RS"
    #     )
        
    # browser()
    # # checar se é todos os gêneros
    # if (todos_os_generos == TRUE) {
    #     
    #     generos <- unique(base$genero)
    #             
    # } else{
    #     
    #     generos <- genero_desejado
    # }
    
    # fazer o cálculo da área
    area <- base |> 
        #dplyr::filter(genero %in% generos)|>
        dplyr::group_by(mapeamento) |> 
        dplyr::summarise(area_total = sum(area_ha, na.rm = TRUE),
                         .groups = 'drop') |> 
        dplyr::pull(area_total) |> 
        scales::number(big.mark = ".", decimal.mark = ",")
    
    
    return(area)
    
}
