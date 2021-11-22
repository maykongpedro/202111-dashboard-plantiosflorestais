
# Carregar bases ----------------------------------------------------------

# dados sobre florestas
dados_uf <- plantiosflorestais::mapeamentos_estados |> 
    dplyr::mutate(base = "Mapeamentos estados")


dados_muni <- plantiosflorestais::mapeamentos_municipios |> 
    dplyr::mutate(base = "Mapeamentos municípios")    


# juntar as duas bases
dados_completos <- dplyr::bind_rows(dados_uf, dados_muni)


# Exportar dados ----------------------------------------------------------
dados_uf |> saveRDS("./data/dados_uf.rds")
dados_muni |> saveRDS("./data/dados_muni.rds")
dados_completos |> saveRDS("./data/dados_completos.rds")
