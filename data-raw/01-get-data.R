
# Carregar bases ----------------------------------------------------------

# dados sobre florestas
dados_uf <- plantiosflorestais::mapeamentos_estados |> 
    dplyr::mutate(base = "Mapeamentos estados")


dados_muni <- plantiosflorestais::mapeamentos_municipios |> 
    dplyr::mutate(base = "Mapeamentos municípios")    


# Agrupar os dados de municípios para ter infos resumidas -----------------

# verificar colunas
dados_muni |> dplyr::glimpse()
dados_uf |> dplyr::glimpse()

# verificar relatórios por base
dados_muni |> dplyr::distinct(mapeamento)
dados_uf |> dplyr::distinct(mapeamento)


# agrupar
dados_muni_agrupados <- dados_muni |> 
    # retirar ageflor porque já existe no mapeamento de estados
    dplyr::filter(!stringr::str_detect(mapeamento, "AGEFLOR")) |> 
    dplyr::group_by(
        mapeamento,
        fonte,
        ano_base, 
        uf, 
        estado,
        nucleo_regional,
        genero, 
        base
        ) |> 
    dplyr::summarise(area_ha = sum(area_ha, na.rm = TRUE)) |> 
    dplyr::ungroup()

# unir bases
dados_completos <- dplyr::bind_rows(dados_uf, dados_muni_agrupados)
dados_completos |> View()


# ajustando o fator dos gênero
dados_completos <- dados_completos |>
    dplyr::mutate(genero = factor(
        genero,
        levels = c("Eucalyptus",
                   "Pinus",
                   "Outros",
                   "Corte",
                   "Acacia",
                   "Tectona"),
        ordered = TRUE
    ))

# Exportar dados ----------------------------------------------------------
dados_uf |> saveRDS("./data/dados_uf.rds")
dados_muni |> saveRDS("./data/dados_muni.rds")
dados_completos |> saveRDS("./data/dados_completos.rds")

