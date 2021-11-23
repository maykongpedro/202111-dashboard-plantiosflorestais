
definir_cores_genero <- function(base_genero){
    
    base_cores <- base_genero |>
        dplyr::mutate(
            cor = dplyr::case_when(
                genero == "Eucalyptus" ~ "#35B779",
                genero == "Pinus" ~ "#ED7953",
                genero == "Outros" ~ "#31688E",
                genero == "Corte" ~ "#575C6D",
                genero == "Acacia" ~ "#721F81",
                genero == "Tectona" ~ "#FFEA46",
                TRUE ~ NA_character_
            )
        ) |> 
        dplyr::arrange(genero)
    
    cores <- unique(base_cores$cor)
    
    return(cores)
    
}
