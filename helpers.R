ensuciar <- function(dataset, umbral) {
  dataset %>%
    mutate(across(
      where(is.numeric), 
      ~ ifelse(rbinom(length(.), 1, 0.05) == 1, . * 10, .))) %>% # Creamos valores erróneos
    mutate(id = 1:nrow(dataset)) %>% # ID para identificar mejor duplicados
    messy(messiness = umbral) %>% # Ensuciamos valores
    #messy_colnames(messiness = umbral) %>% # Ensuciamos nombres de columnas
    bind_rows(slice_sample(., prop = 0.1)) # Duplicados
}
