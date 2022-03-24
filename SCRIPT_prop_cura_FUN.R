#####################################################

# Casos novos curados nos anos das coortes ---------
prop_cura_hans <- function(data, anoPB = anoPB, anoMB = anoMB) {
  # Numerador
  cura_pb <- bd %>%
    filter(MODOENTR == 1, ano_diag == anoPB, TPALTA_N == 1, CLASSATUAL == 1, ESQ_ATU_N == 1) %>% 
    group_by(ufresat) %>%
    summarize(cura_pb = n())
  
  
  cura_mb <- bd %>% 
    filter(MODOENTR == 1, ano_diag == anoMB, TPALTA_N == 1, CLASSATUAL == 2, ESQ_ATU_N == 2) %>% 
    group_by(ufresat) %>%
    summarize(cura_mb = n())
  
  
  cura_coorte <- full_join(cura_pb, cura_mb)
  
  cura_coorte <- replace(x = cura_coorte, list = is.na(cura_coorte), values = 0)
  
  cura_coorte <- cura_coorte %>% mutate(cura = cura_pb + cura_mb)
  
  cura_coorte <- cura_coorte %>% select(ufresat, cura)
  
  
  # Denominador
  saidas_pb <- bd %>%
    filter(MODOENTR == 1, ano_diag == anoPB, TPALTA_N %in% c(0, 1, 2, 3, 6, 7, 9), CLASSATUAL == 1, ESQ_ATU_N == 1) %>% 
    group_by(ufresat) %>%
    summarize(saidas_pb = n())
  
  
  saidas_mb <- bd %>% 
    filter(MODOENTR == 1, ano_diag == anoMB, TPALTA_N %in% c(0, 1, 2, 3, 6, 7, 9), CLASSATUAL == 2, ESQ_ATU_N == 2) %>% 
    group_by(ufresat) %>%
    summarize(saidas_mb = n())
  
  
  saidas_coorte <- full_join(saidas_pb, saidas_mb)
  
  saidas_coorte <- replace(x = saidas_coorte, list = is.na(saidas_coorte), values = 0)
  
  saidas_coorte <- saidas_coorte %>% mutate(saidas = saidas_pb + saidas_mb)
  
  saidas_coorte <- saidas_coorte %>% select(ufresat, saidas)
  
  
  coorte_final <- full_join(cura_coorte, saidas_coorte)
  
  coorte_final <- replace(x = coorte_final, list = is.na(coorte_final), values = 0)
  
  coorte_final <- coorte_final %>% mutate(prop_cura = (cura / saidas) * 100)
  
  return(coorte_final)
}


