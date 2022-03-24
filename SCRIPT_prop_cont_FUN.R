##########################################

prop_contatos <- function(data, anoPB = anoPB, anoMB = anoMB) {
  
  # Contatos examinados PB
  numerador_pb1 <- bd %>% 
    filter(MODOENTR == 1, ano_diag == anoPB, CLASSATUAL == 1, ESQ_ATU_N == 1) %>% 
    dplyr::select(ufresat, CONTEXAM) %>% 
    group_by(ufresat) %>% 
    summarise(exam_pb = sum(CONTEXAM)) %>% 
    ungroup () %>% droplevels(.)
  
  numerador_pb2 <- subset(numerador_pb1, ufresat != 0)
  
  
  # Contatos examinados MB
  numerador_mb1 <- bd %>% 
    filter(MODOENTR == 1, ano_diag == anoMB, CLASSATUAL == 2, ESQ_ATU_N == 2) %>% 
    dplyr::select(ufresat, CONTEXAM) %>% 
    group_by(ufresat) %>% 
    summarise(exam_mb = sum(CONTEXAM)) %>% 
    ungroup () %>% droplevels(.)
  
  numerador_mb2 <- subset(numerador_mb1, ufresat != 0)
  
  # join numerador
  numerador <- full_join(numerador_pb2, numerador_mb2, by = "ufresat")
  
  
  # Contatos registrados PB
  denominador_pb1 <- bd %>% 
    filter(MODOENTR == 1, ano_diag == anoPB, CLASSATUAL == 2, ESQ_ATU_N == 2) %>% 
    dplyr::select(ufresat, CONTREG) %>% 
    group_by(ufresat) %>% 
    summarise(reg_pb = sum(CONTREG)) %>% 
    ungroup () %>% droplevels(.)
  
  denominador_pb2 <- subset(denominador_pb1, ufresat != 0) 
  
  
  # Contatos registrados MB
  denominador_mb1 <- bd %>% 
    filter(MODOENTR == 1, ano_diag == anoMB, CLASSATUAL == 2, ESQ_ATU_N == 2) %>% 
    dplyr::select(ufresat, CONTREG) %>% 
    group_by(ufresat) %>% 
    summarise(reg_mb = sum(CONTREG)) %>% 
    ungroup () %>% droplevels(.)
  
  denominador_mb2 <- subset(denominador_mb1, ufresat != 0)
  
  # join denominador
  denominador <- full_join(denominador_pb2, denominador_mb2, by = "ufresat")
  
  
  # Join final
  contatos1 <- full_join(numerador, denominador, by = "ufresat")
  
  # Converter NAs em 0
  contatos2 <- replace(x = contatos1, 
                       list = is.na(contatos1),
                       values = 0)
  
  contatos3 <- contatos2 %>% mutate(cont_exam = exam_pb + exam_mb,
                                    cont_reg = reg_pb + reg_mb)
  
  
  contatos_final <- contatos3 %>% dplyr::select(ufresat,
                                                cont_exam,
                                                cont_reg)
  
  
  return(contatos_final)
  
}


### FIM ###