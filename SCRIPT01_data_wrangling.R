#####################################

# Selecionando as variáveis
bd <- bd %>% select(DT_NOTIFIC, ID_MUNICIP, 
                    ID_UNIDADE,
                    DT_DIAG, NU_IDADE_N, 
                    CS_SEXO, CS_RACA, 
                    CS_ESCOL_N, SG_UF, 
                    ID_MN_RESI, NM_BAIRRO,
                    NM_LOGRADO, NM_COMPLEM, 
                    NM_REFEREN, ID_OCUPA_N, 
                    NU_LESOES, FORMACLINI, 
                    AVALIA_N, CLASSOPERA, 
                    MODOENTR, MODODETECT, 
                    BACILOSCOP, DTINICTRAT, 
                    ESQ_INI_N, CONTREG, 
                    NERVOSAFET, UFATUAL, 
                    ID_MUNI_AT, NU_NOT_AT, 
                    DT_NOTI_AT, ID_UNID_AT, 
                    UFRESAT, MUNIRESAT, 
                    DTULTCOMP, CLASSATUAL, 
                    AVAL_ATU_N, ESQ_ATU_N, 
                    DOSE_RECEB, EPIS_RACIO, 
                    DTMUDESQ, CONTEXAM, 
                    DTALTA_N, TPALTA_N
)


# Renomeando as variáveis chaves para realizar o merge
bd <- bd %>% 
  mutate(Uni_not = ID_UNIDADE,   # renoamendo variável de unidade de entrada
         Uni_At = ID_UNID_AT)    # renoamendo variável de unidade de acompanhamento


# Criando a variável "ano_diag"
bd$DT_DIAG <- as.Date(bd$DT_DIAG, format = "%d/%m/%Y")

#filtrar pelos anos de análise
bd <- mutate(bd, ano_diag = year(bd$DT_DIAG)) %>% 
  filter(ano_diag >= 2017 & ano_diag <= 2020)


# MERGE (JOIN) --------------------------------------------------------------

# Aqui é realizada a junção dos bancos de dados de hanseníase com os BD
# do CNES pelas chaves Uni_not e Uni_at.. Esse procedimento é realizado 
# para identificar quem foi notificado em uma unidade 
# prisional e/ou quem foi acompanhado em uma unidade prisional
class(bd$Uni_not)
class(cnesnot$Uni_not)

bd$Uni_not <- as.character(as.factor(bd$Uni_not))
bd$Uni_At <- as.character(as.factor(bd$Uni_At))
cnesnot$Uni_not <- as.character(as.factor(cnesnot$Uni_not))
cnesat$Uni_At <- as.character(as.factor(cnesat$Uni_At))

merge1 <- full_join(bd, cnesnot, by = c("Uni_not" = "Uni_not"))
merge2 <- full_join(merge1, cnesat, by = c("Uni_At" = "Uni_At"))


##############################################################

# CONDIÇÕES -------------------------------------------------

# Aqui são criadas condições para selecionar os casos de hanseníase na 
# população prisional utilizando todas as variáveis que contêm essa 
# informação
bd <- merge2 %>% mutate(prisio = case_when(
  ID_OCUPA_N == 999995 |
    NM_BAIRRO == "PRIOSIONAL" | 
    NM_BAIRRO == "PRESIDIO" | 
    NM_BAIRRO == "CADEIA" | 
    NM_BAIRRO == "PENITENCIARIA" | 
    NM_BAIRRO == "DETENCAO" |
    NM_LOGRADO == "PRIOSIONAL" | 
    NM_LOGRADO == "PRESIDIO" | 
    NM_LOGRADO == "CADEIA" | 
    NM_LOGRADO == "PENITENCIARIA" | 
    NM_LOGRADO == "DETENCAO" |
    NM_COMPLEM == "PRIOSIONAL" | 
    NM_COMPLEM == "PRESIDIO" | 
    NM_COMPLEM == "CADEIA" | 
    NM_COMPLEM == "PENITENCIARIA" | 
    NM_COMPLEM == "DETENCAO" |
    NM_REFEREN == "PRIOSIONAL" | 
    NM_REFEREN == "PRESIDIO" | 
    NM_REFEREN == "CADEIA" | 
    NM_REFEREN == "PENITENCIARIA" | 
    NM_REFEREN == "DETENCAO" |
    Uni_Cod_Tipo_Unid.x %in% c(1,2,4,7,22,36) & Uni_Cod_Tipo_Unid.y %in% c(1,2,4,7,22,36) ~ 1
)
)


bd$prisio <- replace(x = bd$prisio, list = is.na(bd$prisio), values = 0)

bd$prisio2 <- ifelse(
  bd$prisio == 0, 'Não PPL',
  ifelse(bd$prisio == 1, 'PPL', NA))

# Banco de dados apenas com a PPL
bd <- bd %>% 
  filter(prisio == 1)


# Retirada dos erros de diagnóstico
bd$TPALTA_N <- as.numeric(as.character(as.factor(bd$TPALTA_N)))

bd$TPALTA_N <- replace(x = bd$TPALTA_N, list = is.na(bd$TPALTA_N), values = 0)

# Exclusão dos erros de diagnóstico
bd <- filter(bd, TPALTA_N != 8)


# Retirada das faixas etárias <18 anos
bd <- bd %>% mutate(idade = str_sub(bd$NU_IDADE_N, 3, 4))

bd$idade <- as.numeric(as.character(as.factor(bd$idade)))

bd <- bd %>%
  filter(idade >= 18)


# Aqui é criada uma variável com os códigos das UFs
# Criação da variável ufres a partir da var ID_MN_RESI
class(bd$ID_MN_RESI)

bd$ID_MN_RESI <- as.character(as.factor(bd$ID_MN_RESI))

bd <- bd %>% mutate(ufres = str_sub(bd$ID_MN_RESI, 0, 2))

bd$ufres <- as.factor(bd$ufres)


# Aqui é criada uma variável com os códigos das UFs
# Criação da variável ufresat a partir da var MUNIRESAT
class(bd$MUNIRESAT)

bd$MUNIRESAT <- as.character(as.factor(bd$MUNIRESAT))

bd <- bd %>% mutate(ufresat = str_sub(bd$MUNIRESAT, 0, 2))

bd$ufresat <- as.factor(bd$ufresat)


# Organização das variáveis ------------------------------------------
# Recorte para criação de um BD só com casos novos
bd <- bd %>%
  filter(MODOENTR == 1)


# SEXO ---------------------------------------------------------------
# 0 - Feminino; 1 - Masculino
bd$sexo <- ifelse(
  bd$CS_SEXO == "M", 'masculino',
  ifelse(bd$CS_SEXO == "F", 'feminino', NA))


## FAIXA ETÁRIA ------------------------------------------------
# fx_etaria: 0 - 0 a 17 anos; 1 - 18 a 29; 2 - 30 a 59; 3 - 60+
bd <- bd %>%
  mutate(fx_etaria = case_when(idade >= 60 ~ '60+',
                           idade >= 30  & idade <= 59 ~ '30 a 59',
                           idade >= 18 & idade <= 29 ~ '18 a 29'))

## RAÇA/COR:  --------------------------------------------------
# raça: 0 - 1, 3 e 5 (não negros); 1 - 2 e 4 (negros)
bd$raca <- ifelse(
  bd$CS_RACA == 1, 'não negros',
  ifelse(bd$CS_RACA == 2, 'negros',
         ifelse(bd$CS_RACA == 3, 'não negros',
                ifelse(bd$CS_RACA == 4, 'negros',
                       ifelse(bd$CS_RACA == 5, 'não negros',
         NA)))))


## ESCOLARIDADE  ----------------------------------------------

# Escolaridade: 0 - 0 (analfabeto); 1 - 1, 2, 3 e 4 (fundamental); 2 - 5 e 6 (médio completo);
# 3 - 7 e 8 (superior)
bd$escol <- ifelse(
  bd$CS_ESCOL_N == 00, 'analfabeto',
  ifelse(bd$CS_ESCOL_N == 0, 'analfabeto',
         ifelse(bd$CS_ESCOL_N == 01, 'EF',
                ifelse(bd$CS_ESCOL_N == 1, 'EF',
                       ifelse(bd$CS_ESCOL_N == 02, 'EF',
                              ifelse(bd$CS_ESCOL_N == 2, 'EF',
                                     ifelse(bd$CS_ESCOL_N == 03, 'EF',
                                            ifelse(bd$CS_ESCOL_N == 3, 'EF',
                                                   ifelse(bd$CS_ESCOL_N == 04, 'EF',
                                            ifelse(bd$CS_ESCOL_N == 4, 'EF',
                                     ifelse(bd$CS_ESCOL_N == 05, 'EM',
                              ifelse(bd$CS_ESCOL_N == 5, 'EM',
                       ifelse(bd$CS_ESCOL_N == 06, 'EM',
                ifelse(bd$CS_ESCOL_N == 6, 'EM',
        ifelse(bd$CS_ESCOL_N == 07, 'ES',
  ifelse(bd$CS_ESCOL_N == 7, 'ES',
        ifelse(bd$CS_ESCOL_N == 08, 'ES',
                ifelse(bd$CS_ESCOL_N == 8, 'ES',
                       ifelse(bd$CS_ESCOL_N == 10, 'não se aplica', NA
                                                                                                                                )))))))))))))))))))

## AVALIAÇÃO DO GIF -----------------------------------------------------------

# 0 - 0 GIF 0; 1-1 GIF 1; 2- 2 GIF 2; 3 - 3 Não avaliado
bd$gif_aval <- ifelse(
  bd$AVALIA_N == 0, '0',
  ifelse(bd$AVALIA_N == 1, '1',
         ifelse(bd$AVALIA_N == 2, '2',
                ifelse(bd$AVALIA_N == 3, 'não avaliado', NA))))


## CLASSIFICAÇÃO OPERACIONAL -------------------------------------------------

# Classificação operacional: 0 - 1 (paucibacilar; 1 - 2 (multibacilar)
bd$classop <- ifelse(
  bd$CLASSOPERA == 1, 'PB',
  ifelse(bd$CLASSOPERA == 2, 'MB', NA))

         
## FORMA CLÍNICA ------------------------------------------------------------

# Forma clínica: 0- 1 (indeterminada); 1- 2 (tuberculóide); 2- 3 (dimorfa);
#               3- 4 (virchowiana); 4- 5 (não classificada)

bd$formaclini <- ifelse(
  bd$FORMACLINI == 1, 'indeterminada',
  ifelse(bd$FORMACLINI == 2, 'tuberculóide',
         ifelse(bd$FORMACLINI == 3, 'dimorfa',
                ifelse(bd$FORMACLINI == 4, 'virchowiana',
                       ifelse(bd$FORMACLINI == 5, 'não classificada', NA)))))



## MODO DE DETECÇÃO -----------------------------------------------------

# Modo de detecção: 0- 1 (encaminhamento); 1- 2 (demanda espontânea);
#                    2- 3 (exame de coletividade); 3- 4 (exame de contatos);
#                    4- 5 (outros modos)
bd$detec <- ifelse(
  bd$MODODETECT == 1, 'encaminhamento',
  ifelse(bd$MODODETECT == 2, 'demanda espontânea',
         ifelse(bd$MODODETECT == 3, 'exame de coletividade',
                ifelse(bd$MODODETECT == 4, 'exame de contatos',
                       ifelse(bd$MODODETECT == 5, 'outros modos',
                              NA)))))


## BACILOSCOPIA --------------------------------------------------------

# Baciloscopia: 0- 1 (positiva); 1- 2 (negativa); 2- 3 (não realizada)
bd$bacilos <- ifelse(
  bd$BACILOSCOP == 1, 'positiva',
  ifelse(bd$BACILOSCOP == 2, 'negativa',
         ifelse(bd$BACILOSCOP == 3, 'não realizada',
                NA)))


# ESQUEMA TERAPÊUTICO ATUAL ------------------------------------------------

# Esquema terapêutico inicial: 0- 1 (PQT 6 doses); 1- 2 (PQT 12 doses);
# 2- 3 (outros esquemas substitutivos)
bd$esq_ini <- ifelse(
  bd$ESQ_INI_N == 1, 'PQT 6 doses',
  ifelse(bd$ESQ_INI_N == 2, 'PQT 12 doses',
         ifelse(bd$ESQ_INI_N == 3, 'OES',
                NA)))


