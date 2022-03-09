#####################################

# Selecionando as variáveis
bd <- bd %>% select(ID_MUNICIP, ID_UNIDADE,
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
                  DTMUDESQ, CONTEXAM, DTALTA_N, 
                  TPALTA_N
                  )


# Renomeando as variáveis chaves para realizar o merge
bd <- bd %>% rename(Uni_not = ID_UNIDADE) # renoamendo variável de unidade de entrada


bd <- bd %>% rename(Uni_At = ID_UNID_AT) # renoamendo variável de unidade de acompanhamento


# Criando a variável "ano_diag"
bd$DT_DIAG <- as.Date(bd$DT_DIAG, format = "%d/%m/%Y")
bd <- mutate(bd, ano_diag = year(bd$DT_DIAG))
bd <- bd %>% filter(ano_diag >= 2019 & ano_diag <= 2020)


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

bd_merge1 <- full_join(bd, cnesnot, by = c("Uni_not" = "Uni_not"))
bd_merge2 <- full_join(bd_merge1, cnesat, by = c("Uni_At" = "Uni_At"))
bd_merge3 <- arrange(bd_merge2, Uni_Cod_Tipo_Unid.y)

table(bd_merge3$Uni_Cod_Tipo_Unid.y)


bd_merge3 %>%
  group_by(Uni_Cod_Tipo_Unid.x, Uni_Cod_Tipo_Unid.y) %>%
  summarize(count = n())

bd_merge3 %>%
  group_by(Uni_Tipo_Unidade.x) %>%
  summarize(count = n())

bd_merge3 %>%
  group_by(Uni_Tipo_Unidade.x, ID_OCUPA_N == 999995) %>%
  summarize(count = n())


##############################################################

# CONDIÇÕES -------------------------------------------------

# Aqui são criadas condições para selecionar os casos de hanseníase na 
# população prisional utilizando todas as variáveis que contêm essa 
# informação
bd_cond_1 <- mutate(bd_merge3, "CONDIC_OCUPA" = ID_OCUPA_N == 999995)

bd_cond_1 %>%
  group_by(CONDIC_OCUPA) %>%
  summarize(count = n())

bd_cond_2 <- mutate(bd_cond_1, "CONDIC_BAIRRO" = grepl("PRISIONAL", bd_cond_1$NM_BAIRRO))

bd_cond_2 %>%
  group_by(CONDIC_BAIRRO) %>%
  summarize(count = n())

bd_cond_3 <- mutate(bd_cond_2, "CONDIC_BAIRRO_2" = grepl("PRESIDIO", bd_cond_2$NM_BAIRRO))

bd_cond_3 %>%
  group_by(CONDIC_BAIRRO_2) %>%
  summarize(count = n())


bd_cond_4 <- mutate(bd_cond_3, "CONDIC_BAIRRO_3" = grepl("CADEIA", bd_cond_3$NM_BAIRRO))

bd_cond_4 %>%
  group_by(CONDIC_BAIRRO_3) %>%
  summarize(count = n())

bd_cond_5 <- mutate(bd_cond_4, "CONDIC_BAIRRO_4" = grepl("PENITENCIARIA", bd_cond_4$NM_BAIRRO))

bd_cond_5 %>%
  group_by(CONDIC_BAIRRO_4) %>%
  summarize(count = n())

bd_cond_6 <- mutate(bd_cond_5, "CONDIC_BAIRRO_5" = grepl("DETENCAO", bd_cond_5$NM_BAIRRO))

bd_cond_6 %>%
  group_by(CONDIC_BAIRRO_5) %>%
  summarize(count = n())

bd_cond_7 <- mutate(bd_cond_6, "CONDIC_LOGRADO" = grepl("DETENCAO", bd_cond_6$NM_LOGRADO))

bd_cond_7 %>%
  group_by(CONDIC_LOGRADO) %>%
  summarize(count = n())

bd_cond_8 <- mutate(bd_cond_7, "CONDIC_LOGRADO_2" = grepl("PRESIDIO", bd_cond_7$NM_LOGRADO))

bd_cond_8 %>%
  group_by(CONDIC_LOGRADO_2) %>%
  summarize(count = n())

bd_cond_9 <- mutate(bd_cond_8, "CONDIC_LOGRADO_3" = grepl("PRISIONAL", bd_cond_8$NM_LOGRADO))

bd_cond_9 %>%
  group_by(CONDIC_LOGRADO_3) %>%
  summarize(count = n())


bd_cond_10 <- mutate(bd_cond_9, "CONDIC_LOGRADO_4" = grepl("CADEIA", bd_cond_9$NM_LOGRADO))

bd_cond_10 %>%
  group_by(CONDIC_LOGRADO_4) %>%
  summarize(count = n())


bd_cond_11 <- mutate(bd_cond_10, "CONDIC_LOGRADO_5" = grepl("PENITENCIARIA", bd_cond_10$NM_LOGRADO))

bd_cond_11 %>%
  group_by(CONDIC_LOGRADO_5) %>%
  summarize(count = n())


bd_cond_12 <- mutate(bd_cond_11, "CONDIC_NM_COMPLEM" = grepl("PRISIONAL", bd_cond_11$NM_COMPLEM))

bd_cond_12 %>%
  group_by(CONDIC_NM_COMPLEM) %>%
  summarize(count = n())

bd_cond_13 <- mutate(bd_cond_12, "CONDIC_NM_COMPLEM_2" = grepl("PRISIDIO", bd_cond_12$NM_COMPLEM))

bd_cond_13 %>%
  group_by(CONDIC_NM_COMPLEM_2) %>%
  summarize(count = n())

bd_cond_14 <- mutate(bd_cond_13, "CONDIC_NM_COMPLEM_3" = grepl("DETENCAO", bd_cond_13$NM_COMPLEM))

bd_cond_14 %>%
  group_by(CONDIC_NM_COMPLEM_3) %>%
  summarize(count = n())


bd_cond_15 <- mutate(bd_cond_14, "CONDIC_NM_COMPLEM_4" = grepl("CADEIA", bd_cond_14$NM_COMPLEM))

bd_cond_15 %>%
  group_by(CONDIC_NM_COMPLEM_4) %>%
  summarize(count = n())


bd_cond_16 <- mutate(bd_cond_15, "CONDIC_NM_COMPLEM_5" = grepl("PENITENCIARIA", bd_cond_15$NM_COMPLEM))

bd_cond_16 %>%
  group_by(CONDIC_NM_COMPLEM_5) %>%
  summarize(count = n())


bd_cond_17 <- mutate(bd_cond_16, "CONDIC_NM_REFEREN" = grepl("PRISIONAL", bd_cond_16$NM_REFEREN))

bd_cond_17 %>%
  group_by(CONDIC_NM_REFEREN) %>%
  summarize(count = n())


bd_cond_18 <- mutate(bd_cond_17, "CONDIC_NM_REFEREN_2" = grepl("PRESIDIO", bd_cond_17$NM_REFEREN))

bd_cond_18 %>%
  group_by(CONDIC_NM_REFEREN_2) %>%
  summarize(count = n())


bd_cond_19 <- mutate(bd_cond_18, "CONDIC_NM_REFEREN_3" = grepl("DETENCAO", bd_cond_18$NM_REFEREN))

bd_cond_19 %>%
  group_by(CONDIC_NM_REFEREN_3) %>%
  summarize(count = n())


bd_cond_20 <- mutate(bd_cond_19, "CONDIC_NM_REFEREN_4" = grepl("CADEIA", bd_cond_19$NM_REFEREN))

bd_cond_20 %>%
  group_by(CONDIC_NM_REFEREN_4) %>%
  summarize(count = n())


bd_cond_21 <- mutate(bd_cond_20, "CONDIC_NM_REFEREN_5" = grepl("PENITENCIARIA", bd_cond_20$NM_REFEREN))

bd_cond_21 %>%
  group_by(CONDIC_NM_REFEREN_5) %>%
  summarize(count = n())


bd_cond_22 <- mutate(bd_cond_21, CONDICAO_UNI_TPX = ifelse(Uni_Cod_Tipo_Unid.x < 37,
                                                           "TRUE", "FALSE"))

bd_cond_22 %>%
  group_by(CONDICAO_UNI_TPX) %>%
  summarize(count = n())


bd_cond_23 <- mutate(bd_cond_22, CONDICAO_UNI_TPY = ifelse(Uni_Cod_Tipo_Unid.y < 37,
                                                           "TRUE", "FALSE"))

bd_cond_23 %>%
  group_by(CONDICAO_UNI_TPY) %>%
  summarize(count = n())


bd_9 <- bd_cond_23


# Criação da variável "prisio" para identificar os casos de hanseníase na PPL
bd_9$prisio <- ifelse(
  bd_9$CONDIC_OCUPA == TRUE, "1",
  ifelse(bd_9$CONDIC_END == TRUE, "1",
         ifelse(bd_9$CONDIC_END_2 == TRUE, "1",
                ifelse(bd_9$CONDIC_LOGRADO == TRUE, "1",
                       ifelse(bd_9$CONDIC_LOGRADO_2 == TRUE, "1",
                              ifelse(bd_9$CONDIC_LOGRADO_3 == TRUE, "1",
                                     ifelse(bd_9$CONDICAO_UNI_TPX == TRUE, "1",
                                            ifelse(bd_9$CONDICAO_UNI_TPY == TRUE, "1", NA))))))))



bd_9 %>%
  group_by(prisio) %>%
  summarize(n = n())


rm(bd_cnes_at, bd_cnes_not, bd_cond_1, bd_cond_2, bd_cond_3, bd_cond_4, bd_cond_5, bd_cond_6, bd_cond_7, bd_cond_8,
   bd_hans, bd_merge, bd_merge2, bd_merge3, hansen, hansen_2016, hansen_2017, hansen_2018, hansen_2019, hansen_2020,
   hansen_2021, join, join2, join3, join4, join5)



#Mudança dos valores NA da variável "prisio" para 0 (zero)
bd_9$prisio <- replace(x = bd_9$prisio, list = is.na(bd_9$prisio),
                       values = 0)



bd_10 <- bd_9 %>%
  filter(prisio == 1)



# Seleção de variáveis 
bd_10 <- select(bd_10, NU_NOTIFIC, DT_NOTIFIC, SG_UF_NOT, ID_MUNICIP,
                DT_DIAG, DT_NASC, NU_IDADE_N, CS_SEXO,
                CS_RACA, CS_ESCOL_N, SG_UF, ID_MN_RESI,
                CS_ZONA, NU_LESOES,
                FORMACLINI, AVALIA_N, CLASSOPERA, MODOENTR, MODODETECT, BACILOSCOP,
                DTINICTRAT, ESQ_INI_N, CONTREG, NERVOSAFET, UFATUAL, ID_MUNI_AT,
                DT_NOTI_AT, UFRESAT, MUNIRESAT, DTULTCOMP,
                CLASSATUAL, AVAL_ATU_N, ESQ_ATU_N, DOSE_RECEB, EPIS_RACIO, DTMUDESQ,
                CONTEXAM, DTALTA_N, TPALTA_N, ano_diag)



# Retirada dos erros de diagnóstico
bd_10$TPALTA_N <- replace(x = bd_10$TPALTA_N, list = is.na(bd_10$TPALTA_N),
                          values = 0)

bd_10 %>%
  group_by(TPALTA_N) %>%
  summarize(count = n())

bd_10 <- filter(bd_10, TPALTA_N != 8)



# Retirada das faixas etárias <18 anos
bd_11 <- bd_10 %>%
  filter(NU_IDADE_N >= 4018)




# Recorte para criação de um BD só com casos novos
bd_cn <- bd_11 %>%
  filter(MODOENTR == 1)



# Organização dos BD SISDEPEN ----------------------------------------------------------------

# Importando bancos de dados do SISDEPEN
depen_2016 <- read.csv2("D:\\TRABALHO\\Publicações\\Artigos\\hans_pop prisional\\BD\\base-infopen-jun-2016.csv", 
                        header = T, dec = ',', sep = ';', quote = '"')

depen_2017 <- read.csv2("D:\\TRABALHO\\Publicações\\Artigos\\hans_pop prisional\\BD\\base-infopen-jun-2017.csv", 
                        header = T, dec = ',', sep = ';', quote = '"')

depen_2018 <- read.csv2("D:\\TRABALHO\\Publicações\\Artigos\\hans_pop prisional\\BD\\base-infopen-jun-2018.csv", 
                        header = T, dec = ',', sep = ';', quote = '"')

depen_2019 <- read.csv2("D:\\TRABALHO\\Publicações\\Artigos\\hans_pop prisional\\BD\\base-infopen-jun-2019.csv", 
                        header = T, dec = ',', sep = ';', quote = '"')

depen_2020 <- read.csv2("D:\\TRABALHO\\Publicações\\Artigos\\hans_pop prisional\\BD\\base-infopen-jun-2020.csv", 
                        header = T, dec = ',', sep = ';', quote = '"')





# União dos bancos de dados SISDEPEN
djoin  <- rbind(depen_2016, depen_2017)
djoin2 <- rbind(djoin, depen_2018)
djoin3 <- rbind(djoin2, depen_2019)
djoin4 <- rbind(djoin3, depen_2020)

bd_depen <- djoin4



# Aqui é criada uma variável com os códigos das UFs
bd_depen$id_uf <- ifelse(
  bd_depen$uf == 'RO', '11', 
  ifelse(bd_depen$uf == 'AC', '12', 
         ifelse(bd_depen$uf == 'AM', '13', 
                ifelse(bd_depen$uf == 'RR', '14', 
                       ifelse(bd_depen$uf == 'PA', '15', 
                              ifelse(bd_depen$uf == 'AP', '16', 
                                     ifelse(bd_depen$uf == 'TO', '17', 
                                            ifelse(bd_depen$uf == 'MA', '21', 
                                                   ifelse(bd_depen$uf == 'PI', '22', 
                                                          ifelse(bd_depen$uf == 'CE', '23', 
                                                                 ifelse(bd_depen$uf == 'RN', '24', 
                                                                        ifelse(bd_depen$uf == 'PB', '25', 
                                                                               ifelse(bd_depen$uf == 'PE', '26',        
                                                                                      ifelse(bd_depen$uf == 'AL', '27',      
                                                                                             ifelse(bd_depen$uf == 'SE', '28',       
                                                                                                    ifelse(bd_depen$uf == 'BA', '29', 
                                                                                                           ifelse(bd_depen$uf == 'MG', '31', 
                                                                                                                  ifelse(bd_depen$uf == 'ES', '32', 
                                                                                                                         ifelse(bd_depen$uf == 'RJ', '33', 
                                                                                                                                ifelse(bd_depen$uf == 'SP', '35', 
                                                                                                                                       ifelse(bd_depen$uf == 'PR', '41', 
                                                                                                                                              ifelse(bd_depen$uf == 'SC', '42', 
                                                                                                                                                     ifelse(bd_depen$uf == 'RS', '43', 
                                                                                                                                                            ifelse(bd_depen$uf == 'MS', '50', 
                                                                                                                                                                   ifelse(bd_depen$uf == 'MT', '51', 
                                                                                                                                                                          ifelse(bd_depen$uf == 'GO', '52', 
                                                                                                                                                                                 ifelse(bd_depen$uf == 'DF', '53', 
                                                                                                                                                                                        NA)))))))))))))))))))))))))))


write_delim(bd_depen, "bd_depen.csv", delim = ";")


# Análise exploratória dos dados -------------------------------------------------------------

# Ano de diagnóstico
bd_cn %>%
  group_by(ano_diag) %>%
  summarize(n = n())

# Sexo
bd_cn %>%
  group_by(CS_SEXO) %>%
  summarize(n = n())



# Idade
summary(bd_cn$NU_IDADE_N)
boxplot(bd_cn$NU_IDADE_N)



# Raça/cor
bd_cn %>%
  group_by(CS_RACA) %>%
  summarize(n = n())


# Escolaridade
bd_cn %>%
  group_by(CS_ESCOL_N) %>%
  summarise(n = n())


# UF
bd_cn %>%
  group_by(SG_UF) %>%
  summarize(n = n()) %>%
  print(n = 1e3)


# Zona de residência
bd_cn %>%
  group_by(CS_ZONA) %>%
  summarize(n = n())


# lesões de pele
summary(bd_cn$NU_LESOES)
boxplot(bd_cn$NU_LESOES)


# Forma clínica
bd_cn %>%
  group_by(FORMACLINI) %>%
  summarize(n = n())


# GIF
bd_cn %>%
  group_by(AVALIA_N) %>%
  summarize(n = n())


# Classificação operacional
bd_cn %>%
  group_by(CLASSOPERA) %>%
  summarize(n = n())


# Modo de detecção
bd_cn %>%
  group_by(MODODETECT) %>%
  summarize(n = n())


# Baciloscopia
bd_cn %>%
  group_by(BACILOSCOP) %>%
  summarize(n = n())


# Esquema inicial de tratamento
bd_cn %>%
  group_by(ESQ_INI_N) %>%
  summarize(n = n())


# contatos registrados
summary(bd_cn$CONTREG)
boxplot(bd_cn$CONTREG)


# Nervos afetados
summary(bd_cn$NERVOSAFET)
boxplot(bd_cn$NERVOSAFET)


# GIF
bd_cn %>%
  group_by(AVALIA_N) %>%
  summarize(n = n())


# Esquema atual
bd_cn %>%
  group_by(ESQ_INI_N) %>%
  summarize(n = n())


# Contatos examinados
summary(bd_11$CONTEXAM)
boxplot(bd_11$CONTEXAM)




# Organização das variáveis ------------------------------------------

bd_h <- bd_cn

# SEXO ---------------------------------------------------------------
# 0 - Feminino; 1 - Masculino
bd_h <- mutate(bd_h, sexo = ifelse(CS_SEXO == "F", 0, 1))


## FAIXA ETÁRIA ------------------------------------------------
# Idade hans: 0 - 0 a 14 anos; 15 e mais

bd_h$DT_NASC <- as.Date(bd_h$DT_NASC, format = "%d/%m/%Y")
bd_h <- mutate(bd_h, ano_nasc = year(bd_h$DT_NASC))

#prisio$DTULTCOMP <- as.Date(prisio$DTULTCOMP, format = "%d/%m/%Y")
#prisio <- mutate(prisio, ulti_comp = year(prisio$DTULTCOMP))


# Calcula a a idade
dif.idade <- (round((difftime(bd_h$DT_DIAG, bd_h$DT_NASC,
                              units = 'days')/365.25), digits = 0))

bd_h["dif.idade"] <- dif.idade #Incorpora a variável dif.idade no BD


# idade_hans: 0 - 0 a 17 anos; 1 - 18 a 29; 2 - 30 a 59; 3 - 60+
bd_h <- bd_h %>%
  mutate(idade = case_when(dif.idade >= 60 ~ '2',
                           dif.idade >= 36  & dif.idade <= 59 ~ '1',
                           dif.idade >= 18 & dif.idade <= 35 ~ '0'))


## RAÇA/COR:  ----------------------------------------------------------


# raça: 0 - 1, 3 e 5 (não negros); 1 - 2 e 4 (negros)
bd_h$raca <- ifelse(
  bd_h$CS_RACA == 1, "0",
  ifelse(bd_h$CS_RACA == 2, "1",
         ifelse(bd_h$CS_RACA == 3, "0",
                ifelse(bd_h$CS_RACA == 4, "1",
                       ifelse(bd_h$CS_RACA == 5, "0", NA)))))


## ESCOLARIDADE  -----------------------------------------------------

# Escolaridade: 0 - 0 (analfabeto); 1 - 1, 2, 3 e 4 (fundamental); 2 - 5 e 6 (médio completo);
# 3 - 7 e 8 (superior)
bd_h$escol <- ifelse(
  bd_h$CS_ESCOL_N == 0, "0",
  ifelse(bd_h$CS_ESCOL_N == 1, "1",
         ifelse(bd_h$CS_ESCOL_N == 2, "1",
                ifelse(bd_h$CS_ESCOL_N == 3, "1",
                       ifelse(bd_h$CS_ESCOL_N == 4, "1",
                              ifelse(bd_h$CS_ESCOL_N == 5, "2",
                                     ifelse(bd_h$CS_ESCOL_N == 6, "2",
                                            ifelse(bd_h$CS_ESCOL_N == 7, "3",
                                                   ifelse(bd_h$CS_ESCOL_N == 8, "3",
                                                          NA)))))))))


## ZONA DE RESIDÊNCIA  ------------------------------------------------------

# Zona de residência: 0 - 1 (urbana); 1 - 2 (rural e periurbana)
bd_h$zona <- ifelse(bd_h$CS_ZONA == 1, "0",
                    ifelse(bd_h$CS_ZONA == 2, "1",
                           ifelse(bd_h$CS_ZONA == 3, "1", NA)))


## AVALIAÇÃO DO GIF -----------------------------------------------------------

# 0 - 0 GIF 0; 1-1 GIF 1; 2- 2 GIF 2; 3 - 3 Não avaliado
bd_h$gif_aval <- ifelse(
  bd_h$AVALIA_N == 0, '0',
  ifelse(bd_h$AVALIA_N == 1, '1',
         ifelse(bd_h$AVALIA_N == 2, '2',
                ifelse(bd_h$AVALIA_N == 3, '3', NA))))


## CLASSIFICAÇÃO OPERACIONAL -------------------------------------------------

# Classificação operacional: 0 - 1 (paucibacilar; 1 - 2 (multibacilar)
bd_h <- mutate(bd_h, classopera = ifelse(CLASSOPERA == 1, 0, 1))


## FORMA CLÍNICA ------------------------------------------------------------

# Forma clínica: 0- 1 (indeterminada); 1- 2 (tuberculóide); 2- 3 (dimorfa);
#               3- 4 (virchowiana); 4- 5 (não classificada)

bd_h$formaclini <- ifelse(
  bd_h$FORMACLINI == 1, '0',
  ifelse(bd_h$FORMACLINI == 2, '1',
         ifelse(bd_h$FORMACLINI == 3, '2',
                ifelse(bd_h$FORMACLINI == 4, '3',
                       ifelse(bd_h$FORMACLINI == 5, '4', NA)))))



## MODO DE DETECÇÃO -----------------------------------------------------

# Modo de detecção: 0- 1 (encaminhamento); 1- 2 (demanda espontânea);
#                    2- 3 (exame de coletividade); 3- 4 (exame de contatos);
#                    4- 5 (outros modos)
bd_h$detec <- ifelse(
  bd_h$MODODETECT == 1, '0',
  ifelse(bd_h$MODODETECT == 2, '1',
         ifelse(bd_h$MODODETECT == 3, '2',
                ifelse(bd_h$MODODETECT == 4, '3',
                       ifelse(bd_h$MODODETECT == 5, '4',
                              NA)))))


## BACILOSCOPIA --------------------------------------------------------

# Baciloscopia: 0- 1 (positiva); 1- 2 (negativa); 2- 3 (não realizada)
bd_h$bacilos <- ifelse(
  bd_h$BACILOSCOP == 1, '0',
  ifelse(bd_h$BACILOSCOP == 2, '1',
         ifelse(bd_h$BACILOSCOP == 3, '2',
                NA)))



# ESQUEMA TERAPÊUTICO ATUAL ------------------------------------------------

# Esquema terapêutico inicial: 0- 1 (PQT 6 doses); 1- 2 (PQT 12 doses);
# 2- 3 (outros esquemas substitutivos)
bd_h$esq_ini <- ifelse(
  bd_h$ESQ_INI_N == 1, '0',
  ifelse(bd_h$ESQ_INI_N == 2, '1',
         ifelse(bd_h$ESQ_INI_N == 3, '2',
                NA)))


# CARACTERIZAÇÃO -----------------------------------------------------------

library(data.table)
?dcast.data.table

bd_h2 <- data.table(bd_h)


# Sexo
dcast.data.table(bd_h2, sexo ~ ano_diag, value.var = 'sexo')


# Faixa etária
dcast.data.table(bd_h2, idade ~ ano_diag, value.var = 'idade')


# Raça/cor
dcast.data.table(bd_h2, raca ~ ano_diag, value.var = 'raca')


# Escolaridade
dcast.data.table(bd_h2, escol ~ ano_diag, value.var = 'escol')


# Zona de residência
dcast.data.table(bd_h2, zona ~ ano_diag, value.var = 'zona')


# GIF
dcast.data.table(bd_h2, gif_aval ~ ano_diag, value.var = 'gif_aval')


# Classificação operacional
dcast.data.table(bd_h2, classopera ~ ano_diag, value.var = 'classopera')


# Forma clínica
dcast.data.table(bd_h2, formaclini ~ ano_diag, value.var = 'formaclini')


# Modo de detecção
dcast.data.table(bd_h2, detec ~ ano_diag, value.var = 'detec')


# Baciloscopia
dcast.data.table(bd_h2, bacilos ~ ano_diag, value.var = 'bacilos')


# Esquema terapêutico atual
dcast.data.table(bd_h2, esq_ini ~ ano_diag, value.var = 'esq_atu')


# UF
dcast.data.table(bd_h2, SG_UF ~ ano_diag, value.var = 'SG_UF')





# Tabela de dados SISDEPEN por UF
bd_d <- data.table(bd_depen)

dcast.data.table(bd_d, pop_prisio ~ id_uf, value.var = 'id_uf')

dcast.data.table(bd_d, ano ~ pop_prisio, value.var = 'id_uf')


ftable(bd_d$pop_prisio, bd_d$ano, dnn = c("pop_prisio", "ano"))


?aggregate
