####################################
glimpse(bd)

# Variáveis sociodemográficas, clínicas e epidemiológicas
bd_tab <- bd %>% filter(MODOENTR == 1, ano_diag %in% c(2019, 2020)) %>% 
  select(prisio2,
         sexo, 
         raca, 
         fx_etaria, 
         escol,
         detec,
         bacilos,
         classop,
         gif_aval,
         formaclini,
         esq_ini,
         ano_diag)

map(bd_tab, ~ table(., bd_tab$prisio2))


##########################################################

# Tree Map
bd4 <- bd %>% 
  filter(MODOENTR == 1, prisio2 == "PPL", ano_diag %in% c(2019, 2020)) %>% 
  group_by(ufres) %>%
  summarize(n = n())


bd4$ufres2 <- ifelse(bd4$ufres == '11', 'RO',
                     ifelse(bd4$ufres == '12', 'AC',
                            ifelse(
                              bd4$ufres == '13', 'AM',
                              ifelse(bd4$ufres == '14',  'RR',
                                     ifelse(
                                       bd4$ufres == '15', 'PA',
                                       ifelse(bd4$ufres == '16',  'AP',
                                              ifelse(
                                                bd4$ufres == '17', 'TO',
                                                ifelse(bd4$ufres == '21', 'MA',
                                                       ifelse(
                                                         bd4$ufres == '22', 'PI',
                                                         ifelse(bd4$ufres ==
                                                                  '23', 'CE',
                                                                ifelse(
                                                                  bd4$ufres == '24', 'RN',
                                                                  ifelse(bd4$ufres ==
                                                                           '25', 'PB',
                                                                         ifelse(
                                                                           bd4$ufres == '26', 'PE',
                                                                           ifelse(bd4$ufres ==
                                                                                    '27', 'AL',
                                                                                  ifelse(
                                                                                    bd4$ufres == '28', 'SE',
                                                                                    ifelse(bd4$ufres == '29', 'BA',
                                                                                           ifelse(
                                                                                             bd4$ufres == '31', 'MG',
                                                                                             ifelse(bd4$ufres == '32', 'ES',
                                                                                                    ifelse(
                                                                                                      bd4$ufres == '33', 'RJ',
                                                                                                      ifelse(bd4$ufres == '35', 'SP',
                                                                                                             ifelse(
                                                                                                               bd4$ufres == '41', 'PR',
                                                                                                               ifelse(bd4$ufres == '42', 'SC',
                                                                                                                      ifelse(
                                                                                                                        bd4$ufres == '43', 'RS',
                                                                                                                        ifelse(bd4$ufres == '50', 'MS',
                                                                                                                               ifelse(
                                                                                                                                 bd4$ufres == '51', 'MT',
                                                                                                                                 ifelse(bd4$ufres == '52', 'GO',
                                                                                                                                        ifelse(bd4$ufres ==
                                                                                                                                                 '53',  'DF',
                                                                                                                                               NA))
                                                                                                                               ))
                                                                                                                      ))
                                                                                                             ))
                                                                                                    ))
                                                                                           ))
                                                                                  ))
                                                                         ))
                                                                ))
                                                       ))
                                              ))
                                     ))
                            )))



ggplot(bd4, aes(area = n, fill = n, label = ufres2)) +
  geom_treemap() +
  geom_treemap_text(colour = "grey10",
                    place = "centre",
                    size = 15) +
  scale_fill_continuous_diverging()



#################################################

bd3 <- bd %>% 
  filter(MODOENTR == 1, prisio2 == "PPL", ano_diag %in% c(2019, 2020)) %>% 
  group_by(ufres, ano_diag) %>%
  summarize(n = n())


bd3$ano_diag <- factor(bd3$ano_diag, levels = c('2019', '2020'), ordered = T)
glimpse(bd3)
class(bd3)
class(bd3$ufres2)
bd3 <- as.data.frame(bd3)

bd3$ufres2 <- ifelse(bd3$ufres == '11', 'Rondônia',
                     ifelse(bd3$ufres == '12', 'Acre',
                            ifelse(
                              bd3$ufres == '13', 'Amazonas',
                              ifelse(bd3$ufres == '14',  'Roraima',
                                     ifelse(
                                       bd3$ufres == '15', 'Pará',
                                       ifelse(bd3$ufres == '16',  'Amapá',
                                              ifelse(
                                                bd3$ufres == '17', 'Tocantins',
                                                ifelse(bd3$ufres == '21', 'Maranhão',
                                                       ifelse(
                                                         bd3$ufres == '22', 'Piauí',
                                                         ifelse(bd3$ufres ==
                                                                  '23', 'Ceará',
                                                                ifelse(
                                                                  bd3$ufres == '24', 'Rio Grande do Norte',
                                                                  ifelse(bd3$ufres ==
                                                                           '25', 'Paraíba',
                                                                         ifelse(
                                                                           bd3$ufres == '26', 'Pernambuco',
                                                                           ifelse(bd3$ufres ==
                                                                                    '27', 'Alagoas',
                                                                                  ifelse(
                                                                                    bd3$ufres == '28', 'Ssergipe',
                                                                                    ifelse(bd3$ufres == '29', 'Bahia',
                                                                                           ifelse(
                                                                                             bd3$ufres == '31', 'Minas Gerais',
                                                                                             ifelse(bd3$ufres == '32', 'Espírito Santo',
                                                                                                    ifelse(
                                                                                                      bd3$ufres == '33', 'Rio de Janeiro',
                                                                                                      ifelse(bd3$ufres == '35', 'São Paulo',
                                                                                                             ifelse(
                                                                                                               bd3$ufres == '41', 'Paraná',
                                                                                                               ifelse(bd3$ufres == '42', 'Santa Catarina',
                                                                                                                      ifelse(
                                                                                                                        bd3$ufres == '43', 'Rio Grande do Sul',
                                                                                                                        ifelse(bd3$ufres == '50', 'Mato Grosso do Sul',
                                                                                                                               ifelse(
                                                                                                                                 bd3$ufres == '51', 'Mato Grosso',
                                                                                                                                 ifelse(bd3$ufres == '52', 'Goiás',
                                                                                                                                        ifelse(bd3$ufres ==
                                                                                                                                                 '53',  'Distrito Federal',
                                                                                                                                               NA))
                                                                                                                               ))
                                                                                                                      ))
                                                                                                             ))
                                                                                                    ))
                                                                                           ))
                                                                                  ))
                                                                         ))
                                                                ))
                                                       ))
                                              ))
                                     ))
                            )))

bd3

cols <- c("Rondônia" = "gray", 
          "Pará" = "royalblue",
          "Tocantins" = "gray", 
          "Maranhão" = "firebrick",
          "Piauí" = "gray", 
          "Pernambuco" = "gray",
          "Minas Gerais" = "gray", 
          "Espírito Santo" = "firebrick",
          "Rio de Janeiro" = "gray",
          "São Paulo" = "firebrick",
          "Paraná" = "gray",
          "Mato Grosso do Sul" = "gray",
          "Mato Grosso" = "firebrick",
          "Goiás" = "gray"
          )


newggslopegraph(dataframe = bd3,
                Times = ano_diag,
                Measurement =  n,
                Grouping = ufres2,
                Title = "",
                SubTitle = "",
                Caption = "Fonte: Sinan/SVS/MS.",
                CaptionJustify = "left",
                XTextSize = 24,    # Size of the times
                YTextSize = 5,     # Size of the groups
                LineThickness = 1.5,
                LineColor = cols,
                DataLabelPadding = 0.5,
                DataTextSize = 5,
                #DataLabelLineSize = 0.1
                )


################################################################

bd %>% 
  filter(prisio == 1, !is.na(gif_aval), !is.na(detec)) %>% 
  ggplot(aes(x = detec)) +
  geom_bar(aes(fill = gif_aval)) +
  coord_flip() +
  facet_wrap( ~ gif_aval) +
  scale_fill_manual(values = c("grey", "sandybrown", "firebrick", "grey")) +
  theme_classic() +
  labs(x = "Frequência absoluta", y = "Modo de detecção", fill = "GIF avaliado")





#################################################

bd$not_time <- bd$DT_NOTIFIC - bd$DT_DIAG

class(bd$not_time)
bd$not_time <- as.numeric(as.character(as.factor(bd$not_time)))

bd$ano_diag <- as.character(as.factor(bd$ano_diag))

bd %>% 
  filter(prisio == 1) %>% 
  ggplot(aes(x = ano_diag, y = not_time)) +
  geom_boxplot(fill = "firebrick") +
  theme_classic() +
  labs(x = "Ano", y = "Dias")



#####################################################

bd$trat_time <- bd$DTINICTRAT - bd$DT_DIAG

class(bd$trat_time)
bd$trat_time <- as.numeric(as.character(as.factor(bd$trat_time)))

bd$ano_diag <- as.character(as.factor(bd$ano_diag))

bd %>% 
  filter(prisio == 1) %>% 
  ggplot(aes(x = ano_diag, y = trat_time)) +
  geom_boxplot(fill = "firebrick") +
  theme_classic() +
  labs(x = "Ano", y = "Dias")



############################################################

class(bd$classop)
class(bd$esq_ini)


bd %>% 
  filter(prisio == 1, !is.na(classop)) %>% 
  ggplot(aes(x = ano_diag)) +
  geom_bar(aes(fill = classop)) +
  facet_wrap(~ classop) +
  scale_fill_manual(values = c("firebrick", "grey")) +
  theme_classic() +
  labs(x = "Ano", y = "Frequência absoluta", fill = "Classificação \n operacional")


##############################################################
bd %>% 
  filter(prisio == 1, !is.na(bacilos)) %>% 
  ggplot(aes(x = classop)) +
  geom_bar(aes(fill = bacilos)) +
  facet_wrap(~ bacilos) +
  scale_fill_manual(values = c("firebrick", "grey", "grey")) +
  theme_classic() +
  labs(x = "Classificação operacional", y = "Frequência absoluta", fill = "Baciloscopia")



###############################################################
bd %>% 
  filter(prisio == 1, !is.na(gif_aval)) %>% 
  ggplot() +
  geom_point(mapping = aes(x = NU_LESOES, y = NERVOSAFET, colour = gif_aval), size = 3) +
  ylim(0, 8) +
  xlim(0, 60) +
  scale_colour_manual(values = c("royalblue", "orange", "firebrick", "grey")) +
  theme_classic() +
  labs(x = "Lesões cutâneas (n)", y = "Nervos afetados (n)", colour = "GIF avaliado")


?position_jitter


###############################################################

bd %>% 
  filter(prisio == 1) %>% 
  ggplot() +
  geom_bar(aes(CONTREG))


