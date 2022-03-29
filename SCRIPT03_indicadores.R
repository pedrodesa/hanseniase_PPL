##############################################

# Base de dados Infopen
ppl <- read.csv2(file = "depen.csv", sep = ";", header = T)

ppl <- ppl[, c(12,436)]
ppl <- ppl %>% rename(ibge = 1,
                      pop_pris = 2)

pop_pl19 <- sum(ppl$pop_pris)
pop_pl20 <- 667541

### - Caso novo geral- ###
hans_cn <- function(data, ano = ano) {
  num_cn <- data %>% 
    filter(MODOENTR == 1, ano_diag == ano) %>% 
    group_by(ufres) %>%
    summarize(caso_novo = n())
  
  return(num_cn)
}

# Taxa
cn19 <- hans_cn(bd, ano = 2019)
taxa19 <- round((sum(cn19$caso_novo) / sum(ppl$pop_pris)) * 100000, 1)

cn20 <- hans_cn(bd, ano = 2020)
taxa20 <- round((sum(cn20$caso_novo) / pop_pl20) * 100000, 1)

taxas <- rbind(taxa19, taxa20)
taxas <- as.data.frame(taxas)
taxas$ano <- c(2019, 2020)
taxas <- taxas %>% rename(taxa = 1)

### - Casos novos GIF avaliado - ###
gifaval_cn <- function(data, ano = ano) {
  num_gif2 <- bd %>% 
    filter(MODOENTR == 1, ano_diag == ano, AVALIA_N %in% c(0,1,2)) %>% 
    group_by(ufres) %>%
    summarize(gif2 = n())
  
  return(num_gif2)
}

# prop avaliados
aval19 <- gifaval_cn(bd, ano = 2019)
aval_19 <- round(sum(aval19$gif2) / sum(cn19$caso_novo) * 100, 1)

aval20 <- gifaval_cn(bd, ano = 2020)
aval_20 <- round(sum(aval20$gif2) / sum(cn20$caso_novo) * 100, 1)


### - Casos novos GIF 2 - ###
gif2_cn <- function(data, ano = ano) {
num_gif2 <- bd %>% 
  filter(MODOENTR == 1, ano_diag == ano, AVALIA_N == 2) %>% 
  group_by(ufres) %>%
  summarize(gif2 = n())

return(num_gif2)
}

# Prop GIF 2
gif2_19 <- gif2_cn(bd, ano = 2019)
grau_19 <- round(sum(gif2_19$gif2) / sum(aval19$gif2) * 100, 1)

gif2_20 <- gif2_cn(bd, ano = 2020)
grau_20 <- round(sum(gif2_20$gif2) / sum(aval20$gif2) * 100, 1)


### - Proporção de contatos examinados - ###
prop19 <- prop_contatos(bd, anoPB = 2018, anoMB = 2017)
prop19$ufresat <- as.character(prop19$ufresat)
arrange(prop19, ufresat)

prop20 <- prop_contatos(bd, anoPB = 2019, anoMB = 2018)
prop20$ufresat <- as.character(prop20$ufresat)
arrange(prop20, ufresat)

# prop contatos examinados
prop19$cont_reg[prop19$cont_reg == 3115] <- 6

cont_19 <- round((sum(prop19$cont_exam) / sum(prop19$cont_reg)) * 100, 1)

cont_20 <- round((sum(prop20$cont_exam) / sum(prop20$cont_reg)) * 100, 1)


### - proporção de cura - ###
bd <- bd %>% arrange(ufresat)

bd[372, 71] <- 26
bd[373, 71] <- 51
bd[374, 71] <- 12

cura2019 <- prop_cura_hans(bd, anoPB = 2018, anoMB = 2017)
cura2020 <- prop_cura_hans(bd, anoPB = 2019, anoMB = 2018)

cura_19 <- round((sum(cura2019$cura) / sum(cura2019$saidas)) * 100, 1)
cura_20 <- round((sum(cura2020$cura) / sum(cura2020$saidas)) * 100, 1)


#############################################################

# Taxa - gráfico de barras
taxas$ano <- as.character(taxas$ano)

taxas %>% 
  ggplot(aes(x = ano, y = taxa, fill = ano)) +
  geom_col() +
  ylim(0, 20) +
  labs(x = "Ano", y = "Taxa de casos novos/100 mil") +
  scale_fill_manual("Ano", values = c("royalblue", "firebrick")) +
  theme_classic()



###############################################################
teste <- rbind(aval_19, aval_20, grau_19, grau_20, cont_19, cont_20, cura_19, cura_20)
teste <- as.data.frame(teste)
teste <- teste %>% rename(prop = 1)
nome <- c("prop. grau avaliado", "prop. grau avaliado", "prop. GIF2", "prop. GIF2", "prop. contatos", "prop. contatos",
          "prop. cura", "prop. cura")

ano <- c("2019", "2020","2019", "2020","2019", "2020","2019", "2020")
prop <- teste$prop

teste2 <- cbind(nome, ano, prop)
class(teste2)
teste2 <- as.data.frame(teste2)
class(teste2$prop)
teste2$prop <- as.numeric(teste2$prop)


cols2 <- c("prop. grau avaliado" = "tan1", 
          "prop. GIF2" = "firebrick1",
          "prop. contatos" = "firebrick", 
          "prop. cura" = "salmon"
)


newggslopegraph(dataframe = teste2,
                Times = ano,
                Measurement =  prop,
                Grouping = nome,
                Title = "",
                SubTitle = "",
                Caption = "Fonte: Sinan/SVS/MS.",
                CaptionJustify = "left",
                XTextSize = 24,    # Size of the times
                YTextSize = 5,     # Size of the groups
                LineThickness = 2,
                LineColor = cols2,
                DataLabelPadding = 0.5,
                DataTextSize = 5,
                #DataLabelLineSize = 0.1,
                
)
?newggslopegraph
