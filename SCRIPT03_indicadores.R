##############################################

### - Caso novo geral- ###
hans_cn <- function(data, ano = ano) {
  num_cn <- data %>% 
    filter(MODOENTR == 1, ano_diag == ano) %>% 
    group_by(ufres) %>%
    summarize(caso_novo = n())
  
  return(num_cn)
}

hans_cn(bd, ano = 2019)
hans_cn(bd, ano = 2020)


### - Casos novos GIF avaliado - ###
gifaval_cn <- function(data, ano = ano) {
  num_gif2 <- bd %>% 
    filter(MODOENTR == 1, ano_diag == ano, AVALIA_N %in% c(0,1,2)) %>% 
    group_by(ufres) %>%
    summarize(gif2 = n())
  
  return(num_gif2)
}

gifaval_cn(bd, ano = 2019)
gifaval_cn(bd, ano = 2020)



### - Casos novos GIF 2 - ###
gif2_cn <- function(data, ano = ano) {
num_gif2 <- bd %>% 
  filter(MODOENTR == 1, ano_diag == ano, AVALIA_N == 2) %>% 
  group_by(ufres) %>%
  summarize(gif2 = n())

return(num_gif2)
}

gif2_cn(bd, ano = 2019)
gif2_cn(bd, ano = 2020)


### - Proporção de contatos examinados - ###
prop19 <- prop_contatos(bd, anoPB = 2018, anoMB = 2017)
prop19$ufresat <- as.character(prop19$ufresat)
arrange(prop19, ufresat)

prop20 <- prop_contatos(bd, anoPB = 2019, anoMB = 2018)
prop20$ufresat <- as.character(prop20$ufresat)
arrange(prop20, ufresat)


### - proporção de cura - ###
cura2019 <- prop_cura_hans(bd, anoPB = 2018, anoMB = 2017)
cura2020 <- prop_cura_hans(bd, anoPB = 2019, anoMB = 2018)
