# Criando o banco para o trabalho em aula

library(dplyr)
library(janitor)
library(data.table)
library(openxlsx)

# Importando os arquivos do IMP que foram baixados e já selecionando o último ano de cada levantamento

setwd("~/R-Projects/ERPAD_TERA/arq_originais")

arq <- list.files()
n_arq <- gsub(".csv", "", arq)

for(i in 1:length(arq)){
  
  a <- data.table::fread(arq[i]) %>%                      
    clean_names() %>%
    filter(periodos == max(periodos)) 
  
  assign(n_arq[i], a)
  
  rm(a)
  
}


# Uma dificuldade dessas tabelas é que as vezes elas tratam de períodos diferentes. 
# Por exemplo, o IDH, como é uma variável calculada a partir do Censo, é relativa à 2010
# Apesar de que o IDH não costuma ter uma variação tão grande em um período de dez anos, o efeito
# dessa variável na nossa escolha final vai estar diluído pelas outras variáveis que já são mais recentes
# Essa é a grande vantagem de trabalhar com dados estaduais, eles costumam ser mais atuais que os nacionais.

# Juntando os bancos:

banco_imp <- imp_IDHs %>%
  rename(idh_2010 = indice_de_desenvolvimento_humano_municipal_idhm) %>%
  select(idh_2010, localidades, cod_ibge) %>%
  left_join(imp_matricula_educacao_basica) %>%      # todos os bancos têm as variaveis com o mesmo nome 
  select(-c(periodos)) %>%
  left_join(imp_populacao_grupos_idades) %>%
  select(-c(periodos)) %>%
  left_join(imp_populacao_situacao_domicilio) %>%
  select(-c(periodos)) %>%
  left_join(imp_renda_per_capta) %>%
  rename(renda_per_capta_2010 = renda_per_capita_censo_demografico_em_reais_correntes) %>%
  select(-c(periodos, domicilios_particulares_com_renda_per_capita_ate_1_2_salario_minimo_censo_demografico_em_percent,
            domicilios_particulares_com_renda_per_capita_ate_1_4_do_salario_minimo_censo_demografico_em_percent)) %>%
  left_join(imp_taxa_crescimento_anual_populacao) %>%
  select(-c(taxa_geometrica_de_crescimento_anual_da_populacao_2000_2010_em_percent_a_a,
            taxa_geometrica_de_crescimento_anual_da_populacao_1991_2000_em_percent_a_a,
            taxa_geometrica_de_crescimento_anual_da_populacao_1980_1991_em_percent_a_a,
            periodos)) %>%
  rename(cresc_perc_medio_2010_2019 = taxa_geometrica_de_crescimento_anual_da_populacao_2010_2019_em_percent_a_a)
  
#Vou transformar as colunas que deveriam ser numéricas em numéricas:

banco_imp[] <- lapply(banco_imp, gsub, pattern=',', replacement=".")

cols <- names(banco_imp)
col.car <- c("localidades", "cod_ibge")
cols.num <- cols[!cols %in% col.car]
  
banco_imp[cols.num] <- sapply(banco_imp[cols.num],as.numeric)




