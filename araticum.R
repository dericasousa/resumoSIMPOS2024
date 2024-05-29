
#### 1. Diretório ####

getwd()
setwd("D:/")  
#setwd('E:/relatorio')
#setwd("~/dropbox_local/derica")

# 1.1. Pacotes
require(gridExtra)
require(ggplot2)
require(dplyr)
require(tidyr)
require(readr)
require(car)
require(multcomp)


#### 2. Dados ####

df <- read_csv(file = 'araticum.csv')
View(df)
dim(df)
head(df)


#### 3. Análise exploratória  ####


# Resumo estatístico

summary(df)

# agrupa por tratamento


df_grupo <- group_by(df,Tratamento)
View(df_grupo)

#calcula média, mediana e desvio padrão de altura e DAC por adubo, 

df_sumario <- summarise(df_grupo, Altura_med = mean(Altura), Altura_median = median(Altura), 
          sd_Altura = sd(Altura),
          DAC_med = mean(DAC), DAC_median = median(DAC), 
          sd_DAC = sd(DAC))  


View(df_sumario)

df$Tratamento <- as.factor(df$Tratamento)

# Teste de Levene (homogeneidade)

?leveneTest
levene_altura <- leveneTest(Altura ~ Tratamento, data = df)
levene_dac <- leveneTest(DAC ~ Tratamento, data = df)

print(levene_altura)
print(levene_dac)


# Teste de Shapiro-Wilk 
?shapiro.test
shapiro_altura <- shapiro.test(df$Altura)
shapiro_dac <- shapiro.test(df$DAC)

print(shapiro_altura)
print(shapiro_dac)



#### 4. Distribuição das variáveis numéricas ####

# Função
create_histogram <- function(data, x, title, fill_color) {
  ggplot(data, aes_string(x = x)) +
    geom_histogram(binwidth = 5, fill = fill_color, color = "black") +
    geom_vline(aes(xintercept = mean(get(x))), color = "blue", linetype = "dashed") +  
    labs(title = title)
}

# Histogramas para cada variável
hist_altura <- create_histogram(df_grupo, "Altura", "Distribuição das Alturas", "skyblue")
#hist_dac <- create_histogram(df, "DAC", "Distribuição dos DAC", "salmon")

grid.arrange(hist_altura, ncol = 2, nrow = 1)


df$Tratamento <- as.factor(df$Tratamento)



#### 5. Normalidade dos dados #### 

# Teste de Shapiro-Wilk para normalidade
shapiro_altura <- shapiro.test(df$Altura)
shapiro_dac <- shapiro.test(df$DAC)

print(shapiro_altura)




# Para homogeneidade das variâncias

# Teste de Levene 
levene_altura <- leveneTest(Altura ~ Tratamento, data = df)
levene_dac <- leveneTest(DAC ~ Tratamento, data = df)
print(levene_altura)
print(levene_dac)



#### 6. Visualização boxplots ####


# Função
create_boxplot_vertical <- function(data, y, title, fill_color) {
  ggplot(data, aes_string(y=y)) +
    geom_boxplot(fill = fill_color, color = "black") +
    labs(title = title) +
    theme(legend.position = "right")
}

# Boxplot para cada variável
box_altura_vertical <- create_boxplot_vertical(df_grupo, "Altura", "Distribuição das Alturas", "skyblue")
box_dac_vertical <- create_boxplot_vertical(df_grupo, "DAC", "Distribuição de DAC", "salmon")

grid.arrange(box_altura_vertical, box_dac_vertical, ncol = 2)








#### 7. Relação entre adubação e altura média ####

ggplot(df_sumario, aes(x = Tratamento, y = Altura_med)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(x = "Tratamento (Adubo)", y = "Altura Média", 
       title = "Relação entre o tipo de adubação e a altura média")

#### 8. Relação entre adubação e DAC média ####

ggplot(df_sumario, aes(x = Tratamento, y = DAC_med)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue") +
  labs(x = "Tratamento (Adubo)", y = "DAC Média", 
       title = "Relação entre o tipo de adubação e o DAC médio")


#### 9. Altura #### 

#análise de variancia para a altura


anova_altura <- aov(Altura ~ Tratamento, data = df_grupo)
summary(anova_altura)

# Teste de Tukey para ANOVA de altura
tukey_altura <- TukeyHSD(anova_altura)
tukey_altura

# Kruskal-Wallis para altura


kruskal.test(Altura~Tratamento, data = df)
pairwise.wilcox.test(df$Altura,
                     df$Tratamento,
                     p.adjust.method="bonferroni")

# Teste de Wilcoxon para altura
pairwise_wilcox_altura <- pairwise.wilcox.test(df$Altura, df$Tratamento, p.adjust.method = "bonferroni")
pairwise_wilcox_altura

#### 10. DAC ####

# ANOVA para DAC
anova_dac <- aov(DAC ~ Tratamento, data = df)
summary(anova_dac)

# Teste de Tukey para ANOVA de DAC
tukey_dac <- TukeyHSD(anova_dac)
tukey_dac


#Teste para a DAC

kruskal.test(DAC_med~Tratamento, data = df_sumario)
pairwise.wilcox.test(df_sumario$DAC_med,
                     df_sumario$Tratamento,
                     p.adjust.method="bonferroni")


kruskal.test(DAC_median~Tratamento, data = df_sumario)
pairwise.wilcox.test(df_sumario$DAC_med,
                     df_sumario$Tratamento,
                     p.adjust.method="bonferroni")


# Teste de Wilcoxon para DAC
pairwise_wilcox_dac <- pairwise.wilcox.test(df$DAC, df$Tratamento, p.adjust.method = "bonferroni")
pairwise_wilcox_dac

#### 11. Independência dos Erros para ANOVA da variável paramétrica ####

#install.packages("lmtest")
library(lmtest)
?dwtest
ind=dwtest(anova_altura)
ind

residuos_anova_altura <- residuals(anova_altura)

plot(residuos_anova_altura, 
     col="blue", las=1, 
     pch=16, ylab="Resíduos brutos", xlab="Índice")
abline(h=0, col="red")


#### 12. Teste de Dunn ####
install.packages('FSA')
library(FSA)
dunnTest(DAC ~ Tratamento, data = df, method = 'bonferroni')



# Teste de Kruskal 2

kruskal_test <- kruskal.test(DAC ~ Tratamento, data = df)
print(kruskal_test)


#### 13. Transformação dos dados em escala logarítimica ####

?log


# nova coluna com a variável DAC transformada (Transformação Logarítimica)

df$log_DAC <- log(df$DAC)
View(df)

# em escala 

# teste de normalidade e homogeneidade

shapiro_log_dac <- shapiro.test(df$log_DAC)
levene_log_dac <- leveneTest(log_DAC ~ Tratamento, data = df)

shapiro_log_dac
levene_log_dac

# teste de kruskal-wallis para log_DAC

kruskal.test(log_DAC~Tratamento, data = df)
pairwise.wilcox.test(df$log_DAC,
                     df$Tratamento,
                     p.adjust.method="bonferroni")








#### 14. Transformação raiz quadrada ####


df$sqrt_DAC <- sqrt(df$DAC)

# normalidade e homogeneidade dos dados transformados
shapiro_sqrt_dac <- shapiro.test(df$sqrt_DAC)
levene_sqrt_dac <- leveneTest(sqrt_DAC ~ Tratamento, data = df)

shapiro_sqrt_dac
levene_sqrt_dac

kruskal.test(sqrt_DAC~Tratamento, data = df)
pairwise.wilcox.test(df$sqrt_DAC,
                     df$Tratamento,
                     p.adjust.method="bonferroni")


