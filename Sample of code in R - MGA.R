###==============================================###
###               R Sample of code               ###
###        From Big Data Final Assignment        ###
###==============================================###

# Author: Matías Güizzo Altube
# Date: December 2020

library(glmnet)
library(pdftools)
library(tidyverse)
library(readxl)
library(ggplot2)

# 1. Dataset preparation --------------------------------------------------

# 1.1. List of law names --------------------------------------------------
setwd("~/Licenciatura en Economía/Semestre de Primavera 2020/Big Data/TP Final")
lista_leyes <- read_xlsx('lista_leyes.xlsx')
archivos <- lista_leyes$law_name
aprobados <- lista_leyes$law_approved
lista_2016 <- read_xlsx('lista_2016.xlsx')
archivos_2016 <- lista_2016$law_name
aprobados_2016 <- lista_2016$law_approved

# 1.2. Variables (words) definition ---------------------------------------
setwd("~/Licenciatura en Economía/Semestre de Primavera 2020/Big Data/TP Final/Proyectos")
leyes <- list()
for(i in 1:length(archivos)){
  ley <- pdf_text(paste(archivos[i],'.pdf', sep = ''))
  ley <- paste(ley, collapse='')
  ley <- str_replace_all(ley, '\n', ' ')
  ley <- ley %>% str_remove_all('\\.') %>% str_remove_all(',') %>% str_remove_all('“') %>% str_remove_all('”') %>% 
    str_remove_all('-') %>% str_remove_all('\\(') %>% str_remove_all('\\)') %>% str_remove_all(':') %>% str_remove_all('–')
  ley <- tolower(ley)
  palabras <- as.factor(str_split(ley, " ", simplify = T))
  lista <- levels(palabras)
  cantidad <- vector()
  for(p in 1:length(lista)){
    cantidad[p] <- sum(palabras == lista[p])
  }
  names(cantidad) <- lista
  df <- as.data.frame(t(cantidad))
  df$law_name <- archivos[i]
  df$law_approved <- aprobados[i]
  leyes[[i]] <- df
}

base <- data.frame()
for(j in 1:length(leyes)){
  base <- base %>% bind_rows(leyes[[j]])
}
base[is.na(base)] <- 0

# 1.3. Summary Statistics -------------------------------------------------
freqs <- vector(length = ncol(base))
names(freqs) <- names(base)
freqs_apr <- freqs
freqs_rej <- freqs
proj_apr <- dplyr::filter(base, law_approved == 1)
proj_rej <- dplyr::filter(base, law_approved == 0)
for(col in 1:length(base)){
  freqs[col] <- mean(base[,col])
  freqs_apr[col] <- mean(proj_apr[,col])
  freqs_rej[col] <- mean(proj_rej[,col])
}
sort(freqs, decreasing = T)[21:60]
sort(freqs_apr, decreasing = T)[1:40]
sort(freqs_rej, decreasing = T)[1:40]

# 1.4. 2016 dataset (external test dataset) -------------------------------
setwd("~/Semestre de Primavera 2020/Big Data/TP Final/Proyectos 2016")
leyes_2016 <- list()
for(i in 1:length(archivos_2016)){
  ley <- pdf_text(paste(archivos_2016[i],'.pdf', sep = ''))
  ley <- paste(ley, collapse='')
  ley <- str_replace_all(ley, '\n', ' ')
  ley <- ley %>% str_remove_all('\\.') %>% str_remove_all(',') %>% str_remove_all('“') %>% str_remove_all('”') %>% 
    str_remove_all('-') %>% str_remove_all('\\(') %>% str_remove_all('\\)') %>% str_remove_all(':') %>% str_remove_all('–')
  ley <- tolower(ley)
  palabras <- as.factor(str_split(ley, " ", simplify = T))
  lista <- levels(palabras)
  cantidad <- vector()
  for(p in 1:length(lista)){
    cantidad[p] <- sum(palabras == lista[p])
  }
  names(cantidad) <- lista
  df <- as.data.frame(t(cantidad))
  df$law_name <- archivos_2016[i]
  df$law_approved <- aprobados_2016[i]
  leyes_2016[[i]] <- df
}

base_2016 <- data.frame()
for(j in 1:length(leyes_2016)){
  base_2016 <- base_2016 %>% bind_rows(leyes_2016[[j]])
}
base_2016[is.na(base_2016)] <- 0
variables_base <- names(base)
vars_presentes <- intersect(variables_base, names(base_2016))
vars_faltantes <- setdiff(variables_base, names(base_2016))
base_2016 <- base_2016 %>% select(all_of(vars_presentes))
for(fal in vars_faltantes){base_2016[,fal] <- 0}

# 2. Prediction model -----------------------------------------------------

# 2.1. Training and test datasets -----------------------------------------
options(expressions = 5e5)
x <- as.matrix(select(base, -law_name, - law_approved))
y <- base$law_approved

set.seed(23)
train <- sample(1:nrow(x), 0.8*nrow(x))
test <- -train
x_train <- x[train,]
y_train <- y[train]
x_test <- x[test,]
y_test <- y[test]

# 2.2. Alpha and lambda selection with Cross-Validation ------------------
folds <- sample(rep(1:10,10), nrow(x_train))
alpha_grid <- seq(0,1,l=21)

cv_error_alphas <- vector(length = length(alpha_grid))
for(a in 1:length(alpha_grid)){
  mod_a <- cv.glmnet(x_train, y_train, family = 'binomial', alpha = alpha_grid[a], foldid = folds)
  cv_error_alphas[a] <- min(mod_a$cvm)
}

# Alpha selection figure (first approach)
setwd('~/Semestre de Primavera 2020/Big Data/TP Final/Figuras')
data.frame(x = alpha_grid, y = cv_error_alphas) %>%
  ggplot(aes(x,y)) + geom_point(col = '#2164AD') + geom_line(col = '#2164AD', linetype = 'dashed') +
  labs(title = '(a) Elección de \u03b1 por cv', x = '\u03b1', y = 'Error CV') + 
  theme_minimal() + theme(title = element_text(size=13), plot.title = element_text(hjust = 0.5))
ggsave('Fig1a.jpeg', width = 6, height = 4.125)

# Expanded grid for higher accuracy
alpha_grid_extra <- seq(0,.2,l=21)
cv_error_alphas_extra <- vector(length = length(alpha_grid_extra))
for(a in 1:length(alpha_grid_extra)){
  mod_a <- cv.glmnet(x_train, y_train, family = 'binomial', alpha = alpha_grid_extra[a], foldid = folds)
  cv_error_alphas_extra[a] <- min(mod_a$cvm)
}

alpha_grid_tot <- c(alpha_grid_extra, alpha_grid[6:21])
cv_error_tot <- c(cv_error_alphas_extra, cv_error_alphas[6:21])

# Alpha selection figure (higher accuracy)
data.frame(x = alpha_grid_tot, y = cv_error_tot) %>%
  ggplot(aes(x,y)) + geom_point(col = '#2164AD') + geom_line(col = '#2164AD', linetype = 'dashed') +
  labs(title = '(a) Elección de \u03b1 por cv', x = '\u03b1', y = 'Error CV') + 
  theme_minimal() + theme(title = element_text(size=13), plot.title = element_text(hjust = 0.5))
ggsave('Fig1a.jpeg', width = 6, height = 4.125)

bestalpha <- alpha_grid_tot[cv_error_tot == min(cv_error_tot)]

# Lambda selection
modelo_cv <- cv.glmnet(x_train, y_train, family = 'binomial', alpha = bestalpha)

data.frame(x = modelo_cv$lambda, y = modelo_cv$cvm) %>%
  ggplot(aes(x,y)) + geom_point(col = '#2164AD') + geom_line(col = '#2164AD', linetype = 'dashed') +
  labs(title = '(b) Elección de \u03BB por cv', x = '\u03BB', y = 'Error CV') + 
  theme_minimal() + theme(title = element_text(size=13), plot.title = element_text(hjust = 0.5))
ggsave('Fig1b.jpeg', width = 6, height = 4.125)

bestlam <- modelo_cv$lambda.min

# 2.3. Relevant words identification --------------------------------------
coefs <- predict(modelo_cv, s = bestlam, type="coefficients")
coefs <- coefs[1:length(coefs),]
relevantes <- coefs[coefs != 0]
sort(relevantes[relevantes>0], decreasing = T)[1:21]
sort(relevantes[relevantes<0])[1:23]
betas <- sort(relevantes)[c(1:10, (length(relevantes)-10):(length(relevantes)-1))]
data.frame(Palabra = factor(names(betas), level = names(betas)), Coeficiente = betas, up = betas, low = 0) %>%
  ggplot(aes(Palabra, Coeficiente)) + geom_errorbar(aes(ymax = up, ymin = low), width = 0, col = 'grey') + 
  geom_point(col = '#F68D70') + labs(title = '20 coeficientes de mayor magnitud') +
  theme_minimal() + theme(title = element_text(size=13), plot.title = element_text(hjust = 0.5),
                          panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
                          panel.grid.minor.x = element_blank()) +
  geom_hline(yintercept = 0, col = '#2164AD') + coord_flip()
ggsave('Fig5.jpeg', width = 6, height = 4.125)

# 3. Evaluation of the model's predictive power ---------------------------

x_2016 <- as.matrix(select(base_2016, -law_name, - law_approved))
y_2016 <- base_2016$law_approved

# MSE in each sample
en_pred_in <- predict(modelo_cv, s = bestlam, newx = x_train, type = 'response')
MSE_in <- mean((en_pred_in - y_train)^2)
en_pred_out <- predict(modelo_cv, s = bestlam, newx = x_test, type = 'response')
MSE_out <- mean((en_pred_out - y_test)^2)
en_pred_2016 <- predict(modelo_cv, s = bestlam, newx = x_2016, type = 'response')
MSE_2016 <- mean((en_pred_2016 - y_2016)^2)
c(MSE_train = MSE_in, MSE_test = MSE_out, MSE_2016 = MSE_2016)

# 3.1. ROC curves for each sample -----------------------------------------
ROC <- function(real_vector, fitted_prob_vector, fig_title = 'Curva ROC'){
  cut_levels <- c(1,sort(unique(as.vector(fitted_prob_vector)), decreasing = T),0)
  cutoffs <- vector(l = length(cut_levels) - 1)
  for(cut in 1:length(cutoffs)){cutoffs[cut] <- mean(c(cut_levels[cut],cut_levels[cut+1]))}
  n_pos <- sum(real_vector)
  n_neg <- sum(1 - real_vector)
  roc <- data.frame(cut = cutoffs)
  for(c in 1:length(cutoffs)){
    pred_pos <- as.vector(fitted_prob_vector) >= cutoffs[c]
    tvp <- sum(pred_pos*real_vector)/n_pos
    tfp <- sum(pred_pos*(1-real_vector))/n_neg
    roc$tvp[c] <- tvp
    roc$tfp[c] <- tfp
  }
  ggplot(roc, aes(x = tfp, y = tvp)) + geom_line(col = '#2164AD') +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = '#F68D70', linetype = 'dashed') +
    labs(title = fig_title, x = 'Tasa de falsos positivos', y = 'Tasa de verdaderos positivos') + theme_minimal() +
    theme(title = element_text(size=13), plot.title = element_text(hjust = 0.5))
}
ROC(y_train, en_pred_in, fig_title = '(a) Muestra de entrenamiento')
ggsave('Fig2a.jpeg', width = 6, height = 4.125)
ROC(y_test, en_pred_out, fig_title = '(b) Muestra de evaluación 2015')
ggsave('Fig2b.jpeg', width = 6, height = 4.125)
ROC(y_2016, en_pred_2016, fig_title = '(c) Muestra de evaluación 2016')
ggsave('Fig2c.jpeg', width = 6, height = 4.125)

# 3.2. Optimal thresholds for classification ------------------------------
optimal_cut <- function(real_vector, fitted_prob_vector){
  cut_levels <- c(1,sort(unique(as.vector(fitted_prob_vector)), decreasing = T),0)
  cutoffs <- vector(l = length(cut_levels) - 1)
  for(cut in 1:length(cutoffs)){cutoffs[cut] <- mean(c(cut_levels[cut],cut_levels[cut+1]))}
  accuracy <- data.frame(cutoffs)
  for (c in 1:length(cutoffs)){
    pred_pos <- as.vector(fitted_prob_vector) >= cutoffs[c]
    vp <- sum(pred_pos*real_vector)
    vn <- sum((1-pred_pos)*(1-real_vector))
    accuracy$accu[c] <- (vp+vn)/length(real_vector)
  }
  optimal_c <- accuracy$cutoff[accuracy$accu == max(accuracy$accu)]
  return(optimal_c)
}

densidades <- function(real_vector, fitted_prob_vector, fig_title = 'Densidades'){
  data.frame(real = real_vector, probs = as.vector(fitted_prob_vector)) %>%
    ggplot(aes(probs, col = as.factor(real), fill = as.factor(real))) + geom_density(alpha = 0.4) + theme_minimal() +
    scale_color_manual(values = c('#2164AD','#F68D70'), name = 'Proyecto', labels = c('Descartado', 'Aprobado')) +
    scale_fill_manual(values = c('#2164AD','#F68D70'), name = 'Proyecto', labels = c('Descartado', 'Aprobado')) +
    labs(title = fig_title, x = 'Probabilidad estimada', y = 'Densidad') +
    theme(title = element_text(size=13), plot.title = element_text(hjust = 0.5)) +
    geom_vline(xintercept = optimal_cut(real_vector, fitted_prob_vector))
}
densidades(y_train, en_pred_in, fig_title = '(a) Muestra de entrenamiento')
ggsave('Fig3a.jpeg', width = 6, height = 4.125)
optimal_cut(y_train, en_pred_in)
densidades(y_test, en_pred_out, fig_title = '(b) Muestra de evaluación 2015')
ggsave('Fig3b.jpeg', width = 6, height = 4.125)
optimal_cut(y_test, en_pred_out)
densidades(y_2016, en_pred_2016, fig_title = '(c) Muestra de evaluación 2016')
ggsave('Fig3c.jpeg', width = 6, height = 4.125)
optimal_cut(y_2016, en_pred_2016) 
