##### Mi modelo de ecuaciones estructurales ########
library(readxl)
library(plspm)
Base <- read_excel("ESPECIALIDAD EME/1 SEMESTRE/Proyecto/Instrumen/Base.xlsx")
View(Base)
dim(Base)
#Arrancamos!
#modelo interno
#matriz de trayectorias se puede definir como:
# rows of path matrix; filas de la matriz de ruta
Calidad= c(0, 0, 0, 0, 0, 0,0,0)
Interaccion=c(0, 0, 0, 0, 0, 0,0,0)
Infestructura=c(0, 0, 0, 0, 0, 0,0,0)
Movilidad=c(0, 0, 0, 0, 0, 0,0,0)
Formacion=c(0, 0, 0, 0, 0, 0,0,0)
Desempeño=c(0, 1, 1, 0, 0, 0,0,0)
Presion=c(0, 0, 0, 1, 1, 0,0,0)
Expectativas=c(1, 0, 0, 0, 0, 1,1,0)
# matrix (by row binding); matriz por enlace de filas
Base_path = rbind(Calidad, Interaccion, Infestructura, Movilidad, Formacion, Desempeño, Presion, Expectativas )
# add column names (optional);no,bre de las comlumas
colnames(Base_path) = rownames(Base_path)
Base_path
# plot the inner matrix; grafica de la matriz interna
innerplot(Base_path, box.size = 0.1)
innerplot(Base_path, box.size = 0.11,colpos = "sky blue",box.col = "gray95",txt.col = "black" )
# outer model;modelo externo
Base_blocks = list(12:16, 17:20, 21:25, 26:29, 30:34, 35:40,41:44,45:49)
# modes (reflective blocks);modo (modo reflexivo)
Base_modes = rep("A", 8)
# Convertir Base a un data frame
Base <- as.data.frame(Base)
# apply plspm
expect_pls1 = plspm(Base, Base_path, Base_blocks, modes = Base_modes)
# print edu_pls1
expect_pls1
# check unidimensionality; checar la unidimensionalodad
expect_pls1$unidim
#plotting the loadings;trazando las cargas
plot(expect_pls1, what = "loadings")
# check outer model;comprobar el modelo externo
expect_pls1$outer_model
# load ggplot2; cargar ggplot2
library(ggplot2)
# barchart of loadings; grafico de barras de las cargas
ggplot(data = expect_pls1$outer_model,
       aes(x = name, y = loading, fill = block)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  # threshold line (to peek acceptable loadings above 0.7)
  geom_hline(yintercept = 0.7, color = 'gray50') +
  # add title
  ggtitle("Barchart of Loadings") +
  # rotate x-axis names
  theme(axis.text.x = element_text(angle = 90))
#crisis
# check cross loadings; comprobar las cargas cruzadas
expect_pls1$crossloadings

# plotting results (inner model);trazando modelo interno
plot(expect_pls1)
# matrix of path coefficients;matriz de coeficientes de trayectoria
expect_pls1$path_coefs
# plotting results (inner model)
plot(edu_pls3, arr.pos = 0.37)
# matrix of path coefficients;matriz de coeficientes de trayectoria 
Paths = expect_pls1$path_coefs
# matrix with values based on path coeffs;matriz con valores basados con coeficiente de ruta
arrow_lwd = 10 * round(Paths, 2)
# how does it look like?
arrow_lwd
# arrows of different sizes reflecting the values of the path coeffs
#flechas de diferentes tamaños que reflejan los valores de los coeficientes de ruta 
plot(expect_pls1, arr.pos = 0.35, arr.lwd = arrow_lwd)
# inner model; modelo interno
expect_pls1$inner_model
# effects;efectos
expect_pls1$effects
# resetting default margins
par(op)
#resumen del modelo interno
expect_pls1$inner_summary
# gof index; índice de gof
expect_pls1$gof
#Validacion bootstrap
# running bootstrap validation
edu_val = plspm(education, edu_path, edu_blocks3, modes = edu_modes,
                boot.val = TRUE, br = 200)
# bootstrap results
edu_val$boot
# bootstrap path coefficients;,coeficiente de ruta de arranque
edu_val$boot$paths
# summary of latent variable scores; resumen de las variables latentes
summary(edu_pls3$scores)
# setting graphic layout and margin sizes;confguracion del diselo y tamaño
op = par(mfrow = c(2, 3), mar = c(4, 5, 2, 0.5))
# for each score
for (j in 1:6) {
  # histogram (with probability density)
  hist(edu_pls3$scores[, j], freq = FALSE, xlab = "", border = "#6A51A3",
       col = "#DADAEB", main = colnames(edu_pls3$scores)[j])
  # add axes
  axis(side = 1, col = "gray70", col.axis = "gray70")
  axis(side = 2, col = "gray70", col.axis = "gray70")
}
par(op)
# rescaling scores;reescalando puntuaciones
Scores = rescale(edu_pls3)
# summary
summary(Scores)  
# setting graphic layout and margin sizes
op = par(mfrow = c(2,3), mar = c(4, 5, 2, 1.5), bty = "n")
# for each score
for (j in 1:6)
{
  # histogram (not showing axes)
  hist(Scores[,j], main = colnames(Scores)[j], axes = FALSE,
       xlim = c(1,7), ylim = c(0, 125), xlab = "",
       border = "#6A51A3", col = "#DADAEB")
  # add horizontal axis
  axis(side = 1, col = "gray70", col.axis = "gray70")
  # add vertical axis
  axis(side = 2, col = "gray70", col.axis = "gray70", las = 2)
}
# resetting default graphical parameters
par(op)
# scatter plots of rescaled scores;diagrama de dispersion de puntuaciones reescaladas
pairs(Scores, pch = 19, cex = 0.7, col = "#807DBA33", cex.axis = 0.8,
      col.axis = "gray70", gap = 0.5)
