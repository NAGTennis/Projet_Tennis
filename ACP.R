#ACP

#install.packages(c("FactoMineR", "factoextra"))

library("FactoMineR")
library("factoextra")
library(dplyr)


str(table_score)
don <- table_score %>% select(-p2_name,-p1_name,-round,-tourney_level,-tourney_date,
                              -tourney_name,-tourney_id,-surface,-coin,-p1_id,-p2_id,-round_num,-match_num)

res_pca <- PCA(don, graph = FALSE)
var <- get_pca_var(res_pca)

eig_val <- get_eigenvalue(res_pca)
eig_val

fviz_eig(res_pca, addlabels = TRUE, ylim = c(0, 50))
fviz_pca_var(res_pca, col.var = "black")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

fviz_pca_biplot (res_pca,
                 col.ind = as.factor(table_score$coin), palette = "jco",
                 addEllipses = TRUE, label = "var",
                 col.var = "black", repel = TRUE,
                 #select.var = list (contrib = 5),
                 legend.title = "Victoire")

install.packages("xplor")
library(xplor)

# fviz_pca_biplot (res.pca, select.ind = list (contrib = 5),
#                  select.var = list (contrib = 5),
#                  ggtheme = theme_minimal())

fviz_pca_ind(res_pca, geom = "point", col.ind = as.factor(table_score$coin),palette = "jco")

fviz_pca_ind(res_pca,
             geom.ind = "point", # Montre les points seulement (mais pas le "text")
             col.ind = as.factor(table_score$coin), # colorer by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Ellipses de concentration
             legend.title = "Victoire"
)

install.packages("Factoshiny")
library(FactoMineR)


install.packages("yaml")
install.packages("colourpicker")
install.packages("Factoshiny")

install.packages("XQuartz")

library(Factoshiny)
pca <- PCAshiny(don)
