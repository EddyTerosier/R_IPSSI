# EXERCICE 2 – RÉGRESSION LOGISTIQUE


# Dataset : health

library(FactoMineR)
library(ggplot2)
library(dplyr)

# Chargement du dataset
data("health")

# Vérification des colonnes
print(names(health))

# 1. Sélection des variables

# Variable cible binaire
target <- "Gender"   

# Variables explicatives quantitatives
var1 <- "Age"                  
var2 <- "Health condition"     

# Construction du dataframe
df <- health[, c(target, var1, var2)]

# Nettoyage
df <- na.omit(df)

# Encodage binaire
df[[target]] <- as.numeric(as.factor(df[[target]])) - 1

# 2. Modèle logistique

formula_logit <- as.formula("Gender ~ Age + `Health condition`")

fit <- glm(formula_logit, data=df, family="binomial")

print("Résumé du modèle logistique :")
print(summary(fit))

# 3. Prédictions

pred <- predict(fit, df, type="response")

results <- data.frame(
  observed = df[[target]],
  predicted = pred,
  error = pred - df[[target]]
)

# 4. Graphique des erreurs

ggplot(results, aes(x = error)) +
  geom_histogram(bins = 20, color="black", fill="lightgreen") +
  labs(
    title="Erreurs de prédiction – Régression logistique",
    x="Erreur (prédit - observé)",
    y="Nombre d'observations"
  )