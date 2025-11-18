# EXERCICE SANS Rcmdr


library(ggplot2)

# 1. Création du jeu de données

data <- data.frame(
  Eleve = c(1, 2, 3, 4, 5, 6),
  Sexe  = c(1, 2, 1, 2, 2, 1),
  Note  = c(12.5, 9, 17, 15.5, 12, 10)
)

# 2. Conversion de Sexe en facteur

data$Sexe <- factor(
  data$Sexe,
  levels = c(1, 2),
  labels = c("Homme", "Femme")
)

# 3. Affichage

print("Jeu de données :")
print(data)


couleurs <- c("Homme" = "#4CAF50",   # VERT
              "Femme" = "#E53935")   # ROUGE

# 4. GRAPHIQUE 1 — Boxplot

ggplot(data, aes(x = Sexe, y = Note, fill = Sexe)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = couleurs) +
  labs(
    title = "Boxplot des notes selon le sexe",
    x = "Sexe",
    y = "Note"
  ) +
  theme_minimal()

# 5. GRAPHIQUE 2 — Histogramme

ggplot(data, aes(x = Note, fill = Sexe)) +
  geom_histogram(
    bins = 6,
    color = "black",
    alpha = 0.7,
    position = "identity"
  ) +
  scale_fill_manual(values = couleurs) +
  labs(
    title = "Histogramme des notes selon le sexe",
    x = "Note",
    y = "Fréquence"
  ) +
  theme_minimal()

# 6. GRAPHIQUE 3 — Barplot effectif par sexe

ggplot(data, aes(x = Sexe, fill = Sexe)) +
  geom_bar() +
  scale_fill_manual(values = couleurs) +
  labs(
    title = "Répartition des élèves selon le sexe",
    x = "Sexe",
    y = "Effectif"
  ) +
  theme_minimal()

# 7. GRAPHIQUE 4 — Scatterplot Élève vs Note

ggplot(data, aes(x = Eleve, y = Note, color = Sexe)) +
  geom_point(size = 4) +
  scale_color_manual(values = couleurs) +
  labs(
    title = "Notes individuelles des élèves",
  )