# === CHAP 0 : INSTALLATION AUTOMATIQUE DES PACKAGES MANQUANTS ===

# Liste des packages nécessaires
packages_requis <- c("tidyverse", "plotly", "scales", "gt")

# Pour chaque package
for (pkg in packages_requis) {
  # Tester s'il est déjà installé
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    # Si non installé → l'installer
    message(paste("Installation de", pkg, "..."))
    install.packages(pkg)
    # Puis le charger
    library(pkg, character.only = TRUE)
  }
}

print("Tous les packages sont prêts et chargés !")

#==== CHAP 1 : CHARGEMENT DES DONNEES VIA API ====
df <- "https://ourworldindata.org/grapher/productivity-vs-annual-hours-worked.csv?v=1&csvType=full&useColumnShortNames=true"
#useColumnShortNames=true <- renomme directement le nom des colonnes, mais pas suffisant : on les simplifie dans la phase de nettoyage
df_raw <- read_csv(df, show_col_types = FALSE) #charge le dataset brut dans une variable df_raw ; show_col_types donne des lignes à l'output pour indiquer les types de données présents dans chaque colonnes. Interessant mais on met FALSE pour éviter de surcharger la console

#==== CHAP 2 : NETTOYAGE ====

message("=== DONNÉES BRUTES ===")
glimpse(df_raw)

# Vérification des valeurs manquantes ; ici il n'y en a pas (test ici + vu sur Excel)
message("=== VÉRIFICATION DES NA ===")
na_count <- colSums(is.na(df_raw))
# Tester s'il y a des NA
total_na <- sum(na_count)

if (total_na == 0) {
  message("Aucune valeur manquante détectée")
} else {
  message(paste("/!\ ", total_na, "valeurs manquantes détectées"))
  print(na_count[na_count > 0])  # Afficher seulement les colonnes avec NA
}

# Nettoyage : renommer + ajouter continent
df_clean <- df_raw |> 
  rename(
    pays = entity,
    code_pays = code,
    annee = year,
    heures = avh,
    productivite = productivity,
    population = population_historical,
    continent = owid_region
  ) |> 
  mutate(
    continent = case_when(
      continent == "Europe" ~ "Europe",
      continent == "Asia" ~ "Asia",
      continent == "Africa" ~ "Africa",
      continent %in% c("North America", "South America") ~ "Americas",
      continent == "Oceania" ~ "Oceania",
      TRUE ~ NA_character_
    )
  )

# Compter combien de NA dans continent
na_continent <- sum(is.na(df_clean$continent))

if (na_continent > 0) {
  message(paste("Suppression de", na_continent, "lignes sans continent"))
  df_clean <- df_clean |> filter(!is.na(continent))
} else {
  message("Aucune ligne à filtrer (tous les continents sont valides)")
}

message("=== DONNÉES NETTOYÉES ===")
glimpse(df_clean)

# Statistiques de nettoyage
message(paste("Lignes initiales :", nrow(df_raw)))
message(paste("Lignes finales :", nrow(df_clean)))
message(paste("Lignes supprimées :", nrow(df_raw) - nrow(df_clean)))

