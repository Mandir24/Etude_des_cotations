################################################################################
# Auteurs     : DIOP Mandir, GAMONDELE Maxime, SAMAKE Salif
# Projet      : SAE 3.03 - Série Chronologique
# Thématique  : Futures de Matières Premières
# Source      : https://fr.investing.com
# Nom fichier : Mission_1
################################################################################


# importation des librairies
library(tabulapdf)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(zoo)
library(dplyr)
library(ggplot2)
library(pdftools)


# importation des jeux de données
D1 = extract_tables("../Data/Futures cacao US - Données Historiques.pdf",col_names = FALSE)
D2 = extract_tables("../Data/Futures café US C - Données Historiques.pdf",col_names = FALSE)
D3 = extract_tables("../Data/Futures jus dorange - Données Historiques.pdf",col_names = FALSE)
D4 = extract_tables("../Data/Futures pétrole Brent - Données Historiques.pdf",col_names = FALSE)
D5 = extract_tables("../Data/Futures sucre Londres - Données Historiques.pdf",col_names = FALSE)



################################################################################
# --------------------------  Preparation des données  ----------------------- #
################################################################################

# fonctions preparation des données (automatisation de la préparation)

# --------------------------  caract()                ----------------------- #
# Renvoie les col au format caractère 
caract <- function(list) {
  obj <- lapply(list, function(df) {
    df[] <- lapply(df, as.character) # Applique as.character à toutes les colonnes
    return(df)
  })
  return(obj)
}

# --------------------------  retrait()                ----------------------- #
# retrait permet de retirer les chr spéciaux tel que . et , afin d'avoir que des nbr
retrait <- function(data) {
  obj <- data %>%
    mutate(across(c(X2, X3, X4, X5), ~ gsub("[\\.,]", "", .)))
  return(obj)
}

# --------------------------  retrai_vir()                ----------------------- #
# retrai_vir permet de retirer lesle virgules
retrait_vir <- function(data) {
  obj <- data %>%
    mutate(across(c(X2, X3, X4, X5), ~ gsub(",", "", .))) # Retire uniquement les virgules
  return(obj)
}

# --------------------------  fusion()                 ----------------------- #
# fusion permet de fusionner les tibble de la liste
fusion = function(lst_tbl){
  obj <- do.call(rbind, lst_tbl)
  return(obj)
}

# --------------------------  virgule()                 ----------------------- #
# virgule ajoute un point au charactères à la position n-x
virgule = function(data,x){
  obj <- data %>%
    mutate(across(c(X2, X3, X4, X5), 
                  ~ paste0(substr(., 1, nchar(.) - x), 
                           ".", 
                           substr(., nchar(.) - 1, nchar(.)))))
  return(obj)
}

# --------------------------  nom_format()             ----------------------- #
# nom_format change les format de colonnes et les noms
nom_format = function(data){
  obj <- data %>%
    # Convertir X1 en format Date (yyyy-mm-dd)
    mutate(X1 = as.Date(X1, format = "%d/%m/%Y")) %>%
    
    # Convertir X2, X3, X4, X5 en format numérique (dbl)
    mutate(across(c(X2,X3,X4,X5), as.numeric)) %>% 
    #Renommer les colonnes
    rename(Date = X1,
           Closed_Cotation = X2,
           Opened_Cotation = X3,
           Highest_Cotation = X4,
           Lowest_Cotation = X5)
  return(obj)
}

# --------------------------  ajout_col()             ----------------------- #
# ajout_col ajoute une collonne avec le nom de la matire premiere 
ajout_col = function(data,nom){
  obj <- data %>%
    mutate(Futures = paste("Cotation du",nom))
}

# --------------------------  colle()                  ----------------------- #
# colle colle les colonnes entre elles car certaines col contiennes deux valeurs
colle = function(data){
  obj <- list()
  for (i in seq_along(data)) {
    obj[[i]] <- data[[i]] %>%
      unite("X1", everything(), sep = "", na.rm = TRUE)
  }
  return(obj)
}

# --------------------------  post_colle()           ----------------------- #
# post_colle permet de cree des collones a partir de ne col collé
post_colle = function(data){
  obj <- data %>%
    mutate(
      X2 = substr(X1, 11, 15),
      X3 = substr(X1, 16, 20),
      X4 = substr(X1, 21, 25),
      X5 = substr(X1, 26, 30),
      X1 = substr(X1, 1, 10))
  return(obj)
}

################################################################################
D1[[1]] <- D1[[1]][-1, ] 

D1[[1]] <- D1[[1]] %>%
  retrait() %>%
  mutate(across(c(X2, X3, X4, X5), ~ as.numeric(.) / 100000))

D1_F = D1 %>%
  fusion() %>%
  mutate(across(c(X2, X3, X4, X5), ~ as.numeric(.) *1000)) %>% 
  nom_format() %>% 
  ajout_col("cacao")

D2_F = D2 %>% 
  fusion() %>% 
  virgule(2) %>% 
  nom_format() %>% 
  ajout_col("café")

D3_F = D3 %>% 
  fusion() %>% 
  virgule(2) %>% 
  nom_format() %>% 
  ajout_col("jus d'orange")

########################################################################
##---- code ajouter - Maxime
######################################################################

# Donné manquante de la dérnière pages n'étant pas détécté en tant que table
# on les récupéres sous un format str
D3_manq <-  pdf_text("../Data/Futures jus dorange - Données Historiques.pdf")[72]
# Structuration des données
D3_manq<- strsplit(D3_manq, "\n")[[1]]
# extraction des donnée
D3_manq <- D3_manq[2:5]
# Création d'une table de données
D3_manq <- do.call(rbind, strsplit(D3_manq, "\\s+"))

# Convertir en data.frame
D3_manq <- as.data.frame(D3_manq, stringsAsFactors = FALSE)

D3_manq <- D3_manq %>%
  rename(X1 = V1, X2 = V2, X3 = V3, X4 = V4, X5 = V5) %>%
  mutate(
    X1 = as.Date(gsub("[^0-9/\\-]", "", X1), format = "%d/%m/%Y"),
    X2 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X2))), nsmall = 2)),
    X3 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X3))), nsmall = 2)),
    X4 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X4))), nsmall = 2)),
    X5 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X5))), nsmall = 2))
  ) %>%
  ajout_col("jus d'orange")

D3_manq <- D3_manq %>% 
  nom_format()

# Ajouter des données de la derniére feuille de jus d'orange
D3_F <- D3_F %>%
  bind_rows(D3_manq)

# Étape 1 : Identifier les indices des éléments à extraire
indices_err1 <- c()
indices_err2 <- c()


defectueux_err1 = list()
for (i in seq_along(D4)){
  if (ncol(D4[[i]]) < 5 ) {
    defectueux_err1 <- c(defectueux_err1, list(D4[[i]]))
    indices_err1 <- c(indices_err1, i)  # Conserver l'indice
  }
}

defectueux_err2 = list()
for (i in seq_along(D4)) {
  if (any(is.na(D4[[i]][[3]]))) {
    defectueux_err2 <- c(defectueux_err2, list(D4[[i]]))
    indices_err2 <- c(indices_err2, i)  # Conserver l'indice
  }
}

for (i in seq_along(defectueux_err1)) {
  defectueux_err1[[i]] <- defectueux_err1[[i]] %>%
    mutate(
      X5 = X4,  # Copier X4 dans X5
      X4 = X3,   # Copier X3 dans X4
      X3 = substr(X2, nchar(X2) - 6, nchar(X2)),
      X2 = substr(X2, 1, 7)
    ) %>%
    select(X1, X2, X3, X4, X5) # Réorganiser les colonnes
}

for (i in seq_along(defectueux_err2)) {
  defectueux_err2[[i]] <- defectueux_err2[[i]] %>%
    mutate(
      X3 = substr(X2, nchar(X2) - 7, nchar(X2)),
      X2 = substr(X2, 1, 7)
    ) %>%
    select(X1, X2, X3, X4, X5) # Réorganiser les colonnes
}



# Étape 3 : Remettre les données transformées à leur position dans D4
for (i in seq_along(indices_err1)) {
  D4[[indices_err1[i]]] <- defectueux_err1[[i]]
}

for (i in seq_along(indices_err2)) {
  D4[[indices_err2[i]]] <- defectueux_err2[[i]]
}

D4_F = D4 %>% 
  fusion() %>%
  retrait() %>% 
  mutate(across(c(X2, X3, X4, X5), ~ as.numeric(.) / 1000)) %>% 
  nom_format() %>% 
  ajout_col("pétrole Brent")

D4_manq <-  pdf_text("../Data/Futures pétrole Brent - Données Historiques.pdf")[73]

# Divisions du texte en lignes
D4_manq <- strsplit(D4_manq, "\n")[[1]]

# Filtrer les lignes contenant des données utiles (par exemple, lignes 2 à 4 ici)
D4_manq <- D4_manq[2:5]  # Ajustez les indices selon vos données

D4_manq <- do.call(rbind, strsplit(D4_manq, "\\s+"))

D4_manq <- as.data.frame(D4_manq, stringsAsFactors = FALSE)

D4_manq <- D4_manq %>%
  rename(X1 = V1, X2 = V2, X3 = V3, X4 = V4, X5 = V5) %>%
  mutate(
    X1 = as.Date(gsub("[^0-9/\\-]", "", X1), format = "%d/%m/%Y"),
    X2 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X2))), nsmall = 2)),
    X3 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X3))), nsmall = 2)),
    X4 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X4))), nsmall = 2)),
    X5 = as.numeric(format(as.numeric(gsub(",", ".", trimws(X5))), nsmall = 2))
  ) %>%
  ajout_col("pétrole Brent")

D4_manq <- D4_manq %>% 
  nom_format()

D4_F <- D4_F %>%
  bind_rows(D4_manq)


D5_F = D5 %>% 
  fusion() %>% 
  virgule(2) %>% 
  nom_format() %>% 
  ajout_col("sucre")


# Création d'une liste de dataframe avec les df finaux
lst_futures = list(D1_F,D2_F,D3_F,D4_F,D5_F)

# Fusion des df finaux pour en avoir que 1
final = fusion(lst_futures)
str(final)

# Mettre en facteur notre variable qualitative nominal
final$Futures <- as.factor(final$Futures)
levels(final$Futures)

saveRDS(final, file = "fichier_de_travail.rds") # Le fichier doit compter nomralement 18857 obs
colnames(final)


# Vérification à l'aide de graphique

series_futures <- final %>%
  group_by(Futures) %>%
  group_modify(~ {
    # Créer l'objet zoo
    zoo_object <- zoo(.x$Lowest_Cotation, order.by = .x$Date)
    
    # Retourner les données sous forme de data frame
    data.frame(Date = index(zoo_object), Value = coredata(zoo_object))
  })

# Visualiser les séries par "Futures"
head(series_futures)

# Tracer pour vérifier les données
ggplot(series_futures, aes(x = Date, y = Value, color = Futures)) +
  geom_line() +
  labs(title = "Série Chronologique par Futures", x = "Date", y = "Value")


ggplot(series_futures, aes(x = Date, y = Value, color = Futures)) +
  geom_line() +                            # Tracer la courbe
  facet_grid(Futures ~ .) +                 # Créer des facettes pour chaque niveau de Futures
  labs(title = "Série Chronologique des Futures", x = "Date", y = "Value") +
  theme_minimal() 

################################################################################
#########################            END             ###########################
################################################################################