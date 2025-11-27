# 📊 Analyse de Séries Temporelles - Cotations de Matières Premières

[![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)](https://www.r-project.org/)
[![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=rstudio&logoColor=white)](https://posit.co/products/open-source/rstudio/)
[![tidyverse](https://img.shields.io/badge/tidyverse-1A162D?style=for-the-badge&logo=tidyverse&logoColor=white)](https://www.tidyverse.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg?style=for-the-badge)](LICENSE)

> **Projet universitaire** - BUT Science des Données (IUT Grand Ouest Normandie, 2024-2025)  
> 👨‍🎓 **Auteurs** : Mandir DIOP, Gamondele Maxime, Samake Salif  
> 📅 **Période** : Janvier 2025

---

## 📖 Description

Ce projet porte sur l'**analyse de séries temporelles** de cotations de matières premières observées depuis le **1er janvier 2010**. L'objectif principal est d'étudier l'évolution des prix, d'identifier les tendances, et de modéliser les variations à l'aide de techniques statistiques avancées, notamment la **régression linéaire par morceaux**.

### 🎯 Matières premières étudiées

| Matière première | Type de contrat | Devise |
|------------------|----------------|--------|
| ☕ **Café** | Futures café US C | USD |
| 🍫 **Cacao** | Futures cacao US | USD |
| 🍊 **Jus d'orange** | Futures jus d'orange | USD |
| 🍬 **Sucre** | Futures sucre Londres | USD |
| 🛢️ **Pétrole Brent** | Futures pétrole Brent | USD |

📌 **Source des données** : [Investing.com](https://www.investing.com)

---

## 🚀 Objectifs du projet

### Mission 1 : Import et préparation des données
- ✅ Extraction des données depuis des fichiers PDF avec `tabulapdf`
- ✅ Structuration des données en tibble avec 5 variables :
  - `Date` : jour de cotation
  - `Closed_Cotation` : valeur à la fermeture des marchés
  - `Opened_Cotation` : valeur à l'ouverture des marchés
  - `Highest_Cotation` : valeur maximale de la journée
  - `Lowest_Cotation` : valeur minimale de la journée

### Mission 2 : Analyses statistiques
- 📊 **Boxplots annuels** : visualisation des distributions de prix par année
- 📈 **Évolution mensuelle** : analyse de la moyenne mensuelle avec courbes de régression lissées
- 📉 **Taux d'évolution** : calcul et visualisation des variations mensuelles
- 🔗 **Corrélation café-cacao** : étude de l'association entre les deux matières premières
- 🛢️ **Modélisation du Brent** :
  - Détection de saisonnalité
  - Régression linéaire par morceaux (2020-2024)
  - Identification des ruptures de tendance
  - Prévision sur 26 mois avec intervalle de confiance à 95%

---

## 📊 Résultats clés

### 🛢️ Modélisation du pétrole Brent (2020-2024)

**Indicateurs de performance :**
- **R² = 92,67%** : le modèle explique plus de 90% de la variance des prix
- **Pente** : β₁ = 0,097 (soit +2,96 $/mois ou +35,56 $/an)
- **Ruptures de tendance** identifiées et liées aux événements géopolitiques (COVID-19, guerre en Ukraine)

### 📈 Interprétation

Le modèle de régression linéaire par morceaux capture efficacement les variations du prix du Brent. Les ruptures de tendance correspondent aux grandes phases économiques :
1. **2020** : Chute brutale liée à la pandémie COVID-19
2. **2021-2022** : Forte reprise et pic lié à la guerre en Ukraine
3. **2023-2024** : Stabilisation progressive avec légère tendance baissière

---

## 🛠️ Technologies utilisées

### Langages & Frameworks
- ![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white) **R** (v4.3+)
- ![tidyverse](https://img.shields.io/badge/tidyverse-1A162D?style=flat&logo=tidyverse&logoColor=white) **tidyverse** (dplyr, ggplot2, tidyr)

### Bibliothèques R principales
```r
library(tidyverse)      # Manipulation et visualisation de données
library(tabulapdf)      # Extraction de données depuis PDF
library(lubridate)      # Gestion des dates
library(ggplot2)        # Visualisations avancées
library(broom)          # Nettoyage des sorties de modèles
```

### Outils de développement
- **RStudio** : environnement de développement intégré
- **R Markdown** : documentation et rapports reproductibles
- **Git & GitHub** : gestion de version

---

## 📁 Structure du projet

```
analyse-series-temporelles/
│
├── data/                          # Données brutes (PDF)
│   ├── Futures cacao US - Données Historiques.pdf
│   ├── Futures café US C - Données Historiques.pdf
│   ├── Futures jus dorange - Données Historiques.pdf
│   ├── Futures pétrole Brent - Données Historiques.pdf
│   └── Futures sucre Londres - Données Historiques.pdf
│
├── scripts/                       # Scripts R
│   ├── Mission_1.R          # Import et nettoyage
│   ├── Misions_2.R          # Analyses exploratoires
│   ├──  # Création des graphiques
│   ├──  # Régression Brent
│   └──          # Prévisions sur 26 mois
│
├── docs/                          # Documentation
│   ├── rapport.pdf               # Rapport complet
│   ├── Poster.pdf                # Poster complet
│   └── presentation.pdf          # Support de présentation
│
├── README.md                      # Ce fichier
├── .gitignore                    # Fichiers à ignorer
└── series_temporelles.Rproj  # Projet RStudio
```

---

## 🚀 Installation et utilisation

### Prérequis
- R (version 4.0 ou supérieure)
- RStudio (recommandé)
- Packages R nécessaires

### Installation des dépendances

```r
# Installation des packages nécessaires
install.packages(c(
  "tidyverse",
  "tabulapdf",
  "lubridate",
  "ggplot2",
  "broom",
  "scales"
))
```

### Exécution du projet

1. **Cloner le dépôt**
```bash
git clone https://github.com/Mandir24/analyse-series-temporelles.git
cd analyse-series-temporelles
```

2. **Ouvrir le projet dans RStudio**
```r
# Double-cliquer sur projet_series_temporelles.Rproj
```

3. **Exécuter les scripts dans l'ordre**
```r
source("scripts/Mission_1.R")
source("scripts/Mission_2.R")
```
---

## 📊 Méthodologie

### 1. Préparation des données
- Import depuis PDF avec `extract_tables()` de `tabulapdf`
- Nettoyage et conversion des types de données
- Gestion des valeurs manquantes et des dates

### 2. Analyse exploratoire
- Statistiques descriptives (moyenne, médiane, quartiles)
- Visualisation des distributions avec `ggplot2`
- Détection de valeurs aberrantes

### 3. Modélisation
- Régression linéaire simple avec `lm()`
- Régression linéaire par morceaux pour le Brent
- Validation des hypothèses (normalité des résidus, homoscédasticité)

### 4. Prévisions
- Utilisation de `predict()` avec intervalles de confiance à 95%
- Extrapolation sur 26 mois

---

## 🎓 Compétences développées

- ✅ Manipulation de séries temporelles en R
- ✅ Extraction de données depuis PDF
- ✅ Visualisation avancée avec ggplot2
- ✅ Modélisation statistique par régression
- ✅ Détection de ruptures de tendance
- ✅ Analyse de corrélation
- ✅ Prévision avec intervalles de confiance
- ✅ Interprétation de modèles économiques

---

## 📚 Références

- **Source des données** : [Investing.com](https://www.investing.com)
- **Documentation R** : [The R Project](https://www.r-project.org/)
- **tidyverse** : [tidyverse.org](https://www.tidyverse.org/)
- **tabulapdf** : [CRAN - tabulapdf](https://cran.r-project.org/package=tabulapdf)

---

## 👥 Contributeurs

<table>
  <tr>
    <td align="center">
      <a href="https://github.com/Mandir24">
        <img src="https://github.com/Mandir24.png" width="100px;" alt="Mandir DIOP"/>
        <br />
        <sub><b>Mandir DIOP</b></sub>
      </a>
    </td>
    <td align="center">
    <a href="https://github.com/gamon-11"> <img src="https://github.com/gamon-11.png" width="100px;" alt=" Gamondele Maxime">
      <sub><b>Gamondele Maxime</b></sub>
    </td>
    <td align="center">
      <sub><b>Samake Salif</b></sub>
    </td>
  </tr>
</table>

---
---

<div align="center">
  <p>⭐ Si ce projet vous a été utile, n'hésitez pas à lui donner une étoile !</p>
  <p>Fait avec ❤️ par l'équipe BUT Science des Données - Lisieux</p>
</div>