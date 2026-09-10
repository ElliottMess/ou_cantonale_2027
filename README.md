# Méthodologie — Ciblage des communes EàG
## 1. Objectif

Identifier, dans le canton de Vaud, les communes où un effort de campagne aurait le meilleur rendement en sièges. Le principe directeur : le rendement d'un effort dans une commune est approximativement le produit de trois facteurs:
- **marge de progression**: le vote de EàG peut-il y croître ?
- **effet de levier sur les sièges**: un report de voix y fait-il basculer un siège ?
- **poids électoral**: y a-t-il assez d'électeurs pour que cela compte ?

## 2. Données

| Source | Contenu | Usage |
|---|---|---|
| Votations 2025–2026 | Résultats par commune, avec orientation politique du « oui » (`positionnement_politique_oui`) | Construire l'indice de positionnement |
| Élections Grand Conseil 2022 | Résultats par liste et par commune (`gc_2022_Gauche`, `gc_2022_PS`, `gc_2022_verts`) | Établir la part EàG et la part de gauche de référence |
| Conseil d'État mars 2026 | Voix pour Agathe Raboud Sidorenko (EàG) par commune — 153 communes où la liste da. a été déposée | **Deuxième référence de performance** (§4, §7) : test électoral réel le plus récent. Données **partielles**, hors ACP |
| Conseils communaux mars 2026 | Répartition des sièges par parti (EàG / da. / S&E) dans 6 communes (Lausanne, Nyon, Corsier-sur-Vevey, Montreux, Vevey, Yverdon) | Indicateur complémentaire de force locale — données **parcellaires**, exclues de l'ACP principale |
| Effectifs électoraux | Bulletins valables par commune (champ `Valables` du CSV élections) | Pondération, distinction persuasion / mobilisation |
| Population 31.12.2025 | Population par arrondissement et sous-arrondissement (source : État de Vaud) | Répartition des mandats 2027 (Art. 46/46a LEDP) |

## 3. Construction de l'indice de positionnement (ACP)

### 3.1 Orientation des résultats

Avant toute analyse, réorienter chaque votation pour qu'une valeur élevée signifie « plus à gauche » :

- votation codée *gauche* ou *centre-gauche* → conserver la part de « oui » ;
- votation codée *droite* ou *extrême-droite* → utiliser la part de « non » (soit `1 − part_oui`).

### 3.2 Données parcellaires (conseils communaux)

Les données de mars 2026 sont de deux natures :

- **Conseil d'État** (`vdce_2026_eag`) : voix pour Raboud Sidorenko (EàG) dans 153 communes. C'est le **test électoral réel le plus récent** de la force d'EàG. Ce n'est **pas** une prise de position sur un objet : c'est une performance électorale, traitée comme la référence 2022 (§4, §7) et **exclue de l'ACP de positionnement**. Les communes non couvertes (liste da. absente) reçoivent la valeur **0** — vrai zéro, pas donnée manquante — mais le drapeau `raboud_couvert` distingue les 153 communes réellement contestées.
- **Conseils communaux** (`cc_2026_eag`) : part de sièges EàG/da./S&E dans 6 grandes communes seulement — trop lacunaire pour l'ACP, utilisé **en aval** comme indicateur de présence institutionnelle locale (§7 et §8).

### 3.3 Sélection des votations

L'ACP mesure le **positionnement sur enjeux** : elle ne retient que des scrutins d'objet, initiatives et lois (type `initiative` ou `loi`). Sont exclus :

- les résultats électoraux (`gc_2022_*`, `vdce_2026_eag`, `cc_2026_eag`) — ce sont des performances, pas des positions ;
- les contre-projets (`_cp`) et questions subsidiaires (`_sub`) — voir §3.4.

`init_durabilite` (initiative « biodiversité », oui codé à droite) est conservée : la couverture communale est complète. La retirer se fait en une ligne dans `analyse.R` (§4).

### 3.4 Traitement des questions liées

Une initiative, son contre-projet et la question subsidiaire (blocs `mormont_*` et `salaire_min_legis_*`) portent sur **un même paquet soumis au même électorat**. Ce ne sont pas des indicateurs indépendants. La stratégie retenue est la **sélection** : seule l'initiative principale est conservée par paquet (`_cp` et `_sub` exclus du filtre).

### 3.5 Analyse en composantes principales

ACP non pondérée sur la matrice `communes × votations orientées`, **variables centrées-réduites** :

```r
library(FactoMineR)

res_pca <- mat_pca |>          # communes en lignes, votations en colonnes
  PCA(scale.unit = TRUE, graph = FALSE)
```

- Les communes ayant plus de 30 % de données manquantes (`SEUIL_NA = 0.3`) sont exclues.
- Les valeurs manquantes restantes sont imputées par la médiane de la votation.
- L'ACP non pondérée traite chaque commune à égalité ; le poids électoral intervient plus tard dans le score (§6).

Interprétation :

- **Première composante (Dim.1)** : capture l'axe gauche–droite. Si la moyenne des loadings est négative, la dimension est réorientée automatiquement pour que « plus élevé = plus à gauche ».
- **Composantes suivantes** : affichées dans le dashboard pour explorer d'éventuels clivages secondaires (urbain/rural, ouverture/souveraineté).

L'indice de positionnement de chaque commune est sa **coordonnée sur Dim.1**.

## 4. Écart de conversion (vote effectif vs attendu)

Régresser la **part EàG 2022** sur l'indice de positionnement, pondérée par l'effectif :

```r
m_ecart <- lm(part_eag_2022 ~ dim1, weights = effectif, data = communes,
              na.action = na.exclude)
communes$ecart <- resid(m_ecart)
```

La référence est la liste EàG seule (`gc_2022_Gauche`), pas le bloc de gauche au complet. Cela mesure directement où EàG sous-capture le potentiel disponible, sans mélanger avec le vote PS/Verts.

Lecture des résidus :

- **positionnement élevé + résidu négatif** → soutien latent non converti : meilleures cibles de persuasion / mobilisation ;
- **résidu positif** → la commune sur-performe déjà : marge de progression faible.

### Second point de référence : Raboud au Conseil d'État 2026

La même régression est ajustée sur `part_raboud_2026` (voix Raboud / effectif) pour les 153 communes contestées. Deux constats :

- le positionnement explique **beaucoup mieux** le vote Raboud 2026 (R² ≈ 0,85) que le vote de liste 2022 (R² ≈ 0,58) — le résultat CE est un signal plus propre de la force potentielle d'EàG ;
- Raboud dépasse la liste 2022 d'environ **+8 pp en médiane** (effet « candidate forte + panachage »).

`tendance_raboud = part_raboud_2026 − part_eag_2022` sert donc de signal de **portée démontrée** (et de tendance depuis 2022). Il alimente la marge de progression au §7. L'inflation liée au panachage est escomptée par `RABOUD_ESCOMPTE` (§0 de `analyse.R`).

## 5. Répartition des mandats 2027

Les sièges sont d'abord répartis entre **arrondissements** par la population (Art. 46 LEDP — méthode des plus grands restes avec quotient ⌈pop/150⌉), puis subdivisés entre **sous-arrondissements** pour les trois arrondissements divisés :

| Arrondissement | Sous-arrondissements |
|---|---|
| Jura-Nord vaudois | La Vallée · Yverdon |
| Lausanne | Lausanne-Ville · Romanel |
| Riviera-Pays-d'Enhaut | Pays-d'Enhaut · Riviera |

La subdivision suit l'Art. 46a LEDP : un sous-arrondissement dont la population est inférieure au double du quotient subsidiaire reçoit 2 mandats ; le reste va à l'autre sous-arrondissement.

La répartition 2027 (base population 31.12.2025) est calculée en tête de `analyse.R` et sert de référence pour l'analyse de levier.

## 6. Conversion du potentiel de voix en potentiel de sièges

Les sièges étant attribués à l'échelle de l'**unité électorale** (arrondissement ou sous-arrondissement), l'effort communal ne compte qu'à travers le total de cette unité.

Pour chaque unité électorale :

1. calculer les voix EàG 2022 et le total des bulletins valables ;
2. estimer le quota Hagenbach-Bischoff : `quota_hb = total_valables / (sieges_2027 + 1)` ;
3. estimer le nombre de sièges EàG actuels : `floor(votes_eag / quota_hb)` ;
4. calculer les **voix manquantes** pour le siège suivant : `quota_hb × (sieges_eag + 1) − votes_eag` ;
5. en déduire le **levier** : `1 / voix_manquantes` — absolu, pas relatif. Chaque vote supplémentaire apporte la même contribution quelle que soit la taille du district ; c'est le nombre de votes manquants seul qui détermine l'effort restant ;
6. calculer `part_manquante = voix_manquantes / total_valables` et **classer le district** :

| Statut | Condition | Traitement dans le ciblage |
|---|---|---|
| **consolidation** | `sieges_eag_22 > 0` | Communes scorées — défense des sièges acquis |
| **offensive** | `part_manquante ≤ SEUIL_ATTEIGNABLE` | Communes scorées — conquête du premier siège |
| **hors_portee** | `part_manquante > SEUIL_ATTEIGNABLE` | Communes **exclues** — aucun score calculé |

Cette classification précède le ciblage communal : il est inutile de hiérarchiser des communes dans un district structurellement hors d'atteinte.

> **Note** : le calcul HB simplifié utilise uniquement les voix EàG, pas les listes concurrentes. Il estime le levier marginal, pas l'attribution exacte. `SEUIL_ATTEIGNABLE = 0.12` est un paramètre ajustable dans `analyse.R`.

## 7. Score de priorité

Les sièges étant attribués **par district électoral**, le score se calcule en deux couches : d'abord le district (quel arrondissement mérite l'effort), puis la commune (où agir à l'intérieur de cet arrondissement). Calculé uniquement pour les districts **consolidation** ou **offensive** (§6) ; les communes des districts **hors_portee** sont exclues en amont — aucun score ne leur est attribué.

### Couche 1 — score de district

```
potentiel_district = Σ_communes (potentiel_commune)
score_district     = potentiel_district × levier
```

où `levier = 1 / voix_manquantes` (§6) est constant sur tout le district. Ce score classe les arrondissements entre eux (colonne `score_district` de `districts_levier.csv`, graphique « Priorité par district » du dashboard).

### Couche 2 — score de commune

```
potentiel_commune = marge_de_progression × effectif
score_priorite    = potentiel_commune × levier
part_score_district = potentiel_commune / potentiel_district
```

où la marge retient le **plus fort de deux signaux** :

```
marge_ecart       = max(0, −résidu)                                  # sous-conversion du positionnement (2022)
marge_raboud      = max(0, part_raboud_2026 − part_eag_2022) × RABOUD_ESCOMPTE   # portée démontrée au CE 2026
marge_de_progression = max(marge_ecart, marge_raboud)
```

`marge_raboud` capte les communes où EàG a déjà rassemblé, via Raboud, un électorat que son score de liste 2022 ne reflète pas — y compris là où le résidu de la régression est nul ou positif. Pour les communes sans liste da. en 2026, `part_raboud_2026 = 0` et seul `marge_ecart` joue.

`score_priorite` s'interprète comme la fraction `part_score_district` du `score_district` : cela permet de choisir les communes à cibler à l'intérieur d'un district priorisé.

En district de **consolidation** (EàG déjà représentée en 2022), `potentiel_commune` utilise `max(marge_de_progression, part_eag_2022)` au lieu de la seule marge, pour valoriser autant la défense des votes acquis que la progression.

## 8. Persuasion vs mobilisation

La participation moyenne aux votations et le profil politique permettent de distinguer quatre catégories :

| Profil | Condition | Action |
|---|---|---|
| **consolidation** | district en consolidation ET dim1 > médiane, OU présence institutionnelle CC 2026 | Défendre les bastions |
| **mobilisation** | dim1 > médiane ET participation < médiane | Faire voter un électorat acquis |
| **persuasion** | dim1 ≤ médiane ET résidu < 0, OU `marge_raboud` dominante et `tendance_raboud` ≥ médiane (communes contestées) | Déplacer le vote |
| **autre** | aucune des conditions ci-dessus | — |

Les seuils de dim1 et de participation sont calculés sur la médiane de l'ensemble des communes retenues. La branche « Raboud » de la persuasion ne se déclenche qu'au-dessus de la médiane de `tendance_raboud` : le socle systémique de +8 pp ne suffit pas, il faut une surperformance **locale** marquée.

## 9. Limites et précautions

- **Vote sur enjeux ≠ vote partisan.** L'indice de positionnement est un indicateur indirect du vote de parti, pas un substitut.
- **CE 2026 ≠ GC 2027.** L'élection au Conseil d'État est majoritaire avec panachage : le score de Raboud agrège des voix PS/Verts qui ne se reporteront pas mécaniquement sur une liste EàG au Grand Conseil. C'est un **plafond de portée**, escompté par `RABOUD_ESCOMPTE`, pas une prédiction. Couverture partielle : 153 communes sur ~300.
- **Sophisme écologique.** L'analyse cible des *lieux*, non des individus ; ne pas inférer de comportements individuels.
- **Décalage temporel.** Résultats de 2022 vs scrutins jusqu'en 2026. L'écart entre le signal récent (positionnement, Raboud) et la référence 2022 constitue lui-même un signal de *tendance*.
- **HB simplifié.** Le levier est estimé à partir des seules voix EàG ; un calcul exact requiert les voix de toutes les listes. Le scénario `sieges_eag_raboud` (sièges à un niveau de soutien « Raboud CE 2026 ») est indicatif, calculé avec le même quota simplifié.
- **SEUIL_ATTEIGNABLE.** Le seuil de 12 % est une hypothèse stratégique, pas un fait électoral. Un district à 14–15 % peut devenir atteignable si la liste se renforce ou si une vague favorable se profile. À calibrer selon les ressources disponibles et les perspectives de croissance ; l'effet sur le ciblage est visible directement dans le tableau des districts du dashboard.

## 10. Outils

### Scripts

| Fichier | Rôle |
|---|---|
| `merge_votations.R` | Consolidation des scrutins bruts (`data/raw/*.xlsx`, Grand Conseil, Conseil d'État) → `data/processed/data_votations_vd.csv` |
| `analyse.R` | Pipeline complet : chargement, ACP, écart, levier, scores → `data/processed/` |
| `app.R` | Dashboard Shiny interactif (lecture des fichiers `data/processed/`) |

### Données géographiques (non versionnées)

Les fonds de carte du dashboard ne sont pas suivis par git (`*.gpkg` dans `.gitignore`, trop
volumineux). À placer manuellement pour exécuter `app.R` :

| Fichier | Source |
|---|---|
| `data/raw/CH_communes_no_lacs.gpkg` | Limites communales suisses sans les lacs (OFS / swissBOUNDARIES3D, couche `CH_communes_no_lacs`) |
| `data/processed/districts_vaud_no_lacs.gpkg` | Districts vaudois sans les lacs, dérivé de swissBOUNDARIES3D (couche `districts`) |

### Packages R

- **tidyverse** — manipulation des données ;
- **FactoMineR** / **factoextra** — ACP et visualisation ;
- **shiny** / **bslib** — dashboard interactif ;
- **plotly** — graphiques interactifs ;
- **DT** — tableaux filtrables.
