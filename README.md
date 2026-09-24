# Méthodologie — Ciblage des communes EàG
## 1. Objectif

Identifier, dans le canton de Vaud, les communes où un effort de campagne aurait le meilleur rendement en sièges. Le principe directeur : le rendement d'un effort dans une commune est approximativement le produit de trois facteurs:
- **marge de progression**: le vote de EàG peut-il y croître ?
- **effet de levier sur les sièges**: un report de voix y fait-il basculer un siège ?
- **poids électoral**: y a-t-il assez d'électeurs pour que cela compte ?

Trois principes encadrent l'analyse :

1. **Aucune commune n'est déclarée hors d'atteinte.** Les 300 sont classées. L'arbitrage se lit sur un classement continu, jamais sur une exclusion binaire.
2. **Raboud au Conseil d'État 2026 est le meilleur proxy disponible** de la portée d'EàG : le test électoral le plus récent et le plus proche en périmètre.
3. **Les autres scrutins servent à caractériser l'électorat** de chaque commune — d'où la typologie du §8, et pas seulement un axe gauche-droite.

## 2. Données

| Source | Contenu | Usage |
|---|---|---|
| Votations 2025–2026 | Résultats par commune, avec orientation politique du « oui » (`positionnement_politique_oui`) | Construire l'indice de positionnement (**seule** source de l'ACP) |
| Élections Grand Conseil 2022 | Voix des **six blocs** par commune (`gc_2022_Gauche`, `_PS`, `_verts`, `_centre`, `_droite-bourgeoise`, `_extreme-droite`) | Part EàG de référence **et** simulation exacte de la répartition des sièges (§6) |
| Conseil d'État mars 2026 | Voix pour Agathe Raboud Sidorenko (EàG) par commune — 153 communes où elle a récolté des voix | **Deuxième référence de performance** (§4, §7) : test électoral réel le plus récent. Hors ACP |
| Conseils communaux mars 2026 | Répartition des sièges par parti (EàG / da. / S&E) dans 6 communes (Lausanne, Nyon, Corsier-sur-Vevey, Montreux, Vevey, Yverdon) | Indicateur complémentaire de force locale — données **parcellaires**, hors ACP |
| Effectifs électoraux | Bulletins valables par commune (champ `Valables`) | Pondération. Attention : la base du CE 2026 n'est **pas** celle du GC 2022 (§4) |
| Population 31.12.2025 | Population par arrondissement et sous-arrondissement (source : État de Vaud) | Répartition des mandats 2027 (Art. 46/46a LEDP) |
| Élu·es du GC 2022 | Les 150 député·es avec arrondissement et liste (`Statistiques_élections_GC_2022_-_2027(1).xlsx`) | **Vérité terrain** pour valider la fonction de répartition (`valider_repartition.R`) |

> **Couverture des listes EàG en 2022.** EàG n'a déposé de liste que dans **7 arrondissements sur 13** : Aigle, Lausanne-Ville, Lavaux-Oron, Ouest lausannois, Riviera, Romanel, Yverdon. Dans les 176 communes des six autres (Broye-Vully, Gros-de-Vaud, La Vallée, Morges, Nyon, Pays-d'Enhaut), `part_eag_2022 = 0` est un **zéro structurel** : il n'y avait pas de liste. Ce fait conditionne toute la §4.

## 3. Construction de l'indice de positionnement (ACP)

### 3.1 Orientation des résultats

Avant toute analyse, réorienter chaque votation pour qu'une valeur élevée signifie « plus à gauche » :

- votation codée *gauche* ou *centre-gauche* → conserver la part de « oui » ;
- votation codée *droite* ou *extrême-droite* → utiliser la part de « non » (soit `1 − part_oui`).

### 3.2 Données parcellaires (conseils communaux)

Les données de mars 2026 sont de deux natures :

- **Conseil d'État** (`vdce_2026_eag`) : voix pour Raboud Sidorenko (EàG). C'est le **test électoral réel le plus récent** de la force d'EàG. Ce n'est **pas** une prise de position sur un objet : c'est une performance électorale, traitée comme la référence 2022 (§4, §7) et **exclue de l'ACP de positionnement**. Raboud était candidate dans tout le canton ; le fichier ne liste que les 153 communes où elle a effectivement récolté des voix, le canton ne publiant que les listes en ayant obtenu. Les 147 autres sont donc de **vrais zéros** et entrent comme telles dans les régressions — les exclure reviendrait à sélectionner sur la variable expliquée. Le drapeau `raboud_voix > 0` reste disponible pour les seuils qui doivent ignorer ces zéros (§8).
- **Conseils communaux** (`cc_2026_eag`) : part de sièges EàG/da./S&E dans 6 grandes communes seulement — trop lacunaire pour l'ACP, utilisé **en aval** comme indicateur de présence institutionnelle locale (§7 et §8).

### 3.3 Sélection des votations

L'ACP mesure le **positionnement sur enjeux** : elle ne retient que des scrutins d'objet, initiatives et lois (type `initiative` ou `loi`). Sont exclus :

- les résultats électoraux (`gc_2022_*`, `vdce_2026_eag`, `cc_2026_eag`) — ce sont des performances, pas des positions ;
- les contre-projets (`_cp`) et questions subsidiaires (`_sub`) — voir §3.4.

> **Pourquoi c'est impératif.** Inclure les élections rendait l'analyse circulaire : `gc_2022_Gauche` (c'est-à-dire `part_eag_2022` elle-même) entrait dans l'ACP, puis la §4 régressait `part_eag_2022` sur une Dim.1 qui la contenait. Le résidu n'était alors plus interprétable. Un `stopifnot()` dans `analyse.R` garantit désormais qu'aucune variable `gc_*`, `vdce_*` ou `cc_*` n'entre dans la matrice.

Il en reste **7** : `mormont`, `droits_pol_etrangers`, `init_avenir`, `init_durabilite`, `fed_service_civil`, `salaire_min_const`, `salaire_min_legis`. Toutes couvrent les 300 communes.

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

- Les communes ayant plus de 30 % de données manquantes (`SEUIL_NA = 0.3`) sont exclues. En pratique la couverture est complète : les 300 communes sont retenues.
- Les valeurs manquantes restantes sont imputées par la médiane de la votation.
- L'ACP non pondérée traite chaque commune à égalité ; le poids électoral intervient plus tard dans le score (§6).

Interprétation :

- **Première composante (Dim.1, ~62 % de variance)** : capture l'axe gauche–droite. Si la moyenne des loadings est négative, la dimension est réorientée automatiquement pour que « plus élevé = plus à gauche ».
- **Composantes suivantes** : clivages secondaires (urbain/rural, ouverture/souveraineté). Elles ne sont plus seulement affichées : Dim.2 et Dim.3 alimentent la typologie d'électorat (§8).

L'indice de positionnement de chaque commune est sa **coordonnée sur Dim.1**.

L'ACP est calculée **une seule fois**, par `analyse.R`, puis sérialisée dans `data/processed/acp_positionnement.rds`. Le dashboard la relit au lieu de la recalculer : auparavant les deux fichiers utilisaient un jeu de variables et un seuil différents, si bien que le biplot et le graphique d'écart affichaient deux axes distincts sous le même nom.

## 4. Écart de conversion (vote effectif vs attendu)

Régresser la **part EàG 2022** sur l'indice de positionnement, pondérée par l'effectif — **uniquement sur les arrondissements où EàG avait une liste** :

```r
communes_avec_liste <- filter(communes, liste_eag_2022)
m_ecart <- lm(part_eag_2022 ~ dim1, weights = effectif,
              data = communes_avec_liste, na.action = na.exclude)
```

La référence est la liste EàG seule (`gc_2022_Gauche`), pas le bloc de gauche au complet. Cela mesure directement où EàG sous-capture le potentiel disponible, sans mélanger avec le vote PS/Verts.

> **Pourquoi restreindre l'ajustement.** Ajustée sur les 300 communes, la régression incorporait 176 zéros structurels (pas de liste déposée). Le résidu mesurait alors surtout *« y avait-il une liste ici ? »* : il attribuait une grosse marge de progression à toute commune sans candidature, et une marge **nulle** aux bastions réels, dont le résidu est positif. `ecart` vaut donc `NA` là où EàG n'a pas présenté de liste, et la marge s'y appuie sur les autres signaux (§7).

Lecture des résidus (124 communes) :

- **positionnement élevé + résidu négatif** → soutien latent non converti : meilleures cibles de persuasion / mobilisation ;
- **résidu positif** → la commune sur-performe déjà : marge de progression faible.

R² ≈ **0,58**.

### Second point de référence : Raboud au Conseil d'État 2026

La même régression est ajustée sur `part_raboud_2026`, sur les **300 communes** (les zéros sont réels, cf. §3.2). R² ≈ **0,66** : le positionnement explique mieux le vote Raboud 2026 que le vote de liste 2022, ce qui confirme que le résultat CE est un signal plus propre de la force potentielle d'EàG.

> **Chaque part sur sa propre base.** `part_raboud_2026` se calcule sur les **bulletins du CE 2026**, et non sur l'effectif GC 2022. Le CE 2026 a compté **~1,6× plus de bulletins** que le GC 2022 (176 428 contre 108 375 sur les mêmes 153 communes, ratio médian 1,61). Rapporter les voix de Raboud à la base 2022 gonflait sa part d'autant et transformait un écart de participation en « progression » :
>
> | | Base GC 2022 (ancien calcul) | Base CE 2026 (correct) |
> |---|---|---|
> | Part Raboud médiane, communes avec voix | 9,9 % | **5,9 %** |
> | Tendance médiane vs liste 2022 | +8,3 pp | **+4,6 pp** |
>
> Soit environ la moitié du « +8 pp » historiquement annoncé. 16 communes changent de signe.

`tendance_raboud = part_raboud_2026 − part_eag_2022` sert de signal de **portée démontrée**. Il alimente la marge de progression au §7. Ce qui reste à escompter est le seul effet « panachage + candidate forte », via `RABOUD_ESCOMPTE` (§0 de `analyse.R`).

## 5. Répartition des mandats 2027

Les sièges sont d'abord répartis entre **arrondissements** par la population (Art. 46 LEDP — méthode des plus grands restes avec quotient ⌈pop/150⌉), puis subdivisés entre **sous-arrondissements** pour les trois arrondissements divisés :

| Arrondissement | Sous-arrondissements |
|---|---|
| Jura-Nord vaudois | La Vallée · Yverdon |
| Lausanne | Lausanne-Ville · Romanel |
| Riviera-Pays-d'Enhaut | Pays-d'Enhaut · Riviera |

La subdivision suit l'Art. 46a LEDP : un sous-arrondissement dont la population est inférieure au double du quotient subsidiaire reçoit 2 mandats ; le reste va à l'autre sous-arrondissement.

La répartition 2027 (base population 31.12.2025) est calculée en tête de `analyse.R`, puis **vérifiée automatiquement** contre `vd_sieges_districts.csv` : elle reproduit exactement la répartition officielle (150 sièges, chaque arrondissement concordant).

## 6. Conversion du potentiel de voix en potentiel de sièges

Les sièges étant attribués à l'échelle de l'**unité électorale** (arrondissement ou sous-arrondissement), l'effort communal ne compte qu'à travers le total de cette unité.

### La règle réelle (Const. VD art. 93 al. 4 ; LEDP art. 73 ss)

Ce n'est **pas** Hagenbach-Bischoff. La répartition vaudoise procède ainsi :

1. **Quorum de 5 %** des voix de l'arrondissement. En dessous, la liste est écartée de toute la répartition. C'est la première barre réelle.
2. **Quotient électoral** = `⌈voix des listes admises ÷ sièges à pourvoir⌉` — donc calculé sur les seules listes admises, pas sur tous les bulletins.
3. Chaque liste reçoit autant de sièges que son total contient de quotients.
4. Les sièges restants vont aux **plus forts restes**.

C'est exactement la forme déjà utilisée par `attribuer_art46()` pour la population ; `repartir_ledp()` l'applique aux listes. Les voix des six blocs étant toutes disponibles (leur somme égale les bulletins valables, ratio médian 1,000), la simulation est **exacte** et non plus approchée.

Deux objectifs chiffrés en découlent, par arrondissement :

- `voix_quorum` = voix manquantes pour franchir les 5 % — le chiffre le plus parlant en campagne ;
- `voix_siege` = plus petit nombre de voix supplémentaires donnant un siège de plus, obtenu en rejouant `repartir_ledp()`. Le **levier** vaut `1 / voix_siege`.

### Ce que corrige ce changement

L'ancien quota `total_valables / (sièges + 1)` surestimait largement l'effort, parce qu'il ignorait à la fois le quorum (plus bas) et les plus forts restes (un siège s'obtient avec bien moins qu'un quotient plein) :

| Arrondissement | Sièges 27 | EàG 22 | **Voix pour 1 siège (LEDP)** | Ancien calcul |
|---|---|---|---|---|
| **Aigle** | 9 | 220 | **169** | 540 |
| Ouest lausannois | 15 | 812 | 207 | 382 |
| Riviera | 14 | 1465 | 211 | 383 |
| Lausanne-Ville | 26 | 2310 | 263 | 448 |
| Yverdon | 15 | 1242 | 356 | 818 |
| **Pays-d'Enhaut** | 2 | 0 | **404** | 365 |
| Broye-Vully | 8 | 0 | 457 | 820 |
| Gros-de-Vaud | 8 | 0 | 520 | 1097 |
| **La Vallée** | 2 | 0 | **540** | 549 |
| **Romanel** | 5 | 92 | **658** | 813 |
| Lavaux-Oron | 11 | 38 | 728 | 1104 |
| Nyon | 19 | 0 | 866 | 822 |
| Morges | 16 | 0 | 891 | 996 |

**Aigle est la meilleure cible offensive du canton** : 169 voix, dont 161 pour franchir le quorum.

### Classement des arrondissements — aucune exclusion

| Statut | Condition | Signification |
|---|---|---|
| **consolidation** | `sieges_eag_22 > 0` | Siège(s) à défendre |
| **offensive** | `part_manquante ≤ SEUIL_ATTEIGNABLE` | Conquête réaliste sur une législature |
| **conquete_longue** | `part_manquante > SEUIL_ATTEIGNABLE` | Plus coûteux en part relative — mais souvent peu cher en voix absolues |

Le statut **sert à lire le classement, jamais à le tronquer**. Les 300 communes reçoivent un score.

> **Pourquoi l'ancien `hors_portee` a disparu.** Quand EàG a ~0 voix, `part_manquante` valait mécaniquement `1/(sièges+1)` : le seuil de 12 % ne mesurait alors que la **taille de l'arrondissement**, pas l'hostilité de l'électorat. Il écartait La Vallée, le Pays-d'Enhaut et Romanel — qui coûtent 540, 404 et 658 voix — tout en conservant Morges (891) et Nyon (866). Le §6 affirmait par ailleurs que le levier est délibérément *absolu*, puis classait sur une mesure *relative* : contradiction interne, et incompatible avec le principe qu'aucune commune n'est hors d'atteinte.

> **Validation.** `valider_repartition.R` rejoue la répartition 2022 et la compare aux 150 élu·es réels : **68 cellules sur 78** (arrondissement × bloc) sont exactes. Les 10 écarts valent ±1 siège et correspondent aux **apparentements** de 2022, non modélisés (§9).

## 7. Score de priorité

Les sièges étant attribués **par district électoral**, le score se calcule en deux couches : d'abord le district (quel arrondissement mérite l'effort), puis la commune (où agir à l'intérieur de cet arrondissement). Il est calculé pour **les 300 communes**, sans exception.

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

où la marge retient le **plus fort de trois signaux** :

```
marge_ecart     = max(0, −résidu)                        # sous-conversion 2022 — NA sans liste (§4)
marge_raboud    = max(0, tendance_raboud) × RABOUD_ESCOMPTE   # portée démontrée au CE 2026
marge_reservoir = part_ps_verts × 0.10                   # réservoir PS/Verts non capté
marge_de_progression = max(marge_ecart, marge_raboud, marge_reservoir)
```

- `marge_raboud` capte les communes où EàG a déjà rassemblé, via Raboud, un électorat que son score de liste 2022 ne reflète pas — y compris là où le résidu est nul ou positif.
- `marge_reservoir` est le seul signal disponible dans les 176 communes où EàG n'a jamais présenté de liste **et** où Raboud n'a pas fait de voix. Il est fortement escompté (facteur 0,10) : convertir un électeur PS ou Vert est plus difficile que mobiliser un sympathisant. Sans lui, ces communes auraient un potentiel nul — soit une exclusion déguisée.

La colonne `source_marge` indique lequel des trois signaux a été retenu pour chaque commune.

`score_priorite` s'interprète comme la fraction `part_score_district` du `score_district` : cela permet de choisir les communes à cibler à l'intérieur d'un district priorisé. Deux colonnes le traduisent en objectifs concrets : `voix_a_trouver` (contribution attendue de la commune au siège du district) et `voix_quorum_commune` (sa part de l'effort pour franchir les 5 %).

En district de **consolidation** (EàG déjà représentée en 2022), `potentiel_commune` utilise `max(marge_de_progression, part_eag_2022)` au lieu de la seule marge, pour valoriser autant la défense des votes acquis que la progression.

### Deux lectures du score : volume et marge

`score_priorite` est dominé par la taille des communes (corrélation de 0,92 avec l'effectif). C'est ce qu'il faut pour **répartir l'effort**, mais cela masque **où se trouve la marge de progression**. On le décline donc en deux scores, exprimés dans la même unité — des multiples des voix manquantes pour le siège suivant :

```
score_priorite = potentiel_commune × levier                 # VOLUME — avec effectif
score_marge    = marge_de_progression × levier × 1000        # MARGE  — sans effectif
```

| Score | Se lit comme | Sert à |
|---|---|---|
| `score_priorite` | part du siège suivant que la commune entière peut apporter | choisir où concentrer l'effort (gros gisements) |
| `score_marge` | part du siège suivant gagnée en convertissant **1000 électeurs** de la commune | voir où chaque contact rapporte le plus |

- Le score marge **garde le levier siège** : à marge égale, une commune d'Aigle (169 voix pour un siège) passe devant une commune de Morges (891).
- Il **n'inclut pas l'ajustement de consolidation** : il mesure la marge de *progression*, pas la défense des votes acquis. Avec l'ajustement, qui s'applique dans 80 des 88 communes des districts de consolidation, Vevey, Renens et Lausanne arriveraient en tête sur leur seul score 2022.
- Hors districts de consolidation, `score_priorite = score_marge × effectif / 1000` exactement. La corrélation du score marge avec l'effectif tombe à 0,16.
- Les très petites communes (quelques dizaines de bulletins) ont une marge bruitée ; `analyse.R` affiche le top du score marge pour les communes d'au moins 100 électeurs, et le dashboard propose un filtre « Effectif min ».

Au niveau du district, l'équivalent est calculé sur la marge moyenne d'un électeur du district :

```
marge_district       = Σ(marge_de_progression × effectif) / Σ effectif
score_marge_district = marge_district × levier × 1000
```

## 8. Profil d'action et type d'électorat

Deux classifications complémentaires : le **profil** dit *quoi faire*, le **type d'électorat** dit *à qui l'on parle*.

### 8.1 Profil d'action

| Profil | Condition | Action | n |
|---|---|---|---|
| **consolidation** | district en consolidation ET dim1 > médiane, OU présence institutionnelle CC 2026 | Défendre les bastions | 50 |
| **mobilisation** | dim1 > médiane ET participation < médiane | Faire voter un électorat acquis | 44 |
| **persuasion** | Raboud y a fait des voix ET `tendance_raboud` ≥ médiane, OU résidu < 0 | Déplacer le vote | 63 |
| **report_gauche** | réservoir PS/Verts ≥ médiane | Différencier la ligne EàG, viser le report intra-gauche | 47 |
| **implantation** | aucune des conditions ci-dessus | Construire une présence : référent·es locaux, visibilité, et d'abord déposer une liste | 96 |

Les seuils de dim1, de participation et du réservoir sont les médianes sur les 300 communes. Le seuil Raboud est la médiane des **seules communes où elle a récolté des voix** (+4,6 pp) : calculé sur les 300, il vaudrait 0 — 147 communes ayant une tendance nulle — et la branche persuasion avalerait tout le canton.

> **Il n'y a plus de catégorie « autre ».** Elle absorbait 42 % des communes et les renvoyait à « ressources à réallouer ailleurs ». Les communes les plus difficiles reçoivent désormais le profil `implantation`, qui est une action, pas un abandon : dans six arrondissements, la première chose à faire est simplement de **déposer une liste**.

### 8.2 Type d'électorat

Classification ascendante hiérarchique (`HCPC`, FactoMineR) sur les trois premiers axes de l'ACP, le réservoir PS/Verts et la participation. Cinq classes, nommées de la plus à gauche à la plus à droite :

| Type | n | Dim.1 moy. | Réservoir | Particip. | Raboud |
|---|---|---|---|---|---|
| 1. Gauche marquée mobilisé | 30 | 1,83 | 37,6 % | 59,9 % | 2,8 % |
| 2. Gauche marquée abstentionniste | 87 | 1,55 | 38,6 % | 49,6 % | 4,9 % |
| 3. Centre mobilisé | 97 | −0,28 | 28,1 % | 57,3 % | 2,9 % |
| 4. Centre | 24 | −0,65 | 27,4 % | 53,1 % | 1,6 % |
| 5. Droite | 62 | −2,36 | 22,2 % | 52,5 % | 1,9 % |

Chaque classe porte un `message_cle` — l'angle de campagne qui lui correspond. La classe 2 est la plus intéressante : 87 communes à gauche, à **participation basse** et où Raboud fait déjà son meilleur score. C'est le gisement principal.

> `part_raboud_2026` est volontairement **exclue des variables de clustering** : valant 0 dans 147 communes, elle se comporterait en indicateur binaire « Raboud a-t-elle fait des voix ici » et dominerait la classification. Elle sert à décrire les classes, pas à les former.

## 9. Limites et précautions

- **Apparentement non modélisé — et c'est le point le plus lourd.** Le scénario retenu est celui d'une **liste EàG seule** devant franchir les 5 % par elle-même. La validation contre 2022 montre précisément ce que cette hypothèse coûte : le modèle prédit 1 siège EàG en Riviera là où la liste en a réellement obtenu **2**, grâce à l'apparentement à gauche. Les estimations de sièges sont donc **conservatrices**. Reste à confirmer un point de droit : le quorum de 5 % s'apprécie-t-il au niveau de la liste seule ou du groupe apparenté ? L'initiative Christen (22_INI_1) visait à inscrire la seconde lecture dans la Constitution, ce qui suggère que la première prévaut — mais si le quorum se lit au niveau du groupe, la carte des cibles change entièrement.
- **Absence de liste ≠ contre-performance.** Dans 176 communes (six arrondissements), EàG n'a pas déposé de liste en 2022. Le résidu de conversion n'y est pas défini et la marge repose sur Raboud et le réservoir PS/Verts. Ne pas lire ces 0 % comme des échecs.
- **Vote sur enjeux ≠ vote partisan.** L'indice de positionnement est un indicateur indirect du vote de parti, pas un substitut.
- **CE 2026 ≠ GC 2027.** L'élection au Conseil d'État est majoritaire avec panachage : le score de Raboud agrège des voix PS/Verts qui ne se reporteront pas mécaniquement sur une liste EàG au Grand Conseil. C'est un **plafond de portée**, escompté par `RABOUD_ESCOMPTE`, pas une prédiction.
- **Voix nouvelles, pas transférées.** `voix_siege` suppose des voix venues de l'abstention, le total du district augmentant d'autant. C'est l'hypothèse prudente : un report depuis PS/Verts coûterait un peu moins, puisqu'il abaisse en même temps le reste des listes concurrentes.
- **Sophisme écologique.** L'analyse cible des *lieux*, non des individus ; ne pas inférer de comportements individuels.
- **Décalage temporel.** Résultats de 2022 vs scrutins jusqu'en 2026. L'écart entre le signal récent (positionnement, Raboud) et la référence 2022 constitue lui-même un signal de *tendance*.
- **SEUIL_ATTEIGNABLE.** Le seuil de 12 % ne sert plus qu'à **étiqueter** les arrondissements, jamais à en exclure. Il reste une hypothèse stratégique : à calibrer selon les ressources disponibles.
- **Le facteur 0,10 de `marge_reservoir`** est un jugement, pas une mesure. Il fixe la difficulté relative d'un report PS/Verts par rapport à une mobilisation. Le faire varier déplace le classement des communes sans liste 2022.

## 10. Outils

### Scripts

| Fichier | Rôle |
|---|---|
| `merge_votations.R` | Consolidation des scrutins bruts (`data/raw/*.xlsx`, Grand Conseil, Conseil d'État) → `data/processed/data_votations_vd.csv` |
| `analyse.R` | Pipeline complet : chargement, ACP, écart, répartition LEDP, scores, typologie → `data/processed/` |
| `valider_repartition.R` | Valide `repartir_ledp()` contre les 150 élu·es réels de 2022 |
| `app.R` | Dashboard Shiny interactif (lecture des fichiers `data/processed/`) |

Sorties de `analyse.R` :

| Fichier | Contenu |
|---|---|
| `communes_scores.csv` | Les 300 communes : `score_priorite` (volume), `score_marge` (sans effectif), profil, type d'électorat, voix à trouver |
| `districts_levier.csv` | Les 13 arrondissements : sièges simulés, `voix_quorum`, `voix_siege`, levier, `score_district`, `marge_district`, `score_marge_district` |
| `types_electorat.csv` | Centroïdes et message clé des classes d'électorat |
| `acp_positionnement.rds` | L'ACP sérialisée, relue telle quelle par `app.R` |

### Données géographiques (non versionnées)

Les fonds de carte du dashboard ne sont pas suivis par git (`*.gpkg` dans `.gitignore`, trop
volumineux). À placer manuellement pour exécuter `app.R` :

| Fichier | Source |
|---|---|
| `data/raw/CH_communes_no_lacs.gpkg` | Limites communales suisses sans les lacs (OFS / swissBOUNDARIES3D, couche `CH_communes_no_lacs`) |

Au premier lancement, `app.R` en extrait les communes vaudoises, construit les 13 districts électoraux par union des communes (même rattachement que les scores), simplifie les deux jeux de limites (20 m pour les communes, 50 m pour les districts) et met le tout en cache dans `data/processed/limites_vaud.rds` (0,45 Mo, non versionné). Le cache n'est reconstruit que si le rattachement commune → district change ou si le gpkg est plus récent ; une fois créé, il suffit à lancer l'app sans le gpkg. Pour forcer la reconstruction, supprimer le fichier. L'ancien `districts_vaud_no_lacs.gpkg` n'est plus nécessaire (Riviera y figurait sous le nom « Vevey », et son champ `kantonsnummer` avait été sommé à la fusion).

### Packages R

- **tidyverse** — manipulation des données ;
- **FactoMineR** / **factoextra** — ACP et visualisation ;
- **shiny** / **bslib** — dashboard interactif ;
- **plotly** — graphiques interactifs ;
- **leaflet** / **sf** — cartes des communes et des districts ;
- **DT** — tableaux filtrables.
