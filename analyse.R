# ============================================================
# Ciblage des communes — EàG, Cantonales Vaud 2027
# ============================================================
# Suit la méthodologie décrite dans README.md
# ============================================================

library(tidyverse)
library(FactoMineR)
library(factoextra)

# ── 0. PARAMÈTRES ────────────────────────────────────────────

# Liste EàG uniquement :
#   - référence pour l'écart de conversion (où EàG sous-performe)
#   - voix retenues pour le calcul HB de sièges (sièges attribués par liste)
LISTE_EAG <- "gc_2022_Gauche"

# Profil électoral de la commune (EàG + PS + Verts) :
#   indicateur "l'électorat est-il fertile pour la gauche ?"
#   PS et Verts = réservoir de votes à conquérir, pas la cible de performance
LISTES_GAUCHE_PROFIL <- c("gc_2022_Gauche", "gc_2022_PS", "gc_2022_verts")

# Seuil de données manquantes acceptables pour une commune (en fraction)
SEUIL_NA <- 0.3

# Quorum légal par arrondissement (Const. VD art. 93 al. 4 ; LEDP art. 73).
# Une liste qui ne l'atteint pas est écartée de la répartition des sièges.
# Scénario retenu : liste EàG SEULE, sans apparentement — le quorum doit donc
# être franchi par la liste elle-même (voir README §9).
QUORUM_LEDP <- 0.05

# Seuil d'AFFICHAGE uniquement : au-delà, un district est signalé comme coûteux.
# Il ne filtre plus rien — aucune commune n'est exclue du ciblage (README §6).
SEUIL_ATTEIGNABLE <- 0.12

# Palette des profils d'action — partagée avec app.R
PROFIL_PAL <- c(
  consolidation = "#4dac26",
  mobilisation  = "#2166ac",
  persuasion    = "#d6604d",
  report_gauche = "#b07aa1",
  implantation  = "#8c8c8c"
)

# Palette de la comparaison 2022 ↔ 2026 (§7bis) — partagée avec app.R
STATUT_COMPARAISON_PAL <- c(
  progression       = "#2166ac",
  stable            = "#999999",
  recul             = "#d6604d",
  "sans liste 2022" = "#f0a500"
)

# Résultat Raboud Sidorenko (EàG) au Conseil d'État, mars 2026 — indicateur
# le plus récent et le plus direct de la portée électorale d'EàG (§3.2, §4 du README).
# L'élection au CE est majoritaire avec panachage : le score de Raboud inclut des
# voix PS/Verts qui ne se reporteront pas forcément sur une liste EàG au GC.
# RABOUD_ESCOMPTE escompte ce « plafond » avant de l'injecter dans la marge de
# progression (1 = aucun escompte, 0 = on ignore Raboud).
#
# NB : la part Raboud est désormais calculée sur les bulletins du CE 2026
# eux-mêmes (et non sur l'effectif GC 2022), ce qui retire l'inflation de
# participation — le CE 2026 a compté ~1,6× plus de bulletins que le GC 2022.
# Ce qui reste à escompter est le seul effet « panachage + candidate forte ».
# 1.0 = hypothèse haute assumée ; c'est ici qu'on ajuste si on la juge optimiste.
RABOUD_ESCOMPTE <- 1.0


# ── FONCTIONS : ATTRIBUTION DES MANDATS (Art. 46 / 46a LEDP) ─
# Les sièges sont attribués aux arrondissements sur la base de la POPULATION
# (pas des votes), ce qui est différent de la méthode Hagenbach-Bischoff
# utilisée ensuite pour attribuer les sièges aux listes à l'intérieur de
# chaque unité électorale.

# Art. 46 — répartition entre arrondissements (ou entre sous-arrondissements
# quand les deux atteignent le quotient subsidiaire).
attribuer_art46 <- function(populations, n_total) {
  quotient   <- ceiling(sum(populations) / n_total)
  base       <- floor(populations / quotient)
  restes     <- populations - base * quotient
  n_restants <- n_total - sum(base)
  if (n_restants > 0) {
    top <- order(restes, decreasing = TRUE)[seq_len(n_restants)]
    base[top] <- base[top] + 1L
  }
  base
}

# Art. 46a — répartition entre deux sous-arrondissements.
# Retourne c(mandats_sub1, mandats_sub2).
attribuer_art46a <- function(pop_arrdt, mandats_arrdt, pop_sub1, pop_sub2) {
  quotient_sub <- ceiling(pop_arrdt / mandats_arrdt) * 2L
  if (pop_sub1 < quotient_sub) {
    c(2L, mandats_arrdt - 2L)
  } else if (pop_sub2 < quotient_sub) {
    c(mandats_arrdt - 2L, 2L)
  } else {
    attribuer_art46(c(pop_sub1, pop_sub2), mandats_arrdt)
  }
}


# ── FONCTIONS : RÉPARTITION DES SIÈGES ENTRE LISTES (Art. 73 ss LEDP) ─
# Attention : ce n'est PAS Hagenbach-Bischoff. La règle vaudoise est :
#   1. écarter les listes sous le quorum de 5 % des voix de l'arrondissement ;
#   2. quotient électoral = ⌈voix des listes admises / sièges à pourvoir⌉ ;
#   3. chaque liste reçoit autant de sièges que son total contient de quotients ;
#   4. les sièges restants vont aux plus forts restes.
# C'est la même forme que attribuer_art46() ci-dessus, appliquée aux listes.
# Conséquence majeure : le quorum est la vraie première barre, et une liste peut
# décrocher un siège avec bien moins qu'un quotient complet, grâce aux restes.

repartir_ledp <- function(voix, n_sieges, quorum = QUORUM_LEDP) {
  voix   <- voix[!is.na(voix)]
  sieges <- setNames(integer(length(voix)), names(voix))
  total  <- sum(voix)
  if (total <= 0 || n_sieges <= 0) return(sieges)

  # 1. quorum — appliqué à la liste seule (pas d'apparentement, cf. §0)
  admises <- voix[voix >= quorum * total]
  if (length(admises) == 0) return(sieges)

  # 2-3. quotient et sièges pleins
  quotient <- ceiling(sum(admises) / n_sieges)
  base     <- floor(admises / quotient)

  # 4. plus forts restes
  restes     <- admises - base * quotient
  n_restants <- n_sieges - sum(base)
  if (n_restants > 0) {
    top <- order(restes, decreasing = TRUE)[seq_len(n_restants)]
    base[top] <- base[top] + 1L
  }

  sieges[names(base)] <- as.integer(base)
  sieges
}

# Plus petit nombre de voix supplémentaires pour que `liste` gagne un siège de plus.
# Modèle : voix nouvelles (abstentionnistes, nouveaux électeurs) — le total du
# district augmente. Hypothèse prudente : un report depuis PS/Verts coûterait
# un peu moins, puisqu'il abaisse en même temps le reste des listes concurrentes.
voix_pour_siege_suivant <- function(voix, n_sieges, liste = LISTE_EAG,
                                    quorum = QUORUM_LEDP) {
  if (!liste %in% names(voix)) return(NA_real_)
  cible <- repartir_ledp(voix, n_sieges, quorum)[[liste]] + 1L
  total <- sum(voix, na.rm = TRUE)

  atteint <- function(ajout) {
    v <- voix
    v[[liste]] <- v[[liste]] + ajout
    repartir_ledp(v, n_sieges, quorum)[[liste]] >= cible
  }

  # Balayage grossier puis affinage. On évite la bissection : la fonction n'est
  # pas strictement monotone (ajouter des voix relève aussi le quotient).
  pas    <- max(1, ceiling(total / 400))
  grille <- seq(0, ceiling(total), by = pas)
  hit    <- Position(atteint, grille)
  if (is.na(hit)) return(NA_real_)

  bas <- if (hit == 1L) 0 else grille[hit - 1L] + 1
  for (a in bas:grille[hit]) if (atteint(a)) return(a)
  grille[hit]
}

# Voix manquantes pour franchir le seul quorum — la première barre concrète,
# et le chiffre le plus parlant en campagne.
voix_pour_quorum <- function(voix, liste = LISTE_EAG, quorum = QUORUM_LEDP) {
  total <- sum(voix, na.rm = TRUE)
  max(0, ceiling(quorum * total) - voix[[liste]])
}


# Normalisation des noms de districts : les fichiers sources ne s'accordent pas
# sur les espaces autour des traits d'union ("Broye-Vully" vs "Broye - Vully",
# "Lavaux-Oron" vs "Lavaux - Oron"). Sans cela, la jointure de mapping échoue
# silencieusement pour 47 communes.
norm_district <- function(x) {
  x %>% str_squish() %>% str_replace_all("\\s*-\\s*", "-")
}

# Sièges par district électoral (référence officielle)
vd_sieges <- read_csv("data/processed/vd_sieges_districts.csv",
                      show_col_types = FALSE) %>%
  mutate(across(c(district_csv, district, district_electoral), norm_district))

# Données de population 31.12.2025 (source : État de Vaud)
pop_brut_2025 <- read_csv("data/processed/vd_pop_2025.csv",
                          show_col_types = FALSE) %>%
  mutate(district      = norm_district(district),
         sous_district = norm_district(sous_district))

pop_2025 <- pop_brut_2025 %>%
  group_by(district) %>%
  summarise(pop = sum(population), .groups = "drop")


pop_sub_2025 <- pop_brut_2025 %>%
  filter(!is.na(sous_district)) %>%
  group_by(district, sous_district) %>%
  summarise(pop = sum(population), .groups = "drop")


# Calculer la répartition 2027 et vérifier contre les valeurs officielles
mandats_district_2027 <- pop_2025 %>%
  mutate(mandats = attribuer_art46(pop, 150L))

cat("Mandats par districts 2027 (calculés) :\n")
print(mandats_district_2027)

# Répartition au niveau des sous-districts
prep_sub <- pop_sub_2025 %>%
  rename(pop_sub = pop) %>%
  left_join(
    mandats_district_2027 %>%
      rename(pop_arrdt = pop, mandats_arrdt = mandats),
    by = "district"
  )

mandats_sub_2027 <- prep_sub %>%
  group_by(district) %>%
  reframe(
    sous_district = sous_district,
    pop_sub    = pop_sub,
    mandats    = attribuer_art46a(
      pop_arrdt     = first(pop_arrdt),
      mandats_arrdt = first(mandats_arrdt),
      pop_sub1      = pop_sub[1],
      pop_sub2      = pop_sub[2]
    )
  )

cat("\nMandats par sous-district 2027 (calculés) :\n")
print(mandats_sub_2027)

# Vérification : la répartition calculée (Art. 46/46a sur la population
# 31.12.2025) doit reproduire exactement les sièges 2027 de référence.
mandats_calcules <- bind_rows(
  mandats_sub_2027 %>%
    transmute(district_electoral = sous_district, mandats),
  mandats_district_2027 %>%
    filter(!district %in% mandats_sub_2027$district) %>%
    transmute(district_electoral = district, mandats)
) %>%
  mutate(district_electoral = norm_district(district_electoral))

controle_mandats <- vd_sieges %>%
  distinct(district_electoral, sieges_2027) %>%
  full_join(mandats_calcules, by = "district_electoral") %>%
  mutate(ecart = mandats - sieges_2027)

if (any(controle_mandats$ecart != 0, na.rm = TRUE) ||
    anyNA(controle_mandats$ecart)) {
  print(controle_mandats %>% filter(is.na(ecart) | ecart != 0))
  warning("Art. 46/46a : la répartition calculée diffère de vd_sieges_districts.csv")
} else {
  cat(sprintf("  ✓ conforme à vd_sieges_districts.csv (%d sièges au total)\n",
              sum(controle_mandats$sieges_2027)))
}


# ── 1. CHARGEMENT DES DONNÉES ────────────────────────────────

dict_vot <- read_csv("data/processed/votations_dict.csv") %>%
  filter(!duplicated(votation))

raw <- read_csv("data/processed/data_votations_vd.csv") %>%
  mutate(
    OUI_prop = OUI_perc    / 100,
    NON_prop = NON_perc    / 100,
    particip = particip_perc / 100,
    votation = str_replace(votation, "vdce_2026_raboud$", "vdce_2026_eag"),  # uniformiser les suffixes
  ) %>%
  left_join(dict_vot %>% select(votation, nom_complet, niveau, type),
            by = "votation")


# ── 2. TABLE DE CORRESPONDANCE DISTRICTS ─────────────────────
# L'attribution des sièges se fait au niveau des SOUS-DISTRICTS pour les
# trois districts divisés (Jura-Nord vaudois, Lausanne, Riviera).
# Le champ `district` du CSV mélange noms d'arrondissement et de
# sous-arrondissement ; `district_electoral` est l'unité de calcul HB.
#
# "Jura - Nord vaudois", "Lausanne" et "Riviera - Pays-d'Enhaut" dans le CSV
# regroupent les communes du sous-arrondissement principal (Yverdon,
# Lausanne-Ville, Riviera). Les sous-arrondissements minoritaires (La Vallée,
# Romanel, Pays-d'Enhaut) sont nommés explicitement dans le CSV.
#
# Les deux fichiers sont passés par norm_district() : sans cela la jointure
# échoue pour Broye-Vully et Lavaux-Oron (47 communes), ce que masquait
# auparavant le repli sur `district_csv`.

map_district <- pop_brut_2025 %>%
  distinct(district, sous_district, commune, num_ofs, population) %>%
  mutate(district_key = if_else(!is.na(sous_district), sous_district, district)) %>%
  left_join(vd_sieges %>% select(district_csv, district_electoral),
            by = c("district_key" = "district_csv")) %>%
  select(district_csv = district, sous_district_csv = sous_district,
         commune_csv = commune, num_ofs_csv = num_ofs,
         population_csv = population, district_electoral)

# La correspondance doit être totale : plus aucun repli silencieux.
if (anyNA(map_district$district_electoral)) {
  print(map_district %>% filter(is.na(district_electoral)) %>%
          distinct(district_csv, sous_district_csv))
  stop("Mapping district incomplet — clés non appariées avec vd_sieges_districts.csv")
}

raw <- raw %>%
  left_join(map_district %>% select(district_csv, sous_district_csv, num_ofs_csv,
                                     population_csv, district_electoral),
            by = c("code_ofs" = "num_ofs_csv"))

# Signaler les communes sans correspondance district
sans_mapping <- raw %>% filter(is.na(district_electoral)) %>%
  distinct(code_ofs)
if (nrow(sans_mapping) > 0) {
  warning("Communes sans mapping district (code_ofs) : ",
          paste(sans_mapping$code_ofs, collapse = ", "))
}


# ── 3. RÉFÉRENCE ÉLECTORALE 2022 ─────────────────────────────
# Part de gauche = Gauche (POP/EàG/sol.) + PS + Verts
# Le champ `Valables` est identique pour toutes les listes d'une même commune
# → il sert d'effectif de base.

elections <- raw %>% filter(type == "election")

effectif_total <- elections %>%
  filter(startsWith(votation, "gc_2022")) %>%
  distinct(Communes, code_ofs, Valables) %>%
  summarise(total_effectif = sum(Valables, na.rm = TRUE)) %>%
  pull(total_effectif)

effectifs_commune <- elections %>%
  filter(startsWith(votation, "gc_2022")) %>%
  distinct(Communes, code_ofs, district_csv, district_electoral, Valables) %>%
  rename(district = district_csv,
         effectif = Valables)

votes_listes <- elections %>%
  select(Communes, code_ofs, votation, OUI) %>%
  pivot_wider(names_from = votation, values_from = OUI,
              values_fill = 0)   # parti absent = 0 voix

# Résultat Raboud (EàG) au CE 2026. On récupère AUSSI les bulletins du CE 2026
# (`Valables`) : la part de Raboud doit se calculer sur sa propre base, pas sur
# l'effectif GC 2022. Le CE 2026 a compté ~1,6× plus de bulletins que le GC
# 2022 ; rapporter ses voix à la base 2022 gonflait sa part d'autant et
# transformait un écart de participation en « progression ».
# Les communes absentes du fichier ont bien 0 voix Raboud (le canton ne publie
# que les listes ayant obtenu des voix) — `raboud_voix` distingue malgré tout
# les communes où elle en a effectivement récolté.
raboud_2026 <- raw %>%
  filter(votation == "vdce_2026_eag") %>%
  distinct(code_ofs, raboud_oui = OUI, bulletins_ce_2026 = Valables)

# EàG n'a déposé de liste au GC 2022 que dans 7 arrondissements sur 13.
# Ailleurs, part_eag_2022 == 0 est un zéro STRUCTUREL (pas de liste), et non
# une contre-performance. Le distinguer est indispensable : sans ce drapeau,
# la régression de conversion (§7) mesure surtout « y avait-il une liste ».
districts_avec_liste <- raw %>%
  filter(votation == LISTE_EAG) %>%
  group_by(district_electoral) %>%
  summarise(liste_eag_2022 = sum(OUI, na.rm = TRUE) > 0, .groups = "drop")

ref_2022 <- effectifs_commune %>%
  left_join(votes_listes, by = c("Communes", "code_ofs")) %>%
  left_join(raboud_2026,  by = "code_ofs") %>%
  left_join(districts_avec_liste, by = "district_electoral") %>%
  mutate(
    # Part EàG : performance propre du parti, base de la régression
    votes_eag     = rowSums(across(all_of(LISTE_EAG)),na.rm = TRUE),
    part_eag_2022 = votes_eag / effectif,
    # Profil électoral gauche : mesure l'électorat favorable (EàG+PS+Verts)
    votes_gauche  = rowSums(across(all_of(LISTES_GAUCHE_PROFIL)), na.rm = TRUE),
    part_gauche_2022 = votes_gauche / effectif,
    # Réservoir PS + Verts : potentiel de report vers EàG
    part_ps_verts    = part_gauche_2022 - part_eag_2022,
    # Portée EàG au CE 2026 (Raboud), sur la base du CE 2026 lui-même
    raboud_voix      = coalesce(raboud_oui, 0),
    part_raboud_2026 = if_else(!is.na(bulletins_ce_2026) & bulletins_ce_2026 > 0,
                               raboud_voix / bulletins_ce_2026, 0),
    # Tendance : deux parts, chacune rapportée à sa propre base de bulletins
    tendance_raboud  = part_raboud_2026 - part_eag_2022
  ) %>%
  select(Communes, code_ofs, district, district_electoral,
         effectif, bulletins_ce_2026, liste_eag_2022,
         part_eag_2022, part_gauche_2022, part_ps_verts,
         raboud_voix, part_raboud_2026, tendance_raboud)

cat(sprintf("\nRéférence 2022 : %d communes — %d avec des voix Raboud (CE 2026), %d dans un arrondissement où EàG avait une liste en 2022\n",
            nrow(ref_2022), sum(ref_2022$raboud_voix > 0),
            sum(ref_2022$liste_eag_2022)))


# ── 4. MATRICE DE VOTATIONS POUR L'ACP ──────────────────────
# Exclure : contre-projets (_cp), questions subsidiaires (_sub)
# Conserver une observation par paquet thématique (initiative principale).
#
# L'ACP mesure le POSITIONNEMENT sur enjeux : elle ne retient que des scrutins
# d'objet (initiatives / lois). Le résultat Raboud au CE 2026 est une PERFORMANCE
# électorale, pas une prise de position — il est traité séparément (§7), comme
# la référence 2022, et n'entre pas dans l'axe gauche-droite.

# Scrutins d'objet UNIQUEMENT. Inclure les élections ici rendait l'analyse
# circulaire : `gc_2022_Gauche` (= part_eag_2022) et `vdce_2026_eag` entraient
# dans l'ACP, puis on régressait part_eag_2022 sur la Dim.1 qui les contenait.
votations_retenues <- dict_vot %>%
  filter(type %in% c("initiative", "loi"))

stopifnot(!any(str_detect(votations_retenues$votation, "^(gc_|vdce_|cc_)")))

# Orientation : valeur élevée = plus à gauche (§3.1 du README)
vot_orient <- raw %>%
  semi_join(votations_retenues, by = "votation") %>%
  mutate(
    score = case_when(
      positionnement_politique_oui %in% c("gauche", "centre-gauche") ~ OUI_prop,
      positionnement_politique_oui %in% c("droite", "extreme-droite", "centre") ~ NON_prop,
      TRUE ~ NA_real_
    )
  ) %>%
  select(Communes, code_ofs, district_electoral, votation, score)

# Passage en format large
mat_wide <- vot_orient %>%
  pivot_wider(names_from = votation, values_from = score)

# Identifier les votations retenues (colonnes dynamiques)
cols_vot <- setdiff(names(mat_wide),
                    c("Communes", "code_ofs", "district_electoral"))

cat(sprintf("Votations dans l'ACP (%d) : %s\n", length(cols_vot),
            paste(cols_vot, collapse = ", ")))

# Éliminer les communes avec trop de données manquantes
n_na_par_commune <- rowSums(is.na(mat_wide[cols_vot]))
mat_complete <- mat_wide %>%
  filter(n_na_par_commune / length(cols_vot) <= SEUIL_NA)

cat(sprintf("Communes retenues après filtre NA : %d / %d\n",
            nrow(mat_complete), nrow(mat_wide)))

# Imputation des valeurs manquantes restantes par la médiane de la votation
for (v in cols_vot) {
  med <- median(mat_complete[[v]], na.rm = TRUE)
  mat_complete[[v]][is.na(mat_complete[[v]])] <- med
}


# ── 5. ACP ───────────────────────────────────────────────────

mat_pca <- mat_complete %>%
  column_to_rownames("Communes") %>%
  select(all_of(cols_vot))

res_pca <- PCA(mat_pca, scale.unit = TRUE, graph = FALSE)

# Variance expliquée par axe
var_exp <- res_pca$eig[, 2]
cat(sprintf("\nVariance expliquée — Dim.1 : %.1f%%  Dim.2 : %.1f%%\n",
            var_exp[1], var_exp[2]))

# Loadings Dim.1 : tous doivent être positifs (= plus à gauche)
cat("\nLoadings Dim.1 :\n")
print(round(res_pca$var$coord[, 1], 3))

# Extraire le score de positionnement (coordonnée sur Dim.1)
dim1 <- res_pca$ind$coord[, 1] %>%
  enframe(name = "Communes", value = "dim1")

# Réorienter si la majorité des loadings sont négatifs
flip_dim1 <- mean(res_pca$var$coord[, 1]) < 0
if (flip_dim1) {
  dim1 <- dim1 %>% mutate(dim1 = -dim1)
  message("Dim.1 réorientée (loadings négatifs en moyenne)")
}

# Axes secondaires — clivages au-delà du gauche-droite (urbain/rural,
# ouverture/souveraineté). Servent à la typologie d'électorat (§10bis),
# ce à quoi Dim.2+ n'était jusqu'ici jamais employée.
n_dim_sup <- min(3L, ncol(res_pca$ind$coord))
dim_sup <- res_pca$ind$coord[, seq_len(n_dim_sup), drop = FALSE] %>%
  as.data.frame() %>%
  setNames(paste0("dim", seq_len(n_dim_sup), "_acp")) %>%
  rownames_to_column("Communes") %>%
  as_tibble()
if (flip_dim1) dim_sup <- dim_sup %>% mutate(dim1_acp = -dim1_acp)


# ── 6. ASSEMBLAGE DU TABLEAU COMMUNAL ────────────────────────

# Participation moyenne aux votations (proxy mobilisation).
# Stockée en FRACTION (0-1), comme toutes les autres colonnes `part_*` :
# `particip_perc` est en points de pourcentage dans le CSV source.
particip_moy <- raw %>%
  filter(type != "election") %>%
  group_by(Communes, code_ofs) %>%
  summarise(particip_moy = mean(particip_perc, na.rm = TRUE) / 100,
            .groups = "drop")

# Présence institutionnelle EàG (CC mars 2026) — 6 communes (README §3.2)
# Part de sièges EàG/da./S&E dans le conseil communal
cc_presence <- raw %>%
  filter(votation == "cc_2026_eag") %>%
  select(Communes, code_ofs, part_eag_cc = OUI_prop)

communes <- mat_complete %>%
  select(Communes, code_ofs, district_electoral) %>%
  left_join(dim1, by = "Communes") %>%
  left_join(dim_sup, by = "Communes") %>%
  left_join(
    ref_2022 %>% select(Communes, code_ofs, effectif, liste_eag_2022,
                       part_eag_2022, part_gauche_2022, part_ps_verts,
                       raboud_voix, part_raboud_2026, tendance_raboud),
    by = c("Communes", "code_ofs")
  ) %>%
  left_join(particip_moy %>% select(code_ofs, particip_moy), by = "code_ofs") %>%
  left_join(cc_presence  %>% select(code_ofs, part_eag_cc),  by = "code_ofs")


# ── 7. ÉCART DE CONVERSION (§4 du README) ───────────────────
# Régression de la part EàG sur le positionnement (dim1), pondérée
# par l'effectif → résidus = communes où EàG sous- ou sur-performe
# compte tenu du profil politique local.
#
# IMPORTANT : la régression n'est ajustée QUE sur les arrondissements où EàG
# avait effectivement une liste en 2022. Ailleurs, part_eag_2022 == 0 est un
# zéro structurel ; les inclure faisait mesurer au résidu « y avait-il une
# liste » plutôt que « EàG sous-convertit-elle son potentiel ».

communes_avec_liste <- communes %>% filter(liste_eag_2022)

m_ecart_2022_dim1 <- lm(part_eag_2022 ~ dim1, weights = effectif,
                        data = communes_avec_liste, na.action = na.exclude)

# Résidu prédit pour TOUTES les communes, mais marqué NA là où la liste
# n'existait pas : on ne peut pas parler de sous-conversion sans candidature.
communes <- communes %>%
  mutate(
    ecart = if_else(
      liste_eag_2022,
      part_eag_2022 - predict(m_ecart_2022_dim1, newdata = communes),
      NA_real_
    )
  )

cat(sprintf("\nR² positionnement → vote EàG 2022 (%d communes, arrondissements avec liste) : %.3f\n",
            nrow(communes_avec_liste), summary(m_ecart_2022_dim1)$r.squared))
cat(sprintf("  (%d communes sans liste EàG en 2022 — écart non défini, marge fondée sur Raboud + réservoir PS/Verts)\n",
            sum(!communes$liste_eag_2022, na.rm = TRUE)))

# Second point de référence : le CE 2026 (Raboud). Raboud était candidate dans
# tout le canton : les communes à 0 voix sont de vrais zéros et doivent rester
# dans l'ajustement — les exclure reviendrait à sélectionner sur la variable
# expliquée et à surestimer le lien positionnement → vote.
m_ecart_raboud <- lm(part_raboud_2026 ~ dim1, weights = effectif,
                     data = communes, na.action = na.exclude)
cat(sprintf("R² positionnement → vote Raboud (CE 2026, %d communes) : %.3f\n",
            nrow(communes), summary(m_ecart_raboud)$r.squared))
avec_voix <- communes$raboud_voix > 0
cat(sprintf("Part Raboud médiane (base CE 2026) : %.1f %% sur les 300 communes, %.1f %% sur les %d où elle a fait des voix\n",
            100 * median(communes$part_raboud_2026, na.rm = TRUE),
            100 * median(communes$part_raboud_2026[avec_voix], na.rm = TRUE),
            sum(avec_voix)))
cat(sprintf("Tendance médiane vs liste 2022 : %+.1f pp (communes avec voix Raboud)\n",
            100 * median(communes$tendance_raboud[avec_voix], na.rm = TRUE)))



# ── 7bis. COMPARAISON EàG 2022 ↔ RABOUD CE 2026 ─────────────
# Analyse dédiée : commune par commune, qu'est-ce qui sépare le vote de liste
# EàG/gauche au GC 2022 du vote Raboud au CE 2026 ?
#
# Les deux chiffres bruts ne sont PAS comparables tels quels : le CE 2026 a
# compté ~1,7× plus de bulletins que le GC 2022, et EàG n'avait de liste que
# dans 7 arrondissements sur 13. La comparaison est donc menée sur trois plans :
#
#   1. EN PART — chaque score rapporté à sa propre base de bulletins.
#      C'est la seule mesure honnête de « performance ».
#   2. EN VOIX À PARTICIPATION CONSTANTE — la part Raboud 2026 appliquée au
#      corps électoral du GC 2022. Répond à : « combien de voix la liste
#      aurait-elle faites en 2022 avec la performance de Raboud ? »
#   3. DÉCOMPOSITION de l'écart de voix brut en deux effets additifs :
#        voix26 − voix22 = part26 × (bull26 − bull22)   ← effet participation
#                        + (part26 − part22) × bull22   ← effet performance
#      L'identité est exacte (vérifiée par stopifnot ci-dessous). L'effet
#      participation n'est PAS un acquis politique : il s'évapore si le GC 2027
#      mobilise comme le GC 2022.
#
# Le statut isole les communes sans liste en 2022 : Raboud y révèle un
# potentiel, elle n'y « progresse » pas — il n'y a aucun point de départ.

# Écart en deçà duquel on parle de stabilité plutôt que de progression/recul.
SEUIL_STABLE_PP <- 0.01   # 1 point de pourcentage

voix_gc_2022 <- raw %>%
  filter(votation == LISTE_EAG) %>%
  distinct(code_ofs,
           voix_eag_2022  = OUI,
           bulletins_2022 = Valables)

voix_ce_2026 <- raw %>%
  filter(votation == "vdce_2026_eag") %>%
  distinct(code_ofs,
           voix_raboud_2026 = OUI,
           bulletins_2026   = Valables)

comparaison <- communes %>%
  select(Communes, code_ofs, district_electoral, liste_eag_2022,
         dim1, particip_moy) %>%
  left_join(voix_gc_2022, by = "code_ofs") %>%
  left_join(voix_ce_2026, by = "code_ofs") %>%
  mutate(
    across(c(voix_eag_2022, voix_raboud_2026), \(x) coalesce(x, 0)),
    # 1. les deux parts, chacune sur sa propre base
    part_eag_2022    = voix_eag_2022    / bulletins_2022,
    part_raboud_2026 = voix_raboud_2026 / bulletins_2026,
    diff_part        = part_raboud_2026 - part_eag_2022,
    ratio_part       = if_else(part_eag_2022 > 0,
                               part_raboud_2026 / part_eag_2022, NA_real_),
    # 2. voix à participation constante (base = corps électoral du GC 2022)
    voix_raboud_base_2022 = part_raboud_2026 * bulletins_2022,
    # 3. décomposition exacte de l'écart de voix brut
    diff_voix           = voix_raboud_2026 - voix_eag_2022,
    effet_participation = part_raboud_2026 * (bulletins_2026 - bulletins_2022),
    effet_performance   = diff_part * bulletins_2022,
    statut_comparaison = case_when(
      !liste_eag_2022              ~ "sans liste 2022",
      diff_part >  SEUIL_STABLE_PP ~ "progression",
      diff_part < -SEUIL_STABLE_PP ~ "recul",
      TRUE                         ~ "stable"
    )
  ) %>%
  select(Communes, code_ofs, district_electoral, liste_eag_2022,
         statut_comparaison,
         bulletins_2022, voix_eag_2022, part_eag_2022,
         bulletins_2026, voix_raboud_2026, part_raboud_2026,
         diff_part, ratio_part, voix_raboud_base_2022,
         diff_voix, effet_participation, effet_performance,
         dim1, particip_moy)

# L'identité de décomposition doit tenir à l'arrondi machine près.
stopifnot(max(abs(comparaison$diff_voix -
                    (comparaison$effet_participation +
                       comparaison$effet_performance))) < 1e-6)

# Même décomposition, agrégée par district électoral
comparaison_district <- comparaison %>%
  group_by(district_electoral, liste_eag_2022) %>%
  summarise(
    n_communes          = n(),
    bulletins_2022      = sum(bulletins_2022),
    bulletins_2026      = sum(bulletins_2026),
    voix_eag_2022       = sum(voix_eag_2022),
    voix_raboud_2026    = sum(voix_raboud_2026),
    effet_participation = sum(effet_participation),
    effet_performance   = sum(effet_performance),
    .groups = "drop"
  ) %>%
  mutate(
    part_eag_2022    = voix_eag_2022    / bulletins_2022,
    part_raboud_2026 = voix_raboud_2026 / bulletins_2026,
    diff_part        = part_raboud_2026 - part_eag_2022,
    diff_voix        = voix_raboud_2026 - voix_eag_2022,
    # Voix que Raboud aurait faites sur le corps électoral du GC 2022
    voix_raboud_base_2022 = part_raboud_2026 * bulletins_2022
  ) %>%
  arrange(desc(diff_part))

cat("\n\n════ COMPARAISON — liste EàG/gauche GC 2022 vs Raboud CE 2026 ════\n")

cat(sprintf("\nCanton (%d communes)\n", nrow(comparaison)))
cat(sprintf("  GC 2022    : %7s voix / %7s bulletins = %5.2f %%\n",
            format(sum(comparaison$voix_eag_2022), big.mark = " "),
            format(sum(comparaison$bulletins_2022), big.mark = " "),
            100 * sum(comparaison$voix_eag_2022) / sum(comparaison$bulletins_2022)))
cat(sprintf("  CE 2026    : %7s voix / %7s bulletins = %5.2f %%\n",
            format(sum(comparaison$voix_raboud_2026), big.mark = " "),
            format(sum(comparaison$bulletins_2026), big.mark = " "),
            100 * sum(comparaison$voix_raboud_2026) / sum(comparaison$bulletins_2026)))
cat(sprintf("  Écart brut : %+d voix, dont %+.0f d'effet participation (%+.0f %% de bulletins)\n",
            sum(comparaison$diff_voix), sum(comparaison$effet_participation),
            100 * (sum(comparaison$bulletins_2026) / sum(comparaison$bulletins_2022) - 1)))
cat(sprintf("               et %+.0f d'effet performance — le seul acquis politique.\n",
            sum(comparaison$effet_performance)))
cat(sprintf("  À participation 2022, Raboud aurait fait %s voix (contre %s à la liste).\n",
            format(round(sum(comparaison$voix_raboud_base_2022)), big.mark = " "),
            format(sum(comparaison$voix_eag_2022), big.mark = " ")))

cat("\nSelon qu'EàG avait ou non une liste en 2022\n")
print(comparaison %>%
        group_by(liste_eag_2022) %>%
        summarise(n         = n(),
                  part_2022 = sum(voix_eag_2022)    / sum(bulletins_2022),
                  part_2026 = sum(voix_raboud_2026) / sum(bulletins_2026),
                  voix_2026 = sum(voix_raboud_2026),
                  .groups   = "drop") %>%
        mutate(ecart_pp  = round(100 * (part_2026 - part_2022), 2),
               part_2022 = round(100 * part_2022, 2),
               part_2026 = round(100 * part_2026, 2)) %>%
        relocate(ecart_pp, .after = part_2026))
cat("  → là où une liste existait, l'écart mesure une performance ;\n")
cat("    ailleurs il part mécaniquement de 0 et mesure un potentiel révélé.\n")

cat("\nPar district électoral (trié par écart de part)\n")
print(comparaison_district %>%
        transmute(district_electoral, liste_2022 = liste_eag_2022, n_communes,
                  part_2022 = round(100 * part_eag_2022, 2),
                  part_2026 = round(100 * part_raboud_2026, 2),
                  ecart_pp  = round(100 * diff_part, 2),
                  voix_2022 = voix_eag_2022,
                  voix_2026 = voix_raboud_2026,
                  voix_2026_base_2022 = round(voix_raboud_base_2022),
                  eff_particip = round(effet_participation),
                  eff_perf     = round(effet_performance)),
      n = Inf)

cat(sprintf("\nRépartition des statuts (seuil de stabilité : ±%.0f pp)\n",
            100 * SEUIL_STABLE_PP))
print(table(comparaison$statut_comparaison))

afficher_comparaison <- function(df, titre, n = 12) {
  cat(sprintf("\n── %s ──\n", titre))
  df %>%
    transmute(Communes, district_electoral,
              part_2022 = round(100 * part_eag_2022, 2),
              part_2026 = round(100 * part_raboud_2026, 2),
              ecart_pp  = round(100 * diff_part, 2),
              voix_2022 = voix_eag_2022,
              voix_2026 = voix_raboud_2026,
              voix_2026_base_2022 = round(voix_raboud_base_2022),
              eff_perf  = round(effet_performance)) %>%
    slice_head(n = n) %>%
    print(n = n)
}

# Les classements ne portent que sur les communes comparables — celles d'un
# arrondissement où EàG avait une liste. Ailleurs, trier par écart revient à
# trier par part 2026, puisque le point de départ vaut 0 partout.
comparables <- comparaison %>% filter(liste_eag_2022)

afficher_comparaison(comparables %>% arrange(desc(diff_part)),
                     "PROGRESSIONS — Raboud au-dessus de la liste 2022")
afficher_comparaison(comparables %>% arrange(diff_part),
                     "RECULS — Raboud en dessous de la liste 2022")
afficher_comparaison(comparables %>% arrange(desc(effet_performance)),
                     "PLUS GROS GAINS EN VOIX (à participation 2022)")
afficher_comparaison(comparaison %>% filter(!liste_eag_2022) %>%
                       arrange(desc(part_raboud_2026)),
                     "POTENTIEL RÉVÉLÉ — meilleures parts là où il n'y avait pas de liste")

# ── 8. LEVIER SIÈGE PAR DISTRICT (§5-6 du README) ───────────
# Répartition des sièges selon la règle vaudoise réelle (LEDP art. 73 ss) :
# quorum 5 % → quotient ⌈voix admises / sièges⌉ → sièges pleins → plus forts
# restes. Les voix des 6 blocs sont toutes disponibles (leur somme égale les
# bulletins valables), la simulation est donc exacte et non plus approchée.

sieges_ref <- vd_sieges %>%
  distinct(district_electoral, sieges_2022, sieges_2027)

LISTES_GC <- dict_vot %>%
  filter(startsWith(votation, "gc_2022")) %>%
  pull(votation)

dist_votes <- raw %>%
  filter(type == "election", startsWith(votation, "gc_2022")) %>%
  group_by(district_electoral, votation) %>%
  summarise(OUI = sum(OUI, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = votation, values_from = OUI, values_fill = 0) %>%
  mutate(
    total_valables = rowSums(across(all_of(LISTES_GC))),
    votes_eag      = rowSums(across(all_of(LISTE_EAG))),
    votes_gauche   = rowSums(across(all_of(LISTES_GAUCHE_PROFIL)))
  ) %>%
  left_join(sieges_ref, by = "district_electoral")

# Voix Raboud (CE 2026) agrégées par district — scénario « portée EàG démontrée ».
# Le CE 2026 ayant compté plus de bulletins que le GC 2022, on transpose la PART
# de Raboud sur le corps électoral du GC plutôt que ses voix brutes.
raboud_dist <- raw %>%
  filter(votation == "vdce_2026_eag") %>%
  group_by(district_electoral) %>%
  summarise(voix_raboud_brut = sum(OUI, na.rm = TRUE),
            bulletins_ce     = sum(Valables, na.rm = TRUE),
            .groups = "drop")

dist_base <- dist_votes %>%
  left_join(raboud_dist, by = "district_electoral") %>%
  mutate(
    part_raboud_dist = if_else(coalesce(bulletins_ce, 0) > 0,
                               voix_raboud_brut / bulletins_ce, 0),
    # Part Raboud ramenée au corps électoral du GC (comparable aux voix 2022)
    votes_raboud     = round(part_raboud_dist * total_valables),
    part_eag_dist    = votes_eag / total_valables,
    part_gauche_dist = votes_gauche / total_valables
  )

# Application de la règle légale, district par district.
mat_voix <- as.matrix(dist_base[, LISTES_GC])
rownames(mat_voix) <- dist_base$district_electoral

resultats_ledp <- map_dfr(seq_len(nrow(dist_base)), function(i) {
  v   <- mat_voix[i, ]
  s22 <- dist_base$sieges_2022[i]
  s27 <- dist_base$sieges_2027[i]

  scenario <- v
  scenario[[LISTE_EAG]] <- dist_base$votes_raboud[i]

  tibble(
    district_electoral = dist_base$district_electoral[i],
    sieges_eag_22      = repartir_ledp(v, s22)[[LISTE_EAG]],
    sieges_eag_27      = repartir_ledp(v, s27)[[LISTE_EAG]],
    sieges_eag_raboud  = repartir_ledp(scenario, s27)[[LISTE_EAG]],
    voix_quorum        = voix_pour_quorum(v),
    voix_siege         = voix_pour_siege_suivant(v, s27)
  )
})

dist_levier <- dist_base %>%
  left_join(resultats_ledp, by = "district_electoral") %>%
  mutate(
    part_quorum    = voix_quorum / total_valables,
    part_manquante = voix_siege  / total_valables,
    levier         = 1 / voix_siege,
    # Statut INFORMATIF — il ne filtre plus aucune commune (§9).
    statut_district = case_when(
      sieges_eag_22 > 0                   ~ "consolidation",
      part_manquante <= SEUIL_ATTEIGNABLE ~ "offensive",
      TRUE                                ~ "conquete_longue"
    )
  ) %>%
  select(district_electoral, total_valables, votes_eag, votes_gauche, votes_raboud,
         part_eag_dist, part_gauche_dist, part_raboud_dist,
         sieges_2022, sieges_2027, sieges_eag_22, sieges_eag_27, sieges_eag_raboud,
         voix_quorum, part_quorum, voix_siege, part_manquante, levier,
         statut_district)

cat("\n── Analyse par district electoral (règle LEDP) ──\n")
print(dist_levier %>%
        arrange(voix_siege) %>%
        select(district_electoral, sieges_2027, votes_eag, part_eag_dist,
               voix_quorum, voix_siege, sieges_eag_22, sieges_eag_raboud,
               statut_district),
      n = Inf)

cat(sprintf("\n── Classification (%d districts, aucune exclusion) ──\n", nrow(dist_levier)))
for (s in c("consolidation", "offensive", "conquete_longue")) {
  cat(sprintf("  %-16s : %d\n", s, sum(dist_levier$statut_district == s)))
}
cat(sprintf("  Sièges EàG estimés 2027 à voix 2022 : %d  |  scénario Raboud : %d\n",
            sum(dist_levier$sieges_eag_27), sum(dist_levier$sieges_eag_raboud)))


# ── 9. SCORE DE PRIORITÉ (§6-7 du README) ────────────────────
# Les sièges sont attribués par district électoral : la priorité est d'abord
# évaluée au niveau du district (couche 1 — quel arrondissement mérite
# l'effort), puis répartie entre ses communes (couche 2).
#
# AUCUNE commune n'est exclue. Les 300 sont scorées et classées de façon
# continue ; `statut_district` sert à lire le classement, pas à le tronquer.
# L'ancienne exclusion « hors_portee » reposait sur part_manquante > 12 %, qui
# valait mécaniquement 1/(sièges+1) dès qu'EàG avait ~0 voix : elle écartait
# les petits arrondissements, pas les électorats hostiles.

communes <- communes %>%
  left_join(
    dist_levier %>% select(district_electoral, levier, part_manquante,
                           voix_quorum, voix_siege, statut_district),
    by = "district_electoral"
  ) %>%
  mutate(
    # (a) Sous-conversion du positionnement — définie seulement là où EàG
    #     avait une liste en 2022 ; NA ailleurs (cf. §7).
    marge_ecart  = if_else(liste_eag_2022, pmax(0, -ecart), NA_real_),
    # (b) Portée démontrée par Raboud au CE 2026, chaque part sur sa propre
    #     base de bulletins, escomptée du panachage (RABOUD_ESCOMPTE, §0).
    marge_raboud = pmax(0, coalesce(tendance_raboud, 0)) * RABOUD_ESCOMPTE,
    # (c) Réservoir PS/Verts non capté. Seul signal disponible là où EàG
    #     n'a jamais présenté de liste ; fortement escompté, car convertir un
    #     électeur PS ou Vert est plus difficile que mobiliser un sympathisant.
    marge_reservoir = part_ps_verts * 0.10,
    # Marge de progression = le plus fort des signaux disponibles
    marge_progression = pmax(coalesce(marge_ecart, 0), marge_raboud,
                             marge_reservoir, na.rm = TRUE),
    source_marge = case_when(
      !is.na(marge_ecart) & marge_ecart >= pmax(marge_raboud, marge_reservoir) ~ "conversion_2022",
      marge_raboud >= marge_reservoir                                          ~ "raboud_2026",
      TRUE                                                                     ~ "reservoir_ps_verts"
    ),
    # Consolidation : défendre les votes acquis a la même valeur que progresser
    potentiel_commune = case_when(
      statut_district == "consolidation" ~ pmax(marge_progression, part_eag_2022),
      TRUE                               ~ marge_progression
    ) * effectif
  )

# Layer 1 — score de district : potentiel total du district × levier siège
district_score <- communes %>%
  group_by(district_electoral) %>%
  summarise(potentiel_district = sum(potentiel_commune, na.rm = TRUE),
            effectif_district  = sum(effectif, na.rm = TRUE),
            # Marge moyenne d'un électeur du district (pondérée par l'effectif),
            # sans l'ajustement de consolidation — cf. score_marge ci-dessous.
            marge_district     = sum(marge_progression * effectif, na.rm = TRUE) /
                                 effectif_district,
            .groups = "drop") %>%
  left_join(dist_levier %>% select(district_electoral, levier), by = "district_electoral") %>%
  mutate(score_district       = potentiel_district * levier,
         score_marge_district = marge_district * levier * 1000)

# Layer 2 — score communal : part du potentiel de district captée par la commune
communes <- communes %>%
  left_join(
    district_score %>% select(district_electoral, potentiel_district, score_district),
    by = "district_electoral"
  ) %>%
  mutate(
    # Score VOLUME : ce que la commune entière peut apporter au siège suivant,
    # en multiples des voix manquantes. Dominé par la taille de la commune —
    # c'est voulu : il dit où concentrer l'effort.
    score_priorite      = potentiel_commune * levier,
    # Score MARGE : le même rendement rapporté à 1000 électeurs, donc SANS
    # l'effectif — il dit où chaque contact rapporte le plus. Part des voix
    # manquantes pour le siège suivant couverte si l'on convertit la marge sur
    # 1000 électeurs. Pas d'ajustement de consolidation : il mesure la marge de
    # PROGRESSION, pas la défense des votes acquis (qui ferait remonter les
    # bastions sur leur seul score 2022). Hors consolidation :
    # score_priorite == score_marge × effectif / 1000.
    score_marge         = marge_progression * levier * 1000,
    part_score_district = if_else(potentiel_district > 0,
                                   potentiel_commune / potentiel_district, 0),
    # Contribution attendue de la commune à l'objectif du district, en voix
    voix_a_trouver      = ceiling(part_score_district * voix_siege),
    voix_quorum_commune = ceiling(part_score_district * voix_quorum)
  )


# ── 10. MOBILISATION VS PERSUASION (§7 du README) ────────────

# Seuils médians. Chaque commune reçoit un profil : il n'y a plus de catégorie
# « autre » fourre-tout, qui absorbait 42 % des communes et les renvoyait à
# « ressources à réallouer ailleurs ».
seuil_dim1      <- median(communes$dim1,          na.rm = TRUE)
seuil_particip  <- median(communes$particip_moy,  na.rm = TRUE)
seuil_reservoir <- median(communes$part_ps_verts, na.rm = TRUE)

# Seuil Raboud : médiane parmi les seules communes où elle a effectivement
# récolté des voix. Calculée sur les 300, elle vaudrait 0 (147 communes à
# tendance nulle) et la branche « persuasion » avalerait tout le canton.
seuil_raboud <- median(communes$tendance_raboud[communes$raboud_voix > 0],
                       na.rm = TRUE)
cat(sprintf("\nSeuil Raboud (médiane des %d communes avec voix) : %+.1f pp\n",
            sum(communes$raboud_voix > 0), 100 * seuil_raboud))

communes <- communes %>%
  mutate(
    profil = case_when(
      # 1. Bastions : siège à défendre, ou présence institutionnelle locale
      statut_district == "consolidation" & dim1 > seuil_dim1 ~ "consolidation",
      !is.na(part_eag_cc) & part_eag_cc > 0                  ~ "consolidation",
      # 2. Électorat acquis qui ne se déplace pas
      dim1 > seuil_dim1 & particip_moy < seuil_particip      ~ "mobilisation",
      # 3. Raboud y a démontré un soutien au-dessus de la médiane des communes
      #    où elle a fait des voix : des électeurs sont convertibles, quel que
      #    soit le positionnement sur enjeux.
      raboud_voix > 0 & tendance_raboud >= seuil_raboud      ~ "persuasion",
      # 4. Sous-conversion avérée du positionnement local
      !is.na(ecart) & ecart < 0                              ~ "persuasion",
      # 5. Gros réservoir PS/Verts encore inexploité : cible de report
      part_ps_verts >= seuil_reservoir                       ~ "report_gauche",
      # 6. Reste : terrain difficile, mais jamais « à abandonner » —
      #    implantation de long terme (présence, liste, visibilité).
      TRUE                                                   ~ "implantation"
    )
  )

cat(sprintf("\nRépartition des profils (%d communes, aucune exclue) :\n", nrow(communes)))
print(table(communes$profil))


# ── 10bis. TYPOLOGIE D'ÉLECTORAT ─────────────────────────────
# Le profil dit QUOI FAIRE ; la typologie dit À QUI ON PARLE. Elle exploite
# les axes secondaires de l'ACP (clivages au-delà du gauche-droite), le
# réservoir PS/Verts, la participation et la portée démontrée par Raboud —
# autant de signaux jusqu'ici calculés mais jamais utilisés pour segmenter.

# La typologie repose sur les traits STRUCTURELS de l'électorat : axes de
# positionnement, réservoir PS/Verts, participation. `part_raboud_2026` en est
# volontairement exclue — valant 0 dans 147 communes, elle se comporterait en
# indicateur binaire « Raboud a-t-elle fait des voix ici » et dominerait la
# classification. Elle sert à DÉCRIRE les classes obtenues, pas à les former.
vars_typo <- c("dim1_acp", "dim2_acp", "dim3_acp",
               "part_ps_verts", "particip_moy")
vars_typo <- intersect(vars_typo, names(communes))

mat_typo <- communes %>%
  select(Communes, all_of(vars_typo)) %>%
  column_to_rownames("Communes") %>%
  mutate(across(everything(), \(x) replace_na(x, median(x, na.rm = TRUE)))) %>%
  scale()

# ACP de travail puis classification ascendante hiérarchique (FactoMineR).
# nb.clust = -1 laisse HCPC couper l'arbre au meilleur saut d'inertie.
set.seed(2027)
res_typo <- HCPC(PCA(as.data.frame(mat_typo), scale.unit = FALSE, ncp = 5,
                     graph = FALSE),
                 nb.clust = -1, min = 4, max = 6, graph = FALSE)

communes <- communes %>%
  left_join(
    tibble(Communes = rownames(res_typo$data.clust),
           cluster  = as.integer(as.character(res_typo$data.clust$clust))),
    by = "Communes"
  )

# Nommer chaque classe d'après son centroïde plutôt que par un numéro.
profil_clusters <- communes %>%
  group_by(cluster) %>%
  summarise(
    n            = n(),
    gauche       = mean(dim1,             na.rm = TRUE),
    reservoir    = mean(part_ps_verts,    na.rm = TRUE),
    particip     = mean(particip_moy,     na.rm = TRUE),
    raboud       = mean(part_raboud_2026, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Nommage déterministe, du plus à gauche au plus à droite. Les seuils sont
  # pris sur la distribution des COMMUNES (et non sur les cinq centroïdes),
  # pour ne pas transformer un écart de participation d'un demi-point en
  # étiquette « mobilisé » vs « abstentionniste ».
  arrange(desc(gauche)) %>%
  mutate(
    rang_gauche = row_number(),
    bloc = case_when(
      gauche >  quantile(communes$dim1, 0.80, na.rm = TRUE) ~ "Gauche marquée",
      gauche >  quantile(communes$dim1, 0.55, na.rm = TRUE) ~ "Gauche",
      gauche >  quantile(communes$dim1, 0.30, na.rm = TRUE) ~ "Centre",
      gauche >  quantile(communes$dim1, 0.10, na.rm = TRUE) ~ "Droite",
      TRUE                                                  ~ "Droite marquée"
    ),
    # Trait de participation revendiqué seulement au-delà de ±2,5 points
    ecart_particip = particip - median(communes$particip_moy, na.rm = TRUE),
    trait = case_when(
      ecart_particip >  0.025 ~ " mobilisé",
      ecart_particip < -0.025 ~ " abstentionniste",
      TRUE                    ~ ""
    ),
    type_electorat = sprintf("%d. %s%s", rang_gauche, bloc, trait),
    a_gauche = startsWith(bloc, "Gauche"),
    message_cle = case_when(
      a_gauche & trait == " abstentionniste" ~
        "Électorat acquis qui ne se déplace pas — priorité absolue au « faire voter »",
      a_gauche & raboud >= median(raboud) ~
        "Gauche implantée où EàG a déjà fait ses preuves — incarner et consolider",
      a_gauche ~
        "Gauche forte mais captée par PS/Verts — différencier la ligne EàG",
      bloc == "Centre" & raboud >= median(raboud) ~
        "Commune disputée où EàG perce déjà — porte-à-porte et présence publique",
      bloc == "Centre" ~
        "Commune disputée, réservoir réel — thèmes sociaux concrets, pas d'étiquette",
      reservoir >= median(reservoir) ~
        "Minorité de gauche réelle en terrain adverse — structurer un noyau militant",
      TRUE ~
        "Terrain difficile — présence, visibilité, et d'abord déposer une liste"
    )
  )

communes <- communes %>%
  left_join(profil_clusters %>% select(cluster, type_electorat, message_cle),
            by = "cluster")

cat(sprintf("\nTypologie d'électorat — %d classes :\n", nrow(profil_clusters)))
print(profil_clusters %>%
        mutate(across(where(is.double), \(x) round(x, 3))) %>%
        select(cluster, n, type_electorat, gauche, reservoir, particip, raboud))


# ── 11. SORTIES ──────────────────────────────────────────────

dist_levier <- dist_levier %>%
  left_join(
    district_score %>% select(district_electoral, potentiel_district, score_district,
                              effectif_district, marge_district, score_marge_district),
    by = "district_electoral"
  )

stopifnot(
  nrow(communes) == nrow(ref_2022),          # aucune commune perdue
  !anyNA(communes$score_priorite),           # ni score manquant
  !anyNA(communes$score_marge),
  !any(communes$profil == "autre")           # ni catégorie fourre-tout
)

write_csv(communes,    "data/processed/communes_scores.csv")
write_csv(dist_levier, "data/processed/districts_levier.csv")
write_csv(profil_clusters, "data/processed/types_electorat.csv")

# Comparaison 2022 ↔ 2026 (§7bis) — table communale et agrégat par district
write_csv(comparaison,          "data/processed/comparaison_eag_raboud.csv")
write_csv(comparaison_district, "data/processed/comparaison_eag_raboud_districts.csv")

# Persister l'ACP pour le dashboard : app.R la recalculait avec un jeu de
# variables et un SEUIL_NA différents, si bien que le biplot et le graphique
# d'écart affichaient deux axes distincts sous le même nom.
saveRDS(
  list(
    ind_coord = res_pca$ind$coord,
    var_coord = res_pca$var$coord,
    var_contrib = res_pca$var$contrib,
    eig       = res_pca$eig,
    flip      = flip_dim1,
    cols_vot  = cols_vot
  ),
  "data/processed/acp_positionnement.rds"
)

# L'ancien découpage « hors portée » n'existe plus : toutes les communes sont
# scorées. On retire le fichier résiduel pour éviter qu'app.R ne le relise.
if (file.exists("data/processed/communes_hors_portee.csv")) {
  invisible(file.remove("data/processed/communes_hors_portee.csv"))
}

# Top 20 par profil
afficher_top <- function(df, profil_cible, n = 20) {
  cat(sprintf("\n── TOP %d — %s ──\n", n, toupper(profil_cible)))
  df %>%
    filter(profil == profil_cible) %>%
    arrange(desc(score_priorite)) %>%
    select(Communes, district_electoral, statut_district, type_electorat,
           dim1, part_eag_2022, part_raboud_2026, part_ps_verts,
           source_marge, particip_moy, effectif, voix_a_trouver, score_priorite,
           score_marge) %>%
    mutate(across(where(is.double), \(x) round(x, 4))) %>%
    slice_head(n = n) %>%
    print(n = n)
}

for (p in names(sort(table(communes$profil), decreasing = TRUE))) {
  afficher_top(communes, p)
}

# Classement par marge, tous profils confondus. Les très petites communes sont
# écartées de l'affichage : sur quelques dizaines de bulletins, la part Raboud
# est trop bruitée pour qu'un rang ait un sens.
cat("\n── TOP 20 — SCORE MARGE (sans effectif, communes ≥ 100 électeurs) ──\n")
communes %>%
  filter(effectif >= 100) %>%
  arrange(desc(score_marge)) %>%
  select(Communes, district_electoral, profil, marge_progression, source_marge,
         voix_siege, effectif, score_marge, score_priorite) %>%
  mutate(across(where(is.double), \(x) round(x, 4))) %>%
  slice_head(n = 20) %>%
  print(n = 20)

# ── Graphiques ───────────────────────────────────────────────

# Biplot ACP
p_biplot <- fviz_pca_biplot(
  res_pca,
  repel     = TRUE,
  col.var   = "contrib",
  gradient.cols = c("#2166ac", "#f7f7f7", "#d6604d"),
  title     = "ACP — Positionnement politique des communes vaudoises"
)

# Scatter positionnement vs vote gauche (avec résidus colorés)
p_ecart <- ggplot(communes, aes(x = dim1, y = part_eag_2022)) +
  geom_point(aes(colour = ecart, size = effectif), alpha = 0.7) +
  geom_smooth(aes(weight = effectif), method = "lm", se = TRUE,
              colour = "grey30", linewidth = 0.8) +
  scale_colour_gradient2(
    low = "#d6604d", mid = "white", high = "#2166ac",
    midpoint = 0, name = "Écart\n(résidu)"
  ) +
  scale_size_continuous(range = c(1, 8), name = "Effectif") +
  labs(
    x     = "Positionnement (Dim.1 ACP — gauche →)",
    y     = "Part EàG 2022",
    title = "Écart entre positionnement et vote effectif"
  ) +
  theme_minimal(base_size = 12)

# Score de priorité par district
p_scores <- communes %>%
  group_by(district_electoral, profil) %>%
  summarise(score_total = sum(score_priorite, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(district_electoral, score_total),
             y = score_total, fill = profil)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = PROFIL_PAL) +
  labs(x = NULL, y = "Score de priorité agrégé",
       title = "Potentiel par district et profil d'action",
       fill = NULL) +
  theme_minimal(base_size = 12)

# Comparaison 2022 ↔ 2026 (§7bis) — la diagonale est la seule référence qui
# compte : au-dessus, Raboud fait mieux que la liste ; en dessous, moins bien.
p_comparaison <- ggplot(comparaison,
                        aes(x = part_eag_2022, y = part_raboud_2026)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
  geom_point(aes(colour = statut_comparaison, size = bulletins_2022), alpha = 0.75) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = STATUT_COMPARAISON_PAL) +
  scale_size_continuous(range = c(1, 8), name = "Bulletins 2022") +
  labs(x = "Part liste EàG/gauche — GC 2022",
       y = "Part Raboud — CE 2026",
       colour = NULL,
       title = "Vote EàG 2022 vs Raboud CE 2026, par commune",
       subtitle = "Chaque part sur sa propre base de bulletins ; diagonale = performance identique") +
  theme_minimal(base_size = 12)

# D'où vient l'écart de voix : mobilisation supplémentaire ou performance ?
p_decompo <- comparaison_district %>%
  select(district_electoral, effet_participation, effet_performance) %>%
  pivot_longer(-district_electoral, names_to = "effet", values_to = "voix") %>%
  mutate(effet = recode(effet,
                        effet_participation = "Effet participation",
                        effet_performance   = "Effet performance")) %>%
  ggplot(aes(x = reorder(district_electoral, voix, sum), y = voix, fill = effet)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Effet participation" = "#9ecae1",
                               "Effet performance"   = "#c0392b")) +
  labs(x = NULL, y = "Voix", fill = NULL,
       title = "Écart de voix 2022 → 2026, décomposé",
       subtitle = "Seul l'effet performance survit à un retour à la participation de 2022") +
  theme_minimal(base_size = 12)

cat("\nFichiers écrits dans data/processed/\n")
cat("  communes_scores.csv\n  districts_levier.csv\n")
cat("  types_electorat.csv\n  acp_positionnement.rds\n")
cat("  comparaison_eag_raboud.csv\n  comparaison_eag_raboud_districts.csv\n")
