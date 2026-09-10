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

# Part maximale de votes manquants (sur bulletins valables) pour qu'un district
# soit considéré atteignable. Au-delà, les ressources sont mieux investies ailleurs.
SEUIL_ATTEIGNABLE <- 0.12

# Résultat Raboud Sidorenko (EàG) au Conseil d'État, mars 2026 — indicateur
# le plus récent et le plus direct de la portée électorale d'EàG (§3.2, §4 du README).
# L'élection au CE est majoritaire avec panachage : le score de Raboud inclut des
# voix PS/Verts qui ne se reporteront pas forcément sur une liste EàG au GC.
# RABOUD_ESCOMPTE escompte ce « plafond » avant de l'injecter dans la marge de
# progression (1 = aucun escompte, 0 = on ignore Raboud).
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

# Données de population 31.12.2025 (source : État de Vaud)
pop_2025 <- read_csv("data/processed/vd_pop_2025.csv") %>%
  group_by(district) %>%
  summarise(pop = sum(population), .groups = "drop")


pop_sub_2025 <- read_csv("data/processed/vd_pop_2025.csv") %>%
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
# Lausanne-Ville, Vevey). Les sous-arrondissements minoritaires (La Vallée,
# Romanel, Pays-d'Enhaut) sont nommés explicitement dans le CSV.

vd_sieges <- read_csv("data/processed/vd_sieges_districts.csv")

map_district <- read_csv("data/processed/vd_pop_2025.csv") %>%
  distinct(district, sous_district, commune, num_ofs, population) %>%
  mutate(district_key = if_else(!is.na(sous_district), sous_district, district)) %>%
  left_join(vd_sieges %>% select(district_csv, district_electoral),
            by = c("district_key" = "district_csv")) %>%
  select(district_csv = district, sous_district_csv = sous_district,
         commune_csv = commune, num_ofs_csv = num_ofs,
         population_csv = population, district_electoral)

raw <- raw %>%
  left_join(map_district %>% select(district_csv, sous_district_csv, num_ofs_csv,
                                     population_csv, district_electoral),
            by = c("code_ofs" = "num_ofs_csv"))%>%
  mutate(district_electoral = if_else(is.na(district_electoral), district_csv, district_electoral))

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

# Résultat Raboud (EàG) au CE 2026 : voix par commune sur la même base
# d'effectif que 2022. Liste da. absente → 0 voix (vrai zéro, README §3.2) ;
# `raboud_couvert` garde la trace des 153 communes réellement contestées.
raboud_2026 <- raw %>%
  filter(votation == "vdce_2026_eag") %>%
  distinct(code_ofs, raboud_oui = OUI)

ref_2022 <- effectifs_commune %>%
  left_join(votes_listes, by = c("Communes", "code_ofs")) %>%
  left_join(raboud_2026,  by = "code_ofs") %>%
  mutate(
    # Part EàG : performance propre du parti, base de la régression
    votes_eag     = rowSums(across(all_of(LISTE_EAG)),na.rm = TRUE),
    part_eag_2022 = votes_eag / effectif,
    # Profil électoral gauche : mesure l'électorat favorable (EàG+PS+Verts)
    votes_gauche  = rowSums(across(all_of(LISTES_GAUCHE_PROFIL)), na.rm = TRUE),
    part_gauche_2022 = votes_gauche / effectif,
    # Réservoir PS + Verts : potentiel de report vers EàG
    part_ps_verts    = part_gauche_2022 - part_eag_2022,
    # Portée EàG au CE 2026 (Raboud) et tendance depuis 2022
    raboud_couvert   = !is.na(raboud_oui),
    part_raboud_2026 = coalesce(raboud_oui, 0) / effectif,
    tendance_raboud  = part_raboud_2026 - part_eag_2022
  ) %>%
  select(Communes, code_ofs, district, district_electoral,
         effectif, part_eag_2022, part_gauche_2022, part_ps_verts,
         raboud_couvert, part_raboud_2026, tendance_raboud)

cat(sprintf("\nRéférence 2022 : %d communes (dont %d avec résultat Raboud CE 2026)\n",
            nrow(ref_2022), sum(ref_2022$raboud_couvert)))


# ── 4. MATRICE DE VOTATIONS POUR L'ACP ──────────────────────
# Exclure : contre-projets (_cp), questions subsidiaires (_sub)
# Conserver une observation par paquet thématique (initiative principale).
#
# L'ACP mesure le POSITIONNEMENT sur enjeux : elle ne retient que des scrutins
# d'objet (initiatives / lois). Le résultat Raboud au CE 2026 est une PERFORMANCE
# électorale, pas une prise de position — il est traité séparément (§7), comme
# la référence 2022, et n'entre pas dans l'axe gauche-droite.

votations_retenues <- dict_vot %>%
  filter(type %in% c("initiative", "loi"))

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
if (mean(res_pca$var$coord[, 1]) < 0) {
  dim1 <- dim1 %>% mutate(dim1 = -dim1)
  message("Dim.1 réorientée (loadings négatifs en moyenne)")
}


# ── 6. ASSEMBLAGE DU TABLEAU COMMUNAL ────────────────────────

# Participation moyenne aux votations (proxy mobilisation)
# Dénominateur : effectif 2022 (Bulletins_rentres à la date la plus récente)
particip_moy <- raw %>%
  filter(type != "election") %>%
  left_join(ref_2022 %>% select(Communes, effectif), by = "Communes") %>%
  mutate(taux_particip = Bulletins_rentres / effectif) %>%
  group_by(Communes, code_ofs) %>%
  summarise(particip_moy = mean(particip_perc, na.rm = TRUE), .groups = "drop")

# Présence institutionnelle EàG (CC mars 2026) — 6 communes (README §3.2)
# Part de sièges EàG/da./S&E dans le conseil communal
cc_presence <- raw %>%
  filter(votation == "cc_2026_eag") %>%
  select(Communes, code_ofs, part_eag_cc = OUI_prop)

communes <- mat_complete %>%
  select(Communes, code_ofs, district_electoral) %>%
  left_join(dim1, by = "Communes") %>%
  left_join(
    ref_2022 %>% select(Communes, code_ofs, effectif,
                       part_eag_2022, part_gauche_2022, part_ps_verts,
                       raboud_couvert, part_raboud_2026, tendance_raboud),
    by = c("Communes", "code_ofs")
  ) %>%
  left_join(particip_moy, by = c("Communes", "code_ofs")) %>%
  left_join(cc_presence,  by = c("Communes", "code_ofs"))


# ── 7. ÉCART DE CONVERSION (§4 du README) ───────────────────
# Régression de la part EàG sur le positionnement (dim1), pondérée
# par l'effectif → résidus = communes où EàG sous- ou sur-performe
# compte tenu du profil politique local.

m_ecart_2022_dim1 <- lm(part_eag_2022 ~ dim1, weights = effectif, data = communes,
              na.action = na.exclude)
communes$ecart <- resid(m_ecart_2022_dim1)   # na.exclude conserve les NA en position

cat(sprintf("\nR² positionnement → vote EàG (2022) : %.3f\n",
            summary(m_ecart_2022_dim1)$r.squared))

# Second point de référence : le CE 2026 (Raboud). Même régression sur les
# communes réellement contestées → où EàG, via sa candidate la plus forte,
# dépasse déjà nettement son score de liste 2022 (marge démontrée, §9).
m_ecart_raboud <- lm(part_raboud_2026 ~ dim1, weights = effectif,
                     data = communes %>% filter(raboud_couvert),
                     na.action = na.exclude)
cat(sprintf("R² positionnement → vote Raboud (CE 2026, %d communes) : %.3f\n",
            sum(communes$raboud_couvert, na.rm = TRUE),
            summary(m_ecart_raboud)$r.squared))
cat(sprintf("Tendance Raboud − EàG 2022 (médiane, communes contestées) : %+.1f pp\n",
            100 * median(communes$tendance_raboud[communes$raboud_couvert], na.rm = TRUE)))


# ── 8. LEVIER SIÈGE PAR DISTRICT (§5 du README) ─────────────
# Attribution des sièges par Hagenbach-Bischoff, simplifiée à la liste gauche.
# NB : le calcul exact requiert les voix de toutes les listes. Ici on estime
# le « votes manquants pour le prochain siège » à partir du quota global.

sieges_ref <- vd_sieges %>%
  distinct(district_electoral, sieges_2022, sieges_2027)

dist_votes <- raw %>%
  filter(type == "election", startsWith(votation, "gc_2022")) %>%
  group_by(district_electoral, votation) %>%
  summarise(OUI = sum(OUI, na.rm = TRUE),
            Valables = sum(Valables, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = votation, values_from = OUI) %>%
  mutate(
    total_valables = Valables,
    votes_eag    = rowSums(across(all_of(LISTE_EAG)), na.rm = TRUE),
    votes_gauche   = rowSums(across(all_of(LISTES_GAUCHE_PROFIL)), na.rm = TRUE)
  ) %>%
  left_join(sieges_ref, by = "district_electoral")

# Voix Raboud (CE 2026) agrégées par district — scénario « portée EàG démontrée »
raboud_dist <- raw %>%
  filter(votation == "vdce_2026_eag") %>%
  group_by(district_electoral) %>%
  summarise(votes_raboud = sum(OUI, na.rm = TRUE), .groups = "drop")

dist_levier <- dist_votes %>%
  left_join(raboud_dist, by = "district_electoral") %>%
  mutate(
    votes_raboud     = coalesce(votes_raboud, 0),
    part_eag_dist    = votes_eag / total_valables,
    part_gauche_dist = votes_gauche / total_valables,
    part_raboud_dist = votes_raboud / total_valables,
    # Quota HB par liste EàG (sièges attribués par liste, pas en bloc)
    quota_hb         = total_valables / (sieges_2027 + 1),
    quota_hb_22      = total_valables / (sieges_2022 + 1),
    sieges_eag_22    = floor(votes_eag / quota_hb_22),
    sieges_eag_27    = floor(votes_eag / quota_hb),
    # Scénario : sièges qu'EàG obtiendrait à un niveau de soutien « Raboud CE 2026 »
    sieges_eag_raboud = floor(votes_raboud / quota_hb),
    voix_manquantes  = quota_hb * (sieges_eag_27 + 1) - votes_eag,
    part_manquante   = voix_manquantes / total_valables,
    levier           = 1 / voix_manquantes,
    statut_district  = case_when(
      sieges_eag_22 > 0                  ~ "consolidation",
      part_manquante <= SEUIL_ATTEIGNABLE ~ "offensive",
      TRUE                               ~ "hors_portee"
    )
  ) %>%
  select(district_electoral, total_valables, votes_eag, votes_gauche, votes_raboud,
         part_eag_dist, part_gauche_dist, part_raboud_dist,
         sieges_2022, sieges_2027, sieges_eag_22, sieges_eag_27, sieges_eag_raboud,
         voix_manquantes, part_manquante, levier, statut_district)

cat("\n── Analyse par district electoral ──\n")
print(dist_levier %>% arrange(statut_district, voix_manquantes))

cat(sprintf("\n── Classification stratégique (%d districts) ──\n", nrow(dist_levier)))
cat(sprintf("  consolidation : %d\n", sum(dist_levier$statut_district == "consolidation")))
cat(sprintf("  offensive     : %d  (part_manquante ≤ %.0f%%)\n",
            sum(dist_levier$statut_district == "offensive"), SEUIL_ATTEIGNABLE * 100))
cat(sprintf("  hors_portee   : %d  (exclus du ciblage communal)\n",
            sum(dist_levier$statut_district == "hors_portee")))


# ── 9. SCORE DE PRIORITÉ (§6-7 du README) ────────────────────
# Les sièges sont attribués par district électoral (Hagenbach-Bischoff) :
# la priorité est donc d'abord évaluée au niveau du district (layer 1 —
# quel district mérite l'effort), puis répartie entre ses communes selon
# leur potentiel propre (layer 2 — quelle commune cibler dans ce district).
# Les districts hors_portee sont exclus en amont (§6 du README) : aucun
# score n'est calculé pour leurs communes.

communes <- communes %>%
  left_join(
    dist_levier %>% select(district_electoral, levier, part_manquante, statut_district),
    by = "district_electoral"
  ) %>%
  filter(statut_district != "hors_portee") %>%
  mutate(
    # Marge issue du positionnement : EàG sous-convertit son profil politique 2022
    marge_ecart  = pmax(0, -ecart),
    # Marge démontrée par Raboud au CE 2026 : soutien EàG déjà atteint au-dessus
    # du score de liste 2022, escompté du panachage (RABOUD_ESCOMPTE, §0).
    marge_raboud = pmax(0, coalesce(tendance_raboud, 0)) * RABOUD_ESCOMPTE,
    # Marge de progression = le meilleur des deux signaux (§7 du README)
    marge_progression = pmax(marge_ecart, marge_raboud),
    # Consolidation : défendre les votes acquis a la même valeur que progresser
    potentiel_commune = case_when(
      statut_district == "consolidation" ~ pmax(marge_progression, part_eag_2022),
      TRUE                               ~ marge_progression
    ) * effectif
  )

# Layer 1 — score de district : potentiel total du district × levier siège
district_score <- communes %>%
  group_by(district_electoral) %>%
  summarise(potentiel_district = sum(potentiel_commune, na.rm = TRUE), .groups = "drop") %>%
  left_join(dist_levier %>% select(district_electoral, levier), by = "district_electoral") %>%
  mutate(score_district = potentiel_district * levier)

# Layer 2 — score communal : part du potentiel de district captée par la commune
communes <- communes %>%
  left_join(
    district_score %>% select(district_electoral, potentiel_district, score_district),
    by = "district_electoral"
  ) %>%
  mutate(
    score_priorite      = potentiel_commune * levier,
    part_score_district = if_else(potentiel_district > 0,
                                   potentiel_commune / potentiel_district, 0),
    voix_manquantes     = ceiling(part_manquante * effectif)
  )


# ── 10. MOBILISATION VS PERSUASION (§7 du README) ────────────

# Seuils médians pour les deux axes
seuil_dim1    <- median(communes$dim1,       na.rm = TRUE)
seuil_particip <- median(communes$particip_moy, na.rm = TRUE)

# Raboud a surperformé la liste 2022 d'environ +8 pp partout (effet « candidate
# forte + panachage »). Pour le profil, on ne retient comme persuasion que les
# communes où la marge démontrée dépasse nettement ce socle systémique.
seuil_raboud <- median(communes$tendance_raboud[communes$raboud_couvert], na.rm = TRUE)

communes <- communes %>%
  mutate(
    profil = case_when(
      statut_district == "consolidation" & dim1 > seuil_dim1 ~ "consolidation",
      !is.na(part_eag_cc) & part_eag_cc > 0                  ~ "consolidation",
      dim1 > seuil_dim1 & particip_moy < seuil_particip       ~ "mobilisation",
      dim1 <= seuil_dim1 & ecart < 0                          ~ "persuasion",
      # Raboud (CE 2026) a démontré un soutien EàG nettement au-delà de la liste
      # 2022 ET au-delà du socle systémique (~+8 pp) : électeurs à convaincre
      # présents, quel que soit le positionnement sur enjeux.
      marge_raboud > marge_ecart & tendance_raboud >= seuil_raboud ~ "persuasion",
      TRUE                                                     ~ "autre"
    )
  )

cat(sprintf("\nRépartition des profils :\n"))
print(table(communes$profil))


# ── 11. SORTIES ──────────────────────────────────────────────

dist_levier <- dist_levier %>%
  left_join(
    district_score %>% select(district_electoral, potentiel_district, score_district),
    by = "district_electoral"
  )

write_csv(communes,    "data/processed/communes_scores.csv")
write_csv(dist_levier, "data/processed/districts_levier.csv")

# Top 20 par profil
afficher_top <- function(df, profil_cible, n = 20) {
  cat(sprintf("\n── TOP %d — %s ──\n", n, toupper(profil_cible)))
  df %>%
    filter(profil == profil_cible) %>%
    arrange(desc(score_priorite)) %>%
    select(Communes, district_electoral, statut_district,
           dim1, part_eag_2022, part_ps_verts, part_eag_cc, ecart,
           particip_moy, effectif, levier, score_priorite) %>%
    mutate(across(where(is.double), \(x) round(x, 4))) %>%
    slice_head(n = n) %>%
    print(n = n)
}

afficher_top(communes, "mobilisation")
afficher_top(communes, "persuasion")
afficher_top(communes, "consolidation")

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
  scale_fill_manual(values = c(mobilisation  = "#2166ac", persuasion    = "#d6604d",
                                consolidation = "#4dac26", autre         = "#aaaaaa")) +
  labs(x = NULL, y = "Score de priorité agrégé",
       title = "Potentiel par district et profil d'action",
       fill = NULL) +
  theme_minimal(base_size = 12)

cat("\nFichiers écrits dans data/processed/\n")
cat("  communes_scores.csv\n  districts_levier.csv\n")
