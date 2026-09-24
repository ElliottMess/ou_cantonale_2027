# ============================================================
# Validation de repartir_ledp() contre le résultat réel 2022
# ============================================================
# Le fichier officiel des 150 député·es élu·es (arrondissement + liste)
# sert de vérité terrain. On rejoue la répartition des sièges 2022 à partir
# des voix par bloc et on compare siège par siège.
#
#   Rscript valider_repartition.R
#
# Résultat attendu : 68 / 78 cellules (arrondissement × bloc) exactes.
# Les 10 écarts résiduels valent ±1 siège et correspondent aux
# APPARENTEMENTS de 2022, que le modèle ignore délibérément (scénario
# « liste EàG seule », cf. analyse.R §0).
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
})

# analyse.R fournit repartir_ledp(), dist_votes, LISTES_GC et norm_district()
source("analyse.R", echo = FALSE)

FICHIER_ELUS <- "data/raw/Statistiques_élections_GC_2022_-_2027(1).xlsx"

# ── Vérité terrain : les 150 élu·es de 2022 ──────────────────
elus <- suppressMessages(
  read_excel(FICHIER_ELUS, sheet = "Fichier de base", skip = 10)
)
names(elus)[1:6] <- c("arrdt", "sous_arrdt", "liste", "nom", "prenom", "suffrages")

# Rattacher chaque liste locale à l'un des six blocs du CSV de votations.
# L'ordre compte : « Union démocratique du centre » contient « centre ».
bloc_de_liste <- function(l) {
  case_when(
    str_detect(l, regex("solidarit|EàG|POP|Fourmi|décroissance|Ensemble à Gauche", TRUE))
      ~ "gc_2022_Gauche",
    str_detect(l, regex("socialiste", TRUE))                      ~ "gc_2022_PS",
    str_detect(l, regex("UDC|démocratique du centre|UDF", TRUE))  ~ "gc_2022_extreme-droite",
    str_detect(l, regex("Vert'?lib|Le Centre|centrist|AC/DC", TRUE)) ~ "gc_2022_centre",
    str_detect(l, regex("Vert", TRUE))                            ~ "gc_2022_verts",
    str_detect(l, regex("PLR", TRUE))                             ~ "gc_2022_droite-bourgeoise",
    TRUE                                                          ~ NA_character_
  )
}

elus <- elus %>%
  filter(!is.na(nom), !is.na(liste), liste != "Liste politique") %>%
  fill(arrdt, .direction = "down") %>%
  mutate(bloc = bloc_de_liste(liste))

if (anyNA(elus$bloc)) {
  print(unique(elus$liste[is.na(elus$bloc)]))
  stop("Listes non rattachées à un bloc — compléter bloc_de_liste()")
}

# Noms d'arrondissement du fichier officiel → district électoral du pipeline
elus <- elus %>%
  mutate(
    district_electoral = norm_district(
      coalesce(na_if(sous_arrdt, "Sous arrondissement"), arrdt)
    ),
    district_electoral = recode(
      district_electoral,
      "Vevey"                 = "Riviera",
      "Jura-Nord vaudois"     = "Yverdon",
      "Lausanne"              = "Lausanne-Ville",
      "Riviera-Pays-d'Enhaut" = "Riviera"
    )
  )

stopifnot(nrow(elus) == 150)
reel <- elus %>% count(district_electoral, bloc, name = "reel")

# ── Simulation : règle LEDP sur les voix 2022 ────────────────
simule <- map_dfr(seq_len(nrow(dist_votes)), function(i) {
  sieges <- repartir_ledp(unlist(dist_votes[i, LISTES_GC]),
                          dist_votes$sieges_2022[i])
  tibble(district_electoral = dist_votes$district_electoral[i],
         bloc               = names(sieges),
         simule             = as.integer(sieges))
})

comp <- full_join(simule, reel, by = c("district_electoral", "bloc")) %>%
  mutate(across(c(simule, reel), \(x) replace_na(x, 0L)),
         ecart = simule - reel)

# ── Rapport ──────────────────────────────────────────────────
cat("\n=== Totaux cantonaux par bloc ===\n")
print(as.data.frame(
  comp %>% group_by(bloc) %>%
    summarise(simule = sum(simule), reel = sum(reel),
              ecart = sum(simule) - sum(reel), .groups = "drop")
), row.names = FALSE)

cat("\n=== Écarts par arrondissement (apparentements 2022 non modélisés) ===\n")
print(as.data.frame(comp %>% filter(ecart != 0) %>% arrange(district_electoral, bloc)),
      row.names = FALSE)

cat("\n=== Bloc EàG / POP / solidaritéS ===\n")
print(as.data.frame(
  comp %>% filter(bloc == "gc_2022_Gauche", simule + reel > 0) %>% arrange(desc(reel))
), row.names = FALSE)

exact <- sum(comp$ecart == 0)
cat(sprintf("\nCellules exactes : %d / %d  (%.0f %%)\n",
            exact, nrow(comp), 100 * exact / nrow(comp)))
cat(sprintf("Total des sièges : simulé %d, réel %d\n",
            sum(comp$simule), sum(comp$reel)))

if (exact < 60) {
  stop("Trop d'écarts : repartir_ledp() ne reproduit plus le scrutin 2022")
}
cat("\n✓ La règle LEDP implémentée reproduit le scrutin 2022 à l'apparentement près.\n")
