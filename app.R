# ============================================================
# Dashboard Shiny — Ciblage EàG, Cantonales Vaud 2027
# ============================================================
library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(here)

# ── Helpers ──────────────────────────────────────────────────
# Espaces insécables : un nombre ne se coupe jamais en fin de ligne (« +4 / 508 »)
fmt_pct <- function(x) paste0(round(x * 100, 1), " %")
fmt_num <- function(x) format(round(x), big.mark = " ", scientific = FALSE)
# Un écart se lit toujours signé : « +4 508 » et non « 4 508 ».
fmt_signe <- function(x) paste0(if_else(x >= 0, "+", "-"), fmt_num(abs(x)))

# Un message clé : titre en gras + une ou deux phrases
bloc_message <- function(titre, ...) {
  div(h6(class = "fw-bold mb-1", titre), p(class = "small mb-2", ...))
}


# ── Constantes ───────────────────────────────────────────────
# Palette des profils d'action — doit rester alignée sur analyse.R §0
PROFIL_PAL <- c(
  consolidation = "#4dac26",
  mobilisation  = "#2166ac",
  persuasion    = "#d6604d",
  report_gauche = "#b07aa1",
  implantation  = "#8c8c8c"
)

STATUT_PAL <- c(
  consolidation   = "#4dac26",
  offensive       = "#f0a500",
  conquete_longue = "#7f8fa6"
)

# Comparaison GC 2022 ↔ CE 2026 — doit rester alignée sur analyse.R §0
STATUT_COMPARAISON_PAL <- c(
  progression       = "#2166ac",
  stable            = "#999999",
  recul             = "#d6604d",
  "sans liste 2022" = "#f0a500"
)

# Écart de conversion (résidu 2022) en trois classes, mêmes couleurs que la
# comparaison ci-dessus : rouge = en dessous, gris = conforme, bleu = au-dessus.
ECART_SEUIL <- 0.01   # ±1 point : en deçà, la commune convertit « comme attendu »
ECART_PAL <- c(
  "Sous-convertit (< −1 pt)" = "#d6604d",
  "Conforme (±1 pt)"         = "#999999",
  "Sur-convertit (> +1 pt)"  = "#2166ac"
)

# ── Données pré-calculées ────────────────────────────────────
# Toutes les communes sont scorées : il n'y a plus de fichier « hors portée ».
communes    <- read_csv("data/processed/communes_scores.csv",  show_col_types = FALSE)
dist_levier <- read_csv("data/processed/districts_levier.csv", show_col_types = FALSE)
types_elec  <- read_csv("data/processed/types_electorat.csv",  show_col_types = FALSE)

# Comparaison GC 2022 ↔ CE 2026 (analyse.R §7bis)
comparaison <- read_csv("data/processed/comparaison_eag_raboud.csv")%>%
  left_join(communes |> select(code_ofs, profil, type_electorat), by = "code_ofs")
comp_dist   <- read_csv("data/processed/comparaison_eag_raboud_districts.csv",
                        show_col_types = FALSE)

# Totaux cantonaux, affichés en tête de la carte de comparaison
comp_tot <- list(
  voix22   = sum(comparaison$voix_eag_2022),
  voix26   = sum(comparaison$voix_raboud_2026),
  part22   = sum(comparaison$voix_eag_2022)    / sum(comparaison$bulletins_2022),
  part26   = sum(comparaison$voix_raboud_2026) / sum(comparaison$bulletins_2026),
  eff_part = sum(comparaison$effet_participation),
  eff_perf = sum(comparaison$effet_performance),
  base22   = sum(comparaison$voix_raboud_base_2022)
)
dict_vot    <- read_csv("data/processed/votations_dict.csv",   show_col_types = FALSE) |>
  filter(!duplicated(votation)) |>
  filter(type %in% c("initiative", "loi"))   # ACP = positionnement sur enjeux uniquement

# ── Messages clés ────────────────────────────────────────────
# Les chiffres et les noms sont CALCULÉS depuis les sorties d'analyse.R, pour
# que la synthèse reste juste après chaque nouvelle exécution ; seul le
# cadrage est rédigé.
liste_fr <- function(x) {
  if (length(x) <= 1) return(x)
  paste(paste(head(x, -1), collapse = ", "), "et", tail(x, 1))
}

cles <- local({
  d <- dist_levier
  moins_chers <- d |> slice_min(voix_siege, n = 4, with_ties = FALSE)
  # Districts où le quorum est la vraie barre : le siège suit à ≤ 100 voix
  quorum_barre <- d |>
    filter(voix_quorum > 0, voix_siege - voix_quorum <= 100) |>
    arrange(voix_siege)
  gains_raboud <- d |>
    filter(sieges_eag_raboud > sieges_eag_27) |>
    arrange(desc(sieges_eag_raboud - sieges_eag_27), voix_siege)
  sans_liste <- comparaison |> filter(!liste_eag_2022)

  # Gisement : le type d'électorat qui pèse le plus dans le score volume
  types <- communes |>
    group_by(type_electorat) |>
    summarise(n = n(), bulletins = sum(effectif), score = sum(score_priorite),
              particip = mean(particip_moy), .groups = "drop") |>
    mutate(part_bulletins = bulletins / sum(bulletins), part_score = score / sum(score))
  gisement <- types |> slice_max(score, n = 1, with_ties = FALSE)

  top_vol   <- communes |> slice_max(score_priorite, n = 10, with_ties = FALSE)
  top_marge <- communes |> filter(effectif >= 100) |>
    slice_max(score_marge, n = 20, with_ties = FALSE)
  dist_marge <- names(which.max(table(top_marge$district_electoral)))

  list(
    s27 = sum(d$sieges_eag_27), srab = sum(d$sieges_eag_raboud),
    gains = gains_raboud$district_electoral,
    moins_chers = moins_chers, quorum_barre = quorum_barre,
    sans_liste_n = nrow(sans_liste),
    sans_liste_part = sum(sans_liste$voix_raboud_2026) / sum(sans_liste$bulletins_2026),
    gisement = gisement,
    gisement_message = types_elec$message_cle[types_elec$type_electorat == gisement$type_electorat][1],
    particip_autres = mean(communes$particip_moy[communes$type_electorat != gisement$type_electorat]),
    top_vol = top_vol,
    part_top_vol = sum(top_vol$score_priorite) / sum(communes$score_priorite),
    dist_marge = dist_marge,
    n_dist_marge = sum(top_marge$district_electoral == dist_marge),
    top_marge_dist = head(top_marge$Communes[top_marge$district_electoral == dist_marge], 3)
  )
})

# ── Limites géographiques (cache) ────────────────────────────
# Les limites officielles sont bien trop détaillées pour une carte cantonale :
# 206 000 sommets pour les communes, soit ~7,5 Mo de JSON envoyés par CHAQUE
# carte communale, et 2,7 s rien que pour lire le gpkg suisse. On construit
# donc une fois des limites vaudoises simplifiées, mises en cache dans
# LIMITES_CACHE, et reconstruites seulement si le rattachement
# commune → district change (nouvelle version de analyse.R) ou si le gpkg source
# est plus récent. Le cache suffit à lancer l'app sans le gpkg.
LIMITES_CACHE <- "data/processed/limites_vaud.rds"
GPKG_COMMUNES <- here::here("data/raw/CH_communes_no_lacs.gpkg")

rattachement <- communes |>
  distinct(code_ofs, district_electoral) |>
  arrange(code_ofs) |>
  as.data.frame()

construire_limites <- function() {
  message("Construction des limites simplifiées (", LIMITES_CACHE, ")…")
  # Filtre SQL à la lecture : 0,5 s au lieu de 2,7 s pour toute la Suisse
  brutes <- read_sf(GPKG_COMMUNES, query =
      "SELECT * FROM CH_communes_no_lacs WHERE kantonsnummer = 22") |>
    st_zm(drop = TRUE) |>
    select(code_ofs = bfs_nummer)

  # Districts électoraux = union des communes, selon le MÊME rattachement que
  # les scores. L'ancien districts_vaud_no_lacs.gpkg était inutilisable tel
  # quel : son `kantonsnummer` avait été sommé à la fusion (le filtre == 22 ne
  # gardait qu'un district sur 13) et Riviera y figurait sous le nom « Vevey ».
  # L'union part des limites COMPLÈTES : unir des communes déjà simplifiées
  # laisserait des interstices, donc des trous dans les districts.
  districts <- brutes |>
    inner_join(rattachement, by = "code_ofs") |>
    group_by(district_electoral) |>
    summarise(n_communes = n(), .groups = "drop")

  # Simplification en LV95 (mètres), avant reprojection. Chaque commune est
  # simplifiée séparément : de fins interstices (< 20 m) peuvent apparaître
  # entre voisines à fort zoom, invisibles à l'échelle du canton.
  list(
    rattachement = rattachement,
    communes  = brutes |>
      st_simplify(dTolerance = 20, preserveTopology = TRUE) |>
      st_transform(4326),
    districts = districts |>
      st_simplify(dTolerance = 50, preserveTopology = TRUE) |>
      st_transform(4326)
  )
}

limites <- if (file.exists(LIMITES_CACHE)) readRDS(LIMITES_CACHE)
gpkg_plus_recent <- file.exists(GPKG_COMMUNES) && file.exists(LIMITES_CACHE) &&
  file.mtime(GPKG_COMMUNES) > file.mtime(LIMITES_CACHE)
if (is.null(limites) || gpkg_plus_recent ||
    !identical(limites$rattachement, rattachement)) {
  limites <- construire_limites()
  saveRDS(limites, LIMITES_CACHE)
}

communes_limites  <- limites$communes  |> left_join(communes,    by = "code_ofs")
districts_limites <- limites$districts |> left_join(dist_levier, by = "district_electoral")

# Point d'étiquetage garanti À L'INTÉRIEUR du polygone (un centroïde tombe
# hors des districts en croissant, comme Lavaux-Oron).
districts_etiquettes <- districts_limites |>
  st_geometry() |>
  st_transform(2056) |>
  st_point_on_surface() |>
  st_transform(4326) |>
  st_coordinates() |>
  as_tibble() |>
  mutate(district_electoral = districts_limites$district_electoral,
         # Autour de Lausanne, quatre petits districts se touchent : on écarte
         # leurs étiquettes pour qu'elles ne se recouvrent pas au zoom initial.
         direction = case_when(
           district_electoral == "Lausanne-Ville"   ~ "bottom",
           district_electoral == "Ouest lausannois" ~ "left",
           district_electoral == "Lavaux-Oron"      ~ "right",
           district_electoral == "Romanel"          ~ "top",
           TRUE                                     ~ "center"
         ))

# Indicateurs proposés sur la carte des districts. `inverse` : plus la valeur
# est BASSE, plus le district est intéressant (coût en voix) — la palette est
# retournée pour que « foncé » veuille toujours dire « prioritaire ».
DMAP_METRIQUES <- list(
  score_district       = list(lab = "Score volume",                   fmt = "num2"),
  score_marge_district = list(lab = "Score marge (sans effectif)",    fmt = "num2"),
  marge_district       = list(lab = "Marge de progression moyenne",   fmt = "pct"),
  voix_siege           = list(lab = "Voix pour un siège",             fmt = "num", inverse = TRUE),
  voix_quorum          = list(lab = "Voix pour le quorum 5 %",        fmt = "num", inverse = TRUE),
  sieges_eag_27        = list(lab = "Sièges EàG 2027 (est.)",         fmt = "num"),
  sieges_eag_raboud    = list(lab = "Sièges EàG — scénario Raboud",   fmt = "num"),
  part_eag_dist        = list(lab = "Part EàG GC 2022",               fmt = "pct"),
  part_raboud_dist     = list(lab = "Part Raboud CE 2026",            fmt = "pct"),
  statut_district      = list(lab = "Statut",                         fmt = "cat")
)
fmt_metrique <- function(x, fmt) {
  switch(fmt,
    pct  = fmt_pct(x),
    num2 = format(round(x, 2), nsmall = 2),
    num  = fmt_num(x),
    as.character(x))
}


# ── ACP (calculée par analyse.R) ─────────────────────────────
# L'app recalculait auparavant sa propre ACP, sur un autre jeu de variables et
# un autre seuil de données manquantes : le biplot et le graphique d'écart
# affichaient deux axes différents sous le même nom. On lit désormais la même
# ACP que celle qui a servi à produire les scores.
acp <- readRDS("data/processed/acp_positionnement.rds")

var_exp  <- acp$eig[, 2]
flip     <- acp$flip
cols_vot <- acp$cols_vot
n_dim    <- nrow(acp$eig)

ind_df <- as.data.frame(acp$ind_coord[, 1:2]) |>
  rownames_to_column("Communes") |>
  rename(Dim1 = Dim.1, Dim2 = Dim.2) |>
  left_join(communes |> select(Communes, district_electoral, profil,
                               type_electorat, message_cle,
                               part_eag_2022, part_gauche_2022, effectif,
                               score_priorite, ecart, dim1, particip_moy,
                               part_eag_cc, part_raboud_2026, tendance_raboud,
                               raboud_voix, liste_eag_2022),
            by = "Communes")
if (flip) ind_df <- mutate(ind_df, Dim1 = -Dim1)
ind_df <- ind_df |>
  mutate(cc_txt = ifelse(!is.na(part_eag_cc),
                         paste0("<br>CC 2026 EàG : ", fmt_pct(part_eag_cc)), ""))

var_df <- as.data.frame(acp$var_coord[, 1:2]) |>
  rownames_to_column("votation") |>
  rename(Dim1 = Dim.1, Dim2 = Dim.2) |>
  left_join(dict_vot |> select(votation, nom_complet), by = "votation")
if (flip) var_df <- mutate(var_df, Dim1 = -Dim1)

districts <- sort(unique(communes$district_electoral))
types_lst <- sort(unique(communes$type_electorat))
profils   <- intersect(names(PROFIL_PAL), unique(communes$profil))

# ── UI ───────────────────────────────────────────────────────
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "cosmo",
    primary    = "#c0392b"
  ),

  # ── En-tête ─────────────────────────────────────────────────
  div(
    class = "py-3 mb-3 border-bottom d-flex justify-content-between align-items-center flex-wrap gap-2",
    style = "border-color: #c0392b !important",
    h3(class = "mb-0 fw-bold", style = "color:#c0392b",
       "EàG — Ciblage communes, Cantonales Vaud 2027"),
    tags$a(
      href   = "https://github.com/ElliottMess/ou_cantonale_2027",
      target = "_blank", rel = "noopener",
      class  = "badge rounded-pill text-bg-dark text-decoration-none fs-6",
      title  = "Code source sur GitHub",
      icon("github"), " GitHub"
    )
  ),
  # ── Guide d'utilisation ──────────────────────────────────────
  card(
    class = "border-0 bg-light mb-1",
    card_body(
      layout_columns(
        col_widths = c(4, 4, 4),

        div(
          h6(class = "fw-bold mb-2", "Le score de priorité"),
          p(class = "small mb-1",
            "Les sièges se gagnent au ", strong("district"), " : on classe donc d'abord les districts, ",
            "puis on répartit l'effort entre leurs communes."),
          tags$ul(class = "small mb-1 ps-3",
            tags$li(strong("1. Score de district"), " — potentiel total du district × levier siège (1 / voix nécessaires pour un siège, calculées selon la règle LEDP)"),
            tags$li(strong("2. Score de commune"), " — part de ce potentiel captée par la commune : sa marge de progression × son effectif")),
          p(class = "small mb-1",
            "La ", strong("marge de progression"), " retient le plus fort des signaux disponibles : ",
            "sous-conversion du positionnement (là où EàG avait une liste en 2022), ",
            "portée démontrée par Raboud au CE 2026, ou réservoir PS/Verts inexploité."),
          div(class = "bg-light rounded p-2 text-center font-monospace small",
            "score marge  =  marge × 1000 / voix pour un siège", tags$br(),
            "score volume ≈ score marge × effectif / 1000"),
          p(class = "small mb-0 mt-1",
            "Deux lectures : le ", strong("score volume"), " dit où concentrer l'effort (les gros ",
            "gisements, donc les grandes communes) ; le ", strong("score marge"), " retire l'effectif et dit ",
            "où chaque contact rapporte le plus — part du siège suivant gagnée pour 1000 électeurs convertis.")
        ),

        div(
          h6(class = "fw-bold mb-2", "Les profils d'action"),
          tags$ul(class = "small mb-0 ps-3",
            tags$li(
              tags$span(style = "color:#4dac26; font-size:1.1em", "●"), " ",
              strong("Consolidation"), " — défendre les sièges et bastions acquis"),
            tags$li(
              tags$span(style = "color:#2166ac; font-size:1.1em", "●"), " ",
              strong("Mobilisation"), " — faire voter un électorat favorable peu participatif"),
            tags$li(
              tags$span(style = "color:#d6604d; font-size:1.1em", "●"), " ",
              strong("Persuasion"), " — convaincre là où EàG sous-performe son potentiel"),
            tags$li(
              tags$span(style = "color:#b07aa1; font-size:1.1em", "●"), " ",
              strong("Report de gauche"), " — gros réservoir PS/Verts encore non capté"),
            tags$li(
              tags$span(style = "color:#8c8c8c; font-size:1.1em", "●"), " ",
              strong("Implantation"), " — terrain difficile : présence, visibilité, dépôt de liste")),
          p(class = "small text-muted mb-0 mt-2",
            "Le ", strong("type d'électorat"), " (colonne « Électorat ») dit à qui l'on parle ; ",
            "le profil dit quoi y faire.")
        ),

        div(
          h6(class = "fw-bold mb-2", "À garder en tête"),
          tags$ul(class = "small mb-0 ps-3",
            tags$li(strong("EàG n'avait de liste que dans 7 arrondissements sur 13 en 2022."),
                    " Ailleurs, 0 % n'est pas une contre-performance : c'est une absence. Le résidu n'y est pas calculé."),
            tags$li("Chaque part est rapportée à sa propre base de bulletins : le CE 2026 a compté ~1,6× plus de bulletins que le GC 2022."),
            tags$li("Le CE 2026 est majoritaire avec panachage : le score de Raboud est un plafond de portée, pas une prédiction de vote de liste."),
            tags$li("Positionnement sur les votations ≠ vote de liste EàG."),
            tags$li("Analyse géographique : ne pas inférer de comportements individuels."))
        )
      ),

      div(
        class = "text-center mt-3",
        tags$a(
          class = "small text-muted",
          style = "cursor:pointer; text-decoration:underline dotted",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = "#methodo-body",
          "Voir la méthodologie complète ▾"
        )
      ),
      div(
        id = "methodo-body", class = "collapse mt-3",
        uiOutput("ui_methodo")
      )
    )
  ),

  # ── Messages clés ────────────────────────────────────────────
  card(
    class = "mb-3",
    card_header(strong("Messages clés de l'analyse")),
    card_body(
      layout_columns(
        col_widths = c(4, 4, 4, 4, 4, 4),

        bloc_message(
          sprintf("De %d à %d sièges à portée", cles$s27, cles$srab),
          sprintf("À voix 2022, la répartition 2027 redonne %d sièges à EàG. ", cles$s27),
          sprintf("Si la liste retrouve au GC 2027 la performance de Raboud au CE 2026, le modèle en donne %d, ",
                  cles$srab),
          sprintf("avec des gains à %s. ", liste_fr(cles$gains)),
          "C'est un plafond, pas une prévision."),

        bloc_message(
          sprintf("%d sièges pour %s voix", nrow(cles$moins_chers),
                  fmt_num(sum(cles$moins_chers$voix_siege))),
          "Les sièges suivants les moins chers se jouent à quelques centaines de voix chacun : ",
          paste0(liste_fr(sprintf("%s (%s)", cles$moins_chers$district_electoral,
                                  trimws(fmt_num(cles$moins_chers$voix_siege)))),
                 ". C'est là que chaque voix gagnée pèse le plus.")),

        bloc_message(
          "Ailleurs, le quorum est la vraie barre",
          sprintf("Dans %d districts (%s), une fois les 5 %% franchis, le siège suit à moins de 100 voix. ",
                  nrow(cles$quorum_barre), liste_fr(cles$quorum_barre$district_electoral)),
          "Le premier objectif y est d'atteindre le quorum, avec une liste qui fasse campagne."),

        bloc_message(
          if (comp_tot$eff_part > comp_tot$eff_perf)
            "Raboud : un progrès réel, mais surtout de participation"
          else "Raboud : un progrès d'abord de performance",
          sprintf("De %s (liste GC 2022) à %s (Raboud CE 2026). ",
                  fmt_pct(comp_tot$part22), fmt_pct(comp_tot$part26)),
          sprintf("Sur l'écart de voix, %s relèvent de la performance ; %s tiennent à la participation du CE ",
                  fmt_signe(comp_tot$eff_perf), fmt_signe(comp_tot$eff_part)),
          "et s'évaporent si le GC 2027 mobilise comme 2022. ",
          sprintf("Dans les %d communes sans liste en 2022, Raboud fait %s : le potentiel y existe.",
                  cles$sans_liste_n, fmt_pct(cles$sans_liste_part))),

        bloc_message(
          sprintf("Le gisement : %s", tolower(sub("^\\d+\\.\\s*", "", cles$gisement$type_electorat))),
          sprintf("Le type « %s » réunit %d communes, %s des bulletins et %s du score volume. ",
                  cles$gisement$type_electorat, cles$gisement$n,
                  fmt_pct(cles$gisement$part_bulletins), fmt_pct(cles$gisement$part_score)),
          sprintf("Participation moyenne de %s, contre %s ailleurs. ",
                  fmt_pct(cles$gisement$particip), fmt_pct(cles$particip_autres)),
          em(paste0(cles$gisement_message, "."))),

        bloc_message(
          "Où sont les voix, où chaque contact rapporte",
          sprintf("%d communes concentrent %s du score volume (%s…) : c'est là que se trouvent les voix. ",
                  nrow(cles$top_vol), fmt_pct(cles$part_top_vol),
                  paste(head(cles$top_vol$Communes, 3), collapse = ", ")),
          sprintf("Au score marge, %s place %d communes dans le top 20 (%s…) : c'est là que chaque contact rapporte le plus.",
                  cles$dist_marge, cles$n_dist_marge, paste(cles$top_marge_dist, collapse = ", ")))
      )
    )
  ),

  # ── Carte districts électoraux ───────────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center w-100",
        span("Carte — districts électoraux"),
        div(class = "d-flex align-items-center gap-1",
          span(class = "small text-muted", "Colorier :"),
          selectInput("dmap_color", NULL,
            choices  = setNames(names(DMAP_METRIQUES),
                                map_chr(DMAP_METRIQUES, "lab")),
            selected = "score_marge_district", width = "260px")
        )
      )
    ),
    leafletOutput("d_map", height = "500px")
  ),

  # ── Carte score priorité ────────────────────────────────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center w-100",
        span("Carte — score de priorité par commune"),
        div(class = "d-flex gap-3 align-items-center",
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Colorier :"),
            selectInput("map_color", NULL,
              choices  = c("Score volume (avec effectif)" = "score_priorite",
                           "Score marge (sans effectif)"  = "score_marge",
                           "Marge de progression"         = "marge_progression",
                           "Profil"                       = "profil"),
              selected = "score_priorite", width = "240px")
          )
        )
      )
    ),
    leafletOutput("p_map", height = "500px")
  ),

  # ── Carte voix manquantes ────────────────────────────────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center w-100",
          span("Carte — voix à trouver par commune"),
          selectInput("map_color_voix", NULL,
                      choices  = c("Pour un siège" = "voix",
                                   "Pour le quorum 5 %" = "part"),
                      selected = "voix", width = "170px")
      )
    ),
    leafletOutput("v_map", height = "500px")
  ),
  

  # ── ACP ─────────────────────────────────────────────────────
  layout_columns(
    col_widths = c(8, 4),
    
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
          div( width = "300px", span(sprintf("ACP — Dim.1 (%.1f %%) × Dim.2 (%.1f %%)",
                       var_exp[1], var_exp[2]))),
          div(class = "d-flex gap-3 align-items-center",
            div(class = "d-flex align-items-center gap-1",
              span(class = "small text-muted", "Colorier :"),
              selectInput("acp_color", NULL,
                choices  = c("Profil d'action"    = "profil",
                             "Type d'électorat"   = "type_electorat",
                             "Part de gauche"     = "part_gauche_2022"),
                selected = "profil", width = "170px")
            ),
            div(class = "d-flex align-items-center gap-1",
              checkboxInput("acp_vars", "Votations", TRUE)
            )
          )
        )
      ),
      plotlyOutput("p_biplot", height = "460px")
    ),
    tagList(
      card(
        card_header("Éboulis de variance"),
        plotlyOutput("p_scree", height = "180px")
      ),
      card(
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
            span("Contributions  "),
            selectInput("contrib_dim", NULL,
              choices  = paste0("Dim.", seq_len(min(5, n_dim))),
              selected = "Dim.1", width = "100px")
          )
        ),
        plotlyOutput("p_contrib", height = "260px")
      )
    )
  ),

  # ── Écart de conversion ─────────────────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center",
        span("Écart de conversion — positionnement ACP vs vote EàG"),
        div(class = "d-flex gap-3 align-items-center",
          div(class = "d-flex align-items-center gap-1",
            checkboxInput("ecart_raboud", "Panneau Raboud CE 2026", TRUE)
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "District :"),
            selectInput("ecart_dist", NULL,
              choices = c("Tous", districts), selected = "Tous", width = "180px")
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Effectif min :"),
            numericInput("ecart_eff", NULL, value = 0, min = 0, max = 10000,
                         step = 200, width = "90px")
          )
        )
      )
    ),
    plotlyOutput("p_ecart", height = "460px")
  ),

  # ── Comparaison GC 2022 ↔ CE 2026 ───────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        span("Comparaison — liste EàG/gauche GC 2022 vs Raboud CE 2026"),
        div(class = "d-flex gap-3 align-items-center flex-wrap",
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "District :"),
            selectInput("cmp_dist", NULL,
              choices = c("Tous", districts), selected = "Tous", width = "180px")
          ),
          div(class = "d-flex align-items-center gap-1",
            checkboxInput("cmp_comparables",
                          "Communes comparables seulement (liste en 2022)", FALSE)
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Top :"),
            numericInput("cmp_n", NULL, value = 20, min = 5, max = 60,
                         step = 5, width = "70px")
          )
        )
      )
    ),
    card_body(
      # Les deux scores ne se lisent pas l'un à côté de l'autre sans précaution :
      # on rappelle les trois plans de lecture avant les graphiques.
      div(class = "small text-muted mb-2",
        "Les deux scrutins n'ont ni la même base ni le même mode. Trois lectures, dans l'ordre : ",
        strong("(1) en part"), " — chaque score sur ses propres bulletins, seule mesure de performance ; ",
        strong("(2) en voix à participation 2022"), " — la part de Raboud appliquée au corps électoral du GC ; ",
        strong("(3) décomposition"), " de l'écart de voix brut en effet participation + effet performance."),

      layout_columns(
        col_widths = c(7, 5),
        div(
          h6(class = "fw-bold small mb-1", "1. En part — la diagonale est la référence"),
          p(class = "small text-muted mb-1",
            "Au-dessus de la diagonale, Raboud fait mieux que la liste 2022 ; en dessous, moins bien. ",
            "Les communes en orange n'avaient pas de liste : elles partent de 0 par construction, ",
            "leur position mesure un potentiel révélé, pas une progression."),
          plotlyOutput("p_cmp_scatter", height = "420px")
        ),
        div(
          h6(class = "fw-bold small mb-1", "3. D'où vient l'écart de voix, par district"),
          p(class = "small text-muted mb-1",
            "Barres empilées : la part bleue ne tient qu'à la participation du CE 2026 ",
            "(+67 % de bulletins face au GC 2022)."),
          plotlyOutput("p_cmp_decompo", height = "420px")
        )
      ),

      div(class = "mt-3",
        h6(class = "fw-bold small mb-1", "2. Écart commune par commune"),
        div(class = "d-flex align-items-center gap-2 mb-1",
          span(class = "small text-muted", "Classer par :"),
          selectInput("cmp_tri", NULL,
            choices = c("Écart de part (pp)"                = "diff_part",
                        "Gain en voix à participation 2022" = "effet_performance",
                        "Part Raboud 2026"                  = "part_raboud_2026"),
            selected = "diff_part", width = "280px")
        ),
        plotlyOutput("p_cmp_communes", height = "520px")
      ),

      div(class = "mt-3", DTOutput("tbl_comparaison"))
    )
  ),

  # ── Districts ────────────────────────────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        div(
          span("Priorité par district (layer 1) — potentiel du district × levier siège"),
          div(class = "small text-muted",
              "Les sièges se gagnent au district : ce score classe les districts avant le ciblage communal.")
        ),
        div(class = "d-flex align-items-center gap-1",
          span(class = "small text-muted", "Score :"),
          selectInput("dist_score", NULL,
            choices  = c("Volume (avec effectif)" = "score_district",
                         "Marge (sans effectif)"  = "score_marge_district"),
            selected = "score_marge_district", width = "210px")
        )
      )
    ),
    plotlyOutput("p_dist_score", height = "320px")
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Voix manquantes pour le prochain siège EàG"),
      plotlyOutput("p_voix", height = "340px")
    ),
    card(
      card_header("Sièges EàG — 2022 vs estimation 2027"),
      plotlyOutput("p_sieges", height = "340px")
    )
  ),

  # ── Communes prioritaires ────────────────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center",
        span("Communes prioritaires"),
        div(class = "d-flex gap-3 align-items-center flex-wrap",
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "District :"),
            selectInput("c_dist", NULL,
              choices = c("Tous", districts), selected = "Tous", width = "160px")
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Profil :"),
            checkboxGroupInput("c_profil", NULL, inline = TRUE,
              choices  = profils, selected = profils)
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Électorat :"),
            selectInput("c_type", NULL,
              choices = c("Tous", types_lst), selected = "Tous", width = "200px")
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Classer par :"),
            selectInput("c_tri", NULL,
              choices  = c("Score volume" = "score_priorite",
                           "Score marge"  = "score_marge"),
              selected = "score_marge", width = "150px")
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Effectif min :"),
            numericInput("c_eff", NULL, value = 0, min = 0, max = 10000,
                         step = 100, width = "90px")
          ),
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Top :"),
            numericInput("c_n", NULL, value = 25, min = 5, max = 60,
                         step = 5, width = "70px")
          )
        )
      )
    ),
    layout_columns(
      col_widths = c(5, 7),
      plotlyOutput("p_comm_bar", height = "520px"),
      DTOutput("tbl_communes")
    )
  ),

  # ── Tableau districts (repliable) ────────────────────────────
  card(
    card_header(
      span("Tableau détaillé — districts électoraux"),
      style = "cursor:pointer",
      `data-bs-toggle` = "collapse",
      `data-bs-target` = "#tbl-dist-body"
    ),
    div(id = "tbl-dist-body", class = "collapse",
      card_body(DTOutput("tbl_districts"))
    )
  )

  # # ── Méthodologie (repliable) ─────────────────────────────────
  # card(
  #   card_header(
  #     span("Méthodologie"),
  #     style = "cursor:pointer",
  #     `data-bs-toggle` = "collapse",
  #     `data-bs-target` = "#methodo-body"
  #   ),
  #   div(id = "methodo-body", class = "collapse",
  #     card_body(uiOutput("ui_methodo"))
  #   )
  # )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  comm_filt <- reactive({
    df <- communes
    if (!is.null(input$c_dist) && input$c_dist != "Tous")
      df <- filter(df, district_electoral == input$c_dist)
    if (!is.null(input$c_type) && input$c_type != "Tous")
      df <- filter(df, type_electorat == input$c_type)
    if (!is.null(input$c_profil))
      df <- filter(df, profil %in% input$c_profil)
    # Le score marge fait remonter de très petites communes (35-60 électeurs)
    # dont la part Raboud, sur si peu de bulletins, est surtout du bruit.
    eff <- if (is.na(input$c_eff %||% NA)) 0 else input$c_eff
    filter(df, effectif >= eff)
  })

  # Score de classement des communes et libellés associés
  c_tri <- reactive(input$c_tri %||% "score_priorite")
  c_tri_lab <- reactive(if (c_tri() == "score_marge")
    "Score marge (part du siège suivant / 1000 électeurs)" else "Score volume")

  ecart_filt <- reactive({
    df <- communes
    if (!is.null(input$ecart_dist) && input$ecart_dist != "Tous")
      df <- filter(df, district_electoral == input$ecart_dist)
    eff <- if (is.na(input$ecart_eff)) 0 else input$ecart_eff
    filter(df, effectif >= eff)
  })

  # ACP biplot
  output$p_biplot <- renderPlotly({
    df      <- ind_df |> filter(!is.na(profil))
    scale_f <- max(abs(c(df$Dim1, df$Dim2)), na.rm = TRUE) * 0.55
    var_s   <- var_df |> mutate(x1 = Dim1 * scale_f, y1 = Dim2 * scale_f)
    sizes   <- 7

    if (input$acp_color %in% c("profil", "type_electorat")) {
      grp  <- df[[input$acp_color]]
      pal  <- if (input$acp_color == "profil") PROFIL_PAL else "Set2"
      pt <- plot_ly(
        df, x = ~Dim1, y = ~Dim2, type = "scatter", mode = "markers",
        color = grp, colors = pal,
        marker = list(size = sizes, opacity = 0.75,
                      line = list(width = 0.3, color = "#333")),
        text  = ~paste0("<b>", Communes, "</b> (", district_electoral, ")<br>",
                        "EàG 2022 : ", fmt_pct(part_eag_2022), "<br>",
                        "Gauche 2022 : ", fmt_pct(part_gauche_2022), "<br>",
                        "Profil : ", profil, "<br>",
                        "Type d'électorat : ", type_electorat, cc_txt),
        hovertemplate = "%{text}<extra></extra>"
      )
    } else {
      col_val <- df[[input$acp_color]]
      pt <- plot_ly(
        df, x = ~Dim1, y = ~Dim2, type = "scatter", mode = "markers",
        marker = list(
          color      = col_val,
          colorscale = list(c(0, "#d6604d"), c(0.5, "#f7f7f7"), c(1, "#2166ac")),
          showscale  = TRUE,
          colorbar   = list(title = "", tickformat = ".0%"),
          size       = sizes, opacity = 0.8
        ),
        text = ~paste0("<b>", Communes, "</b> (", district_electoral, ")<br>",
                       "EàG 2022 : ", fmt_pct(part_eag_2022), "<br>",
                       "Profil : ", profil, cc_txt),
        hovertemplate = "%{text}<extra></extra>"
      )
    }

    if (isTRUE(input$acp_vars)) {
      arrows <- lapply(seq_len(nrow(var_s)), function(i) {
        list(type = "line", xref = "x", yref = "y",
             x0 = 0, y0 = 0, x1 = var_s$x1[i], y1 = var_s$y1[i],
             line = list(color = "#555", width = 1.8))
      })
      pt <- pt |>
        add_annotations(x = var_s$x1, y = var_s$y1,
                        text = var_s$nom_complet, showarrow = FALSE,
                        font = list(size = 9, color = "#333"), xanchor = "center") |>
        layout(shapes = arrows)
    }

    pt |> layout(
      xaxis  = list(title = sprintf("Dim.1 — %.1f %% (gauche →)", var_exp[1]),
                    zeroline = TRUE, zerolinecolor = "#ccc"),
      yaxis  = list(title = sprintf("Dim.2 — %.1f %%", var_exp[2]),
                    zeroline = TRUE, zerolinecolor = "#ccc"),
      legend = list(title = list(
        text = if (input$acp_color == "type_electorat") "Type d'électorat" else "Profil"))
    )
  })

  output$p_scree <- renderPlotly({
    n <- min(8, n_dim)
    eig_df <- data.frame(
      dim = factor(paste0("Dim.", seq_len(n)), levels = paste0("Dim.", seq_len(n))),
      pct = acp$eig[seq_len(n), 2]
    )
    plot_ly(eig_df, x = ~dim, y = ~pct, type = "bar",
            marker = list(color = "#c0392b"),
            text = ~paste0(round(pct, 1), " %"), textposition = "outside") |>
      layout(xaxis = list(title = ""), yaxis = list(title = "% variance"),
             showlegend = FALSE, margin = list(t = 5, b = 30))
  })

  output$p_contrib <- renderPlotly({
    dim_idx <- as.integer(sub("Dim\\.", "", input$contrib_dim))
    contrib_df <- data.frame(
      votation = rownames(acp$var_contrib),
      contrib  = acp$var_contrib[, dim_idx]
    ) |>
      left_join(dict_vot |> select(votation, nom_complet), by = "votation") |>
      arrange(contrib) |>
      mutate(nom_complet = factor(nom_complet, levels = nom_complet))

    plot_ly(contrib_df, x = ~contrib, y = ~nom_complet, type = "bar",
            orientation = "h", marker = list(color = "#2166ac")) |>
      layout(xaxis = list(title = "Contribution (%)"), yaxis = list(title = ""),
             showlegend = FALSE, margin = list(l = 200, t = 5))
  })

  # Écart de conversion
  # Deux panneaux côte à côte plutôt qu'une superposition : les 300 points
  # Raboud recouvraient les 124 points 2022 et la droite de régression.
  output$p_ecart <- renderPlotly({
    tous <- ecart_filt() |> filter(!is.na(dim1))
    # Le nuage 2022 ne porte que sur les arrondissements où EàG avait une
    # liste : ailleurs, une part de 0 % ne mesure aucune performance.
    df   <- tous |> filter(liste_eag_2022, !is.na(ecart))
    if (nrow(df) < 3) return(NULL)

    droite_reg <- function(d, y) {
      m  <- lm(reformulate("dim1", y), weights = effectif, data = d)
      xr <- seq(min(d$dim1), max(d$dim1), length.out = 120)
      data.frame(dim1 = xr, y = predict(m, newdata = data.frame(dim1 = xr)))
    }
    # Droite tracée APRÈS les points, sur un halo blanc, pour rester lisible
    # quand elle traverse le nuage.
    ajouter_droite <- function(p, fit, nom, couleur) {
      p |>
        add_trace(data = fit, x = ~dim1, y = ~y, type = "scatter", mode = "lines",
                  line = list(color = "white", width = 6), hoverinfo = "skip",
                  showlegend = FALSE, inherit = FALSE) |>
        add_trace(data = fit, x = ~dim1, y = ~y, type = "scatter", mode = "lines",
                  name = nom, line = list(color = couleur, width = 2),
                  hoverinfo = "skip", inherit = FALSE)
    }
    # Taille ∝ log de l'effectif (la régression est pondérée), 8 à ~16 px
    taille_point <- function(eff) 8 + 3 * log10(pmax(eff, 30) / 30)

    # Le résidu est déjà la distance verticale à la droite : la couleur n'a pas
    # à le redire en continu, trois classes suffisent.
    df <- df |> mutate(
      classe  = factor(case_when(ecart < -ECART_SEUIL ~ names(ECART_PAL)[1],
                                 ecart >  ECART_SEUIL ~ names(ECART_PAL)[3],
                                 TRUE                 ~ names(ECART_PAL)[2]),
                       levels = names(ECART_PAL)),
      taille  = taille_point(effectif),
      tooltip = paste0("<b>", Communes, "</b> (", district_electoral, ")<br>",
                       "Positionnement : ", round(dim1, 2), "<br>",
                       "EàG 2022 : ", fmt_pct(part_eag_2022), "<br>",
                       "Résidu : ", sprintf("%+.1f", ecart * 100), " pt (",
                       fmt_signe(ecart * effectif), " voix)<br>",
                       "Raboud CE 2026 : ", fmt_pct(part_raboud_2026), "<br>",
                       "Effectif : ", fmt_num(effectif), "<br>",
                       "Profil : ", profil))

    p1 <- plot_ly(df, x = ~dim1, y = ~part_eag_2022, type = "scatter", mode = "markers",
                  color = ~classe, colors = ECART_PAL,
                  marker = list(size = ~taille, opacity = 0.75,
                                line = list(width = 1, color = "white")),
                  text = ~tooltip, hovertemplate = "%{text}<extra></extra>") |>
      ajouter_droite(droite_reg(df, "part_eag_2022"), "Régression 2022", "#333333")

    # Raboud était candidate dans tout le canton : les 300 communes comptent,
    # y compris celles où elle n'a fait aucune voix (vrais zéros).
    dr <- tous |> filter(!is.na(part_raboud_2026))
    avec_raboud <- isTRUE(input$ecart_raboud) && nrow(dr) >= 3
    if (avec_raboud) {
      dr <- dr |> mutate(
        taille  = taille_point(effectif),
        tooltip = paste0("<b>", Communes, "</b> (", district_electoral, ")<br>",
                         "Positionnement : ", round(dim1, 2), "<br>",
                         "Raboud CE 2026 : ", fmt_pct(part_raboud_2026), "<br>",
                         "EàG 2022 : ", fmt_pct(part_eag_2022),
                         ifelse(liste_eag_2022, "", " <i>(pas de liste)</i>"), "<br>",
                         "Effectif : ", fmt_num(effectif)))
      p2 <- plot_ly(dr, x = ~dim1, y = ~part_raboud_2026, type = "scatter", mode = "markers",
                    name = "Raboud CE 2026", showlegend = FALSE,
                    marker = list(color = "#8c6bb1", size = ~taille, opacity = 0.45,
                                  line = list(width = 1, color = "white")),
                    text = ~tooltip, hovertemplate = "%{text}<extra></extra>") |>
        ajouter_droite(droite_reg(dr, "part_raboud_2026"), "Régression Raboud 2026", "#54278f")
    }

    # Axe vertical ancré à 0 : les droites prolongées sous 0 % ne veulent rien
    # dire et mangeaient un cinquième de la hauteur.
    y_max <- max(df$part_eag_2022, if (avec_raboud) dr$part_raboud_2026, na.rm = TRUE) * 1.08
    x_rng <- range(tous$dim1) + c(-0.3, 0.3)
    axe_x <- list(title = "Positionnement (Dim.1 ACP — gauche →)", range = x_rng,
                  zeroline = FALSE)

    # Étiquettes : les plus fortes sous-conversions EN VOIX (résidu × effectif),
    # regroupées en colonne dans le coin bas-droit, peu peuplé, avec un trait de
    # rappel — posées sur les points, elles se chevaucheraient (plusieurs
    # communes de Lavaux-Oron sont au même endroit).
    top <- df |>
      filter(ecart < -ECART_SEUIL) |>
      mutate(voix = -ecart * effectif) |>
      slice_max(voix, n = 5, with_ties = FALSE) |>
      arrange(part_eag_2022)          # le point le plus bas reçoit l'étiquette du bas
    x_lab <- x_rng[1] + 0.76 * diff(x_rng)
    y_pas <- 0.05 * y_max
    y_lab <- 0.04 * y_max + y_pas * (seq_len(nrow(top)) - 1)
    etiquettes <- lapply(seq_len(nrow(top)), function(i) list(
      x = top$dim1[i], y = top$part_eag_2022[i], xref = "x", yref = "y",
      ax = x_lab, ay = y_lab[i], axref = "x", ayref = "y", xanchor = "left",
      text = sprintf("%s  %s voix", top$Communes[i], fmt_signe(-top$voix[i])),
      showarrow = TRUE, arrowhead = 0, arrowwidth = 1, arrowcolor = "#999",
      font = list(size = 10, color = "#333"), bgcolor = "rgba(255,255,255,0.85)"))
    if (nrow(top) > 0)
      etiquettes <- c(etiquettes, list(list(
        x = x_lab, y = max(y_lab) + y_pas, xref = "x", yref = "y",
        xanchor = "left", showarrow = FALSE,
        text = "<i>Plus fortes sous-conversions</i>",
        font = list(size = 10, color = "#666"))))

    # Titres de panneau (la marge de subplot laisse [0, 0.47] et [0.53, 1])
    titre_panneau <- function(x, texte) list(
      x = x, y = 1.02, xref = "paper", yref = "paper", xanchor = "left",
      yanchor = "bottom", showarrow = FALSE, text = paste0("<b>", texte, "</b>"),
      font = list(size = 12, color = "#333"))
    titres <- list(titre_panneau(0, sprintf("GC 2022 — liste EàG (%d communes)", nrow(df))))
    if (avec_raboud)
      titres <- c(titres, list(titre_panneau(0.53, sprintf("Raboud CE 2026 (%d communes)", nrow(dr)))))

    fig <- if (avec_raboud)
      subplot(p1, p2, nrows = 1, shareY = TRUE, titleX = TRUE, margin = 0.03)
    else p1

    fig |> layout(
      xaxis  = axe_x,
      xaxis2 = if (avec_raboud) axe_x,
      yaxis  = list(title = "Part des bulletins", tickformat = ".0%",
                    range = c(0, y_max), zeroline = FALSE),
      annotations = c(titres, etiquettes),
      legend = list(orientation = "h", y = -0.18),
      margin = list(t = 40)
    )
  })

  # ── Comparaison GC 2022 ↔ CE 2026 ─────────────────────────
  cmp_filt <- reactive({
    df <- comparaison
    if (!is.null(input$cmp_dist) && input$cmp_dist != "Tous")
      df <- filter(df, district_electoral == input$cmp_dist)
    if (isTRUE(input$cmp_comparables))
      df <- filter(df, liste_eag_2022)
    df
  })

  # Infobulle commune : les trois lectures d'un coup, pour qu'aucun chiffre
  # brut ne soit lu sans sa base. Ajoutée comme COLONNE, et non passée en
  # vecteur à `text` : plotly scinde le data frame par couleur, et un vecteur
  # brut ne suivrait pas la découpe (infobulles décalées).
  ajouter_tooltip <- function(df) {
    df |> mutate(tooltip = paste0(
      "<b>", Communes, "</b> (", district_electoral, ")<br>",
      "GC 2022 : ", fmt_pct(part_eag_2022), " — ", fmt_num(voix_eag_2022),
      " voix / ", fmt_num(bulletins_2022), " bulletins<br>",
      "CE 2026 : ", fmt_pct(part_raboud_2026), " — ", fmt_num(voix_raboud_2026),
      " voix / ", fmt_num(bulletins_2026), " bulletins<br>",
      "Écart de part : ", sprintf("%+.1f", diff_part * 100), " pp<br>",
      "À participation 2022 : ", fmt_num(voix_raboud_base_2022), " voix (",
      fmt_signe(effet_performance), ")<br>",
      "Écart de voix brut : ", fmt_signe(diff_voix),
      " (dont ", fmt_signe(effet_participation), " de participation)<br>",
      "Statut : ", statut_comparaison))
  }

  output$p_cmp_scatter <- renderPlotly({
    df <- ajouter_tooltip(cmp_filt())
    if (nrow(df) == 0) return(NULL)
    lim <- max(df$part_eag_2022, df$part_raboud_2026, na.rm = TRUE) * 1.05

    plot_ly(
      df, x = ~part_eag_2022, y = ~part_raboud_2026,
      type = "scatter", mode = "markers",
      color = ~statut_comparaison, colors = STATUT_COMPARAISON_PAL,
      marker = list(size = ~pmax(4, log(bulletins_2022 + 1) * 2.2),
                    opacity = 0.7, line = list(width = 0.4, color = "#444")),
      text = ~tooltip, hovertemplate = "%{text}<extra></extra>"
    ) |>
      add_segments(x = 0, xend = lim, y = 0, yend = lim,
                   inherit = FALSE, showlegend = FALSE, hoverinfo = "skip",
                   line = list(color = "#666", dash = "dash", width = 1.5)) |>
      layout(
        xaxis  = list(title = "Part liste EàG/gauche — GC 2022",
                      tickformat = ".0%", range = c(0, lim)),
        yaxis  = list(title = "Part Raboud — CE 2026",
                      tickformat = ".0%", range = c(0, lim)),
        legend = list(orientation = "h", y = -0.15, title = list(text = ""))
      )
  })

  output$p_cmp_decompo <- renderPlotly({
    df <- comp_dist
    if (!is.null(input$cmp_dist) && input$cmp_dist != "Tous")
      df <- filter(df, district_electoral == input$cmp_dist)
    if (isTRUE(input$cmp_comparables)) df <- filter(df, liste_eag_2022)
    if (nrow(df) == 0) return(NULL)

    df <- df |> mutate(district_electoral = fct_reorder(district_electoral, diff_voix))

    plot_ly(df, y = ~district_electoral, x = ~effet_participation, type = "bar",
            orientation = "h", name = "Effet participation",
            marker = list(color = "#9ecae1"),
            text = ~paste0("<b>", district_electoral, "</b><br>",
                           "Effet participation : ", sprintf("%+.0f", effet_participation),
                           " voix<br>Bulletins : ", fmt_num(bulletins_2022), " → ",
                           fmt_num(bulletins_2026)),
            hovertemplate = "%{text}<extra></extra>") |>
      add_trace(x = ~effet_performance, name = "Effet performance",
                marker = list(color = "#c0392b"),
                text = ~paste0("<b>", district_electoral, "</b><br>",
                               "Effet performance : ", sprintf("%+.0f", effet_performance),
                               " voix<br>Part : ", fmt_pct(part_eag_2022), " → ",
                               fmt_pct(part_raboud_2026)),
                hovertemplate = "%{text}<extra></extra>") |>
      layout(barmode = "relative",
             xaxis  = list(title = "Voix"),
             yaxis  = list(title = ""),
             legend = list(orientation = "h", y = -0.15),
             margin = list(l = 120))
  })

  output$p_cmp_communes <- renderPlotly({
    tri <- input$cmp_tri %||% "diff_part"
    df  <- cmp_filt()
    if (nrow(df) == 0) return(NULL)
    n   <- max(1, input$cmp_n %||% 20)

    # On garde les n plus forts ET les n plus faibles : un classement qui ne
    # montrerait que les hausses masquerait les reculs, qui sont l'information
    # la plus coûteuse politiquement.
    df <- df |> arrange(desc(.data[[tri]]))
    df <- bind_rows(head(df, n), tail(df, n)) |>
      distinct(code_ofs, .keep_all = TRUE) |>
      ajouter_tooltip() |>
      mutate(
        # `val` devient une colonne pour survivre à la découpe par couleur.
        val      = if (tri == "diff_part") diff_part * 100 else .data[[tri]],
        Communes = fct_reorder(Communes, val)
      )

    lab <- switch(tri,
      diff_part         = "Écart de part 2022 → 2026 (points)",
      effet_performance = "Voix gagnées/perdues à participation 2022",
      part_raboud_2026  = "Part Raboud CE 2026")

    plot_ly(df, x = ~val, y = ~Communes, type = "bar", orientation = "h",
            color = ~statut_comparaison, colors = STATUT_COMPARAISON_PAL,
            text = ~tooltip, hovertemplate = "%{text}<extra></extra>") |>
      layout(xaxis  = list(title = lab, zeroline = TRUE, zerolinecolor = "#888"),
             yaxis  = list(title = ""),
             legend = list(orientation = "h", y = -0.08, title = list(text = "")),
             margin = list(l = 170))
  })

  output$tbl_comparaison <- renderDT({
    cmp_filt() |>
      arrange(desc(diff_part)) |>
      mutate(
        part_eag_2022         = round(part_eag_2022 * 100, 2),
        part_raboud_2026      = round(part_raboud_2026 * 100, 2),
        diff_part             = round(diff_part * 100, 2),
        voix_raboud_base_2022 = round(voix_raboud_base_2022),
        effet_performance     = round(effet_performance),
        effet_participation   = round(effet_participation)
      ) |>
      select(Communes, district_electoral, statut_comparaison, profil,
             bulletins_2022, voix_eag_2022, part_eag_2022,
             bulletins_2026, voix_raboud_2026, part_raboud_2026,
             diff_part, voix_raboud_base_2022, diff_voix,
             effet_performance, effet_participation) |>
      rename(Commune = Communes, District = district_electoral,
             Statut = statut_comparaison, Profil = profil,
             "Bull. 22" = bulletins_2022, "Voix 22" = voix_eag_2022,
             "Part 22 (%)" = part_eag_2022,
             "Bull. 26" = bulletins_2026, "Voix 26" = voix_raboud_2026,
             "Part 26 (%)" = part_raboud_2026,
             "Écart (pp)" = diff_part,
             "Voix 26 à particip. 22" = voix_raboud_base_2022,
             "Écart voix brut" = diff_voix,
             "Effet perf." = effet_performance,
             "Effet particip." = effet_participation) |>
      datatable(filter = "top",
                options = list(pageLength = 12, scrollX = TRUE,
                               order = list(list(10, "desc"))),
                rownames = FALSE) |>
      formatStyle("Statut",
                  backgroundColor = styleEqual(
                    names(STATUT_COMPARAISON_PAL),
                    c("#d6e6f4", "#eeeeee", "#fadbd5", "#fdf0d5")))
  })

  # Districts
  output$p_dist_score <- renderPlotly({
    tri <- input$dist_score %||% "score_district"
    df <- dist_levier |>
      filter(!is.na(.data[[tri]])) |>
      mutate(val = .data[[tri]],
             district_electoral = fct_reorder(district_electoral, val))
    plot_ly(df, x = ~val, y = ~district_electoral, type = "bar",
            orientation = "h",
            color = ~statut_district, colors = STATUT_PAL,
            text = ~paste0("<b>", district_electoral, "</b> (", statut_district, ")<br>",
                           "Score volume : ", round(score_district, 2), "<br>",
                           "Score marge : ", round(score_marge_district, 2),
                           " (marge moyenne ", fmt_pct(marge_district), ")<br>",
                           "Potentiel (voix) : ", round(potentiel_district), "<br>",
                           "Voix pour un siège : ", round(voix_siege),
                           " (", round(part_manquante * 100, 1), "% du total)<br>",
                           "dont pour le quorum 5 % : ", round(voix_quorum), "<br>",
                           "Sièges EàG 2022 : ", sieges_eag_22),
            hovertemplate = "%{text}<extra></extra>") |>
      layout(xaxis  = list(title = if (tri == "score_marge_district")
                             "Score marge du district (part du siège suivant / 1000 électeurs)"
                           else "Score volume du district"),
             yaxis  = list(title = ""),
             legend = list(title = list(text = "Statut")))
  })

  output$p_voix <- renderPlotly({
    df <- dist_levier |>
      mutate(district_electoral = fct_reorder(district_electoral, -voix_siege))
    plot_ly(df, x = ~voix_siege, y = ~district_electoral, type = "bar",
            orientation = "h",
            color = ~statut_district, colors = STATUT_PAL,
            text  = ~paste0("<b>", district_electoral, "</b><br>",
                            round(voix_siege), " voix pour un siège (",
                            round(part_manquante * 100, 1), "% du total)<br>",
                            "dont ", round(voix_quorum), " pour franchir le quorum de 5 %"),
            hovertemplate = "%{text}<extra></extra>") |>
      layout(xaxis  = list(title = "Voix supplémentaires pour un siège (règle LEDP)"),
             yaxis  = list(title = ""),
             legend = list(title = list(text = "Statut")))
  })

  output$p_sieges <- renderPlotly({
    df_long <- dist_levier |>
      select(district_electoral, sieges_eag_22, sieges_eag_27, sieges_eag_raboud) |>
      pivot_longer(c(sieges_eag_22, sieges_eag_27, sieges_eag_raboud),
                   names_to = "annee", values_to = "sieges") |>
      mutate(
        annee = recode(annee, sieges_eag_22 = "2022", sieges_eag_27 = "2027 (est.)",
                       sieges_eag_raboud = "Scénario Raboud CE 2026"),
        annee = factor(annee, levels = c("2022", "2027 (est.)", "Scénario Raboud CE 2026")),
        district_electoral = fct_reorder(district_electoral, sieges, max)
      )
    # `barmode` est un attribut de layout, pas de trace : le passer à plot_ly()
    # déclenche un avertissement plotly à chaque rendu.
    plot_ly(df_long, x = ~sieges, y = ~district_electoral, type = "bar",
            orientation = "h", color = ~annee,
            colors = c("2022" = "#888", "2027 (est.)" = "#c0392b",
                       "Scénario Raboud CE 2026" = "#8c6bb1")) |>
      layout(barmode = "group",
             xaxis = list(title = "Sièges EàG"),
             yaxis = list(title = ""),
             legend = list(title = list(text = ""), orientation = "h", y = -0.15))
  })

  output$tbl_districts <- renderDT({
    dist_levier |>
      arrange(desc(score_district)) |>
      mutate(
        part_eag_dist    = paste0(round(part_eag_dist    * 100, 1), " %"),
        part_gauche_dist = paste0(round(part_gauche_dist * 100, 1), " %"),
        part_raboud_dist = paste0(round(part_raboud_dist * 100, 1), " %"),
        part_manquante   = paste0(round(part_manquante   * 100, 1), " %"),
        voix_quorum      = round(voix_quorum),
        voix_siege       = round(voix_siege),
        score_district   = round(score_district, 2),
        score_marge_district = round(score_marge_district, 3),
        marge_district   = paste0(round(marge_district * 100, 1), " %")
      ) |>
      select(district_electoral, score_district, score_marge_district, marge_district,
             total_valables, votes_eag, part_eag_dist,
             part_raboud_dist, part_gauche_dist, sieges_2022, sieges_2027,
             sieges_eag_22, sieges_eag_27, sieges_eag_raboud,
             voix_quorum, voix_siege, part_manquante, statut_district) |>
      rename(District = district_electoral, "Score volume" = score_district,
             "Score marge" = score_marge_district, "Marge moy." = marge_district,
             Votants = total_valables,
             "Voix EàG" = votes_eag, "Part EàG" = part_eag_dist,
             "Part Raboud 26" = part_raboud_dist,
             "Part gauche" = part_gauche_dist,
             "Sièges 22" = sieges_2022, "Sièges 27" = sieges_2027,
             "EàG 22" = sieges_eag_22, "EàG 27 (est.)" = sieges_eag_27,
             "EàG scén. Raboud" = sieges_eag_raboud,
             "Voix quorum 5 %" = voix_quorum, "Voix pour 1 siège" = voix_siege,
             "Manquant (%)" = part_manquante,
             Statut = statut_district) |>
      datatable(options = list(dom = "ft", pageLength = 20), rownames = FALSE) |>
      formatStyle("Statut",
                  backgroundColor = styleEqual(
                    c("consolidation", "offensive", "conquete_longue"),
                    c("#d4edda",       "#fff3cd",   "#e8eaed")))
  })

  # Communes
  output$p_comm_bar <- renderPlotly({
    tri <- c_tri()
    # `val` en colonne pour survivre à la découpe par couleur de plotly
    df <- comm_filt() |>
      mutate(val = .data[[tri]]) |>
      filter(val > 0) |>
      arrange(desc(val)) |>
      slice_head(n = max(1, input$c_n %||% 25)) |>
      mutate(Communes = fct_reorder(Communes, val))
    if (nrow(df) == 0) return(NULL)

    plot_ly(df, x = ~val, y = ~Communes, type = "bar",
            orientation = "h", color = ~profil, colors = PROFIL_PAL,
            text  = ~paste0("<b>", Communes, "</b> — ", district_electoral, "<br>",
                            "Score volume : ", round(score_priorite, 2), "<br>",
                            "Score marge : ", round(score_marge, 2),
                            " (marge ", fmt_pct(marge_progression), ")<br>",
                            "Voix à trouver ici : ", voix_a_trouver, "<br>",
                            "Part du potentiel du district : ", fmt_pct(part_score_district), "<br>",
                            "EàG 2022 : ", fmt_pct(part_eag_2022),
                            ifelse(liste_eag_2022, "", " <i>(pas de liste)</i>"), "<br>",
                            "Raboud CE 2026 : ", ifelse(raboud_voix > 0, fmt_pct(part_raboud_2026), "0"), "<br>",
                            "Signal retenu : ", source_marge, "<br>",
                            "Type d'électorat : ", type_electorat, "<br>",
                            "Effectif : ", fmt_num(effectif)),
            hovertemplate = "%{text}<extra></extra>") |>
      layout(xaxis  = list(title = c_tri_lab()),
             yaxis  = list(title = ""),
             legend = list(title = list(text = "Profil")),
             margin = list(l = 150))
  })

  output$tbl_communes <- renderDT({
    comm_filt() |>
      arrange(desc(.data[[c_tri()]])) |>
      mutate(
        part_eag_2022    = round(part_eag_2022 * 100, 1),
        part_gauche_2022 = round(part_gauche_2022 * 100, 1),
        part_ps_verts    = round(part_ps_verts * 100, 1),
        # particip_moy est stockée en fraction depuis analyse.R
        particip_moy     = round(particip_moy * 100, 1),
        dim1             = round(dim1, 2),
        ecart            = round(ecart * 100, 1),
        part_raboud_2026 = round(part_raboud_2026 * 100, 1),
        tendance_raboud  = round(tendance_raboud * 100, 1),
        score_priorite   = round(score_priorite, 2),
        score_marge      = round(score_marge, 3),
        marge_progression = round(marge_progression * 100, 1),
        part_score_district = round(part_score_district * 100, 1),
        part_eag_cc      = round(part_eag_cc * 100, 1)
      ) |>
      select(Communes, district_electoral, profil, type_electorat,
             part_eag_2022, part_raboud_2026, tendance_raboud, part_gauche_2022, part_ps_verts,
             dim1, ecart, particip_moy, effectif, voix_a_trouver, score_priorite,
             score_marge, marge_progression,
             part_score_district, part_eag_cc, source_marge) |>
      rename(Commune = Communes, District = district_electoral, Profil = profil,
             "Électorat" = type_electorat,
             "EàG 22 (%)" = part_eag_2022, "Raboud 26 (%)" = part_raboud_2026,
             "Tend. (pp)" = tendance_raboud, "Gauche 22 (%)" = part_gauche_2022,
             "PS+V (%)" = part_ps_verts, "Pos." = dim1,
             "Résidu (pp)" = ecart, "Part. (%)" = particip_moy,
             Effectif = effectif, "Voix à trouver" = voix_a_trouver,
             "Score volume" = score_priorite,
             "Score marge" = score_marge, "Marge (pp)" = marge_progression,
             "Part district (%)" = part_score_district,
             "CC EàG (%)" = part_eag_cc, "Signal" = source_marge) |>
      datatable(filter = "top",
                options = list(pageLength = 12, scrollX = TRUE),
                rownames = FALSE) |>
      formatStyle("Profil",
                  backgroundColor = styleEqual(
                    names(PROFIL_PAL),
                    c("#d5f5d5", "#d0e8f7", "#fde0d0", "#ecdcea", "#eeeeee")
                  ))
  })

  # Carte leaflet scores
  output$p_map <- renderLeaflet({
    df <- communes_limites

    if (input$map_color == "profil") {
      # `levels` et non `domain` : colorFactor trie un domain par ordre
      # alphabétique puis applique les couleurs dans l'ordre de la palette,
      # ce qui décalait les couleurs par rapport à PROFIL_PAL.
      pal <- colorFactor(
        unname(PROFIL_PAL), levels = names(PROFIL_PAL), na.color = "#cccccc"
      )
      fill_col      <- pal(df$profil)
      legend_pal    <- pal
      legend_values <- names(PROFIL_PAL)
      legend_title  <- "Profil"
    } else {
      # Seul le score volume, très asymétrique (Lausanne écrase tout), passe
      # en échelle log ; le score marge et la marge se lisent en linéaire.
      log_ech <- input$map_color == "score_priorite"
      brut    <- df[[input$map_color]]
      metric  <- if (log_ech) log1p(brut) else brut
      pal <- colorNumeric("YlOrRd", domain = metric, na.color = "#cccccc")
      fill_col      <- pal(metric)
      legend_pal    <- pal
      legend_values <- metric
      legend_title  <- switch(input$map_color,
        score_priorite    = "Score volume",
        score_marge       = "Score marge<br><small>siège suivant / 1000 élect.</small>",
        marge_progression = "Marge de progression")
    }

    popup_txt <- paste0(
      "<b>", df$Communes, "</b> (", df$district_electoral, ")<br>",
      "Profil : ", df$profil, "<br>",
      "Électorat : ", df$type_electorat, "<br>",
      "<i>", df$message_cle, "</i><br>",
      "Score volume : ", round(df$score_priorite, 2), "<br>",
      "Score marge : ", round(df$score_marge, 2),
      " (marge ", fmt_pct(df$marge_progression), ", ", df$source_marge, ")<br>",
      "Voix à trouver ici : ", df$voix_a_trouver, "<br>",
      "Part du potentiel du district : ", fmt_pct(df$part_score_district), "<br>",
      "EàG 2022 : ", fmt_pct(df$part_eag_2022),
      ifelse(df$liste_eag_2022, "", " <i>(pas de liste déposée)</i>"), "<br>",
      "Raboud CE 2026 : ", fmt_pct(df$part_raboud_2026), "<br>",
      "Effectif : ", fmt_num(df$effectif)
    )

    leaflet(df) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      addPolygons(
        fillColor   = fill_col,
        fillOpacity = 0.75,
        color       = "#555", weight = 0.5, opacity = 1,
        highlightOptions = highlightOptions(
          weight = 2, color = "#222", fillOpacity = 0.9, bringToFront = TRUE
        ),
        popup = popup_txt,
        label = ~Communes
      ) |>
      addLegend(
        position = "bottomright", pal = legend_pal, values = legend_values,
        title = legend_title, opacity = 0.85, na.label = "Sans donnée",
        labFormat = switch(input$map_color,
          score_priorite    = labelFormat(transform = function(x) round(expm1(x))),
          marge_progression = labelFormat(suffix = " %", transform = function(x) round(x * 100, 1)),
          labelFormat())
      )
  })

  # Carte leaflet des districts électoraux
  output$d_map <- renderLeaflet({
    df  <- districts_limites
    col <- input$dmap_color %||% "score_district"
    def <- DMAP_METRIQUES[[col]]

    if (def$fmt == "cat") {
      pal <- colorFactor(unname(STATUT_PAL), levels = names(STATUT_PAL),
                         na.color = "#cccccc")
      legend_values <- names(STATUT_PAL)
      lab_format    <- labelFormat()
    } else {
      pal <- colorNumeric("YlOrRd", domain = df[[col]], na.color = "#cccccc",
                          reverse = isTRUE(def$inverse))
      legend_values <- df[[col]]
      lab_format <- switch(def$fmt,
        pct = labelFormat(suffix = " %", transform = function(x) round(x * 100, 1)),
        labelFormat(big.mark = " "))
    }

    popup_txt <- paste0(
      "<b>", df$district_electoral, "</b> — ", df$statut_district,
      " (", df$n_communes, " communes)<br>",
      "Sièges 2027 : ", df$sieges_2027,
      " — EàG : ", df$sieges_eag_22, " en 2022, ", df$sieges_eag_27, " estimé 2027, ",
      df$sieges_eag_raboud, " scénario Raboud<br>",
      "Voix pour un siège : ", fmt_num(df$voix_siege),
      " (", fmt_pct(df$part_manquante), " du district)<br>",
      "Voix pour le quorum 5 % : ", fmt_num(df$voix_quorum), "<br>",
      "Score volume : ", round(df$score_district, 2), "<br>",
      "Score marge : ", round(df$score_marge_district, 2),
      " (marge moyenne ", fmt_pct(df$marge_district), ")<br>",
      "Part EàG GC 2022 : ", fmt_pct(df$part_eag_dist), "<br>",
      "Part Raboud CE 2026 : ", fmt_pct(df$part_raboud_dist), "<br>",
      "Part gauche 2022 : ", fmt_pct(df$part_gauche_dist)
    )

    carte <- leaflet(df) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      addPolygons(
        fillColor   = pal(df[[col]]),
        fillOpacity = 0.75,
        color       = "#333", weight = 1.2, opacity = 1,
        highlightOptions = highlightOptions(
          weight = 3, color = "#111", fillOpacity = 0.9, bringToFront = TRUE
        ),
        popup = popup_txt,
        label = paste0(df$district_electoral, " — ", def$lab, " : ",
                       fmt_metrique(df[[col]], def$fmt))
      )

    # labelOptions() n'accepte qu'une direction par couche : une couche par direction
    for (dir in unique(districts_etiquettes$direction)) {
      carte <- carte |>
        addLabelOnlyMarkers(
          data  = filter(districts_etiquettes, direction == dir), lng = ~X, lat = ~Y,
          label = ~district_electoral,
          labelOptions = labelOptions(
            noHide = TRUE, textOnly = TRUE, direction = dir,
            style  = list("font-weight" = "bold", "font-size" = "11px",
                          "color" = "#222", "text-shadow" = "0 0 3px #fff, 0 0 3px #fff")
          )
        )
    }

    carte |>
      addLegend(
        position = "bottomright", pal = pal, values = legend_values,
        title = paste0(def$lab, if (isTRUE(def$inverse)) "<br><small>foncé = moins cher</small>"),
        opacity = 0.85, labFormat = lab_format
      )
  })

  # Carte leaflet voix manquantes
  output$v_map <- renderLeaflet({
    df <- communes_limites

    if (input$map_color_voix == "voix") {
      metric        <- df$voix_a_trouver
      legend_title  <- "Voix à trouver dans la commune"
    } else {
      metric        <- df$voix_quorum_commune
      legend_title  <- "Contribution au franchissement du quorum 5 %"
    }

    pal           <- colorNumeric("YlOrRd", domain = log1p(metric), na.color = "#cccccc")
    fill_col      <- pal(log1p(metric))
    legend_pal    <- pal
    legend_values <- log1p(metric)

    popup_txt <- paste0(
      "<b>", df$Communes, "</b> (", df$district_electoral, ")<br>",
      "Profil : ", df$profil, "<br>",
      "Score : ", round(df$score_priorite, 2), "<br>",
      "EàG 2022 : ", fmt_pct(df$part_eag_2022), "<br>",
      "Voix à trouver ici : ", df$voix_a_trouver, "<br>",
      "dont pour le quorum 5 % : ", df$voix_quorum_commune, "<br>",
      "Objectif du district : ", df$voix_siege, " voix pour un siège<br>",
      "Effectif : ", fmt_num(df$effectif)
    )

    leaflet(df) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      addPolygons(
        fillColor   = fill_col,
        fillOpacity = 0.75,
        color       = "#555", weight = 0.5, opacity = 1,
        highlightOptions = highlightOptions(
          weight = 2, color = "#222", fillOpacity = 0.9, bringToFront = TRUE
        ),
        popup = popup_txt,
        label = ~Communes
      ) |>
      addLegend(
        position = "bottomright", pal = legend_pal, values = legend_values,
        title = legend_title, opacity = 0.85,
        labFormat = if (input$map_color_voix == "voix")
          labelFormat(transform = function(x) round(expm1(x)))
        else
          labelFormat(suffix = " %", transform = function(x) round(expm1(x) * 100, 1))
      )
  })

  # Méthodologie
  output$ui_methodo <- renderUI({
    div(class = "d-flex flex-column gap-4 py-1",

      p(class = "text-muted",
        "Ce dashboard classe les communes vaudoises par rendement attendu en sièges pour les cantonales Grand Conseil 2027."),

      # Score
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Le score de priorité"),
        p("Les sièges du Grand Conseil sont attribués ", strong("par district électoral"),
          " . Le rendement d'un effort de campagne se décide donc d'abord ",
          "au niveau du district, et ensuite au niveau de la commune."),
        tags$ol(class = "mb-2",
          tags$li(strong("Score de district (couche 1) : "),
            "on additionne le potentiel de progression de toutes les communes du district ",
            "(marge de progression × effectif), puis on multiplie par le ", strong("levier siège"),
            " (1 / voix manquantes pour le prochain siège). Ce score classe les districts ",
            "— voir le graphique « Priorité par district »."),
          tags$li(strong("Score de commune (couche 2) : "),
            "à l'intérieur d'un district, chaque commune reçoit la part du potentiel du district ",
            "qu'elle représente (sa marge de progression × son effectif). ",
            "La colonne « Part district » du tableau indique ce poids relatif.")),
        tags$ul(class = "mb-2",
          tags$li(strong("Marge de progression :"),
            " le plus fort de deux signaux — (a) la commune vote plus à gauche que ce qu'EàG y a obtenu en 2022 ",
            "(résidu négatif dans « Écart de conversion ») ; (b) Raboud, au Conseil d'État 2026, y a rassemblé ",
            "nettement plus de voix que la liste EàG en 2022. ",
            "Le second signal capte les communes où EàG a déjà démontré une portée que son score de liste ne reflète pas."),
          tags$li(strong("Levier siège :"),
            " combien de voix manquent à EàG pour décrocher le prochain siège dans ce district ? ",
            "Plus ce nombre est faible, plus chaque vote supplémentaire compte."),
          tags$li(strong("Effectif électoral :"),
            " combien d'électeurs sont dans la commune ? Une grande commune contribue davantage au total du district.")),
        div(class = "bg-light rounded p-2 text-center font-monospace small",
          "score commune  =  marge de progression  ×  (1 / voix manquantes)  ×  effectif"),
        p(class = "text-muted small mt-2 mb-0",
          "En district de consolidation (EàG déjà représentée), la marge est remplacée par ",
          "max(marge, part EàG 2022) pour valoriser aussi la défense des votes acquis.")),

      # Deux lectures du score
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Deux lectures : score volume et score marge"),
        p("Le score ci-dessus est dominé par la taille des communes. ",
          "C'est ce qu'il faut pour répartir l'effort, mais cela masque ", strong("où se trouve la marge"),
          ". Il est donc décliné en deux :"),
        tags$table(class = "table table-sm table-bordered mb-2",
          tags$thead(class = "table-light",
            tags$tr(tags$th("Score"), tags$th("Formule"), tags$th("Question à laquelle il répond"))),
          tags$tbody(
            tags$tr(
              tags$td(strong("Volume"), tags$br(), tags$small("avec effectif")),
              tags$td(tags$code("marge × effectif / voix pour un siège")),
              tags$td("Combien de fois les voix manquantes pour le siège suivant la commune peut-elle ",
                      "apporter à elle seule ? → où concentrer l'effort.")),
            tags$tr(
              tags$td(strong("Marge"), tags$br(), tags$small("sans effectif")),
              tags$td(tags$code("marge × 1000 / voix pour un siège")),
              tags$td("Quelle part du siège suivant gagne-t-on en convertissant 1000 électeurs de ",
                      "cette commune ? → où chaque contact rapporte le plus.")))),
       ),

      # Règle d'attribution des sièges
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Comment se gagne un siège au Grand Conseil vaudois"),
        p("La répartition suit la ", strong("règle vaudoise"), " (Const. VD art. 93 al. 4 ; LEDP art. 73 ss):"),
        tags$ol(class = "mb-2",
          tags$li(strong("Quorum de 5 %"), " des voix de l'arrondissement. En dessous, la liste est ",
                  "écartée de toute la répartition. C'est la ", strong("première barre réelle"),
                  " et la seule qui compte tant qu'elle n'est pas franchie."),
          tags$li(strong("Quotient électoral"), " = voix des listes admises ÷ sièges à pourvoir."),
          tags$li("Chaque liste reçoit autant de sièges que son total contient de quotients."),
          tags$li(strong("Plus forts restes"), " pour les sièges qui restent. C'est ainsi qu'une petite ",
                  "liste décroche son premier siège, avec bien moins qu'un quotient complet.")),
        p(class = "small mb-2",
          "Le scénario modélisé est celui d'une ", strong("liste EàG seule, sans apparentement"),
          " : le quorum doit être franchi par la liste elle-même."),
        p(class = "text-muted small mb-0",
          "Les colonnes « Voix quorum 5 % » et « Voix pour 1 siège » du tableau des districts ",
          "donnent les deux objectifs chiffrés. La simulation utilise les voix réelles des six blocs ",
          "politiques en 2022.")),

      # Classement des districts
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Comment lire le classement des districts"),
        p("Le statut sert à ", strong("lire"), " le classement : ",
          "les 300 communes reçoivent un score"),
        tags$table(class = "table table-sm table-bordered mb-2",
          tags$thead(class = "table-light",
            tags$tr(
              tags$th("Statut"), tags$th("Condition"), tags$th("Ce que cela signifie"))),
          tags$tbody(
            tags$tr(
              tags$td(tags$span(class = "badge", style = "background:#4dac26", "consolidation")),
              tags$td("EàG avait ≥ 1 siège en 2022"),
              tags$td("Siège(s) à défendre")),
            tags$tr(
              tags$td(tags$span(class = "badge", style = "background:#f0a500; color:#000", "offensive")),
              tags$td("Premier siège à ≤ 12 % des voix du district"),
              tags$td("Conquête réaliste sur une législature")),
            tags$tr(
              tags$td(tags$span(class = "badge", style = "background:#7f8fa6", "conquête longue")),
              tags$td("Premier siège à > 12 % des voix du district"),
              tags$td("Plus coûteux en part relative, mais souvent peu cher en voix absolues, ",
                      "car ces arrondissements sont petits. Toujours classé.")))),
        ),

      # Profils
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Que faire dans chaque commune ?"),
        p("Le profil indique le type d'action le plus adapté, selon le positionnement de la commune et son taux de participation aux votations :"),
        tags$table(class = "table table-sm table-bordered mb-2",
          tags$thead(class = "table-light",
            tags$tr(
              tags$th("Profil"), tags$th("Diagnostic"), tags$th("Action prioritaire"))),
          tags$tbody(
            tags$tr(style = "background:#d5f5d5",
              tags$td(strong("Consolidation")),
              tags$td("District avec siège EàG, ou présence au conseil communal"),
              tags$td("Maintenir et mobiliser la base: permanences, affichage, réseaux militants")),
            tags$tr(style = "background:#d0e8f7",
              tags$td(strong("Mobilisation")),
              tags$td("Électorat favorable mais participation faible"),
              tags$td("Faire voter des gens déjà acquis: tractage ciblé, rappels de vote")),
            tags$tr(style = "background:#fde0d0",
              tags$td(strong("Persuasion")),
              tags$td("Raboud y a fait nettement mieux que la médiane, ou EàG y sous-performe son potentiel"),
              tags$td("Convaincre des électeurs proches: porte-à-porte, événements publics, médias locaux")),
            tags$tr(style = "background:#ecdcea",
              tags$td(strong("Report de gauche")),
              tags$td("Réservoir PS/Verts supérieur à la médiane, non capté par EàG"),
              tags$td("Différencier la ligne: thèmes sociaux, logement, salaires ; viser le report intra-gauche")),
            tags$tr(style = "background:#eeeeee",
              tags$td(strong("Implantation")),
              tags$td("Aucune des conditions ci-dessus"),
              tags$td("Construire une présence : militant·es référent·es, visibilité locale, et d'abord déposer une liste")))),
        p(class = "text-muted small mb-0",
          "Les seuils (médianes de dim1, de participation et du réservoir PS/Verts) portent sur les 300 communes. ",
          "Le seuil Raboud est la médiane des seules communes où elle a récolté des voix. ",
          "Aucune commune n'est laissée sans profil.")),

      # Comparaison 2022 / 2026
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Comparer le GC 2022 et le CE 2026 sans se tromper"),
        p("Mettre côte à côte ", strong("6 179"), " voix de liste en 2022 et ", strong("17 970"),
          " voix Raboud en 2026 donne un « ×2,9 » qui ne veut rien dire : les deux scrutins ",
          "n'ont ni la même base, ni le même mode, ni la même géographie de candidature."),
        tags$ol(class = "mb-2",
          tags$li(strong("En part. "),
            "Chaque score est rapporté à ses propres bulletins : 4,46 % au GC 2022, 7,77 % au CE 2026. ",
            "C'est la seule mesure de performance."),
          tags$li(strong("Décomposition de l'écart de voix. "),
            "L'identité ", tags$code("voix26 − voix22 = part26 × (bull26 − bull22) + (part26 − part22) × bull22"),
            " est exacte et sépare deux choses très différentes :")),
        tags$table(class = "table table-sm table-bordered mb-2",
          tags$thead(class = "table-light",
            tags$tr(tags$th("Effet"), tags$th("Voix"), tags$th("Ce que cela vaut pour 2027"))),
          tags$tbody(
            tags$tr(
              tags$td(tags$span(class = "badge", style = "background:#9ecae1; color:#000",
                                "participation")),
              tags$td("+7 283"),
              tags$td("Le CE 2026 a compté +67 % de bulletins. Rien d'acquis : cet écart s'évapore ",
                      "si le GC 2027 mobilise comme le GC 2022.")),
            tags$tr(
              tags$td(tags$span(class = "badge", style = "background:#c0392b", "performance")),
              tags$td("+4 508"),
              tags$td("Le vrai gain politique : à participation égale, Raboud attire ",
                      "plus d'électeurs que la liste de 2022.")))),
        p(class = "text-muted small mb-0",
          strong("Les 176 communes sans liste en 2022 ne « progressent » pas. "),
          "Elles partent de 0 par construction ; leur score Raboud mesure un potentiel révélé. ",
          "Elles sont isolées en orange, et le filtre « communes comparables » les retire du classement. ",
          "Attention aussi aux arrondissements où la liste 2022 existait sur le papier mais ne pesait ",
          "presque rien (Lavaux-Oron : 0,28 %) : l'écart y est mécaniquement spectaculaire.")),

      # Caveats
      div(class = "alert mb-0", style = "background:#fff8e1; border-left: 4px solid #f0a500;",
        h6(class = "fw-bold", "Points d'attention"),
        tags$ul(class = "mb-0 small",
          tags$li(strong("Absence de liste ≠ échec. "),
            "EàG n'avait de liste au GC 2022 que dans 7 arrondissements sur 13 : Aigle, Lausanne-Ville, Lavaux-Oron, ",
            "Ouest lausannois, Riviera, Romanel, Yverdon. Dans les 176 communes des six autres, une part de 0 % ne ",
            "mesure rien ; le résidu de conversion n'y est pas défini et la marge repose sur Raboud et le réservoir PS/Verts."),
          tags$li(strong("CE 2026 = plafond, pas prédiction. "),
            "L'élection au Conseil d'État est majoritaire avec panachage : le score de Raboud inclut des voix PS/Verts ",
            "qui ne se reporteront pas mécaniquement sur une liste EàG au GC."),
          tags$li(strong("Bases de bulletins distinctes. "),
            "Le CE 2026 a compté environ 1,6× plus de bulletins que le GC 2022. Chaque part est donc calculée sur ",
            "sa propre base ; la tendance médiane réelle est de +4,6 points, et non +8."),
          tags$li(strong("Vote sur enjeux ≠ vote de liste. "),
            "L'indice de positionnement reflète les votations, pas directement le vote Grand Conseil. Une commune à gauche sur les enjeux ne vote pas forcément EàG."),
          tags$li(strong("Niveau géographique. "),
            "L'analyse cible des communes, pas des individus. Ne pas en déduire le comportement d'électeurs spécifiques."),
          tags$li(strong("Voix nouvelles, pas transférées. "),
            "Le coût d'un siège suppose des voix venues de l'abstention. Un report depuis PS/Verts coûterait ",
            "un peu moins, puisqu'il abaisse en même temps le reste des listes concurrentes."),
          tags$li(strong("Apparentement non modélisé: estimations conservatrices. "),
            "Le modèle suppose une liste EàG seule devant franchir les 5 % par elle-même. Rejouée sur 2022, ",
            "la règle implémentée retrouve 68 des 78 répartitions arrondissement × bloc ; les écarts valent ±1 siège ",
            "et viennent des apparentements. En Riviera notamment, EàG a réellement obtenu ", strong("2 sièges"),
            " là où le modèle n'en prédit qu'un — l'alliance à gauche y valait un siège. ",
            "Reste à confirmer si le quorum de 5 % s'apprécie au niveau de la liste seule ou du groupe apparenté : ",
            "dans le second cas, la carte des cibles change entièrement.")))
    )
  })
}

shinyApp(ui, server)
