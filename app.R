# ============================================================
# Dashboard Shiny — Ciblage EàG, Cantonales Vaud 2027
# ============================================================
#
# OBJECTIF
#   Identifier les communes vaudoises où un effort de campagne a le meilleur
#   rendement en sièges pour les cantonales Grand Conseil 2027.
#   Formule de base : score = marge_de_progression × levier × effectif
#
# PIPELINE (calculé par analyse.R, lu ici depuis data/processed/)
#
#   1. ACP — indice de positionnement
#      Variables : votations cantonales/fédérales 2025–2026 + CE 2026 (Raboud).
#      Orientation : valeur élevée = plus à gauche.
#      dim1 = coordonnée sur Dim.1 = indice gauche–droite de la commune.
#
#   2. Écart de conversion
#      Régression pondérée : part_eag_2022 ~ dim1 (poids = effectif).
#      Résidu (ecart) = sur- ou sous-performance d'EàG vs le potentiel local.
#      marge_progression = max(0, −ecart)  →  potentiel latent non capté.
#
#   3. Classification stratégique des districts  [ÉTAPE PRÉALABLE AU SCORE]
#      Avant de scorer des communes, on écarte les districts sans perspective.
#        consolidation : EàG avait ≥ 1 siège en 2022  →  défendre
#        offensive     : voix manquantes ≤ 12 % du total  →  conquérir
#        hors_portee   : trop loin du prochain siège  →  EXCLU du ciblage
#      Paramètre : SEUIL_ATTEIGNABLE = 0.12 (ajustable dans analyse.R).
#
#   4. Levier (Hagenbach-Bischoff simplifié, par district)
#      quota_hb        = total_valables / (sieges_2027 + 1)
#      voix_manquantes = quota_hb × (sieges_eag + 1) − votes_eag
#      levier          = 1 / voix_manquantes  (absolu — indépendant de la
#                        taille du district, contrairement à total/manquants)
#
#   5. Score communal
#      Présence/offensive : marge_progression × levier × effectif
#      Consolidation      : max(marge_progression, part_eag_2022) × levier × effectif
#
#   6. Profils d'action (sur les communes retenues)
#      consolidation : district avec siège EàG  OU  présence CC 2026
#      mobilisation  : dim1 > médiane  ET  participation < médiane
#      persuasion    : dim1 ≤ médiane  ET  ecart < 0
#      autre         : aucune des conditions ci-dessus
#
# LIMITES
#   - Référence électorale 2022 : décalage temporel de 5 ans. L'écart entre
#     le positionnement récent (ACP 2025–2026) et la base 2022 est lui-même
#     un signal de tendance, pas un biais à corriger.
#   - HB simplifié : voix_manquantes calculé sur les seules voix EàG.
#     Le calcul exact requiert les voix de toutes les listes concurrentes.
#   - Sophisme écologique : l'analyse cible des lieux, pas des individus.
# ============================================================
library(shiny)
library(bslib)
library(tidyverse)
library(FactoMineR)
library(DT)
library(plotly)
library(leaflet)
library(sf) 
library(here)

# ── Helpers ──────────────────────────────────────────────────
fmt_pct <- function(x) paste0(round(x * 100, 1), " %")
fmt_num <- function(x) format(round(x), big.mark = " ", scientific = FALSE)


# ── Constantes ───────────────────────────────────────────────
SEUIL_NA  <- 0.3
PROFIL_PAL <- c(
  mobilisation  = "#2166ac",
  persuasion    = "#d6604d",
  consolidation = "#4dac26",
  autre         = "#aaaaaa"
)

# ── Données pré-calculées ────────────────────────────────────
communes    <- read_csv("data/processed/communes_scores.csv",  show_col_types = FALSE)
dist_levier <- read_csv("data/processed/districts_levier.csv", show_col_types = FALSE)
dict_vot    <- read_csv("data/processed/votations_dict.csv",         show_col_types = FALSE) |>
  filter(!duplicated(votation))%>%
  filter(type %in% c("initiative", "loi") | votation %in% c("vdce_2026_eag"))

communes_limites <- read_sf(
    here::here("data/raw/CH_communes_no_lacs.gpkg"),
    layer = "CH_communes_no_lacs"
  ) |>
  filter(kantonsnummer == 22) |>
  st_zm(drop = TRUE) |>
  st_transform(4326) |>
  select(code_ofs = bfs_nummer) |>
  left_join(communes, by = "code_ofs")

# ── Reconstruction ACP ───────────────────────────────────────
raw_vot <- read_csv("data/processed/data_votations_vd.csv", show_col_types = FALSE) |>
  mutate(OUI_prop = OUI_perc / 100,
         NON_prop = NON_perc / 100) |>
  left_join(dict_vot |> select(votation, nom_complet, type), by = "votation")

vot_orient <- raw_vot |>
  semi_join(dict_vot, by = "votation") |>
  mutate(score = case_when(
    positionnement_politique_oui %in% c("gauche", "centre-gauche") ~ OUI_prop,
    positionnement_politique_oui %in% c("droite", "extreme-droite", "centre") ~ NON_prop,
    TRUE ~ NA_real_
  )) |>
  select(Communes, code_ofs, votation, score)

mat_wide <- vot_orient |>
  pivot_wider(names_from = votation, values_from = score)
cols_vot <- setdiff(names(mat_wide), c("Communes", "code_ofs"))

# vdce_2026_eag : communes non couvertes par la liste da. = 0 voix Raboud
if ("vdce_2026_eag" %in% cols_vot)
  mat_wide$vdce_2026_eag[is.na(mat_wide$vdce_2026_eag)] <- 0

mat_complete <- mat_wide |>
  filter(rowSums(is.na(across(all_of(cols_vot)))) / length(cols_vot) <= SEUIL_NA)
for (v in cols_vot) {
  med <- median(mat_complete[[v]], na.rm = TRUE)
  mat_complete[[v]][is.na(mat_complete[[v]])] <- med
}

res_pca <- mat_complete |>
  column_to_rownames("Communes") |>
  select(all_of(cols_vot)) |>
  PCA(scale.unit = TRUE, graph = FALSE)

var_exp <- res_pca$eig[, 2]
flip    <- mean(res_pca$var$coord[, 1]) < 0

ind_df <- as.data.frame(res_pca$ind$coord[, 1:2]) |>
  rownames_to_column("Communes") |>
  rename(Dim1 = Dim.1, Dim2 = Dim.2) |>
  left_join(communes |> select(Communes, district_electoral, profil,
                               part_eag_2022, part_gauche_2022, effectif,
                               score_priorite, ecart, dim1, particip_moy,
                               part_eag_cc),
            by = "Communes")
if (flip) ind_df <- mutate(ind_df, Dim1 = -Dim1)
ind_df <- ind_df |>
  mutate(cc_txt = ifelse(!is.na(part_eag_cc),
                         paste0("<br>CC 2026 EàG : ", fmt_pct(part_eag_cc)), ""))

var_df <- as.data.frame(res_pca$var$coord[, 1:2]) |>
  rownames_to_column("votation") |>
  rename(Dim1 = Dim.1, Dim2 = Dim.2) |>
  left_join(dict_vot |> select(votation, nom_complet), by = "votation")
if (flip) var_df <- mutate(var_df, Dim1 = -Dim1)

districts <- sort(unique(communes$district_electoral))

# ── UI ───────────────────────────────────────────────────────
ui <- page_fluid(
  theme = bs_theme(
    bootswatch = "cosmo",
    primary    = "#c0392b"
  ),

  # ── En-tête ─────────────────────────────────────────────────
  div(
    class = "py-3 mb-3 border-bottom",
    style = "border-color: #c0392b !important",
    h3(class = "mb-0 fw-bold", style = "color:#c0392b",
       "EàG — Ciblage communes, Cantonales Vaud 2027")
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
            "Chaque commune est classée selon trois facteurs multipliés :"),
          tags$ul(class = "small mb-1 ps-3",
            tags$li(strong("Marge de progression"), " — potentiel EàG non capté en 2022"),
            tags$li(strong("Levier siège"), " — proximité du prochain siège dans le district"),
            tags$li(strong("Effectif"), " — nombre d'électeurs de la commune")),
          div(class = "bg-light rounded p-2 text-center font-monospace small",
          "score  =  marge de progression  ×  (1 / voix manquantes)  ×  effectif"),
          p(class = "small text-muted mb-0",
            "Seuls les districts à moins de 12 % du prochain siège EàG",
            " (ou déjà représentés) sont inclus dans le ciblage.")
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
              tags$span(style = "color:#aaaaaa; font-size:1.1em", "●"), " ",
              strong("Autre"), " — faible priorité, ressources à réallouer"))
        ),

        div(
          h6(class = "fw-bold mb-2", "À garder en tête"),
          tags$ul(class = "small mb-0 ps-3",
            tags$li("La référence électorale est 2022 — 5 ans d'écart avec 2027."),
            tags$li("Positionnement sur les votations ≠ vote de liste EàG."),
            tags$li("Analyse géographique : ne pas inférer de comportements individuels."),
            tags$li("Le seuil de 12 % est un choix stratégique, pas un fait électoral."))
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

  # ── Carte ────────────────────────────────────────────────────
  card(
    card_header(
      div(class = "d-flex justify-content-between align-items-center",
        span("Carte — score de priorité par commune"),
        div(class = "d-flex gap-3 align-items-center",
          div(class = "d-flex align-items-center gap-1",
            span(class = "small text-muted", "Colorier :"),
            selectInput("map_color", NULL,
              choices  = c("Score" = "score_priorite", "Profil" = "profil"),
              selected = "score_priorite", width = "120px")
          )
        )
      )
    ),
    leafletOutput("p_map", height = "500px")
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
                choices  = c("Profil" = "profil",
                             "Part de gauche" = "part_gauche_2022"),
                selected = "profil", width = "160px")
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
              choices  = paste0("Dim.", seq_len(min(5, nrow(res_pca$eig)))),
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
        span("Écart de conversion — positionnement ACP vs vote EàG 2022"),
        div(class = "d-flex gap-3 align-items-center",
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

  # ── Districts ────────────────────────────────────────────────
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
              choices  = c("mobilisation", "persuasion", "consolidation", "autre"),
              selected = c("mobilisation", "persuasion", "consolidation"))
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
    if (!is.null(input$c_profil))
      df <- filter(df, profil %in% input$c_profil)
    df
  })

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
    sizes   <- if (isTRUE(input$acp_size)) pmax(3, sqrt(df$effectif) / 15) else 7

    if (input$acp_color == "profil") {
      pt <- plot_ly(
        df, x = ~Dim1, y = ~Dim2, type = "scatter", mode = "markers",
        color = ~profil, colors = PROFIL_PAL,
        marker = list(size = sizes, opacity = 0.75,
                      line = list(width = 0.3, color = "#333")),
        text  = ~paste0("<b>", Communes, "</b> (", district_electoral, ")<br>",
                        "EàG 2022 : ", fmt_pct(part_eag_2022), "<br>",
                        "Gauche 2022 : ", fmt_pct(part_gauche_2022), "<br>",
                        "Profil : ", profil, cc_txt),
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
      legend = list(title = list(text = "Profil"))
    )
  })

  output$p_scree <- renderPlotly({
    n <- min(8, nrow(res_pca$eig))
    eig_df <- data.frame(
      dim = factor(paste0("Dim.", seq_len(n)), levels = paste0("Dim.", seq_len(n))),
      pct = res_pca$eig[seq_len(n), 2]
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
      votation = rownames(res_pca$var$contrib),
      contrib  = res_pca$var$contrib[, dim_idx]
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
  output$p_ecart <- renderPlotly({
    df <- ecart_filt() |> filter(!is.na(ecart), !is.na(dim1))
    if (nrow(df) < 3) return(NULL)

    m   <- lm(part_eag_2022 ~ dim1, weights = effectif, data = df)
    xr  <- seq(min(df$dim1), max(df$dim1), length.out = 120)
    fit <- data.frame(dim1 = xr, y = predict(m, newdata = data.frame(dim1 = xr)))
    lim <- max(abs(df$ecart), na.rm = TRUE)

    plot_ly() |>
      add_trace(
        data = df, x = ~dim1, y = ~part_eag_2022,
        type = "scatter", mode = "markers", name = "Communes",
        marker = list(
          color      = ~ecart,
          colorscale = list(c(0, "#d6604d"), c(0.5, "#f7f7f7"), c(1, "#2166ac")),
          cmin = -lim, cmax = lim,
          colorbar = list(title = "Résidu"),
          size     = ~pmax(4, log(effectif + 1) * 2.2),
          opacity  = 0.8, line = list(width = 0.4, color = "#444")
        ),
        text = ~paste0("<b>", Communes, "</b> (", district_electoral, ")<br>",
                       "Positionnement : ", round(dim1, 2), "<br>",
                       "EàG 2022 : ", fmt_pct(part_eag_2022), "<br>",
                       "Résidu : ", round(ecart * 100, 1), " pp<br>",
                       "Effectif : ", fmt_num(effectif), "<br>",
                       "Profil : ", profil),
        hovertemplate = "%{text}<extra></extra>"
      ) |>
      add_trace(
        data = fit, x = ~dim1, y = ~y,
        type = "scatter", mode = "lines", name = "Régression",
        line = list(color = "#333", width = 2, dash = "dash"), hoverinfo = "skip"
      ) |>
      layout(
        xaxis  = list(title = "Positionnement (Dim.1 ACP — gauche →)"),
        yaxis  = list(title = "Part EàG 2022", tickformat = ".0%"),
        legend = list(orientation = "h", y = -0.12)
      )
  })

  # Districts
  output$p_voix <- renderPlotly({
    df <- dist_levier |>
      mutate(district_electoral = fct_reorder(district_electoral, voix_manquantes))
    plot_ly(df, x = ~voix_manquantes, y = ~district_electoral, type = "bar",
            orientation = "h",
            color = ~statut_district,
            colors = c(consolidation = "#4dac26", offensive = "#f0a500", hors_portee = "#cccccc"),
            text  = ~paste0("<b>", district_electoral, "</b><br>",
                            round(voix_manquantes), " voix (", round(part_manquante * 100, 1), "% du total)"),
            hovertemplate = "%{text}<extra></extra>") |>
      layout(xaxis  = list(title = "Voix manquantes"),
             yaxis  = list(title = ""),
             legend = list(title = list(text = "Statut")))
  })

  output$p_sieges <- renderPlotly({
    df_long <- dist_levier |>
      select(district_electoral, sieges_eag_22, sieges_eag_27) |>
      pivot_longer(c(sieges_eag_22, sieges_eag_27),
                   names_to = "annee", values_to = "sieges") |>
      mutate(
        annee = recode(annee, sieges_eag_22 = "2022", sieges_eag_27 = "2027 (est.)"),
        district_electoral = fct_reorder(district_electoral, sieges, max)
      )
    plot_ly(df_long, x = ~sieges, y = ~district_electoral, type = "bar",
            orientation = "h", color = ~annee, barmode = "group",
            colors = c("2022" = "#888", "2027 (est.)" = "#c0392b")) |>
      layout(barmode = "group",
             xaxis = list(title = "Sièges EàG"),
             yaxis = list(title = ""),
             legend = list(title = list(text = "")))
  })

  output$tbl_districts <- renderDT({
    dist_levier |>
      mutate(
        part_eag_dist    = paste0(round(part_eag_dist    * 100, 1), " %"),
        part_gauche_dist = paste0(round(part_gauche_dist * 100, 1), " %"),
        part_manquante   = paste0(round(part_manquante   * 100, 1), " %"),
        voix_manquantes  = round(voix_manquantes)
      ) |>
      select(district_electoral, total_valables, votes_eag, part_eag_dist,
             part_gauche_dist, sieges_2022, sieges_2027,
             sieges_eag_22, sieges_eag_27, voix_manquantes, part_manquante,
             statut_district) |>
      rename(District = district_electoral, Votants = total_valables,
             "Voix EàG" = votes_eag, "Part EàG" = part_eag_dist,
             "Part gauche" = part_gauche_dist,
             "Sièges 22" = sieges_2022, "Sièges 27" = sieges_2027,
             "EàG 22" = sieges_eag_22, "EàG 27 (est.)" = sieges_eag_27,
             "Voix manq." = voix_manquantes, "Manquant (%)" = part_manquante,
             Statut = statut_district) |>
      datatable(options = list(dom = "ft", pageLength = 20), rownames = FALSE) |>
      formatStyle("Statut",
                  backgroundColor = styleEqual(
                    c("consolidation", "offensive", "hors_portee"),
                    c("#d4edda",       "#fff3cd",   "#f8d7da")))
  })

  # Communes
  output$p_comm_bar <- renderPlotly({
    df <- comm_filt() |>
      filter(score_priorite > 0) |>
      arrange(desc(score_priorite)) |>
      slice_head(n = max(1, input$c_n %||% 25)) |>
      mutate(Communes = fct_reorder(Communes, score_priorite))

    plot_ly(df, x = ~score_priorite, y = ~Communes, type = "bar",
            orientation = "h", color = ~profil, colors = PROFIL_PAL,
            text  = ~paste0("<b>", Communes, "</b> — ", district_electoral, "<br>",
                            "Score : ", round(score_priorite), "<br>",
                            "EàG 2022 : ", fmt_pct(part_eag_2022), "<br>",
                            "Résidu : ", round(ecart * 100, 1), " pp<br>",
                            "Effectif : ", fmt_num(effectif)),
            hovertemplate = "%{text}<extra></extra>") |>
      layout(xaxis  = list(title = "Score de priorité"),
             yaxis  = list(title = ""),
             legend = list(title = list(text = "Profil")),
             margin = list(l = 150))
  })

  output$tbl_communes <- renderDT({
    comm_filt() |>
      arrange(desc(score_priorite)) |>
      mutate(
        part_eag_2022    = round(part_eag_2022 * 100, 1),
        part_gauche_2022 = round(part_gauche_2022 * 100, 1),
        part_ps_verts    = round(part_ps_verts * 100, 1),
        particip_moy     = round(particip_moy * 100, 1),
        dim1             = round(dim1, 2),
        ecart            = round(ecart * 100, 1),
        score_priorite   = round(score_priorite),
        part_eag_cc      = round(part_eag_cc * 100, 1)
      ) |>
      select(Communes, district_electoral, profil,
             part_eag_2022, part_gauche_2022, part_ps_verts,
             dim1, ecart, particip_moy, effectif, score_priorite,
             part_eag_cc) |>
      rename(Commune = Communes, District = district_electoral, Profil = profil,
             "EàG 22 (%)" = part_eag_2022, "Gauche 22 (%)" = part_gauche_2022,
             "PS+V (%)" = part_ps_verts, "Pos." = dim1,
             "Résidu (pp)" = ecart, "Part. (%)" = particip_moy,
             Effectif = effectif, Score = score_priorite,
             "CC EàG (%)" = part_eag_cc) |>
      datatable(filter = "top",
                options = list(pageLength = 12, scrollX = TRUE),
                rownames = FALSE) |>
      formatStyle("Profil",
                  backgroundColor = styleEqual(
                    c("mobilisation", "persuasion", "consolidation", "autre"),
                    c("#d0e8f7", "#fde0d0", "#d5f5d5", "#eeeeee")
                  ))
  })

  # Carte leaflet
  output$p_map <- renderLeaflet({
    df <- communes_limites

    if (input$map_color == "profil") {
      pal <- colorFactor(
        PROFIL_PAL, domain = names(PROFIL_PAL), na.color = "#cccccc"
      )
      fill_col      <- pal(df$profil)
      legend_pal    <- pal
      legend_values <- names(PROFIL_PAL)
      legend_title  <- "Profil"
    } else {
      pal <- colorNumeric(
        "YlOrRd", domain = log1p(df$score_priorite), na.color = "#cccccc"
      )
      fill_col      <- pal(log1p(df$score_priorite))
      legend_pal    <- pal
      legend_values <- log1p(df$score_priorite)
      legend_title  <- "Score"
    }

    popup_txt <- paste0(
      "<b>", df$Communes, "</b> (", df$district_electoral, ")<br>",
      "Profil : ", df$profil, "<br>",
      "Score : ", round(df$score_priorite), "<br>",
      "EàG 2022 : ", fmt_pct(df$part_eag_2022), "<br>",
      "Résidu : ", round(df$ecart * 100, 1), " pp<br>",
      "Effectif : ", fmt_num(df$effectif)
    )

    leaflet(df) |>
      addProviderTiles(providers$CartoDB.Positron) |>
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
        labFormat = if (input$map_color == "score_priorite")
          labelFormat(transform = function(x) round(expm1(x)))
        else
          labelFormat()
      )
  })

  # Méthodologie
  output$ui_methodo <- renderUI({
    div(class = "d-flex flex-column gap-4 py-1",

      p(class = "text-muted",
        "Ce dashboard classe les communes vaudoises par rendement attendu en sièges pour les cantonales Grand Conseil 2027. ",
        "Il ne dit pas combien de ressources investir au total — il dit ", strong("où"), " les concentrer pour maximiser les chances d'EàG."),

      # Score
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Le score de priorité"),
        p("Chaque commune reçoit un score qui combine trois facteurs :"),
        tags$ul(class = "mb-2",
          tags$li(strong("Marge de progression :"),
            " la commune vote-t-elle plus à gauche que ce qu'EàG y a obtenu en 2022 ? ",
            "Un résidu négatif dans le graphique « Écart de conversion » signale un potentiel non capté."),
          tags$li(strong("Levier siège :"),
            " combien de voix manquent à EàG pour décrocher le prochain siège dans ce district ? ",
            "Plus ce nombre est faible, plus chaque vote supplémentaire compte."),
          tags$li(strong("Effectif électoral :"),
            " combien d'électeurs sont dans la commune ? Une grande commune contribue davantage au total du district.")),
        div(class = "bg-light rounded p-2 text-center font-monospace small",
          "score  =  marge de progression  ×  (1 / voix manquantes)  ×  effectif"),
        p(class = "text-muted small mt-2 mb-0",
          "En district de consolidation (EàG déjà représentée), la marge est remplacée par ",
          "max(marge, part EàG 2022) pour valoriser aussi la défense des votes acquis.")),

      # District classification
      div(class = "border rounded p-3",
        h5(class = "fw-bold mb-3", "Pourquoi certains districts n'apparaissent-ils pas dans le ciblage ?"),
        p("Avant de classer les communes, les districts sont évalués stratégiquement. ",
          "Un district structurellement hors d'atteinte ne doit pas mobiliser de ressources, même si certaines de ses communes semblent prometteuses."),
        tags$table(class = "table table-sm table-bordered mb-2",
          tags$thead(class = "table-light",
            tags$tr(
              tags$th("Statut"), tags$th("Condition"), tags$th("Ce que cela signifie"))),
          tags$tbody(
            tags$tr(
              tags$td(tags$span(class = "badge", style = "background:#4dac26", "consolidation")),
              tags$td("EàG avait ≥ 1 siège en 2022"),
              tags$td("Siège(s) à défendre — toutes les communes du district sont scorées")),
            tags$tr(
              tags$td(tags$span(class = "badge", style = "background:#f0a500; color:#000", "offensive")),
              tags$td("Voix manquantes ≤ 12 % du total"),
              tags$td("Premier siège à portée — toutes les communes du district sont scorées")),
            tags$tr(
              tags$td(tags$span(class = "badge bg-secondary", "hors portée")),
              tags$td("Voix manquantes > 12 % du total"),
              tags$td("Exclu du ciblage — aucun score calculé pour ces communes")))),
        p(class = "text-muted small mb-0",
          "Le seuil de 12 % est un choix politique, pas un fait électoral. ",
          "Il peut être modifié dans ", tags$code("analyse.R"), " selon les ressources disponibles et les ambitions de la liste.")),

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
              tags$td("Maintenir et mobiliser la base — permanences, affichage, réseaux militants")),
            tags$tr(style = "background:#d0e8f7",
              tags$td(strong("Mobilisation")),
              tags$td("Électorat favorable mais participation faible"),
              tags$td("Faire voter des gens déjà acquis — tractage ciblé, rappels de vote, covoiturage")),
            tags$tr(style = "background:#fde0d0",
              tags$td(strong("Persuasion")),
              tags$td("Commune moins à gauche, mais EàG y sous-performe son potentiel"),
              tags$td("Convaincre des électeurs proches — porte-à-porte, événements publics, médias locaux")),
            tags$tr(style = "background:#eeeeee",
              tags$td(strong("Autre")),
              tags$td("Aucune des conditions ci-dessus"),
              tags$td("Faible priorité — ressources à réallouer ailleurs")))),
        p(class = "text-muted small mb-0",
          "Les seuils (médiane de dim1 et de participation) sont calculés sur l'ensemble des communes retenues.")),

      # Caveats
      div(class = "alert mb-0", style = "background:#fff8e1; border-left: 4px solid #f0a500;",
        h6(class = "fw-bold", "Points d'attention"),
        tags$ul(class = "mb-0 small",
          tags$li(strong("Référence 2022. "),
            "Le score compare la situation actuelle (votations 2025–2026) à la performance EàG de 2022. ",
            "Une commune avec un fort résidu négatif est peut-être déjà en train de progresser depuis 2022 — le graphique CE 2026 permet de le vérifier."),
          tags$li(strong("Vote sur enjeux ≠ vote de liste. "),
            "L'indice de positionnement reflète les votations, pas directement le vote Grand Conseil. Une commune à gauche sur les enjeux ne vote pas forcément EàG."),
          tags$li(strong("Niveau géographique. "),
            "L'analyse cible des communes, pas des individus. Ne pas en déduire le comportement d'électeurs spécifiques."),
          tags$li(strong("HB simplifié. "),
            "Le calcul des sièges utilise uniquement les voix EàG. L'attribution réelle dépend du résultat de toutes les listes.")))
    )
  })
}

shinyApp(ui, server)
