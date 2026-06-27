# Merge all votation xlsx files → data_votations_vd.csv + votations_dict.csv

library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(here)
library(tidyr)

raw_dir <- here::here("data", "raw")
processed_dir <- here::here("data", "processed")
# Reference files
pop_vd_25 <- read_csv(here::here(processed_dir, "vd_pop_2025.csv"), show_col_types = FALSE)%>%
  distinct()

sieges_vd <- read_csv(here::here(processed_dir, "vd_sieges_districts.csv"), show_col_types = FALSE)%>%
  distinct()
# ── OFS commune code lookup ───────────────────────────────────────────────────

get_ofs <- function(name, ref = read_csv(here::here(processed_dir, "vd_pop_2025.csv"), show_col_types = FALSE)%>%
  distinct()) {
    ofs_map <- setNames(ref[["num_ofs"]], as.character(ref[["commune"]]))

  NAME_MAP <- c(
    "Arzier - Le Muids"       = "Arzier-Le Muids",
    "Blonay - Saint-Légier"   = "Blonay-St-Légier",
    "Cugy VD"                 = "Cugy",
    "Dompierre VD"            = "Dompierre",
    "Ecublens VD"             = "Ecublens",
    "Ependes VD"              = "Ependes",
    "Lully VD"                = "Lully",
    "Mex VD"                  = "Mex",
    "Moiry VD"                = "Moiry",
    "Mollens VD"              = "Mollens",
    "Morrens VD"              = "Morrens",
    "Ollon VD"                = "Ollon",
    "Onnens VD"               = "Onnens",
    "Renens VD"               = "Renens",
    "Roche VD"                = "Roche",
    "Saint-Barthélemy VD"     = "Saint-Barthélemy",
    "Saint-Saphorin (Lavaux)" = "St-Saphorin (Lavaux)",
    "Treytorrens (Payerne)"   = "Treytorrens",
    "Villeneuve VD"           = "Villeneuve"
  )

  name    <- trimws(as.character(name))
  code <- ofs_map[name]
  if (!is.na(code)) return(unname(code))
  alt  <- NAME_MAP[name]
  if (!is.na(alt)) {
    code2 <- ofs_map[unname(alt)]
    if (!is.na(code2)) return(unname(code2))
  }
  ""
}

# ── Pourcentage ───────────────────────────────────────────────────────────────
pct <- function(num, denom) {
  n <- suppressWarnings(as.numeric(num))
  d <- suppressWarnings(as.numeric(denom))
  if (is.na(n) || is.na(d) || d == 0) return("")
  suppressWarnings(as.numeric(n / d * 100))
}

# ── Sous-arrondissements ──────────────────────────────────────────────────────

resolve_district <- function(district, commune, ref = read_csv(here::here("data", "processed", "vd_pop_2025.csv"), show_col_types = FALSE)%>%distinct()) {
  sous_district <- ref%>%
    filter(!is.na(sous_district)) %>%
    pull(sous_district, name = commune)
  c <- trimws(as.character(commune))
  r <- sous_district[c]
  if (!is.na(r)) return(unname(r))
  d  <- trimws(as.character(district))
  return(d)
}


# ── Helpers ───────────────────────────────────────────────────────────────────
SKIP <- c("Canton de Vaud", "Suisses de l'étranger")

as_int   <- function(x) suppressWarnings(as.integer(x))
safe_int <- function(x) {
  v <- suppressWarnings(as.integer(x))
  ifelse(is.na(v), 0L, v)
}

make_row <- function(votation, date, pos, district, commune, ofs,
                     electeurs, bulletins, blancs,valables, nuls, oui, non) {
  oui_i <- safe_int(oui)
  non_i <- safe_int(non)
  blancs_i <- safe_int(blancs)

  tibble(
    votation                     = votation,
    date                         = date,
    positionnement_politique_oui = pos,
    district                     = district,
    Communes                     = commune,
    code_ofs                     = as_int(ofs),
    Electeurs_inscrits           = as_int(electeurs),
    Bulletins_rentres            = as_int(bulletins),
    Blancs                       = safe_int(blancs),
    Nuls                         = safe_int(nuls),
    Valables                     = as_int(valables),
    OUI                          = as_int(oui),
    OUI_perc                     = as.numeric(pct(oui_i, Valables)),
    NON                          = as_int(non),
    NON_perc                     = as.numeric(pct(non_i, Valables)),
    particip_perc                = as.numeric(pct(bulletins, as.numeric(Electeurs_inscrits)))
  )
}

# ── Itérateur communes ────────────────────────────────────────────────────────
# Shared structure across all votation XLSX files:
#   rows 1-2 = headers (skipped); rows with "District …" = district headers;
#   commune rows have a numeric value in column 2.
# Returns a tibble with columns: district | commune | <col_names…>
# col_names names the xlsx data columns (col 2 onwards); if NULL, left as …1/…2/…

# Column layouts for each file type
CHVO_COLS <- c("electeurs", "bulletins", "blancs", "nuls", "valables",
               "nb_oui", "perc_oui", "nb_non", "perc_non", "participation")

VDVO_SIMPLE_COLS <- c("electeurs", "bulletins", "nuls", "valables_f",
                      "blancs", "perc_val", "nb_oui", "perc_oui",
                      "nb_non", "perc_non", "participation")

VDVO_COMPLEX_COLS <- c(
  "electeurs", "bulletins", "blancs",         "nuls",          "valables_f",    "participation",
  "init_blancs", "init_oui", "init_oui_p", "init_non",      "init_non_p",
  "cp_blancs",   "cp_oui",   "cp_oui_p",   "cp_non",        "cp_non_p",
  "sub_blancs",  "sub_oui",  "sub_oui_p",  "sub_cp_val", "sub_cp_p"
)

VDCE_RABOUD_COLS <- c("ofs", "commune", "x3", "bulletins", "x5", "is_raboud")

GC_COLS <- c("ofs", "commune", "no_candidat", "liste", "nom", "prénom", "non_modifiés", "modifiés", "total")

iter_communes <- function(path, col_names = NULL) {
  raw        <- suppressMessages(read_excel(path, col_names = FALSE, .name_repair = "minimal"))
  names(raw) <- paste0("col", seq_len(ncol(raw)))   # avoid ...N special names
  district   <- NA_character_
  result   <- list()
  for (i in seq_len(nrow(raw))) {
    if (i <= 2L) next
    r    <- as.list(raw[i, ])
    if (is.na(r[[1]])) next
    name <- trimws(as.character(r[[1]]))
    if (startsWith(name, "District")) {
      district <- sub("^District ", "", name); next
    }
    if (name %in% SKIP) next
    if (is.na(suppressWarnings(as.numeric(r[[2]])))) next
    result[[length(result) + 1L]] <- c(list(district = district, commune = name), r[-1])
  }
  df <- bind_rows(result)
  if (!is.null(col_names) && ncol(df) > 2L) {
    n <- min(ncol(df) - 2L, length(col_names))
    names(df)[seq(3L, 2L + n)] <- col_names[seq_len(n)]
  }
  df
}

# ── parse_chvo ────────────────────────────────────────────────────────────────
# Layout: Communes | Electeurs | Bulletins | Blancs | Nuls | Valables | OUI | % | NON | % | Part.
parse_chvo <- function(fname, date, votation, pos) {
  iter_communes(here::here(raw_dir, fname), CHVO_COLS) |>
    pmap_dfr(\(district, commune, electeurs, bulletins, blancs, nuls, valables, nb_oui, nb_non, ...) {
      make_row(votation, date, pos,
               resolve_district(district, commune), commune, get_ofs(commune),
               electeurs, bulletins, blancs,valables, nuls, nb_oui, nb_non)
    })
}

# ── parse_vdvo_simple ─────────────────────────────────────────────────────────
# Layout: Communes | Electeurs | Bulletins | Nuls | Valables* | Blancs | % | OUI | % | NON | % | Part.
parse_vdvo_simple <- function(fname, date, votation, pos) {
  iter_communes(here::here(raw_dir, fname), VDVO_SIMPLE_COLS) |>
    pmap_dfr(\(district, commune, electeurs, bulletins, nuls, valables_f,
                blancs, nb_oui, nb_non, ...) {
      make_row(votation, date, pos,
               resolve_district(district, commune), commune, get_ofs(commune),
               electeurs, bulletins, blancs,valables_f, nuls, nb_oui, nb_non)
    })
}

# ── parse_vdvo_complex ────────────────────────────────────────────────────────
# Initiative + contre-projet + question subsidiaire in one sheet.

commune_vdvo_complex <- function(district, commune,
                                  electeurs, bulletins, blancs, nuls, valables_f, participation,
                                  init_blancs, init_oui, init_oui_p, init_non, init_non_p,
                                  cp_blancs, cp_oui, cp_oui_p, cp_non, cp_non_p,
                                  sub_blancs, sub_oui, sub_oui_p, sub_cp_val, sub_cp_p,
                                  date, v_init, p_init, v_cp, p_cp, v_sub, p_sub, ...) {
  d <- resolve_district(district, commune)
  o <- get_ofs(commune)
  cp_sub <- if (!is.na(suppressWarnings(as.numeric(sub_cp_val)))) as.numeric(sub_cp_val)
            else safe_int(valables_f) - safe_int(sub_blancs) - safe_int(sub_oui)
  bind_rows(
    make_row(v_init, date, p_init, d, commune, o, electeurs, bulletins, safe_int(init_blancs) + safe_int(blancs),valables_f, nuls, init_oui, init_non),
    make_row(v_cp,   date, p_cp,   d, commune, o, electeurs, bulletins, safe_int(cp_blancs) + safe_int(blancs), valables_f,   nuls, cp_oui,   cp_non),
    make_row(v_sub,  date, p_sub,  d, commune, o, electeurs, bulletins, safe_int(sub_blancs) + safe_int(blancs),  valables_f, nuls, sub_oui,  cp_sub)
  )
}

parse_vdvo_complex <- function(fname, date,
                               v_init, p_init, v_cp, p_cp, v_sub, p_sub) {
  iter_communes(here::here(raw_dir, fname), VDVO_COMPLEX_COLS) |>
    pmap_dfr(\(...) commune_vdvo_complex(...,
                                          date = date, v_init = v_init, p_init = p_init,
                                          v_cp = v_cp, p_cp = p_cp, v_sub = v_sub, p_sub = p_sub))
}

# ── Conseil d'État 2026 ───────────────────────────────────────────────────────
build_ofs_district_map <- function() {
  df     <- iter_communes(here::here(raw_dir, "CHVO20251130-02.xlsx"), CHVO_COLS)
  result <- list()
  for (i in seq_len(nrow(df))) {
    ofs <- get_ofs(df$commune[i])
    if (nchar(ofs) > 0)
      result[[ofs]] <- list(name     = df$commune[i],
                            district = resolve_district(df$district[i], df$commune[i]))
  }
  result
}

ofs_district <- build_ofs_district_map()

# VDCE xlsx columns
parse_vdce_raboud <- function(fname, date, ref_district = pop_vd_25) {
  candidats_CE_26 <- tibble(
    nom = c("Nordmann", "Thuillard", "Raboud"),
    pos = c("centre-gauche", "extreme-droite", "gauche"),
    liste = c("PS", "Alliance VD", "EàG")
  )
  
  cand_name <- unique(candidats_CE_26$nom)
  
  raw  <- suppressMessages(read_excel(here::here(raw_dir, fname),
                                       col_names = c("ofs", "commune", "nb_bulletin", cand_name, "eparses"), .name_repair = "minimal"))
  data <- raw[-1L, ]%>%   # 1 header row
    mutate(nb_bulletin = safe_int(nb_bulletin),
           ofs = safe_int(ofs),
           across(c(all_of(cand_name), "eparses"), ~safe_int(.x)*safe_int(nb_bulletin)))%>%
    pivot_longer(c(all_of(cand_name), "eparses"), names_to = "candidat", values_to = "votes")%>%
    group_by(ofs, commune, candidat) %>%
    summarise(
      nb_bulletin = sum(nb_bulletin),
      value       = sum(votes),
      .groups     = "drop"
    )%>%
    left_join(candidats_CE_26, by = c("candidat" = "nom"))%>%
    filter(candidat != "eparses")

  data <- data |>
    left_join(select(ref_district, -population, - commune), by = c("ofs" = "num_ofs"))%>%
    mutate(district = map2_chr(.data$district, .data$commune, resolve_district))

  unmapped <- unique(data$ofs[is.na(data$district)])
  if (length(unmapped)){
    message("VDCE — OFS codes sans correspondance district : ",
            paste(sort(unmapped), collapse = ", "))
  }
  
  data |>
    filter(!is.na(.data$district)) |>
    pmap_dfr(\(ofs, candidat, nb_bulletin, value, pos, district, commune, ...) {
      make_row(
        paste0("vdce_2026_", tolower(candidat)), date, pos,
        district, commune, ofs,
        NA, nb_bulletin, 0L, nb_bulletin, 0L, value, nb_bulletin - value
      )
    }) %>% 
      mutate(particip_perc = as_int(particip_perc))
}

# ── Conseils communaux 2026 ───────────────────────────────────────────────────
CC_EAG_BLOCS <- c("EàG", "da.", "Solidarité & Ecologie", "POP-EàG")

CC_META <- list(
  list("repartitions_sieges_cc_5586.csv", "Lausanne",          "5586", "Lausanne"),
  list("repartitions_sieges_cc_5724.csv", "Nyon",              "5724", "Nyon"),
  list("repartitions_sieges_cc_5884.csv", "Corsier-sur-Vevey", "5884", "Riviera-Pays-d'Enhaut"),
  list("repartitions_sieges_cc_5886.csv", "Montreux",          "5886", "Riviera-Pays-d'Enhaut"),
  list("repartitions_sieges_cc_5890.csv", "Vevey",             "5890", "Riviera-Pays-d'Enhaut"),
  list("repartitions_sieges_cc_5938.csv", "Yverdon-les-Bains", "5938", "Jura-Nord vaudois")
)

parse_cc_eag <- function(year = 2026L, date = "08.03.26") {
  map_dfr(CC_META, function(m) {
    df    <- read_csv(here::here(raw_dir, m[[1]]), show_col_types = FALSE,
                      locale = locale(encoding = "UTF-8"))
    names(df) <- str_remove(names(df), "^﻿")   # strip BOM if present
    df    <- filter(df, .data[["année"]] == year)
    total <- sum(df[["sièges"]])
    eag   <- sum(df[["sièges"]][trimws(df[["parti"]]) %in% CC_EAG_BLOCS])
    if (!total) return(tibble())
    make_row(paste0("cc_", year, "_eag"), date, "gauche",
             resolve_district(m[[4]], m[[2]]), m[[2]], m[[3]],
             NA, total, 0L, total, 0L, eag, total - eag)
  })
}

# ── Grand Conseil 2022 ────────────────────────────────────────────────────────
GC_BLOCS <- list(
  PS                 = c("PS", "PSV", "PSL", "PSGDV", "PSVdJ", "PSO"),
  Gauche             = c("POP", "sol-POP", "POP - FR", "da. (EàG)", "S&E",
                         "L'Aurore", "CS", "SEàG - POP"),
  `extreme-droite`   = c("UDC - UDF", "UDC Riviera", "UDC", "ADL",
                          "AdLibertés", "AdL"),
  `droite-bourgeoise`= c("PLR", "PLR Riviera", "LES LIBRES", "Les Libres",
                          "PdR - Les Libres", "PPVD", "UDF"),
  centre             = c("Le Centre", "ACDC", "Centre-PEV", "Centre UDF", "LC",
                          "LC-L", "PEV", "PEV-UDF", "Indépendants VD", "PPV",
                          "PVL", "pvl", "PLV", "PVL et LC", "Vert'lib", "VL"),
  verts              = c("Les Vert·e·s", "Les Verts", "Vert·e·s", "Verts",
                          "VERTS", "Vert-e-s", "Les Vert.e.s", "Les Verte.e.s",
                          "Vert.e.s")
)

GC_POSITION <- c(
  PS                  = "centre-gauche",
  Gauche              = "gauche",
  `extreme-droite`    = "extreme-droite",
  `droite-bourgeoise` = "droite",
  centre              = "centre",
  verts               = "centre-gauche"
)

gc_bloc <- function(liste) {
  for (b in names(GC_BLOCS)) if (liste %in% GC_BLOCS[[b]]) return(b)
  "autre"
}

parse_grand_conseil <- function(subdir, date, col_names = NULL,
  sieges_vd = read_csv(here::here(processed_dir, "vd_sieges_districts.csv"), show_col_types = FALSE)%>%
  distinct()) {
  directory    <- here::here(raw_dir, subdir)
  files        <- sort(list.files(directory, pattern = "\\.xlsx$", full.names = TRUE))
  all_unmapped <- character()
  all_rows     <- list()

  for (fpath in files) {
    title_df <- suppressMessages(
      read_excel(fpath, col_names = FALSE, n_max = 1, .name_repair = "minimal"))
    title       <- as.character(title_df[1, 1])
    parts       <- strsplit(title, " / ", fixed = TRUE)[[1]]
    after_slash <- if (length(parts) >= 2L) parts[2L] else title
    district    <- trimws(sub("\\s*-\\s*\\d{2}\\.\\d{2}\\.\\d{4}$", "", after_slash))

    raw_cand <- suppressMessages(
      read_excel(fpath, col_names = FALSE, skip = 3, .name_repair = "minimal"))
    
    raw_cand <- raw_cand[1:length(col_names)]
    names(raw_cand) <- col_names

    if (nrow(raw_cand) == 0L) next

    cand <- raw_cand |>
      filter(!is.na(commune), !is.na(liste)) |>
      transmute(
        ofs_code = trimws(as.character(ofs)),
        commune  = trimws(as.character(commune)),
        liste    = trimws(as.character(liste)),
        votes    = suppressWarnings(as.numeric(total))
      )
    if (nrow(cand) == 0L) next

    n_seats <- sieges_vd$sieges_2022[sieges_vd$district_csv == district]
    true_district <- resolve_district(district, cand$commune[1])
    
    cand <- mutate(cand, bloc = vapply(liste, gc_bloc, character(1L)))
    all_unmapped <- unique(c(all_unmapped, cand$liste[cand$bloc == "autre"]))

    bloc_agg <- cand |>
      filter(bloc != "autre", !is.na(votes)) |>
      group_by(ofs_code, commune, bloc) |>
      summarise(votes = sum(votes), .groups = "drop")

    total_agg <- cand |>
      filter(!is.na(votes)) |>
      group_by(ofs_code, commune) |>
      summarise(total_votes = sum(votes), .groups = "drop") |>
      mutate(bulletins = round(total_votes / n_seats))

    for (i in seq_len(nrow(total_agg))) {
      oc  <- total_agg$ofs_code[i]
      com <- total_agg$commune[i]
      bul <- total_agg$bulletins[i]
      if (!bul) next
      for (bloc in names(GC_POSITION)) {
        v   <- bloc_agg %>%
          filter(ofs_code == oc, commune == com, bloc == !!bloc) %>%
          pull(votes)

        oui <- round(if (length(v) > 0L) v / n_seats else 0)
        all_rows[[length(all_rows) + 1L]] <- make_row(
          paste0("gc_2022_", bloc), date, GC_POSITION[bloc],
          district, com, oc,
          NA, bul,valables = bul, 0L, 0L, oui, bul - oui
        )
      }
    }
  }

  if (length(all_unmapped))
    message("Grand Conseil — listes non mappées : ",
            paste(sort(all_unmapped), collapse = ", "))
  bind_rows(all_rows) %>% 
    mutate(particip_perc = as_int(particip_perc))
}

# ── Assemblage (ordre chronologique) ─────────────────────────────────────────
message("Grand Conseil 2022...")
rows_gc    <- parse_grand_conseil("grand_conseil_2021", "20.03.22", GC_COLS)
message("Votations 28.09.25...")
rows_mort  <- parse_vdvo_complex("VDVO20250928-01.xlsx", "28.09.25",
                 "mormont",        "gauche",
                 "mormont_cp",     "droite",
                 "mormont_sub",    "gauche")
rows_dpe   <- parse_vdvo_simple("VDVO20250928-03.xlsx", "28.09.25",
                 "droits_pol_etrangers", "gauche")
message("Votations 30.11.25...")
rows_avnr  <- parse_chvo("CHVO20251130-02.xlsx", "30.11.25",
                 "init_avenir", "gauche")
message("Conseil d'État 08.03.26...")
rows_ce    <- parse_vdce_raboud("VDCE20260308-VD-bulletins.xlsx", "08.03.26")
rows_cc    <- parse_cc_eag(2026L, "08.03.26")
message("Votations 14.06.26...")
rows_dur   <- parse_chvo("CHVO20260614-01.xlsx", "14.06.26",
                 "init_durabilite", "extreme-droite")
rows_sc    <- parse_chvo("CHVO20260614-02.xlsx", "14.06.26",
                 "fed_service_civil", "droite")
rows_smc   <- parse_vdvo_simple("VDVO20260614-01.xlsx", "14.06.26",
                 "salaire_min_const", "gauche")
rows_sml   <- parse_vdvo_complex("VDVO20260614-02.xlsx", "14.06.26",
                 "salaire_min_legis",     "gauche",
                 "salaire_min_legis_cp",  "droite",
                 "salaire_min_legis_sub", "gauche")

all_rows <- bind_rows(rows_gc, rows_mort, rows_dpe, rows_avnr,
                      rows_ce, rows_cc, rows_dur, rows_sc, rows_smc, rows_sml)

# ── Écriture data_votations_vd.csv ───────────────────────────────────────────
COLS <- c("votation", "date", "positionnement_politique_oui", "district",
          "Communes", "code_ofs", "Electeurs_inscrits", "Bulletins_rentres",
          "Blancs", "Nuls", "Valables", "OUI", "OUI_perc", "NON", "NON_perc",
          "particip_perc")

main_csv <- here::here(processed_dir, "data_votations_vd.csv")
write_csv(select(all_rows, all_of(COLS)), main_csv, na = "")
message(sprintf("Wrote %d rows → %s", nrow(all_rows), main_csv))
