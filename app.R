library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)
library(htmltools)
library(DT)
library(plotly)

# ── Flexible date parsing ───────────────────────────────────────────────────
# data_processing.R used to read dates as plain CSV text ("09/01/2024"),
# which passed straight through unchanged. It now reads the source
# workbook with read_xlsx(), which returns a real Date object - and
# write.csv() serializes a Date column as ISO text ("2024-09-01") by
# default, not mm/dd/yyyy. Every date field downstream of that switch
# silently changed format, which is exactly what was breaking FY (and
# therefore the "recent match" filter) and Partner Since: both were
# parsing a date that no longer matched the format string they assumed.
# Handling both formats here, rather than hardcoding one, means a future
# re-export - in either direction - won't quietly break this again.
parse_flexible_date <- function(x) {
  x <- trimws(as.character(x))
  is_iso <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  out <- as.Date(rep(NA_character_, length(x)))
  if (any(is_iso, na.rm = TRUE)) {
    out[is_iso] <- as.Date(x[is_iso], format = "%Y-%m-%d")
  }
  slashed <- !is_iso & grepl("/", x)
  if (any(slashed, na.rm = TRUE)) {
    year_part <- sub(".*/", "", x[slashed])
    fmt       <- ifelse(nchar(year_part) == 4, "%m/%d/%Y", "%m/%d/%y")
    out[slashed] <- as.Date(x[slashed], format = fmt)
  }
  out
}

# ── Data loading ──────────────────────────────────────────────────────────────
orgs     <- read.csv("Accounts_wideCategories_Geocoded.csv", stringsAsFactors = FALSE)
websites <- read.csv("websites.csv", stringsAsFactors = FALSE)
orgs     <- orgs |> left_join(websites, by = "Account.Name")

# ── Manual geocode corrections ─────────────────────────────────────────────
fix_org <- function(data, name, addr1, city, state, zip, lat, lon) {
  idx <- which(data$Account.Name == name)
  if (length(idx) == 0) return(data)
  data$Billing.Address.Line.1[idx]  <- addr1
  data$Billing.City[idx]            <- city
  data$Billing.State.Province[idx]  <- state
  data$Billing.Zip.Postal.Code[idx] <- zip
  data$latitude[idx]                <- lat
  data$longitude[idx]               <- lon
  data
}

# These two rows had missing/malformed source addresses that the geocoder arcgis failed on.
orgs <- orgs |>
  fix_org("Detroit People's Platform",
          addr1 = "7700 Second Ave. #509", city = "Detroit", state = "MI", zip = "48202",
          lat = 42.3760, lon = -83.0780) |>
  fix_org("Next Chapter Bookclub of Saline",
          addr1 = "6711 Robison Ln", city = "Saline", state = "MI", zip = "48176",
          lat = 42.1934, lon = -83.7075)

# Remaining accounts that still have no usable address on file.
no_geo_names <- c(
  "Resource Generation", "Justice InDeed",
  "The Equitable Ann Arbor Land Trust",
  "Our Village", "National Wild Turkey Federation - Michigan State Chapter",
  "Breyko", "Reproductive Freedom for All - Michigan", "Depot Town Events",
  "Concert Music Outreach Collective", "Superhero Training Academy",
  "Voce Velata", "Greater Health Institute", "Laotian American Community of Michigan",
  "FutureRoot", "Huron Valley Worker Organizing and Research Center"
)

# ── Upcoming fields (not in the data yet) ──────────────────────────────────
# A field  coming later: a whole-number "total matches" count per org.
if (!"Total.Matches" %in% names(orgs)) {
  orgs$Total.Matches <- NA_integer_
}
if (!"Created.Date" %in% names(orgs)) {
  orgs$Created.Date <- as.Date(NA)
} else {
  orgs$Created.Date <- parse_flexible_date(orgs$Created.Date)
}
# Mission Statement backs up Description in the "About" section on an org's page when there's no description on file.
if (!"Mission.Statement" %in% names(orgs)) {
  orgs$Mission.Statement <- NA_character_
}

# Turns a Created Date into just its year, e.g. "2017", for the "Partner Since" field. 
format_partner_since <- function(created_date) {
  if (is.null(created_date) || length(created_date) == 0 || is.na(created_date)) {
    return(NA_character_)
  }
  format(created_date, "%Y")
}

cat_cols <- grep("^Category\\.[0-9]+$", names(orgs), value = TRUE)
orgs <- orgs |>
  mutate(
    Categories = apply(orgs[, cat_cols], 1, function(x) {
      vals <- x[!is.na(x) & x != ""]
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
    })
  )

# ── Focus area groups ─────────────────────────────────────────────────────────
group_map <- list(
  "Animal Welfare"                                  = "Animal Welfare",
  "Arts & Culture"                                   = "Arts & Culture",
  "Basic Needs & Material Assistance"                = "Basic Needs & Material Assistance",
  "Community Development & Organizational Capacity"  = "Community Development & Organizational Capacity",
  "Economic Stability & Workforce Development"       = "Economic Stability & Workforce Development",
  "Education & Learning"                             = "Education & Learning",
  "Environment & Sustainability"                     = "Environment & Sustainability",
  "Food Access & Food Security"                      = "Food Access & Food Security",
  "Health & Wellness"                                = "Health & Wellness",
  "Housing & Homelessness"                           = "Housing & Homelessness",
  "Human Services & Social Support"                  = "Human Services & Social Support",
  "Justice, Legal Services & Public Safety"          = "Justice, Legal Services & Public Safety",
  "Public Participation & Community Voice"           = "Public Participation & Community Voice",
  "Social Justice & Equity"                          = "Social Justice & Equity",
  "Youth Development"                                = "Youth Development"
)

cat_to_group <- setNames(
  rep(names(group_map), lengths(group_map)),
  unlist(group_map, use.names = FALSE)
)

# ── Skill area groups ──────────────────────────────────────────────────────
skill_group_map <- list(
  "Communications, Marketing & Design"       = "Communications, Marketing & Design",
  "Community Engagement & Facilitation"      = "Community Engagement & Facilitation",
  "Data, Evaluation & Research"              = "Data, Evaluation & Research",
  "Fundraising & Resource Development"       = "Fundraising & Resource Development",
  "Organizational Management & Operations"   = "Organizational Management & Operations",
  "Policy & Advocacy"                        = "Policy & Advocacy",
  "Program Design & Delivery"                = "Program Design & Delivery",
  "Teaching, Tutoring & Mentorship"          = "Teaching, Tutoring & Mentorship",
  "Technology, Web & Engineering"            = "Technology, Web & Engineering"
)

skill_to_group <- setNames(
  rep(names(skill_group_map), lengths(skill_group_map)),
  unlist(skill_group_map, use.names = FALSE)
)

# ── Projects ──────────────────────────────────────────────────────────────────

# ── Org name lookup: resolve matches-file abbreviations to real account
#    names, using the old tracking workbook's "Initiative Account" column
#    as the source of truth wherever the same match still exists there. ──
build_org_lookup <- function(xlsx_path = "Mapping CP Network.xlsx") {
  read_one <- function(sheet) {
    read_xlsx(xlsx_path, sheet = sheet) |>
      select(`Match: Match Name`, `Initiative Account`)
  }
  bind_rows(read_one("FY25"), read_one("FY26")) |>
    distinct(`Match: Match Name`, .keep_all = TRUE)
}

# A few matches in the new file aren't in the old workbook at all, so there's no Initiative Account to borrow.
org_overrides <- c(
  "HRWC" = "Huron River Watershed Council",
  "RGI"  = "Refugee Garden Initiatives"
)

# Derives a fiscal year label (e.g. "FY25") from a completed-match date -
# fiscal year runs July-June, so a date in Sept 2024 is FY25.
fy_label <- function(d) {
  yr     <- as.integer(format(d, "%Y"))
  mo     <- as.integer(format(d, "%m"))
  fy_end <- ifelse(mo >= 7, yr + 1, yr)
  ifelse(is.na(d), NA_character_, paste0("FY", sprintf("%02d", fy_end %% 100)))
}

load_matches <- function(path = "Matches_Final.csv",
                         xlsx_path = "Mapping CP Network.xlsx") {
  raw <- read.csv(path, check.names = FALSE)
  
  blank <- is.na(names(raw)) | names(raw) == ""
  if (any(blank)) names(raw)[blank] <- paste0("blank_col_", seq_len(sum(blank)))
  
  org_lookup <- build_org_lookup(xlsx_path)
  
  raw <- raw |>
    left_join(org_lookup, by = "Match: Match Name") |>
    mutate(
      abbrev    = trimws(sub(":.*$", "", `Initiative: Initiative Name`)),
      Org       = coalesce(`Initiative Account`, unname(org_overrides[abbrev]), abbrev),
      Project   = `Initiative: Initiative Name`,
      Completed = parse_flexible_date(`Match Completed Date`),
      FY        = fy_label(Completed)
    )
  
  skill_rows <- raw |>
    tidyr::pivot_longer(
      cols      = c(`Skill Area 1`, `Skill Area 2`, `Skill Area 3`),
      values_to = "Category"
    ) |>
    filter(!is.na(Category), Category != "") |>
    transmute(Org, Project, Resource.Offering = `Resource Offering`,
              UM.Connection = `UM Connection`,
              Offering = NA_character_, Completed, FY, Category)
  
  engagement_rows <- raw |>
    tidyr::pivot_longer(
      cols      = c(`Form of Engagement 1`, `Form of Engagement 2`,
                    `Form of Engagement 3`, `Form of Engagement 4`),
      values_to = "Offering"
    ) |>
    filter(!is.na(Offering), Offering != "") |>
    transmute(Org, Project, Resource.Offering = `Resource Offering`,
              UM.Connection = `UM Connection`,
              Offering, Completed, FY, Category = NA_character_)
  
  bind_rows(skill_rows, engagement_rows)
}

projects <- load_matches()
# ── Aggregated matches: one row per actual match ────────────────────────────

projects_agg <- projects |>
  mutate(skill_group = skill_to_group[Category]) |>
  group_by(Org, Project, FY) |>
  summarise(
    Completed = if (all(is.na(Completed))) as.Date(NA) else min(Completed, na.rm = TRUE),
    # Resource Offering and UM Connection are both properties of the match.
    Resource.Offering = {
      vals <- unique(Resource.Offering[!is.na(Resource.Offering) & Resource.Offering != ""])
      if (length(vals) == 0) NA_character_ else vals[1]
    },
    UM.Connection = {
      vals <- unique(UM.Connection[!is.na(UM.Connection) & UM.Connection != ""])
      if (length(vals) == 0) NA_character_ else vals[1]
    },
    Skill.Areas = {
      vals <- unique(na.omit(skill_group))
      if (length(vals) == 0) NA_character_ else paste(sort(vals), collapse = "; ")
    },
    Forms.of.Engagement = {
      vals <- unique(Offering[!is.na(Offering) & Offering != ""])
      if (length(vals) == 0) NA_character_ else paste(sort(vals), collapse = "; ")
    },
    .groups = "drop"
  )

# ── Map data ──────────────────────────────────────────────────────────────────

orgs <- orgs |>
  mutate(
    primary_category = apply(pick(all_of(cat_cols)), 1, function(x) {
      vals <- x[!is.na(x) & x != ""]
      if (length(vals) == 0) NA_character_ else vals[[1]]
    }),
    primary_group = cat_to_group[primary_category]
  )

map_orgs <- orgs |>
  filter(!is.na(latitude), !is.na(longitude), !(Account.Name %in% no_geo_names))

# Accounts with no usable geographic data - shown in their own tab instead
problem_orgs <- orgs |>
  filter(Account.Name %in% no_geo_names) |>
  arrange(Account.Name)

# ── Filter choices ──────────────────────────────────────────────────────────

# "Last 2 fiscal years" - computed from whatever FY values exist in the matches data, so this stays correct as new FY sheets get added later.
recent_fys <- tail(sort(unique(projects$FY)), 2)

# Form of Engagement / Skill Area come from the matches data.
engagement_choices <- sort(unique(projects$Offering[!is.na(projects$Offering) & projects$Offering != ""]))
skill_choices       <- names(skill_group_map)

# School / College / Unit is match-level, not org-level. It is UM.Connection
scu_choices <- sort(unique(projects$UM.Connection[!is.na(projects$UM.Connection) & projects$UM.Connection != ""]))

# ── Color palette ─────────────────────────────────────────────────────────────

group_colors <- c(
  "Animal Welfare"                                  = "#bab0ac",
  "Arts & Culture"                                   = "#e15759",
  "Basic Needs & Material Assistance"                = "#9c755f",
  "Community Development & Organizational Capacity"  = "#76b7b2",
  "Economic Stability & Workforce Development"       = "#f28e2b",
  "Education & Learning"                             = "#4e79a7",
  "Environment & Sustainability"                     = "#499894",
  "Food Access & Food Security"                      = "#edc948",
  "Health & Wellness"                                = "#59a14f",
  "Housing & Homelessness"                           = "#d37295",
  "Human Services & Social Support"                  = "#8cd17d",
  "Justice, Legal Services & Public Safety"          = "#b07aa1",
  "Public Participation & Community Voice"           = "#86bcb6",
  "Social Justice & Equity"                          = "#ff9d9a",
  "Youth Development"                                = "#79706e"
)

group_pal <- colorFactor(
  palette  = group_colors,
  domain   = names(group_map),
  na.color = "#aaaaaa"
)

# Single uniform color for map markers (no longer tied to focus-area group)
marker_color <- "#00274C"

# ── Infographics data ───────────────────────────────────────────────────────

# -- Geographic classification -----------------------------------------
# No explicit country column in the source data, so this is inferred:
#   - "Unknown"            : accounts with no usable address at all
#   - "Southeast Michigan" : Billing State = MI AND falls inside a rough
#                            SE Michigan bounding box (Wayne/Oakland/Macomb/
#                            Washtenaw/Livingston/Monroe/St. Clair area).
#   - "Rest of Michigan"   : Billing State = MI, outside that box
#   - "USA (Other States)" : Billing State is a US state/territory, not MI
#   - "International"      : anything else (foreign address, or a Billing
#                            State that isn't a recognized US abbreviation)
us_state_abbrevs <- c(state.abb, "DC")
se_mi_box <- list(lat_min = 41.7, lat_max = 43.1, lon_min = -84.3, lon_max = -82.3)

geo_bucket_vec <- local({
  state <- orgs$Billing.State.Province
  lat   <- orgs$latitude
  lon   <- orgs$longitude
  
  in_se_mi <- !is.na(lat) & !is.na(lon) &
    lat >= se_mi_box$lat_min & lat <= se_mi_box$lat_max &
    lon >= se_mi_box$lon_min & lon <= se_mi_box$lon_max
  
  bucket <- case_when(
    orgs$Account.Name %in% no_geo_names        ~ "Unknown",
    !is.na(state) & state == "MI" & in_se_mi    ~ "Southeast Michigan",
    !is.na(state) & state == "MI"                ~ "Rest of Michigan",
    !is.na(state) & state %in% us_state_abbrevs ~ "USA (Other States)",
    TRUE                                          ~ "International"
  )
  bucket
})
orgs$Geo.Bucket <- geo_bucket_vec

geo_counts <- orgs |>
  count(Geo.Bucket, name = "n") |>
  arrange(match(Geo.Bucket, c("Southeast Michigan", "Rest of Michigan",
                              "USA (Other States)", "International", "Unknown")))

n_total        <- nrow(orgs)
n_se_mi        <- sum(orgs$Geo.Bucket == "Southeast Michigan")
n_mi_total     <- sum(orgs$Geo.Bucket %in% c("Southeast Michigan", "Rest of Michigan"))
n_usa_other    <- sum(orgs$Geo.Bucket == "USA (Other States)")
n_international <- sum(orgs$Geo.Bucket == "International")

# -- Top community priority areas (by number of distinct orgs involved) --
org_priority_groups <- apply(orgs[, cat_cols], 1, function(x) {
  vals   <- x[!is.na(x) & x != ""]
  groups <- unique(cat_to_group[vals])
  groups[!is.na(groups)]
})
priority_counts <- sort(table(unlist(org_priority_groups)), decreasing = TRUE)
top3_priorities <- head(priority_counts, 3)

# -- Matches leaderboard (all-time, from the FY25+FY26 matches data) -----
matches_leaderboard <- projects_agg |>
  count(Org, name = "Matches") |>
  arrange(desc(Matches)) |>
  slice_head(n = 10)

# -- Last-2-FY summary -----------------------------------------------------
recent_matches_agg <- projects_agg |> filter(FY %in% recent_fys)
n_recent_matches    <- nrow(recent_matches_agg)

recent_by_fy <- recent_matches_agg |> count(FY, name = "n")

recent_matches_grouped <- recent_matches_agg |>
  left_join(orgs |> select(Account.Name, primary_group), by = c("Org" = "Account.Name"))


top_priorities_recent <- recent_matches_grouped |>
  filter(!is.na(primary_group)) |> count(primary_group, name = "n") |> arrange(desc(n))

top_skills_recent <- recent_matches_grouped |>
  filter(!is.na(Skill.Areas)) |>
  tidyr::separate_rows(Skill.Areas, sep = "; ") |>
  count(Skill.Areas, name = "n") |> arrange(desc(n))

top_engagement_recent <- recent_matches_grouped |>
  filter(!is.na(Forms.of.Engagement)) |>
  tidyr::separate_rows(Forms.of.Engagement, sep = "; ") |>
  count(Forms.of.Engagement, name = "n") |> arrange(desc(n))

geo_colors <- c(
  "Southeast Michigan"  = "#00274C",  # Blue
  "Rest of Michigan"    = "#407EC9",  # Arboretum Blue
  "USA (Other States)"  = "#D86018",  # Ross School Orange
  "International"       = "#702082",  # Ann Arbor Amethyst
  "Unknown"              = "#bbbbbb"
)

geo_counts <- geo_counts |>
  mutate(color = unname(geo_colors[Geo.Bucket]))

top3_priorities_df <- data.frame(
  Priority = names(top3_priorities),
  n        = as.integer(top3_priorities),
  stringsAsFactors = FALSE
)


brand_qualitative <- c("#00274C", "#FFCB05", "#9A3324", "#00B2A9", "#D86018",
                       "#702082", "#407EC9", "#59a14f", "#e15759", "#9c755f")

bar_colors_for <- function(n) {
  rep(brand_qualitative, length.out = n)
}

# ── Chart helpers ────────────────────────────────────────────────────────

make_horiz_bar <- function(df, cat_col, val_col, colors = "#00274C", unit_label = "match") {
  df <- df[order(df[[val_col]]), , drop = FALSE]  # ascending so the biggest bar plots on top
  cats <- df[[cat_col]]
  vals <- df[[val_col]]
  
  bar_colors <- if (length(colors) > 1) {
    if (!is.null(names(colors))) unname(colors[cats]) else rep(colors, length.out = length(cats))
  } else {
    colors
  }
  bar_colors[is.na(bar_colors)] <- "#cccccc"
  
  plot_ly(
    x = vals,
    y = factor(cats, levels = cats),
    type = "bar",
    orientation = "h",
    marker = list(color = bar_colors),
    hovertemplate = paste0("%{y}<br>%{x} ", unit_label, "s<extra></extra>")
  ) |>
    layout(
      xaxis  = list(title = "", zeroline = FALSE, showgrid = TRUE, gridcolor = "#f0f2f5"),
      yaxis  = list(title = "", automargin = TRUE),
      margin = list(l = 10, r = 16, t = 10, b = 10),
      font   = list(family = "Roboto Condensed, sans-serif", size = 12, color = "#333")
    ) |>
    config(displayModeBar = FALSE)
}

make_donut <- function(df, label_col, val_col, colors) {
  labels <- df[[label_col]]
  plot_ly(
    labels = labels, values = df[[val_col]],
    type = "pie", hole = 0.58,
    marker = list(colors = unname(colors[labels]), line = list(color = "#ffffff", width = 2)),
    textinfo = "label+value",
    textposition = "outside",
    hovertemplate = "%{label}: %{value} orgs<extra></extra>"
  ) |>
    layout(
      showlegend = FALSE,
      margin = list(l = 10, r = 10, t = 10, b = 10),
      font   = list(family = "Roboto Condensed, sans-serif", size = 12, color = "#333")
    ) |>
    config(displayModeBar = FALSE)
}

selected_tags_ui <- function(values, colors = NULL) {
  if (is.null(values) || length(values) == 0) return(NULL)
  tags$div(
    class = "selected-tags-row",
    lapply(values, function(v) {
      style <- if (!is.null(colors) && !is.na(colors[v])) {
        paste0("background:", colors[v], "; color:#ffffff;")
      } else {
        ""
      }
      tags$span(class = "selected-tag", style = style, v)
    })
  )
}

stat_tile <- function(number, label, color = "#00274C") {
  tags$div(
    class = "stat-tile",
    tags$div(class = "stat-number", style = paste0("color:", color, ";"), number),
    tags$div(class = "stat-label", label)
  )
}


field_table <- function(rows) {
  th_style <- "text-align:left;padding:6px 10px;border-bottom:2px solid #e8edf2;color:#00274C;font-size: 15px;text-transform:uppercase;letter-spacing:0.05em;"
  td1_style <- "padding:7px 10px;border-bottom:1px solid #f0f2f5;font-weight:600;color:#333;white-space:nowrap;vertical-align:top;"
  td2_style <- "padding:7px 10px;border-bottom:1px solid #f0f2f5;color:#555;line-height:1.5;"
  tags$table(
    style = "width:100%; border-collapse:collapse; font-size: 15px;",
    tags$thead(
      tags$tr(tags$th(style = th_style, "Field"), tags$th(style = th_style, "Description"))
    ),
    tags$tbody(
      lapply(rows, function(r) {
        tags$tr(tags$td(style = td1_style, r[[1]]), tags$td(style = td2_style, r[[2]]))
      })
    )
  )
}

# ── Tooltip / popup builders ──────────────────────────────────────────────────

make_label <- function(data) {
  lapply(seq_len(nrow(data)), function(i) {
    o       <- data[i, ]
    grp     <- o$primary_group
    grp_col <- if (!is.na(grp)) group_pal(grp) else "#888888"
    
    status_line <- if (!is.na(o$Ginsberg.Partner.Status) && o$Ginsberg.Partner.Status != "")
      paste0("<div style='font-size: 15px;color:#666;margin-top:2px;'>",
             o$Ginsberg.Partner.Status, " Partner</div>")
    else ""
    
    group_line <- if (!is.na(grp))
      paste0("<div style='margin-top:5px;'><span style='background:", grp_col,
             ";color:#fff;padding:2px 9px;border-radius:20px;font-size: 15px;font-weight:600;'>",
             grp, "</span></div>")
    else ""
    
    city_line <- if (!is.na(o$Billing.City) && o$Billing.City != "")
      paste0("<div style='font-size: 15px;color:#888;margin-top:4px;'>",
             o$Billing.City, ", ", o$Billing.State.Province, "</div>")
    else ""
    
    HTML(paste0(
      "<div style='font-family:system-ui,-apple-system,sans-serif;padding:5px 7px;min-width:170px;'>",
      "<strong style='font-size: 15px;color:#1a1a1a;'>", htmlEscape(o$Account.Name), "</strong>",
      status_line, group_line, city_line,
      "</div>"
    ))
  })
}

make_popup <- function(o) {
  grp     <- o$primary_group
  grp_col <- if (!is.na(grp)) group_pal(grp) else "#888888"
  
  status_badge <- if (!is.na(o$Ginsberg.Partner.Status) && o$Ginsberg.Partner.Status != "") {
    bg  <- if (o$Ginsberg.Partner.Status == "Active") "#198754" else "#e6a817"
    txt <- if (o$Ginsberg.Partner.Status == "Active") "#fff" else "#1a1a1a"
    paste0("<span style='background:", bg, ";color:", txt,
           ";padding:3px 10px;border-radius:20px;font-size: 15px;font-weight:700;margin-right:5px;'>",
           o$Ginsberg.Partner.Status, "</span>")
  } else ""
  
  group_badge <- if (!is.na(grp))
    paste0("<span style='background:", grp_col,
           ";color:#fff;padding:3px 10px;border-radius:20px;font-size: 15px;font-weight:700;'>",
           grp, "</span>")
  else ""
  
  has_zip <- !is.na(o$Billing.Zip.Postal.Code) && o$Billing.Zip.Postal.Code != ""
  addr_line <- if (!is.na(o$Billing.Address.Line.1) && o$Billing.Address.Line.1 != "")
    paste0("<div style='font-size: 15px;color:#555;margin-top:8px;line-height:1.45;'>",
           htmlEscape(o$Billing.Address.Line.1), "<br>",
           htmlEscape(o$Billing.City), ", ", o$Billing.State.Province,
           if (has_zip) paste0(" ", o$Billing.Zip.Postal.Code), "</div>")
  else ""
  
  website_line <- if (!is.na(o$Website) && o$Website != "")
    paste0("<div style='margin-top:7px;'><a href='", o$Website,
           "' target='_blank' rel='noopener' style='font-size: 15px;color:#4e79a7;text-decoration:none;'>",
           "\U0001F517 ", htmlEscape(o$Website), "</a></div>")
  else ""
  
  # ── Stats: total matches + partnership length ────────────────────────
  matches_text <- if (!is.na(o$Total.Matches))
    paste0("<strong style='color:#1a1a1a;'>", o$Total.Matches, "</strong> match",
           if (o$Total.Matches != 1) "es" else "")
  else
    "<span style='color:#bbb;font-style:italic;'>Match count coming soon</span>"
  
  since_text <- if (!is.na(o$Created.Date))
    paste0("<strong style='color:#1a1a1a;'>", format_partner_since(o$Created.Date), "</strong>")
  else
    "<span style='color:#bbb;font-style:italic;'>Partnership length coming soon</span>"
  
  stats_line <- paste0(
    "<div style='font-size: 15px;color:#555;margin-top:8px;line-height:1.6;'>",
    matches_text, " &nbsp;\u2022&nbsp; ", since_text,
    "</div>"
  )
  
  HTML(paste0(
    "<div style='font-family:system-ui,-apple-system,sans-serif;min-width:250px;max-width:320px;padding:6px 2px;'>",
    "<div style='font-size: 15px;font-weight:700;color:#00274C;line-height:1.25;margin-bottom:9px;'>",
    htmlEscape(o$Account.Name), "</div>",
    "<div style='margin-bottom:4px;'>", status_badge, group_badge, "</div>",
    addr_line, website_line, stats_line,
    "<div style='font-size: 15px;color:#bbb;margin-top:10px;border-top:1px solid #f0f0f0;padding-top:7px;'>",
    "See sidebar for full details &amp; matches &rarr;</div>",
    "</div>"
  ))
}

# ── Theme ─────────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version      = 5,
  primary      = "#00274C",
  secondary    = "#FFCB05",
  success      = "#59a14f",
  info         = "#4e79a7",
  bg           = "#f0f2f5",
  fg           = "#1a1a1a",
  "navbar-bg"               = "#00274C",
  "navbar-dark-color"       = "rgba(255,255,255,0.85)",
  "navbar-dark-hover-color" = "#FFCB05",
  "navbar-dark-active-color"= "#FFCB05",
  "navbar-dark-brand-color" = "#ffffff",
  "font-size-base"          = "1rem",
  "border-radius"           = "0.5rem",
  "border-radius-sm"        = "0.3rem",
  "card-border-width"       = "0",
  "card-box-shadow"         = "0 2px 14px rgba(0,0,0,0.07)",
  base_font    = font_google("Roboto Condensed"),
  heading_font = font_google("Fjalla One")
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title    = tags$span(
    class = "brand-wrap",
    style = "display:flex; align-items:end; gap:16px;",
    tags$img(id = "brand-logo", src = "Edward-Ginsberg-Center_web-logo.png", height = "50", style = "display:block; margin-left: 70px;"),
    tags$span(
      class = "brand-text",
      style = "font-weight:700; letter-spacing:-0.2px; color:rgba(255,255,255,0.85); font-size: 16px;",
      "Community Partners"
    )
  ),
  theme    = app_theme,
  fillable = c("Map", "Accounts with No Geographic Data"),
  
  # ── Head: global CSS + listbox JS ─────────────────────────────────────────
  tags$head(
    tags$style(HTML("

      /* ── Smoother sidebar collapse/expand ──────────────────────────────
         bslib exposes these as CSS variables specifically so the built-in
         collapse animation can be tuned without fighting its internals. */
      :root {
        --bslib-sidebar-transition-duration: 0.32s;
        --bslib-sidebar-transition-easing-x: cubic-bezier(0.4, 0, 0.2, 1);
      }

      /* ── Global text size ─────────────────────────────────────────────
         Every font-size in this app (sidebar labels, badges, popups,
         match cards, tables, etc.) is now set as an explicit px value of
         at least 16px/12pt, rather than rem relative to this root - that
         was flipped deliberately so text can't quietly shrink below a
         readable size if this root or a parent element's size changes.
         This root size itself still only affects truly unstyled text
         (default Bootstrap elements with no font-size override, which
         is covered by font-size-base in the theme above instead). */
      html { font-size: 15px; }

      /* ── Base layout stabilization ────────────────────────────────────
         Without an explicit height/width here, html/body only take the
         height of their content by default. The map's 'fillable' height
         (100% cascaded down from here) and the navbar's 100% width can
         then both get measured against a stale/incorrect size on mobile,
         depending on timing - this is what was collapsing the map to
         near-zero height and intermittently leaving the navbar short of
         full width. overflow-x: hidden also guards against any one
         element (even briefly, mid-layout) pushing the whole page wider
         than the viewport, which throws off every percentage below it. */
      html, body {
        height: 100%;
        width: 100%;
        margin: 0;
        overflow-x: hidden;
      }

      /* ── Force height through bslib's sidebar layout at every width ────
         layout_sidebar() switches the sidebar to an overlay below bslib's
         own internal breakpoint, and that switch is handled entirely by
         bslib's own CSS - the html-fill-item/html-fill-container classes
         added to our own map div/card further below only cascade height
         DOWN from an ancestor that already has one. If this ancestor
         (bslib's own grid/main wrapper) loses its definite height at that
         breakpoint, nothing below it can recover it. Forcing it here
         directly, at every width, is the actual fix for the map
         collapsing specifically when the window narrows past that point. */
      .bslib-sidebar-layout {
        height: 100% !important;
        min-height: 0 !important;
      }
      .bslib-sidebar-layout > .main {
        height: 100% !important;
        min-height: 0 !important;
      }

      /* ── Navbar: tall navy logo band + maize nav-link band underneath ──
         (mirrors the Ginsberg Center site's own header structure) */
      .navbar {
        width: 100%;
        padding: 0 !important;
        box-shadow: 0 2px 10px rgba(0,0,0,0.18);
      }
      .navbar > .container-fluid {
        flex-wrap: wrap;
        padding: 0 !important;
        align-items: stretch;
      }
      /* Row 1: navy brand/logo band - tall, full width */
      .navbar-brand {
        width: 100%;
        margin: 0 !important;
        padding: 26px 72px !important;
        background: #00274C;
      }
      /* Row 2: maize nav-link band - full width, sits under the logo band */
      .navbar-collapse {
        width: 100%;
        background: #FFCB05;
        border-top: 3px solid #E8B900;
      }
      .navbar-nav {
        width: 100%;
        padding: 0 55px;
      }
      .navbar-nav .nav-link {
        font-weight: 700; font-size: 15px; padding: 0.85rem 1.1rem;
        color: #1a1a1a !important;
      }
      .navbar-nav .nav-link:hover { color: #00274C !important; background: rgba(0,0,0,0.05); }
      .navbar-nav .nav-link.active {
        color: #00274C !important;
        background: rgba(0,0,0,0.07);
        text-decoration: underline;
        text-decoration-color: #00274C;
        text-decoration-thickness: 2px;
      }
      .navbar-toggler { margin: 20px 32px; }

      /* ── Cards ──────────────────────────────────────────────────────── */
      .card { border: none !important; box-shadow: 0 2px 14px rgba(0,0,0,0.07); }

      /* ── Sidebar ────────────────────────────────────────────────────── */
      .bslib-sidebar-layout > .sidebar { background: #ffffff !important; border-right: 1px solid #e4e8ee; }
      .sidebar-section-label {
        font-size: 15px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.09em; color: #00274C; margin: 0 0 6px 0;
      }

      /* ── Community Priority / Engagement / Skill / SCU listboxes ──────── */
      #priority_filter, #engagement_filter, #skill_filter, #scu_filter {
        border-radius: 8px !important; border: 1px solid #dde2e8 !important;
        font-size: 15px; overflow-y: auto;
      }
      #priority_filter option, #engagement_filter option,
      #skill_filter option, #scu_filter option { padding: 7px 10px; line-height: 1.5; }
      #priority_filter option:checked, #engagement_filter option:checked,
      #skill_filter option:checked, #scu_filter option:checked { filter: brightness(0.78) !important; }

      /* ── Radio buttons ──────────────────────────────────────────────── */
      .form-check-input:checked { background-color: #00274C !important; border-color: #00274C !important; }

      /* ── Map popup ──────────────────────────────────────────────────── */
      .leaflet-popup-content-wrapper {
        border-radius: 12px !important;
        box-shadow: 0 6px 28px rgba(0,0,0,0.15) !important;
        padding: 0 !important;
        overflow: hidden;
      }
      .leaflet-popup-content { margin: 15px 18px !important; }
      .leaflet-popup-tip-container { display: none !important; }
      .leaflet-popup-close-button {
        top: 8px !important; right: 10px !important;
        font-size: 17px !important; color: #999 !important;
      }

      /* ── Map legend ─────────────────────────────────────────────────── */
      .info.legend {
        background: rgba(255,255,255,0.95) !important;
        border-radius: 10px !important;
        box-shadow: 0 2px 12px rgba(0,0,0,0.12) !important;
        font-size: 15px !important;
        padding: 10px 14px !important;
        line-height: 1.6 !important;
      }

      /* ── About box ──────────────────────────────────────────────────── */
      .about-box {
        background: #f0f4f8;
        border-radius: 8px;
        padding: 13px 15px;
        border-left: 4px solid #00274C;
        margin-bottom: 12px;
      }
      .about-title {
        font-weight: 700; font-size: 15px; color: #00274C; margin-bottom: 9px;
      }
      .about-box p {
        font-size: 15px; color: #444; line-height: 1.55; margin-bottom: 7px;
      }
      .about-box p:last-child { margin-bottom: 0; }

      /* ── Instruction box ────────────────────────────────────────────── */
      .instruction-box {
        background: #e8f0fe; border-radius: 8px;
        padding: 10px 13px; margin-top: 4px;
        font-size: 15px; color: #1a3a6b; line-height: 1.45;
      }

      /* ── Org panel ──────────────────────────────────────────────────── */
      .org-name {
        font-size: 15px; font-weight: 700; color: #00274C;
        line-height: 1.3; margin-bottom: 3px;
      }
      .org-website {
        font-size: 15px; color: #4e79a7 !important;
        text-decoration: none !important; display: block; margin-bottom: 5px;
      }
      .org-website:hover { text-decoration: underline !important; }

      /* ── Match cards ────────────────────────────────────────────────── */
      .match-card {
        background: #f7f9fc; border-radius: 8px;
        padding: 11px 13px; margin-bottom: 9px;
        border-left: 4px solid #dee2e6;
      }
      .match-card.fy25 { border-left-color: #4e79a7; }
      .match-card.fy26 { border-left-color: #b8940a; }

      /* ── Map markers: make them read as clickable buttons ────────────── */
      .leaflet-interactive {
        cursor: pointer !important;
        transition: filter 0.12s ease-out, transform 0.12s ease-out;
        transform-box: fill-box;
        transform-origin: center;
      }
      .leaflet-interactive:hover {
        filter: brightness(1.12) drop-shadow(0 2px 5px rgba(0,0,0,0.45));
        transform: scale(1.15);
      }
      .leaflet-interactive:active {
        transform: scale(0.95);
      }
      .leaflet-marker-cluster { cursor: pointer !important; }

      /* ── Selected-value chips under each multi-select filter ─────────── */
      .selected-tags-row {
        display: flex; flex-wrap: wrap; gap: 6px;
        margin-top: 8px; margin-bottom: 4px;
      }
      .selected-tag {
        display: inline-block; font-size: 15px; font-weight: 600;
        padding: 3px 10px; border-radius: 20px;
        background: #eef1f5; color: #00274C;
        white-space: nowrap;
      }

      /* ── Live result count pill (top of Filters section) ─────────────── */
      .result-count-pill {
        font-size: 15px; color: #444; background: #f0f4f8;
        border-radius: 8px; padding: 8px 12px; margin-bottom: 14px;
      }
      .result-count-pill strong { color: #00274C; font-size: 15px; }
      .result-count-pill.result-count-zero {
        background: #fdf3e7; color: #9a5b00; font-weight: 600;
      }

      /* ── Org detail drawer: overlays the map's right edge instead of
             permanently reserving column width ─────────────────────────── */
      .org-detail-drawer {
        position: absolute;
        top: 0; right: 0; bottom: 0;
        width: 400px;
        max-width: 92%;
        background: #ffffff;
        box-shadow: -6px 0 20px rgba(0,0,0,0.18);
        z-index: 1200;
        overflow-y: auto;
        padding: 18px 20px;
        border-left: 1px solid #e4e8ee;
        transform: translateX(100%);
        opacity: 0;
        pointer-events: none;
        transition: transform 0.32s cubic-bezier(0.4, 0, 0.2, 1),
                    opacity 0.24s ease;
      }
      .org-detail-drawer.open {
        transform: translateX(0);
        opacity: 1;
        pointer-events: auto;
      }
      .map-hint-pill {
        position: absolute;
        left: 16px; bottom: 16px;
        z-index: 900;
        background: rgba(255,255,255,0.95);
        border: 1px solid #e4e8ee;
        border-radius: 20px;
        padding: 7px 14px;
        font-size: 15px;
        color: #555;
        box-shadow: 0 2px 10px rgba(0,0,0,0.12);
        pointer-events: none;
      }

      /* ── Close button under the org detail panel ──────────────────────── */
      .close-panel-btn {
        display: inline-flex;
        align-items: center;
        gap: 5px;
        font-size: 15px;
        font-weight: 600;
        color: #666 !important;
        text-decoration: none !important;
        padding: 5px 12px;
        border-radius: 20px;
        border: 1px solid #dde2e8;
        transition: background 0.12s ease-out, color 0.12s ease-out;
      }
      .close-panel-btn:hover {
        background: #f0f4f8;
        color: #00274C !important;
      }

      /* ── Nav tabs in sidebar ────────────────────────────────────────── */
      .nav-tabs { border-bottom: 2px solid #e8edf2 !important; }
      .nav-tabs .nav-link {
        font-size: 15px; font-weight: 500; color: #666;
        border: none !important; padding: 6px 12px;
      }
      .nav-tabs .nav-link.active {
        font-weight: 700; color: #00274C !important;
        border-bottom: 2px solid #00274C !important;
        background: transparent !important;
      }
      .nav-tabs .nav-link:hover { color: #00274C; background: transparent; }

      /* ── Infographics ──────────────────────────────────────────────── */
      .infographic-section-title {
        font-size: 15px; font-weight: 700; color: #00274C;
        margin-bottom: 14px; padding-bottom: 8px;
        border-bottom: 2px solid #FFCB05;
      }
      .stat-tile-row {
        display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 22px;
      }
      .stat-tile {
        flex: 1 1 140px;
        background: #f7f9fc;
        border-radius: 10px;
        padding: 14px 12px;
        text-align: center;
        border: 1px solid #eef1f5;
      }
      .stat-number { font-size: 26px; font-weight: 800; line-height: 1.1; }
      .stat-label {
        font-size: 15px; font-weight: 600; color: #888;
        text-transform: uppercase; letter-spacing: 0.06em; margin-top: 5px;
      }
      .chart-card-title {
        font-size: 15px; font-weight: 700; color: #00274C;
        text-transform: uppercase; letter-spacing: 0.06em;
        margin-bottom: 8px;
      }
      .chart-panel {
        background: #ffffff; border: 1px solid #eef1f5; border-radius: 10px;
        padding: 14px 16px; height: 100%;
      }

      /* ── DT tables: let them scroll horizontally instead of overflowing
             the viewport width on narrow screens ─────────────────────────── */
      .dataTables_wrapper { overflow-x: auto; width: 100%; }

      /* ── Mobile layout ─────────────────────────────────────────────────
             Everything above assumes a desktop-width window (the logo
             margin, navbar padding, and drawer width in particular are all
             fixed px values). This breakpoint pulls those back in so the
             page doesn't scroll sideways on a phone; bslib's own sidebar
             (collapsible drawer) and plotly charts are already responsive
             on their own and don't need help here. ────────────────────── */
      @media (max-width: 768px) {
        html { font-size: 15px; }

        .navbar-brand {
          padding: 14px 16px !important;
        }
        .brand-wrap { gap: 10px !important; }
        #brand-logo {
          height: 34px !important;
          margin-left: 0 !important;
        }
        .brand-text { font-size: 15px !important; }

        .navbar-nav { padding: 0 10px !important; }
        .navbar-nav .nav-link {
          padding: 0.7rem 0.85rem !important;
          font-size: 15px !important;
        }
        .navbar-toggler { margin: 10px 14px !important; }

        /* Org detail drawer: take the full screen instead of a fixed
           400px panel, which would otherwise leave barely any of the
           map visible next to it. */
        .org-detail-drawer {
          width: 100% !important;
          max-width: 100% !important;
        }

        .about-box, .instruction-box { padding: 11px 13px; }

        .stat-tile-row { gap: 8px; }
        .stat-tile { flex: 1 1 45%; }

        /* Give listboxes (Community Priority, Skill Area, etc.) a bit
           less height so the sidebar doesn't turn into one long scroll
           before you even reach the map/table on a small screen. */
        #priority_filter { height: 140px !important; }
        #skill_filter, #engagement_filter, #scu_filter { height: 110px !important; }
      }

    ")),
    tags$script(HTML("
      $(document).ready(function() {
        var groupColors = {
          'Animal Welfare':                                 '#bab0ac',
          'Arts & Culture':                                  '#e15759',
          'Basic Needs & Material Assistance':               '#9c755f',
          'Community Development & Organizational Capacity': '#76b7b2',
          'Economic Stability & Workforce Development':      '#f28e2b',
          'Education & Learning':                            '#4e79a7',
          'Environment & Sustainability':                    '#499894',
          'Food Access & Food Security':                     '#edc948',
          'Health & Wellness':                               '#59a14f',
          'Housing & Homelessness':                          '#d37295',
          'Human Services & Social Support':                 '#8cd17d',
          'Justice, Legal Services & Public Safety':         '#b07aa1',
          'Public Participation & Community Voice':          '#86bcb6',
          'Social Justice & Equity':                         '#ff9d9a',
          'Youth Development':                               '#79706e'
        };
        $('#priority_filter option').each(function() {
          var col = groupColors[$(this).val()];
          if (col) {
            $(this).css({ 'background-color': col, 'color': '#fff', 'font-weight': '600' });
          }
        });
      });

      // Slide the org detail drawer open/closed. The container itself is
      // always in the DOM (see #org-detail-drawer) - only this class
      // toggles, which is what lets the CSS transition play both ways
      // instead of the panel just popping in/out.
      Shiny.addCustomMessageHandler('toggleOrgDrawer', function(open) {
        var el = document.getElementById('org-detail-drawer');
        if (!el) return;
        el.classList.toggle('open', open);
      });

      // Leaflet measures its container's size once, the moment it first
      // renders, and caches it - it does NOT notice on its own if that
      // size was wrong or changes later. On mobile, this app's layout
      // (collapsible sidebar, navbar that wraps to two rows, bslib's
      // fillable height calculation) can still be settling after Leaflet's
      // first render, which is what was collapsing the map to near-zero
      // height. Telling it to re-measure on resize/orientation change,
      // and once more shortly after load to catch that initial settling,
      // fixes both - sized correctly from here on regardless of when the
      // surrounding layout actually finishes.
      function fixMapSize() {
        var widget = HTMLWidgets.find('#map');
        if (!widget || !widget.getMap) return;
        var map = widget.getMap();
        if (map) map.invalidateSize();
      }
      $(document).ready(function() {
        setTimeout(fixMapSize, 400);
        setTimeout(fixMapSize, 1000);
      });
      $(window).on('resize orientationchange', function() {
        setTimeout(fixMapSize, 200);
        setTimeout(fixMapSize, 600);
      });
    "))
  ),
  
  # ── Map tab ──────────────────────────────────────────────────────────────
  nav_panel(
    "Map",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        bg      = "white",
        width   = 400,
        open    = list(desktop = "open", mobile = "closed"),
        padding = "16px",
        div(class = "about-box",
            div(class = "about-title", "About the Map"),
            tags$p("The Ginsberg Center began tracking community partner relationships in Salesforce in 2017.
                  The data on this map comes from those Salesforce records, so the earliest relationship
                  that may appear is January 2017."),
            tags$p("This map represents relationships documented in our Salesforce system and is not a
                  complete history of the Ginsberg Center\u2019s work with community partners. Ginsberg
                  Center has worked with communities and organizations for many years prior to adopting
                  Salesforce, and some of our current relationships began before 2017. Likewise, some
                  former community partners may no longer be active or may not appear because of how
                  relationships are recorded in Salesforce."),
            tags$p("As a result, the number of years shown for a relationship may not reflect the full
                  length of our relationship with a community partner. A relationship that began before
                  2017, for example, may appear as beginning in 2017 because that is the earliest point
                  represented in this dataset."),
            tags$p("We share this map as a way to visualize the community partnerships documented in our
                  current data, not to define the full history, depth, or significance of Ginsberg
                  Center\u2019s relationships with communities.")
        ),
        tags$div(class = "instruction-box", style = "margin-bottom:18px;",
                 tags$strong("Click a marker"), " on the map to view organization details and matched projects."
        ),
        tags$div(class = "infographic-section-title", "Filters"),
        uiOutput("map_result_count"),
        tags$p(class = "sidebar-section-label", "Partner Status"),
        radioButtons(
          "status_filter", label = NULL,
          choices = c("All", "Active", "Lead"), selected = "All", inline = TRUE
        ),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        checkboxInput(
          "recent_match_filter",
          label = paste0("Only show orgs with a match in the last 2 fiscal years (",
                         paste(recent_fys, collapse = " or "), ")"),
          value = TRUE
        ),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "Community Priority"),
          actionLink("clear_filter", "Clear",
                     style = "font-size: 15px; color:#aaa; text-decoration:none;")
        ),
        tags$p(
          style = "font-size: 15px; color:#bbb; margin-bottom:8px; line-height:1.4;",
          "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
        ),
        selectInput(
          "priority_filter", label = NULL,
          choices   = names(group_map), selected = NULL,
          multiple  = TRUE, selectize = FALSE, size = 8, width = "100%"
        ),
        uiOutput("priority_selected_tags"),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "Skill Area"),
          if (length(skill_choices) > 0)
            actionLink("clear_skill_filter", "Clear",
                       style = "font-size: 15px; color:#aaa; text-decoration:none;")
        ),
        if (length(skill_choices) > 0) {
          tagList(
            tags$p(
              style = "font-size: 15px; color:#bbb; margin-bottom:8px; line-height:1.4;",
              "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
            ),
            selectInput(
              "skill_filter", label = NULL,
              choices = skill_choices, selected = NULL,
              multiple = TRUE, selectize = FALSE, size = 5, width = "100%"
            ),
            uiOutput("skill_selected_tags")
          )
        } else
          tags$p(style = "font-size: 15px;color:#bbb;font-style:italic;", "No match data available yet."),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "Form of Engagement"),
          if (length(engagement_choices) > 0)
            actionLink("clear_engagement_filter", "Clear",
                       style = "font-size: 15px; color:#aaa; text-decoration:none;")
        ),
        if (length(engagement_choices) > 0) {
          tagList(
            tags$p(
              style = "font-size: 15px; color:#bbb; margin-bottom:8px; line-height:1.4;",
              "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
            ),
            selectInput(
              "engagement_filter", label = NULL,
              choices = engagement_choices, selected = NULL,
              multiple = TRUE, selectize = FALSE, size = 5, width = "100%"
            ),
            uiOutput("engagement_selected_tags")
          )
        } else
          tags$p(style = "font-size: 15px;color:#bbb;font-style:italic;", "No match data available yet."),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "School / College / Unit"),
          if (length(scu_choices) > 0)
            actionLink("clear_scu_filter", "Clear",
                       style = "font-size: 15px; color:#aaa; text-decoration:none;")
        ),
        if (length(scu_choices) > 0) {
          tagList(
            tags$p(
              style = "font-size: 15px; color:#bbb; margin-bottom:8px; line-height:1.4;",
              "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
            ),
            selectInput(
              "scu_filter", label = NULL,
              choices = scu_choices, selected = NULL,
              multiple = TRUE, selectize = FALSE, size = 5, width = "100%"
            ),
            uiOutput("scu_selected_tags")
          )
        } else
          tags$p(style = "font-size: 15px;color:#bbb;font-style:italic;", "No match data available yet.")
      ),
      div(
        class = "html-fill-item html-fill-container",
        style = "position:relative; height:100%; display:flex; min-height:0;",
        card(
          class = "html-fill-item",
          full_screen = TRUE, style = "flex:1; min-width:0; min-height:0;",
          leafletOutput("map", height = "100%")
        ),
        tags$div(
          id    = "org-detail-drawer",
          class = "org-detail-drawer",
          uiOutput("org_side_panel")
        ),
        tags$div(
          class = "map-hint-pill",
          "\U0001F4CD Click a marker to see account details"
        )
      )
    )
  ),
  
  # ── No-geo-data tab ────────────────────────────────────────────────────────
  nav_panel(
    "Accounts with No Geographic Data",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        bg      = "white",
        width   = 390,
        open    = list(desktop = "open", mobile = "closed"),
        padding = "16px",
        uiOutput("problem_org_panel")
      ),
      card(
        full_screen = TRUE,
        card_header("Accounts with No Geographic Data"),
        card_body(
          tags$p(
            style = "font-size: 15px; color:#888; margin-bottom:12px; line-height:1.45;",
            "These accounts don't have a usable address on file, so they can't be placed on the map yet. ",
            "Click a row to see their details and matched projects."
          ),
          DTOutput("problem_table")
        )
      )
    )
  ),
  
  # ── Data Dictionary tab ───────────────────────────────────────────────────
  nav_panel(
    "Data Dictionary",
    div(
      style = "max-width:900px; margin:0 auto; padding:8px 4px 24px;",
      card(
        card_body(
          accordion(
            open = "Accounts Data",
            
            accordion_panel(
              "Accounts Data",
              tags$p(style = "font-size: 15px;color:#888;margin-bottom:10px;",
                     "From ", tags$code("Accounts_wideCategories_Geocoded.csv"), ", one row per community partner."),
              field_table(list(
                c("Account Name", "The organization's official name."),
                c("Description", "Free-text summary of the org's programs and work, shown in the org detail panel."),
                c("Mission Statement", "Free-text summary of the org's mission statement, shown in the org detail panel if a description is not available."),
                c("Billing Address", "The org's primary mailing address on file."),
                c("Ginsberg Partner Status", "Where an organization is currently Active or a Lead."),
                c("Category 1\u20139", "Up to nine Community Priority tags per org, from the new accounts export's picklist. Already at the right granularity for map coloring/filtering, so each is used directly (see Community Priority Groups below) rather than rolled up further."),
                c("Website", "Joined in from a separate file, websites.csv, by account name."),
                c("Latitude / Longitude", "Geocoded coordinates. A few rows had missing/bad source addresses that geocoded to bogus points; those are either manually corrected or, if no address exists at all, excluded from the map and listed on the \u201cAccounts with No Geographic Data\u201d tab instead."),
                c("Total Matches", "Whole-number count of matches per org, from Salesforce."),
                c("Created Date (\u201cPartner Since\u201d)", "Date the org was added to Salesforce, shown as just the year in the \u201cPartner Since\u201d field.")
              ))
            ),
            
            accordion_panel(
              "Matches Data",
              tags$p(style = "font-size: 15px;color:#888;margin-bottom:10px;",
                     "From ", tags$code("Matches_Final.csv"), ", built by ", tags$code("data_processing.R"),
                     " from the FY25-26 matches export. One row per (Org, Project, FY, tag) - a
                     match with 2 Skill Areas and 1 Form of Engagement becomes 3 rows here."),
              field_table(list(
                c("Initiative: Initiative Name \u2192 Account Name \u2192 Org",
                  "Which community partner the match belongs to. The export itself only has combined \u201cOrgAbbrev: Project Title\u201d text, not a clean org field, so Account Name is resolved by joining each match's ID against the old tracking workbook's \u201cInitiative Account\u201d column, with a small manual override table and district-level fallbacks (e.g. a school match with no school-specific account falls back to the district) for the few that still don't resolve."),
                c("Initiative: Initiative Name \u2192 Project", "Name/title of the specific match or project (kept as-is, combined org+title text)."),
                c("Skill Areas \u2192 Category", "The skill/expertise area(s) the match called for \u2014 already at the group level in this export (e.g. \u201cData, Evaluation & Research\u201d)"),
                c("Form of Engagement \u2192 Offering", "What kind of engagement the match was (e.g. a course-based project, a volunteer opportunity). Powers the \u201cForm of Engagement\u201d filter."),
                c("Resource Offering \u2192 Resource.Offering", "The specific course, program, or semester the match ran under (e.g. \u201cFY25 Fall Rackham Intern Fellows\u201d). Shown as-is on each match card for context."),
                c("UM Connection \u2192 UM.Connection", "The U-M school, college, or unit involved in this match. Powers the \u201cSchool/College/Unit\u201d filter and is shown on each match card \u2014 this is match-level, not an org-level attribute, since the same org can have matches through different units."),
                c("Match Completed Date \u2192 Completed", "When the match was marked complete."),
                c("Fiscal Year", "Derived from Completed - fiscal year runs July\u2013June, so a date in Sept 2024 is FY25.")
              ))
            ),
            
            accordion_panel(
              "Community Priority Groups",
              tags$p(style = "font-size: 15px;color:#888;margin-bottom:10px;",
                     "The 15 values used for map coloring and the Community Priority filter, taken directly from
                     Category 1\u20139."),
              tags$ul(style = "font-size: 15px;color:#555;padding-left:18px;margin:0;",
                      lapply(names(group_map), tags$li))
            ),
            
            accordion_panel(
              "Skill Area Groups",
              tags$p(style = "font-size: 15px;color:#888;margin-bottom:10px;",
                     "The 9 values used for the Skill Area filter, taken directly from Skill Area 1\u20133."),
              tags$ul(style = "font-size: 15px;color:#555;padding-left:18px;margin:0;",
                      lapply(names(skill_group_map), tags$li))
            ),
            
            accordion_panel(
              "Filters on the Map Tab",
              field_table(list(
                c("Partner Status", "Active / Lead / All."),
                c("Recent match (last 2 FYs)", "Toggle \u2014 shows only orgs with at least one match in the two most recent fiscal years found in the matches data. Checked by default."),
                c("Community Priority", "Org-level. An org qualifies if any of its Category 1\u20139 tags matches a selected value."),
                c("Form of Engagement", "Match-level. An org qualifies if any of its matches has a selected engagement type."),
                c("Skill Area", "Match-level. An org qualifies if any of its matches has a selected skill area."),
                c("School / College / Unit", "Match-level, from UM Connection. An org qualifies if any of its matches involved a selected U-M school/college/unit.")
              ))
            ),
            
            accordion_panel(
              "Known Data Quality Notes",
              tags$ul(
                style = "font-size: 15px;color:#555;line-height:1.7;padding-left:20px;",
                tags$li(paste0(length(no_geo_names), " accounts have no address on file at all and can't be geocoded. They're excluded from the map and listed on the \u201cAccounts with No Geographic Data\u201d tab.")),
                tags$li("Two accounts (Detroit People's Platform; Next Chapter Bookclub of Saline) had bad source addresses that geocoded to another country. Both were manually corrected with verified addresses/coordinates."),
                tags$li("The Infographics tab's Southeast Michigan / Rest of Michigan split is a rough latitude/longitude bounding-box approximation, not actual county boundaries.")
              )
            )
          )
        )
      )
    )
  ),
  
  # ── Infographics tab ──────────────────────────────────────────────────────
  nav_panel(
    "Infographics",
    div(
      style = "max-width:1100px; margin:0 auto; padding:8px 4px 24px;",
      
      # ── Section 1: Community Partners at a Glance ──────────────────────
      card(
        card_body(
          tags$div(class = "infographic-section-title", "Community Partners at a Glance"),
          
          tags$div(
            class = "stat-tile-row",
            stat_tile(n_total, "Total Community Partners"),
            stat_tile(n_se_mi, "Southeast Michigan", geo_colors[["Southeast Michigan"]]),
            stat_tile(n_mi_total, "Total State of Michigan", geo_colors[["Rest of Michigan"]]),
            stat_tile(n_usa_other, "USA (Other States)", geo_colors[["USA (Other States)"]]),
            stat_tile(n_international, "International", geo_colors[["International"]])
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Where Our Partners Are Located"),
              plotlyOutput("geo_donut", height = "260px")
            ),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Top 3 Community Priority Areas"),
              tags$p(
                style = "font-size: 15px;color:#999;margin-top:-4px;margin-bottom:10px;",
                "By number of partners working in that area"
              ),
              plotlyOutput("top3_priority_bar", height = "220px")
            )
          )
        )
      ),
      
      tags$div(style = "height:20px;"),
      
      # ── Section 2: Matches Over Time ────────────────────────────────────
      card(
        card_body(
          tags$div(class = "infographic-section-title", "Matches Over Time"),
          
          div(
            class = "chart-panel",
            style = "margin-bottom:18px;",
            tags$div(class = "chart-card-title", "Partners With the Most Matches (All-Time)"),
            plotlyOutput("leaderboard_bar", height = "320px")
          ),
          
          tags$div(
            class = "stat-tile-row",
            stat_tile(n_recent_matches,
                      paste0("Matches in the Last 2 FYs (", paste(recent_fys, collapse = " + "), ")")),
            div(
              class = "chart-panel", style = "flex:2 1 260px;",
              tags$div(class = "chart-card-title", "By Fiscal Year"),
              plotlyOutput("fy_comparison_bar", height = "110px")
            )
          ),
          
          tags$p(
            style = "font-size: 15px;color:#888;margin-bottom:10px;",
            "Most common skill areas, forms of engagement, and community priorities among matches in the last 2 fiscal years:"
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Skill Areas"),
              plotlyOutput("skill_recent_bar", height = "230px")
            ),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Forms of Engagement"),
              plotlyOutput("engagement_recent_bar", height = "230px")
            ),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Community Priorities"),
              plotlyOutput("priority_recent_bar", height = "230px")
            )
          )
        )
      )
    )
  ),
  
  # ── About tab ─────────────────────────────────────────────────────────────
  nav_panel(
    "About",
    div(
      style = "max-width:800px; margin:0 auto; padding:8px 4px 24px;",
      card(
        card_body(
          tags$div(class = "infographic-section-title", "About This Map"),
          tags$p(
            style = "font-size: 15px;color:#444;line-height:1.65;margin-bottom:14px;",
            "This site visualizes the Edward Ginsberg Center's community partnerships. It displays where our ",
            "partners are located, what they focus on, and the matches we've facilitated between them ",
            "and University of Michigan students, faculty, and staff."
          ),
          div(class = "about-box",
              div(class = "about-title", "Where the Data Comes From"),
              tags$p("The Ginsberg Center began tracking community partner relationships in Salesforce in 2017.
                    The account data on this map comes from those Salesforce records, and the matches shown
                    come from the FY25 and FY26 match-tracking spreadsheet."),
              tags$p("This map is not a complete history of the Center's work, as relationships that predate
                    Salesforce, or that aren't fully reflected in how matches are recorded, may be under-
                    represented here. Treat this project as a current, evolving snapshot instead of a definitive record.")
          ),
          
          tags$div(class = "infographic-section-title", style = "margin-top:24px;", "Credits"),
          field_table(list(
            c("Built by:", "\u201cGinsberg Center Data & Evaluation Team\u201d"),
            c("Data sources:", "Salesforce account records; the FY25\u2013FY26 match-tracking spreadsheet (\u201cMapping CP Network.xlsx\u201d)"),
            c("Categorization:", "Community Priority and Skill Area groupings developed in partnership with Ginsberg Center staff (see Data Dictionary tab)"),
            c("Design:", "Built to the Ginsberg Center / University of Michigan brand style guide"),
            c("Built with:", "R, Shiny, leaflet, plotly, bslib, and DT"),
            c("Questions or corrections?", "[Placeholder \u2014 add a contact email or feedback link.]")
          ))
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  # Logo Output
  # Does an org match any selected focus area group?
  org_matches <- function(org_row, selected_groups) {
    selected_cats <- unlist(group_map[selected_groups], use.names = FALSE)
    org_cats      <- unlist(org_row[, cat_cols], use.names = FALSE)
    org_cats      <- org_cats[!is.na(org_cats) & org_cats != ""]
    any(org_cats %in% selected_cats)
  }
  
  # ── Initial map render ────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet(map_orgs) |>
      addProviderTiles("OpenStreetMap") |>
      setView(lng = -83.5, lat = 42.4, zoom = 9) |>
      addCircleMarkers(
        lng          = ~longitude,
        lat          = ~latitude,
        layerId      = ~Account.Name,
        label        = make_label(map_orgs),
        labelOptions = labelOptions(textsize = "13px"),
        group        = "all",
        radius       = 9,
        color        = "#ffffff",
        fillColor    = marker_color,
        fillOpacity  = 0.95,
        weight       = 2,
        opacity      = 1,
        clusterOptions = markerClusterOptions()
      ) |>
      addEasyButton(easyButton(
        icon    = "fa-crosshairs",
        title   = "Reset view",
        onClick = JS("function(btn, map){ map.setView([42.4, -83.5], 9); }")
      ))
  })
  
  # ── Live result count, shown at the top of the Filters section ───────────
  result_count <- reactiveVal(nrow(map_orgs))
  
  output$map_result_count <- renderUI({
    n <- result_count()
    pill <- if (n == 0) {
      tags$div(
        class = "result-count-pill result-count-zero",
        "\u26A0\uFE0F No accounts match these filters"
      )
    } else {
      tags$div(
        class = "result-count-pill",
        tags$strong(format(n, big.mark = ",")), "results."
      )
    }
    tagList(
      pill,
      tags$div(
        style = "margin-top:-8px; margin-bottom:14px;",
        actionLink("clear_all_filters", "Clear all filters",
                   style = "font-size: 15px; color:#aaa; text-decoration:none;")
      )
    )
  })
  
  # ── Selected-value chips (so a selection doesn't visually disappear once
  #    you scroll to a different filter or click into another listbox) ────
  output$priority_selected_tags <- renderUI({
    selected_tags_ui(input$priority_filter, colors = group_colors)
  })
  output$skill_selected_tags <- renderUI({
    selected_tags_ui(input$skill_filter)
  })
  output$engagement_selected_tags <- renderUI({
    selected_tags_ui(input$engagement_filter)
  })
  output$scu_selected_tags <- renderUI({
    selected_tags_ui(input$scu_filter)
  })
  
  # ── Re-render markers when filters change ─────────────────────────────────
  observe({
    selected   <- input$priority_filter
    status     <- input$status_filter
    recent_only <- input$recent_match_filter
    engagement <- input$engagement_filter
    skill      <- input$skill_filter
    scu        <- input$scu_filter
    
    base <- if (is.null(status) || status == "All") map_orgs
    else map_orgs |> filter(Ginsberg.Partner.Status == status)
    
    if (isTRUE(recent_only)) {
      recent_orgs <- projects |> filter(FY %in% recent_fys) |> pull(Org) |> unique()
      base        <- base |> filter(Account.Name %in% recent_orgs)
    }
    
    if (!is.null(engagement) && length(engagement) > 0) {
      eng_orgs <- projects |> filter(Offering %in% engagement) |> pull(Org) |> unique()
      base     <- base |> filter(Account.Name %in% eng_orgs)
    }
    
    if (!is.null(skill) && length(skill) > 0) {
      skill_orgs <- projects |> filter(skill_to_group[Category] %in% skill) |> pull(Org) |> unique()
      base       <- base |> filter(Account.Name %in% skill_orgs)
    }
    
    if (!is.null(scu) && length(scu) > 0) {
      scu_orgs <- projects |> filter(UM.Connection %in% scu) |> pull(Org) |> unique()
      base     <- base |> filter(Account.Name %in% scu_orgs)
    }
    
    leafletProxy("map") |>
      clearMarkers() |>
      clearMarkerClusters() |>
      clearGroup("highlight") |>
      clearPopups()
    
    # Nothing to plot - stop here. (Without this guard, sapply() over a
    # zero-row data frame below returns list() instead of logical(0), and
    # indexing a data frame with list() throws an error - that was the
    # source of the crash when a filter combination matched nothing.)
    if (nrow(base) == 0) {
      result_count(0)
      return(invisible(NULL))
    }
    
    if (length(selected) == 0) {
      result_count(nrow(base))
      leafletProxy("map") |>
        addCircleMarkers(
          data         = base,
          lng          = ~longitude,
          lat          = ~latitude,
          layerId      = ~Account.Name,
          label        = make_label(base),
          labelOptions = labelOptions(textsize = "13px"),
          group        = "all",
          radius       = 9,
          color        = "#ffffff",
          fillColor    = marker_color,
          fillOpacity  = 0.95,
          weight       = 2,
          opacity      = 1,
          clusterOptions = markerClusterOptions()
        )
    } else {
      matched   <- base[sapply(seq_len(nrow(base)), function(i) org_matches(base[i, ], selected)), ]
      unmatched <- base[sapply(seq_len(nrow(base)), function(i) !org_matches(base[i, ], selected)), ]
      result_count(nrow(matched))
      
      if (nrow(unmatched) > 0) {
        leafletProxy("map") |>
          addCircleMarkers(
            data         = unmatched,
            lng          = ~longitude,
            lat          = ~latitude,
            layerId      = ~Account.Name,
            label        = make_label(unmatched),
            labelOptions = labelOptions(textsize = "13px"),
            radius       = 5,
            color        = "#bbbbbb",
            fillColor    = "#cccccc",
            fillOpacity  = 0.3,
            weight       = 1,
            opacity      = 0.4,
            group        = "unmatched"
          )
      }
      if (nrow(matched) > 0) {
        leafletProxy("map") |>
          addCircleMarkers(
            data         = matched,
            lng          = ~longitude,
            lat          = ~latitude,
            layerId      = ~Account.Name,
            label        = make_label(matched),
            labelOptions = labelOptions(textsize = "13px"),
            radius       = 10,
            color        = "#ffffff",
            fillColor    = marker_color,
            fillOpacity  = 0.95,
            weight       = 2.5,
            opacity      = 1,
            group        = "matched"
          )
      }
    }
  })
  
  # ── Clear community priority filter ───────────────────────────────────────
  observeEvent(input$clear_filter, {
    updateSelectInput(session, "priority_filter", selected = character(0))
  })
  
  # ── Clear form of engagement filter ───────────────────────────────────────
  observeEvent(input$clear_engagement_filter, {
    updateSelectInput(session, "engagement_filter", selected = character(0))
  })
  
  # ── Clear skill area filter ───────────────────────────────────────────────
  observeEvent(input$clear_skill_filter, {
    updateSelectInput(session, "skill_filter", selected = character(0))
  })
  
  # ── Clear school/college/unit filter ──────────────────────────────────────
  observeEvent(input$clear_scu_filter, {
    updateSelectInput(session, "scu_filter", selected = character(0))
  })
  
  # ── Clear every filter on the Map tab, back to its default state ──────────
  observeEvent(input$clear_all_filters, {
    updateRadioButtons(session, "status_filter", selected = "All")
    updateCheckboxInput(session, "recent_match_filter", value = TRUE)
    updateSelectInput(session, "priority_filter", selected = character(0))
    updateSelectInput(session, "skill_filter", selected = character(0))
    updateSelectInput(session, "engagement_filter", selected = character(0))
    updateSelectInput(session, "scu_filter", selected = character(0))
  })
  
  # ── Selected org state ────────────────────────────────────────────────────
  selected_org <- reactiveVal(NULL)
  
  observeEvent(input$deselect_org, {
    selected_org(NULL)
    leafletProxy("map") |> clearGroup("highlight") |> clearPopups()
  })
  
  # ── Marker click: highlight + popup + sidebar ─────────────────────────────
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      selected_org(click$id)
      org <- map_orgs |> filter(Account.Name == click$id)
      if (nrow(org) > 0) {
        leafletProxy("map") |>
          clearGroup("highlight") |>
          clearPopups() |>
          addCircleMarkers(
            lng         = click$lng,
            lat         = click$lat,
            radius      = 16,
            color       = "#FFD700",
            fillOpacity = 0,
            weight      = 3,
            opacity     = 1,
            group       = "highlight",
            options     = pathOptions(interactive = FALSE)
          ) |>
          addPopups(
            lng   = click$lng,
            lat   = click$lat,
            popup = make_popup(org)
          )
      }
    }
  })
  
  # Clicking blank map clears highlight and popup only
  # (do NOT reset selected_org — marker clicks propagate to map_click too)
  observeEvent(input$map_click, {
    leafletProxy("map") |> clearGroup("highlight") |> clearPopups()
  })
  
  # ── Shared org detail builder (Details/Matches tabs) ─────────────────────
  # Used by both the Map tab's sidebar and the "No Geographic Data" tab's
  # sidebar, so the two stay visually and behaviorally consistent.
  build_org_detail_ui <- function(org_name, data, deselect_id) {
    org <- data |> filter(Account.Name == org_name)
    if (nrow(org) == 0) return(NULL)
    
    grp     <- org$primary_group
    grp_col <- if (!is.na(grp)) group_pal(grp) else "#cccccc"
    
    org_projects <- projects_agg |> filter(Org == org_name) |> arrange(FY, Project)
    
    has_zip <- !is.na(org$Billing.Zip.Postal.Code) && org$Billing.Zip.Postal.Code != ""
    addr_parts <- c(
      org$Billing.Address.Line.1,
      if (!is.na(org$Billing.Address.Line.2) && org$Billing.Address.Line.2 != "NA")
        org$Billing.Address.Line.2,
      paste0(org$Billing.City, ", ", org$Billing.State.Province,
             if (has_zip) paste0(" ", org$Billing.Zip.Postal.Code))
    )
    addr <- paste(addr_parts[addr_parts != "" & !is.na(addr_parts)], collapse = "\n")
    
    # Build match cards - one per actual match (see projects_agg above),
    # not one per skill-area tag that match happened to carry.
    if (nrow(org_projects) == 0) {
      proj_html <- tags$p(
        style = "color:#aaa; font-style:italic; font-size: 15px;",
        "No matched projects on record."
      )
    } else {
      rows <- lapply(seq_len(nrow(org_projects)), function(i) {
        p      <- org_projects[i, ]
        fy_bg  <- if (p$FY == "FY25") "#4e79a7" else "#FFCB05"
        fy_txt <- if (p$FY == "FY25") "#fff"    else "#1a1a1a"
        tags$div(
          class = paste("match-card", tolower(p$FY)),
          # Top row: FY badge + date
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:flex-start; margin-bottom:6px;",
            tags$span(
              style = paste0("background:", fy_bg, ";color:", fy_txt,
                             ";padding:2px 9px;border-radius:20px;font-size: 15px;font-weight:700;"),
              p$FY
            ),
            if (!is.na(p$Completed))
              tags$span(
                style = "font-size: 15px; color:#bbb; white-space:nowrap; margin-left:4px;",
                paste0("Completed: ", format(p$Completed, "%b %d, %Y"))
              )
          ),
          # Project title
          tags$p(
            style = "font-size: 15px; font-weight:600; color:#1a1a1a; margin:0 0 5px 0;",
            p$Project
          ),
          # Resource Offering: the course/program/semester this match ran
          # under (e.g. "FY25 Fall Rackham Intern Fellows"), for context.
          if (!is.na(p$Resource.Offering))
            tags$p(
              style = "font-size: 15px; color:#666; margin:0 0 2px 0;",
              tags$strong(style = "color:#00274C;", "Resource Offering: "), p$Resource.Offering
            ),
          # Aggregated skill areas + form of engagement for this match
          if (!is.na(p$Skill.Areas))
            tags$p(
              style = "font-size: 15px; color:#666; margin:0 0 2px 0;",
              tags$strong(style = "color:#00274C;", "Skill Areas: "), p$Skill.Areas
            ),
          if (!is.na(p$Forms.of.Engagement))
            tags$p(
              style = "font-size: 15px; color:#666; margin:0 0 2px 0;",
              tags$strong(style = "color:#00274C;", "Form of Engagement: "), p$Forms.of.Engagement
            ),
          # School/College/Unit: the U-M school, college, or unit involved
          # in this match, from the matches data's UM Connection column.
          if (!is.na(p$UM.Connection))
            tags$p(
              style = "font-size: 15px; color:#666; margin:0;",
              tags$strong(style = "color:#00274C;", "School/College/Unit: "), p$UM.Connection
            )
        )
      })
      proj_html <- tagList(rows)
    }
    
    tagList(
      # ── Org header: group color accent stripe ────────────────────────────
      tags$div(
        style = paste0(
          "border-left:4px solid ", grp_col, "; padding-left:10px; margin-bottom:10px;"
        ),
        tags$div(class = "org-name", org_name),
        if (!is.na(org$Website) && org$Website != "")
          tags$a(class = "org-website", href = org$Website,
                 target = "_blank", rel = "noopener noreferrer", org$Website),
        if (!is.na(org$Ginsberg.Partner.Status) && org$Ginsberg.Partner.Status != "")
          tags$span(
            style = paste0(
              "display:inline-block;padding:2px 10px;border-radius:20px;",
              "font-size: 15px;font-weight:700;",
              if (org$Ginsberg.Partner.Status == "Active")
                "background:#198754;color:#fff;"
              else
                "background:#FFCB05;color:#1a1a1a;"
            ),
            org$Ginsberg.Partner.Status
          )
      ),
      
      # ── Stats: total matches + partnership length ────────────────────────
      tags$div(
        style = "display:flex; gap:22px; margin-bottom:14px;",
        tags$div(
          tags$p(
            style = "font-size: 15px;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:3px;",
            "Total Matches"
          ),
          if (!is.na(org$Total.Matches))
            tags$p(style = "font-size: 16px;font-weight:700;color:#1a1a1a;margin:0;", org$Total.Matches)
          else
            tags$p(style = "font-size: 15px;font-weight:500;color:#bbb;font-style:italic;margin:0;", "Coming soon")
        ),
        tags$div(
          tags$p(
            style = "font-size: 15px;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:3px;",
            "Partner Since"
          ),
          if (!is.na(org$Created.Date))
            tags$p(style = "font-size: 15px;font-weight:700;color:#1a1a1a;margin:0;", format_partner_since(org$Created.Date))
          else
            tags$p(style = "font-size: 15px;font-weight:500;color:#bbb;font-style:italic;margin:0;", "Coming soon")
        )
      ),
      
      # ── Details / Matches tabs ───────────────────────────────────────────
      navset_tab(
        nav_panel(
          "Details",
          tags$div(
            style = "padding-top:10px;",
            # Address
            if (!is.na(org$Billing.Address.Line.1) && org$Billing.Address.Line.1 != "")
              tags$p(
                style = "font-size: 15px;color:#666;white-space:pre-line;margin-bottom:12px;",
                addr
              ),
            # Focus area badges
            if (!is.na(org$Categories)) {
              cat_vals <- unlist(org[, cat_cols], use.names = FALSE)
              cat_vals <- cat_vals[!is.na(cat_vals) & cat_vals != ""]
              tags$div(
                style = "margin-bottom:14px;",
                tags$p(
                  style = "font-size: 15px;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:7px;",
                  "Focus Areas"
                ),
                tagList(lapply(cat_vals, function(cat) {
                  bg <- group_pal(cat_to_group[cat])
                  tags$span(
                    style = paste0(
                      "background:", bg, ";color:#fff;",
                      "padding:3px 10px;border-radius:20px;",
                      "font-size: 15px;font-weight:500;",
                      "margin:2px;display:inline-block;"
                    ),
                    cat
                  )
                }))
              )
            },
            # About: prefer Description, fall back to Mission Statement
            # when there's no description on file for this org.
            {
              about_text <- if (!is.na(org$Description) && org$Description != "")
                org$Description
              else if (!is.na(org$Mission.Statement) && org$Mission.Statement != "")
                org$Mission.Statement
              else
                NA_character_
              
              if (!is.na(about_text))
                tags$div(
                  tags$p(
                    style = "font-size: 15px;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:5px;",
                    "About"
                  ),
                  tags$p(
                    style = "font-size: 15px;color:#555;line-height:1.55;margin:0;",
                    about_text
                  )
                )
            }
          )
        ),
        nav_panel(
          paste("Matches", if (nrow(org_projects) > 0) paste0("(", nrow(org_projects), ")")),
          tags$div(style = "padding-top:10px;", proj_html)
        )
      ),
      
      # ── Close button, sits under the tabs content (not floating) ────────
      tags$div(
        style = "margin-top:16px; padding-top:14px; border-top:1px solid #eef1f5;",
        actionLink(deselect_id, "\u00D7 Close", class = "close-panel-btn")
      )
    )
  }
  
  # ── Map tab: right-hand detail drawer ────────────────────────────────────
  # Overlays the map's right edge only when an account is selected, rather
  # than permanently reserving column width the way the old left-sidebar
  # panel did - the map stays full width until something is clicked.
  output$org_side_panel <- renderUI({
    org_name <- selected_org()
    if (is.null(org_name)) return(NULL)
    build_org_detail_ui(org_name, map_orgs, "deselect_org")
  })
  
  # Toggle the drawer's open/closed CSS class whenever the selection
  # changes, rather than inserting/removing the container itself - that's
  # what lets the slide-in/out transition actually play both ways.
  observe({
    session$sendCustomMessage("toggleOrgDrawer", !is.null(selected_org()))
  })
  
  # ── No-geo-data tab: table + detail panel ────────────────────────────────
  selected_problem_org <- reactiveVal(NULL)
  
  output$problem_table <- renderDT({
    datatable(
      problem_orgs |>
        transmute(
          `Account Name`   = Account.Name,
          `Partner Status` = ifelse(is.na(Ginsberg.Partner.Status) | Ginsberg.Partner.Status == "",
                                    "\u2014", Ginsberg.Partner.Status),
          `Focus Areas`    = ifelse(is.na(Categories), "\u2014", Categories)
        ),
      selection = "single",
      rownames  = FALSE,
      options   = list(pageLength = 25, dom = "ftip"),
      class     = "display"
    )
  })
  
  observeEvent(input$problem_table_rows_selected, {
    sel <- input$problem_table_rows_selected
    if (length(sel) == 0) {
      selected_problem_org(NULL)
    } else {
      selected_problem_org(problem_orgs$Account.Name[sel])
    }
  })
  
  observeEvent(input$deselect_problem_org, {
    selected_problem_org(NULL)
    dataTableProxy("problem_table") |> selectRows(NULL)
  })
  
  output$problem_org_panel <- renderUI({
    org_name <- selected_problem_org()
    
    if (is.null(org_name)) {
      return(div(class = "instruction-box",
                 tags$strong("Click an account"), " in the table to view its details and matched projects."
      ))
    }
    
    build_org_detail_ui(org_name, problem_orgs, "deselect_problem_org")
  })
  
  # ── Infographics charts ──────────────────────────────────────────────────
  # All static/non-reactive - the underlying data doesn't depend on any
  # filter or selection, so these just render once per session.
  
  output$geo_donut <- renderPlotly({
    make_donut(
      geo_counts |> filter(Geo.Bucket != "Unknown"),
      label_col = "Geo.Bucket", val_col = "n", colors = geo_colors
    )
  })
  
  output$top3_priority_bar <- renderPlotly({
    make_horiz_bar(top3_priorities_df, "Priority", "n", colors = group_colors, unit_label = "partner")
  })
  
  output$leaderboard_bar <- renderPlotly({
    make_horiz_bar(matches_leaderboard, "Org", "Matches", colors = "#00274C", unit_label = "match")
  })
  
  output$fy_comparison_bar <- renderPlotly({
    make_horiz_bar(
      recent_by_fy, "FY", "n",
      colors = bar_colors_for(nrow(recent_by_fy)), unit_label = "match"
    )
  })
  
  output$skill_recent_bar <- renderPlotly({
    make_horiz_bar(top_skills_recent, "Skill.Areas", "n",
                   colors = bar_colors_for(nrow(top_skills_recent)), unit_label = "match")
  })
  
  output$engagement_recent_bar <- renderPlotly({
    make_horiz_bar(top_engagement_recent, "Forms.of.Engagement", "n",
                   colors = bar_colors_for(nrow(top_engagement_recent)), unit_label = "match")
  })
  
  output$priority_recent_bar <- renderPlotly({
    make_horiz_bar(top_priorities_recent, "primary_group", "n",
                   colors = group_colors, unit_label = "match")
  })
}

shinyApp(ui = ui, server = server)