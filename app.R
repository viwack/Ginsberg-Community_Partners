library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)
library(htmltools)

# ── Data loading ──────────────────────────────────────────────────────────────

orgs <- read.csv("Accounts_wideCategories_Geocoded.csv", stringsAsFactors = FALSE)

# Gather category columns into a single comma-separated string per org
cat_cols <- grep("^Category\\.", names(orgs), value = TRUE)
orgs <- orgs |>
  mutate(
    Categories = apply(orgs[, cat_cols], 1, function(x) {
      vals <- x[!is.na(x) & x != ""]
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
    })
  )

# All unique focus area values (for filter menu)
all_focus_areas <- sort(unique(na.omit(unlist(orgs[, cat_cols], use.names = FALSE))))
all_focus_areas <- all_focus_areas[all_focus_areas != ""]

# Matched projects from FY25 and FY26
fy25 <- read_xlsx("Mapping CP Network.xlsx", sheet = "FY25") |>
  select(
    Org       = `Initiative Account`,
    Project   = `Initiative`,
    Offering  = `Resource Offering`,
    Completed = `Match Completed Date`,
    Category  = `Match_Category__r.Name`
  ) |>
  mutate(FY = "FY25")

fy26 <- read_xlsx("Mapping CP Network.xlsx", sheet = "FY26") |>
  select(
    Org       = `Initiative Account`,
    Project   = `Initiative`,
    Offering  = `Resource Offering`,
    Completed = `Match Completed Date`,
    Category  = `Match_Category__r.Name`
  ) |>
  mutate(FY = "FY26")

projects <- bind_rows(fy25, fy26)

# Only keep orgs with valid coordinates
map_orgs <- orgs |>
  filter(!is.na(latitude), !is.na(longitude))

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Ginsberg Center Community Partners",
  sidebar = sidebar(
    width = 380,
    open = "open",
    uiOutput("org_panel"),
    tags$hr(),
    tags$label(
      class = "form-label fw-semibold",
      style = "font-size:0.85rem;",
      "Partner Status"
    ),
    radioButtons(
      "status_filter",
      label    = NULL,
      choices  = c("All", "Active", "Lead"),
      selected = "All",
      inline   = TRUE
    ),
    tags$hr(),
    tags$div(
      style = "display:flex; justify-content:space-between; align-items:baseline;",
      tags$label(
        class = "form-label fw-semibold",
        style = "font-size:0.85rem; margin-bottom:0;",
        "Filter Map by Focus Area"
      ),
      actionLink("clear_filter", "Clear", style = "font-size:0.78rem;")
    ),
    tags$p(
      style = "font-size:0.78rem; color:#666; margin-bottom:6px;",
      "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
    ),
    selectInput(
      "focus_filter",
      label      = NULL,
      choices    = all_focus_areas,
      selected   = NULL,
      multiple   = TRUE,
      selectize  = FALSE,
      size       = 12,
      width      = "100%"
    )
  ),
  card(
    full_screen = TRUE,
    leafletOutput("map", height = "100%")
  )
)

# ── Server ───────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Helper: does an org row match any of the selected focus areas?
  org_matches <- function(org_row, selected) {
    org_cats <- unlist(org_row[, cat_cols], use.names = FALSE)
    org_cats <- org_cats[!is.na(org_cats) & org_cats != ""]
    any(selected %in% org_cats)
  }

  # ── Initial map render ────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet(map_orgs) |>
      addTiles() |>
      setView(lng = -83.5, lat = 42.4, zoom = 9) |>
      addMarkers(
        lng            = ~longitude,
        lat            = ~latitude,
        layerId        = ~Account.Name,
        label          = ~Account.Name,
        group          = "all",
        clusterOptions = markerClusterOptions()
      ) |>
      addEasyButton(easyButton(
        icon    = "fa-crosshairs",
        title   = "Reset view",
        onClick = JS("function(btn, map){ map.setView([42.4, -83.5], 9); }")
      ))
  })

  # ── Update markers when either filter changes ─────────────────────────────
  observe({
    selected <- input$focus_filter
    status   <- input$status_filter

    # Apply status filter first
    base <- if (is.null(status) || status == "All") {
      map_orgs
    } else {
      map_orgs |> filter(Ginsberg.Partner.Status == status)
    }

    leafletProxy("map") |>
      clearMarkers() |>
      clearMarkerClusters()

    if (length(selected) == 0) {
      # No focus filter — show status-filtered orgs as clustered markers
      leafletProxy("map") |>
        addMarkers(
          data           = base,
          lng            = ~longitude,
          lat            = ~latitude,
          layerId        = ~Account.Name,
          label          = ~Account.Name,
          group          = "all",
          clusterOptions = markerClusterOptions()
        )
    } else {
      # Split status-filtered orgs into focus-matched vs. unmatched
      matched   <- base[sapply(seq_len(nrow(base)), function(i) org_matches(base[i, ], selected)), ]
      unmatched <- base[sapply(seq_len(nrow(base)), function(i) !org_matches(base[i, ], selected)), ]

      # Unmatched: small gray circles, low opacity
      if (nrow(unmatched) > 0) {
        leafletProxy("map") |>
          addCircleMarkers(
            data        = unmatched,
            lng         = ~longitude,
            lat         = ~latitude,
            layerId     = ~Account.Name,
            label       = ~Account.Name,
            radius      = 5,
            color       = "#aaaaaa",
            fillColor   = "#cccccc",
            fillOpacity = 0.4,
            weight      = 1,
            opacity     = 0.5,
            group       = "unmatched"
          )
      }

      # Matched: larger blue circles, fully opaque, clustered
      if (nrow(matched) > 0) {
        leafletProxy("map") |>
          addCircleMarkers(
            data           = matched,
            lng            = ~longitude,
            lat            = ~latitude,
            layerId        = ~Account.Name,
            label          = ~Account.Name,
            radius         = 9,
            color          = "#00274C",
            fillColor      = "#00B2A9",
            fillOpacity    = 0.9,
            weight         = 2,
            opacity        = 1,
            group          = "matched"
          )
      }
    }
  })

  # ── Marker click → show org details ──────────────────────────────────────
    observeEvent(input$clear_filter, {
    updateSelectInput(session, "focus_filter", selected = character(0))
  })

  selected_org <- reactiveVal(NULL)

  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      selected_org(click$id)
    }
  })

  output$org_panel <- renderUI({
    org_name <- selected_org()

    if (is.null(org_name)) {
      return(
        div(
          style = "color: #666; padding: 12px;",
          tags$p(tags$strong("Click a marker on the map"), " to view organization details and matched projects."),
          tags$p(style = "margin-top: 8px;", "Use the filter above the map to highlight organizations by focus area.")
        )
      )
    }

    org <- map_orgs |> filter(Account.Name == org_name)
    if (nrow(org) == 0) return(NULL)

    org_projects <- projects |>
      filter(Org == org_name) |>
      arrange(FY, Project)

    # Build address string
    addr_parts <- c(
      org$Billing.Address.Line.1,
      if (!is.na(org$Billing.Address.Line.2) && org$Billing.Address.Line.2 != "NA") org$Billing.Address.Line.2,
      paste0(org$Billing.City, ", ", org$Billing.State.Province, " ", org$Billing.Zip.Postal.Code)
    )
    addr <- paste(addr_parts[addr_parts != "" & !is.na(addr_parts)], collapse = "\n")

    # Build project rows HTML
    if (nrow(org_projects) == 0) {
      proj_html <- tags$p(style = "color:#888; font-style:italic;", "No matched projects on record.")
    } else {
      rows <- lapply(seq_len(nrow(org_projects)), function(i) {
        p <- org_projects[i, ]
        tags$div(
          class = "mb-3 pb-2",
          style = "border-bottom: 1px solid #eee;",
          tags$div(
            tags$span(class = "badge text-bg-secondary me-1", p$FY),
            if (!is.na(p$Category)) tags$span(class = "badge text-bg-light border", p$Category)
          ),
          tags$p(class = "mb-0 mt-1 fw-semibold", style = "font-size:0.9rem;", p$Project),
          if (!is.na(p$Offering))  tags$p(class = "mb-0 text-muted", style = "font-size:0.8rem;",  p$Offering),
          if (!is.na(p$Completed)) tags$p(class = "mb-0 text-muted", style = "font-size:0.78rem;", paste("Completed:", p$Completed))
        )
      })
      proj_html <- tagList(rows)
    }

    tagList(
      # Header
      tags$div(
        style = "margin-bottom: 12px;",
        tags$h5(style = "margin-bottom: 4px;", org_name),
        if (!is.na(org$Ginsberg.Partner.Status) && org$Ginsberg.Partner.Status != "")
          tags$span(
            class = if (org$Ginsberg.Partner.Status == "Active") "badge text-bg-success" else "badge text-bg-warning",
            org$Ginsberg.Partner.Status
          )
      ),
      # Address
      if (!is.na(org$Billing.Address.Line.1) && org$Billing.Address.Line.1 != "")
        tags$p(style = "font-size:0.85rem; color:#555; white-space: pre-line;", addr),
      # Categories
      if (!is.na(org$Categories))
        tags$div(
          style = "margin-bottom: 10px;",
          tags$strong(style = "font-size:0.82rem;", "Focus Areas: "),
          tags$span(style = "font-size:0.82rem; color:#444;", org$Categories)
        ),
      # Description
      if (!is.na(org$Description) && org$Description != "")
        tags$div(
          tags$strong(style = "font-size:0.82rem;", "About"),
          tags$p(style = "font-size:0.82rem; margin-top:4px; color:#444;", org$Description)
        ),
      tags$hr(),
      # Projects
      tags$h6(paste("Matched Projects", if (nrow(org_projects) > 0) paste0("(", nrow(org_projects), ")") else "")),
      proj_html
    )
  })
}

shinyApp(ui = ui, server = server)
