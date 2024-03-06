# 1. PACKAGES

if (
    !require(
        "pacman"
    )
) {
    install.packages(
        "pacman"
    )
}

pacman::p_load(
    tidyverse, sf,
    anyflights, igraph,
    tidygraph, ggraph
)

sf::sf_use_s2(FALSE)

# 2. US STATES

us_states <- map_data("states")

us_states_sf <- sf::st_as_sf(
    us_states,
    coords = c("long", "lat"),
    crs = 4326
) |>
    dplyr::group_by(
        group
    ) |>
    dplyr::summarise(
        geometry = sf::st_combine(
            geometry # no brackets
        )
    ) |>
    sf::st_cast("POLYGON") |>
    sf::st_transform(
        "ESRI:102003"
    )

plot(sf::st_geometry(us_states_sf))

# 3. AIRPORTS

us_airports <- anyflights::get_airports() |>
    dplyr::select(1, 3:4)

head(us_airports)

us_airports_sf <- us_airports |>
    sf::st_as_sf(
        coords = c("lon", "lat"),
        crs = 4326,
        remove = FALSE
    ) |>
    sf::st_transform(crs = "ESRI:102003") |>
    sf::st_intersection(us_states_sf)

plot(sf::st_geometry(us_airports_sf))

coords <- sf::st_coordinates(
    us_airports_sf
)

us_airports_df <- 
us_airports_sf |>
sf::st_drop_geometry() |>
dplyr::select(1) |>
dplyr::mutate(
    lon = coords[, 1],
    lat = coords[, 2]
)

head(us_airports_df)

# 4. FLIGHTS

station <- unique(us_airports_df$faa)

options(timeout = 600)

us_flights <- anyflights::get_flights(
    station = station,
    year = 2023,
    month = 11
)

head(us_flights)

us_flights_distinct <- us_flights |>
    dplyr::select(
        "origin",
        "dest"
    ) |>
    dplyr::distinct()

vertices <- dplyr::filter(
    us_airports_df,
    faa %in% us_flights_distinct$origin &
    faa %in% us_flights_distinct$dest
)

us_flights_distinct <- dplyr::filter(
    us_flights_distinct,
    origin %in% vertices$faa &
    dest %in% vertices$faa
)

# 5. NETWORK GRAPH

g <- igraph::graph_from_data_frame(
    d = us_flights_distinct,
    directed = TRUE,
    vertices = vertices
)

gr <- tidygraph::as_tbl_graph(
    g
)

# 6. MAP

map <- ggraph::ggraph(
    gr,
    x = lon,
    y = lat
) +
geom_sf(
    data = us_states_sf,
    fill = "grey10",
    color = "white",
    linewidth = .3
) +
ggraph::geom_edge_bundle_path(
    color = "#FA57B1",
    width = .025,
    alpha = .4
) +
coord_sf(crs = sf::st_crs(us_states_sf)) +
theme_void()

ggsave(
    filename = "us-flight-routes.png",
    width = 7.5,
    height = 7,
    background = "white",
    map
)
