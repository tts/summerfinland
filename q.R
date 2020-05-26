library(SPARQL)
library(tidyverse)
library(sf)
library(pxweb)
library(htmltools)
library(leaflet)

#---------------------------
# Water vs land in Finland
#---------------------------

endpoint <- "http://193.167.189.160/igalod/fuseki/ds/query"

q <- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX db: <http://dbpedia.org/ontology/>
PREFIX paik: <http://paikkatiedot.fi/def/au/ont#>
PREFIX inspire: <http://inspire.ec.europa.eu/ont/au#>

SELECT ?municipalityString ?land ?totalArea ?shareOfWater ?shareOfFreshwater ?shareOfSeawater ?code ?geomString
WHERE {
  GRAPH <http://paikkatiedot.fi/ds/igalod/kunnat2019>
  {
    ?s rdfs:label ?municipality ;
       db:landArea ?land ;
       paik:freshWaterArea ?freshwater ;
       paik:seaWaterArea ?seawater ;
       db:waterArea ?water ;
       inspire:AdministrativeUnit.nationalCode ?code ;
       inspire:AdministrativeUnit.geometry ?geom .
       
  FILTER (lang(?municipality) = 'fi') 
  BIND (str(?municipality) AS ?municipalityString)
  BIND (str(?geom) AS ?geomString)
  BIND (?water + ?land AS ?totalArea)
  BIND (?water / ?totalArea  AS ?shareOfWater)
  BIND (?seawater / ?totalArea AS ?shareOfSeawater)
  BIND (?freshwater / ?totalArea AS ?shareOfFreshwater)
  }
}"

result <- SPARQL(url = endpoint, query = q)$results

result_geom <- result %>% 
  mutate(municipality = iconv(municipalityString, from = "UTF-8", to = "ISO-8859-1"),
         municipality = ifelse(municipality == 'Maarianhamina', 'Maarianhamina - Mariehamn', municipality),
         shareOfWater = round(shareOfWater, 3) * 100,
         shareOfFreshwater = round(shareOfFreshwater, 3) * 100,
         shareOfSeawater = round(shareOfSeawater, 3) * 100) %>% 
  select(municipality, code, land, area = totalArea, starts_with("share"), geomString)

water <- st_as_sf(result_geom, wkt = "geomString", crs = 4326)

#---------------------
# Summer cottages
#---------------------

# See 
# https://ropengov.github.io/geofi/articles/geofi_tutorial.html#joining-municipality-level-data-from-statistics-finland
pxweb_query_list <-
  list("Alue"=c("*"),
       "Tiedot"=c("*"),
       "Vuosi"=c("2018"))

px_data <-
  pxweb_get(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/rakke/statfin_rakke_pxt_116j.px",
            query = pxweb_query_list)

cottages <- as.data.frame(px_data$data, stringsAsFactors = FALSE)

#---------
# Join
#---------

water_and_cottages <- left_join(water, cottages, by = c("municipality"="Alue"))

water_and_cottages <- water_and_cottages %>% 
  rename(cottages_total = `Kesämökkejä (lkm)`) %>% 
  mutate(cottagesByLandArea = round(cottages_total / land), 1) %>% 
  select(-Vuosi)

#----------
# Labels
#-----------
data_for_labs <- st_drop_geometry(water_and_cottages)

labs_water <- lapply(seq(nrow(data_for_labs)), function(i) {
  paste0( '<b>', data_for_labs[i, "municipality"], '</b><br/>', 
          'Share of water ', data_for_labs[i, "shareOfWater"], ' %<br/>', 
          'Share of fresh water ', data_for_labs[i, "shareOfFreshwater"], ' %<br/>',
          'Share of sea water ', data_for_labs[i, "shareOfSeawater"], ' %<br/>')
})

labs_cottages <- lapply(seq(nrow(data_for_labs)), function(i) {
  paste0( '<b>', data_for_labs[i, "municipality"], '</b><br/>', 
          'Total area km2: ', data_for_labs[i, "area"], '<br/>',
          'Land area km2: ', data_for_labs[i, "land"], '<br/>',
          'Water % ', data_for_labs[i, "shareOfWater"], '<br/>',
          'Nr of cottages ', data_for_labs[i, "cottages_total"], '<br/>',
          'Nr of cottages by land km2: ', data_for_labs[i, "cottagesByLandArea"])
})


#--------------
# Leaflet map
#---------------

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 40%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: #ebf5fb;
    font-weight: bold;
    font-family: verdana;
    font-size: 20px;
  }
"))

wword <- "<font style='color:#3498db'>water</font>"
cword <- "<font style='color:green'>cottages</font>"

title <- tags$div(
  tag.map.title, HTML(paste0("For many a Finn (but not all) summer is mostly about ", wword, " and ", cword))
)  

wpal <- colorBin(palette = "Blues", 
                   domain = water_and_cottages$shareOfWater, 
                   bins = 9, pretty = FALSE)

cpal <- colorBin(palette = "Greens", 
                   domain = water_and_cottages$cottagesByLandArea, 
                   bins = 5, pretty = FALSE)

m <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%  
  addTiles(urlTemplate = "", 
           attribution = '|Data: Statistics Finland, National Land Survey of Finland|@ttso') %>% 
  addControl(title, position = "topleft", className = "map-title") %>% 
  addPolygons(
    data = water_and_cottages,
    group = "Water",
    color = ~wpal(shareOfWater),
    weight = 1,
    fillOpacity = 0.8,
    label = lapply(labs_water, HTML),
    labelOptions = labelOptions(noHide = F, 
                                direction = "right",
                                offset=c(20,-15),
                                style = list(
                                  "color" = "coral4",
                                  "font-family" = "verdana",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)")),
    highlight = highlightOptions(
        weight = 1,
        fillOpacity = 0,
        color = "black",
        opacity = 1.0,
        bringToFront = TRUE,
        sendToBack = TRUE)) %>% 
  addPolygons(
    data = water_and_cottages,
    group = "Cottages",
    color = ~cpal(cottagesByLandArea),
    weight = 1,
    fillOpacity = 0.8,
    label = lapply(labs_cottages, HTML),
    labelOptions = labelOptions(noHide = F, 
                                direction = "right",
                                offset=c(20,-15),
                                style = list(
                                  "color" = "coral4",
                                  "font-family" = "verdana",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)")),
    highlight = highlightOptions(
      weight = 1,
      fillOpacity = 0,
      color = "black",
      opacity = 1.0,
      bringToFront = TRUE,
      sendToBack = TRUE)) %>% 
  addLegend(
    data = water_and_cottages,
    group = "Water",
    position = "bottomright", 
    pal = wpal, 
    values = ~wpal(shareOfWater),
    title = "Water % from total area",
    opacity = 1) %>% 
  addLegend(
    data = water_and_cottages,
    group = "Cottages",
    position = "bottomright", 
    pal = cpal, 
    values = ~cpal(cottagesByLandArea),
    title = "Cottages by land km2",
    opacity = 1) %>% 
  addLayersControl(
    baseGroups =  c("Water", "Cottages"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

mapview::mapshot(m, "summerfi.html")

