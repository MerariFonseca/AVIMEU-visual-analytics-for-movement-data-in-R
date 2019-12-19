library(leaflet)
library(htmlwidgets)
library(htmltools)
#library(mapview)


# the Leaflet.hotline plugin has to be locally downloaded

path <- "C:/Users/usuario/Desktop/Aplicativo511Veolia/AplicativoPres" # wherever the plugin was saved, path relative to current dir

hotPlugin <- htmlDependency("Leaflet.hotline", version = "0.4.0",
                            src = normalizePath(path),
                            script = c("leaflet.hotline.js", "coords.js")
)

# A function that takes the plugin htmlDependency and adds it to the map. 
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

m <- leaflet() %>% 
  addTiles() %>%
  # Register plugin on this map instance
  registerPlugin(hotPlugin)

m %>%
  # debe realizarse un arreglo con los 
  onRender("function(el, x, data) {
  
   var latlngs = [
    [[45.51, -122.68, 0],
     [37.77, -122.43, 0.5],
     [34.04, -118.2, 0.2]],
    [[40.78, -73.91, 1],
     [41.83, -87.62, 1],
     [32.76, -96.72, 0.5]]
];
  
  //data = HTMLWidgets.dataframeToD3(data);
  //data = data.map(function(val) { return [val.lat, val.lon, val.vel]; });
  //require('leaflet-hotline')(L);
  
  var hotlineLayer = new L.Hotline(latlngs, {
                                          palette: {0: 'green', 100: 'red'},
                                          weight: 2,
                                          outlineWidth: 0,
                                          max: 100,
                                          min: 0}).addTo(this);
  var bounds = hotlineLayer.getBounds();
	this.fitBounds(bounds);
  
  }")
