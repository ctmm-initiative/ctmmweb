### Map usage
- Interactive maps will be built with **current subset of individuals**.
  - You can use the base map layer selector to switche among terrain, satelite, hybrid etc.
  - The data layer selector can turn on/off
      - `_graticule_` aka the grid lines
      - each individual. The color of individuals are consistent with the color in other plots.
      - the home range polygons of each selected model, if models are available. Note selected models are independent from individuals thus can have multiple models of same individual.
      - The model with smaller `AICc` value will have brighter color.
  - The measure tool can add lines or polygons and their measurements.
  - `Reset Map View` button will reset current map to a view that cover all points and some edges.
  - `Map Height` can change map height, resizing the app window can change map width.

### Heatmap
- The heatmap is a fast overview of all points of all indivduals. 
  - The color only represent the intensity of points. 
  - Because a `Point` map can be slow to render and interact, data set with `> 25000` pionts will active the heatmap by default. 
  - You can pan and zoom the heatmap, then switch to point map, which can sync to roughly same bounds if `Apply Heatmap Range to Point Map` is selected. This cannot replicate the exactly same bounds and zoom because of limit of leaflet library.

### Report and Download
- Each map was saved and included in the report zip. There may be some dependency folder in zip but the map html itself should be self contained and enough for sharing.
- `Download Map` will save current map with current bounds.
