
- Home ranges are caculated for the selected models in model selection table. All home ranges are calculated in same grid so it's easier to calculate overlaps afterwards.
- Check [ctmm::plot.telemetry](https://ctmm-initiative.github.io/ctmm/reference/plot.telemetry.html) for details:
  - `Contour`: `level.UD`. You can input a series of comma separated values like `20, 50, 95` in contours input, note they are percentage values so the real value will be `0.20, 0.50, 0.95`. 
  - `Confidence Envelopes` : `level`.
  - `Location Points` : also draw telemetry locations.
- Home range can be exported to shapefile or raster format. See help inside the export dialog.
