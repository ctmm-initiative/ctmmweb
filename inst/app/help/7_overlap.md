### Overlap of Home Ranges
- Home range Overlap are calculated based on selected models in `Model Selection` page.
- When multiple models for same individual are selected and compared, `animal name - model type` will be used as identifier. Otherwise just `animal name` is used.
- By default all meaningful combinations (v1 v2 are always sorted in alphabeta order, since the order doesn't matter) are calculated and sorted by the highest value.
- You can sort the table by name, value, or filter the table with search keywords. The `Value Range` plot will update accordingly and always in same order.
- Clicking on rows will highlight the corresponding part in `Value Range` plot.

### Hom Range Plot
- By default home range of all non-zero overlap pairs are plotted. Sometimes there is no enough space to draw all pairs, try increasing canvas height or manual select some pairs.
- If some rows in overlap table are selected, cooresponding pairs will be plotted.
- Check [ctmm::plot.telemetry](https://ctmm-initiative.github.io/ctmm/reference/plot.telemetry.html) for details:
  - `Contour`: `level.UD`. You can input a series of comma separated values like `20, 50, 95` in contours input, note they are percentage values so the real value will be `0.20, 0.50, 0.95`. 
  - `Confidence Envelopes` : `level`.
  - `Location Points` : also draw telemetry locations.
