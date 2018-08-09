### Overlap of Home Ranges
- Home range [Overlap](https://ctmm-initiative.github.io/ctmm/reference/overlap.html) are calculated based on selected models in `Model Selection` page.
- When multiple models for same individual are selected and compared, `animal name - model type` will be used as identifier. Otherwise just `animal name` is used.
- By default all meaningful combinations (v1 v2 are always sorted in alphabeta order, since the order doesn't matter) are calculated and sorted by the highest value.

### Value Range Plot
- The value range of overlaps in current page are plotted. The page length (how many rows in one page) can be adjusted in page length menu.
- You can sort the table by name, value, or filter the table with search keywords. The `Value Range` plot will update accordingly and always in same order.
- Clicking on rows will highlight the corresponding part in `Value Range` plot.

### Hom Range Plot
- By default home range of all non-zero overlap pairs in current page are plotted. Too many pairs may cause problem in plot, it's better to choose a smaller page length for overlap table.
- If some rows in overlap table are selected, cooresponding pairs will be plotted.
- If the default various colors on too many individuals become difficult to recognize in some pair, choose `Two Colors Only` to use fixed colors for every plot. This make the plot easy to read but the color for animal will not be consistent.
- Check [ctmm::plot.telemetry](https://ctmm-initiative.github.io/ctmm/reference/plot.telemetry.html) for details:
  - `Contour`: `level.UD`. You can input a series of comma separated values like `20, 50, 95` in contours input, note they are percentage values so the real value will be `0.20, 0.50, 0.95`. 
  - `Confidence Envelopes` : `level`.
  - `Location Points` : also draw telemetry locations.

### Reference
- [K. Winner, M. J. Noonan, C. H. Fleming, K. Olson, T. Mueller, D. Sheldon, J. M. Calabrese. Statistical inference for home range overlap. Methods in Ecology and Evolution, DOI:10.1111/2041-210X.13027 (2018).](https://doi.org/10.1111/2041-210X.13027)
