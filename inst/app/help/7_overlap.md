### Overlap of Home Ranges
- Home range Overlap are calculated based on selected models in `Model Selection` page.
- When multiple models for same individual are selected and compared, `animal name - model type` will be used as identifier. Otherwise just `animal name` is used.
- By default all meaningful combinations (v1 v2 are always sorted in alphabeta order, since the order doesn't matter) are calculated and sorted by the highest value.
- You can sort the table by name, value, or filter the table with search keywords. The `Value Range` plot will update accordingly and always in same order.
- Clicking on rows will highlight the corresponding part in `Value Range` plot.

### Hom Range Plot
- By default home range of all non-zero overlap pairs are plotted.
- If some rows in overlap table are selected, cooresponding pairs will be plotted.
- When switched back to model page and changed model selection, switching back to overlap page sometimes cause the home range plot to draw twice in a row. The first plot is caused by temporary data mismatch between new home range data and not yet updated overlap table. There is some limitation in Shiny that preventing to fix this problem completely. The second plot will be the intended plot.
