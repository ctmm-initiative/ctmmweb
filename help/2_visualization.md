### Current subset of individuals
All plots and actions are based on **current subset of individuals**.

- You can select rows in table `1. Individuals` by 
  - mouse clicking
  - `Select All` and `Clear Selection` buttons
- If no rows are selected, the individuals in current page are current subset.
- `Time subsetting` feature only work with single individual.

### Individuals table
- The color of rows match the colors in other plots. You can *customize rows per page*, *sort the rows by column*, or *use the search box to filter by name*.

### Plots
Select more individuals can show them all at once, select less individuals can show more details.
- `2. Overview` plot showed individual relative locations. 
  - By default it shows all individual in current page. If you choose some rows in table `1. Individuals`, selected individuals will be higlighted with same color in table. This is the **current subset** concept used everywhere.
  - All individuals in dataset is drawn as a gray background for context, which can be turnd off to show more focused details.
- `3. Facet` draw each individual separately which avoided the overlap. It's easy to compare patterns because X axes are aligned, and all scales are same.
- `4. Individual` render individuals separately, centered by each, and still in same scale. The zoom slider will zoom into the center portion of all plots at the same time. This can exclude the outlier points in view quickly.

### Zoom feature
For all independent scatter plot (except facet or other grouped plot) in app (like plot 2 in this page), you can draw a rectangle with mouse button pressed, then double click inside the rectangle to **zoom in**. Double click in plot reset the zoom in.
