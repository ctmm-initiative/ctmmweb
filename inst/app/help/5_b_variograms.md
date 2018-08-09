- [Variograms](https://ctmm-initiative.github.io/ctmm/articles/variogram.html) of individuals in current subset are plotted here.
- Empirical variograms are added with a layer of automatic [`Guesstimate`](https://ctmm-initiative.github.io/ctmm/reference/variogram.fit.html) parameters. 
- You can further fine-tune the parameters by selecting individual in the drop down list.
    - The drop down list support search filter and direct editing.
    - There will be a pop up window for fine-tune parameters.
    - `Center current values` will update sliders range to twice of current value, so it's easier to deal with very small or large values.
    - `Apply` butto will accept adjusted parameters and update the variogram.
- Moving to `Modeled` tab will try various models with current guesstimate parameters as initial condition.

### Reference
- [C. H. Fleming, J. M. Calabrese, T. Mueller, K.A. Olson, P. Leimgruber, W. F. Fagan. From fine-scale foraging to home ranges: A semi-variance approach to identifying movement modes across spatiotemporal scales. The American Naturalist, 183:5, E154-E167 (2014).](https://doi.org/10.1086/675504)

- [J. M. Calabrese., C. H. Fleming, E. Gurarie. ctmm: an r package for analyzing animal relocation data as a continuous‐time stochastic process. Methods in Ecology and Evolution 7:9, 1124-1132 (2016).](https://doi.org/10.1111/2041-210X.12559)
