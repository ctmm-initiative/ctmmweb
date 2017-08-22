Variograms of individuals in current subset are plotted here.

- The figure height and columns of figures in `control` box are applied to plots in `variogram`, `home range`, `occurrence` page too.
- The slider control the Time-lag range in plots. It's in Logarithmic scale, ranged from `0.001 (0.1%)` to `1 (100%)` of the total range. Some browser in certain platform may have compatibility issues and showing the labels wrong. Firefox and Chrome are recommended browsers.
- The units of X (Time-lag) and Y (Semi-variance) may change according to data to better represent the values.
- `Absolute` mode operate on the max Time-lag range individual in group, and all others scaled with same X, Y axes for easier comparison.
- `Relative` mode zoom every plot by fraction of their own Time-lag range. The X, Y axes are not synced.
- `Guesstimate` will add a layer of model fit with parameters determined automatically. In this state, you can further fine-tune the parameters by selecting individual in the drop down list.
    - The drop down list support search filter and direct editing.
    - There will be a pop up window for fine-tune parameters.
    - `Center current values` will update sliders 2, 3, 4 range to twice of current value. This can extend range with slider in right end, or increase resolution with slider in left end.
    - `Apply` butto will accept adjusted parameters and update the variogram.
- `Modeled` mode is enabled after models are fitted and selected.
- Clicking `Fit Models` will start to fit possible models for every individual in current subset. It could be time taking so it need to be manually activated.
    + Depend on platform (Windows/Mac/Linux), different parallel processes are used. There will be a time report in console when running app in local mode. 
    + If the app was terminated by force, there could be forked R sessions remained active. It's better to clean up all R sessions in your system's task manager after force quitting the app.
    + `data.table` is known to have conflict with `mclapply` in Mac R 3.4 with openMP. The error is somewhat random, restart R session can often make it dissappear.
