- Variograms of individuals in current subset are plotted here.
- `Guesstimate` will add a layer of model fit with parameters determined automatically. In this state, you can further fine-tune the parameters by selecting individual in the drop down list.
    - The drop down list support search filter and direct editing.
    - There will be a pop up window for fine-tune parameters.
    - `Center current values` will update sliders 2, 3, 4 range to twice of current value. This can extend range with slider in right end, or increase resolution with slider in left end.
    - `Apply` butto will accept adjusted parameters and update the variogram.
- `Modeled` mode is enabled after models are tried and selected.
- Clicking `Try Models` will start to test possible models for every individual in current subset. It could be time taking so it need to be manually activated.
    + Depend on platform (Windows/Mac/Linux), different parallel processes are used. There will be a time report in console when running app in local mode. 
    + If the app was terminated by force, there could be forked R sessions remained active. It's better to clean up all R sessions in your system's task manager after force quitting the app.
    + `data.table` is known to have conflict with `mclapply` in Mac R 3.4 with openMP. The error is somewhat random, restart R session can often make it dissappear.
