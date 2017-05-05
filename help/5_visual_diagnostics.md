Variograms of individuals in current subset are plotted here.

- The figure height and columns of figures can be adjusted.
- The slider control the Time-lag range in plots. It's in Logarithmic scale, ranged from `0.001 (0.1%)` to `1 (100%)` of the total range.
- The units of X (Time-lag) and Y (Semi-variance) may change according to data to better represent the values.
- `Absolute` mode operate on the max Time-lag range individual in group, and all others scaled with same X, Y axes for easier comparison.
- `Relative` mode zoom every plot by fraction of their own Time-lag range. The X, Y axes are not synced.
- `Guesstimate model` will add a layer of model fit with parameters determined automatically.
- You can further fine-tune the parameters by selecting individual in the drop down list.
    - The drop down list support search filter and direct editing.
    - There will be a pop up window for fine-tune parameters.
    - `Center current values` will update sliders 2, 3, 4 range to twice of current value. This can extend range with slider in right end, or increase resolution with slider in left end.
    - `Apply` butto will accept adjusted parameters and update the variogram.
