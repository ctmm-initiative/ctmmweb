
## Device error
- Learn more about device error in [Telemetry error](https://ctmm-initiative.github.io/ctmm/articles/error.html)
- Also see [`uere`](https://ctmm-initiative.github.io/ctmm/reference/uere.html), [`as.telemetry`](https://ctmm-initiative.github.io/ctmm/reference/as.telemetry.html)

## Calibration
- You can load calibration data as regular telemetry data and use app features to edit it, like time subsetting, outlier removal, then save the edited data set through `Export Current` in box 1 of *Visualization* page.
- You can also load regular data first, then load calibration data for the device through `Load Calibration Data` button in box 5 `Error`. App will calculate `UERE` from the calibration data. `Apply to Current` will apply the calibration data to current regular data, i.e. *Calibrate* them.
- You can also input `UERE` manually and apply to current if it's provided by manufacturer.
