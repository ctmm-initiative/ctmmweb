### Internal Data
- You can use `buffalo` data in `ctmm` package to test the app.
- A sample of `buffalo` data can be used to test app features in `Model Selection`, `Home Range` and `Occurrence`, since these tasks can be time consuming with full size data.

### Upload Data
- Data must be csv file in [Movebank format](https://www.movebank.org/node/13).
- The csv file can be compressed in `zip`, `gz`, `bzip` etc, but there can only be one file in a zip.
- You can upload file by clicking `Browse` directly without having to choose the radio button.
- You can also drag and drop file into the file uploader.
- Multiple files can be uploaded at the same time. This is not supported by some old browsers, like IE and RStudio's builtin browser for shiny app. Chrome is the recommended browser.

### Restore Progress
- Using `Save Progress` button in sidebar to save input data, cached calculation results and work report.
- Uploading the saved zip with `Restore Progress` will restore the input data and previous caches. The time consuming operations like model selection, home range and occurrence can be finished instanly if the calculation is exactly same, i.e. same data and same selections.
- The saved work report is also opened for reference if app is running in local mode.

