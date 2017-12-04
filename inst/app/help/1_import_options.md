### Internal Data
- You can use `buffalo` data in `ctmm` package to test the app.
- The `Sample Buffalo` data option can be used to test app features in `Model Selection`, `Home Range` and `Occurrence`, since these tasks can be time consuming with full size data.

### Upload Data
- Data must be csv file in [Movebank format](https://www.movebank.org/node/13).
- The csv file can be compressed in `zip`, `gz`, `bzip` etc, but there can only be one file in a zip.
- You can upload file by clicking `Browse` directly without having to choose the radio button.
- Starting from shiny 1.0.4, you can drag and drop file into the file uploader.

### Load Cache
- Using `Save Cache` button in `Work Report` page, input data, cached calculation results and work report can be saved.
- Uploading the saved zip with `Load Cache` will restore the input data and previous caches. The time consuming operations like model selection, home range and occurrence can be finished instanly if the calculation is exactly same, i.e. same data and same selections.
- The saved work report is also opened for reference if app is running in local mode.

