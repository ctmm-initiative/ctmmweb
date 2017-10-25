### Internal Data
- You can use `buffalo` data in `ctmm` package to test the app.
- The `Sample Buffalo` data option can be used to test app features in `Model Selection`, `Home Range` and `Occurrence`, since these tasks can be time consuming with full size data.

### Upload Data
- Data must be csv file in [Movebank format](https://www.movebank.org/node/13).
- The csv file can be compressed in `zip`, `gz`, `bzip` etc, but there can only be one file in a zip.
- You can upload file by clicking `Browse` directly without having to choose the radio button.
- Starting from shiny 1.0.4, you can drag and drop file into the file uploader.

### Record Actions
- Every meaningful actions by user can be recorded and compiled into a html work report in `Work Report` page. All plots and most tables are also saved. The option can be turned off to improve performance.

### Load Session
- Using `Save Session` button in `Work Report` page, input data and cached calculation results can be saved.
- Uploading the session data zip with `Load Session` will restore them. Exactly same calculation can be executed instantly because of cache. The cache also helps in same session if some calculations have bee run before.
- The work report of that session is also opened for reference if app is running in local mode.

