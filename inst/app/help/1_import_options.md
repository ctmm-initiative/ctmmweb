### Dataset in ctmm package
- You can use `buffalo` data in `ctmm` package to test the app.
- In exploring app features, You can take a sample of dataset so that the time consuming tasks can be finished quickly.

### Upload Data
- Data must be (multiple) (compressed) csv file in [Movebank format](https://www.movebank.org/node/13).
- The csv file can be compressed in `zip`, `gz`, `bzip` etc, but there can only be one file in a zip.
- You can drag and drop files into the file uploader. Multiple files upload is not supported by all browsers. Chrome is the recommended browser.

### Restore Progress
- Using `Save Progress` button in sidebar to save input data, cached calculation results and work report.
- Uploading the saved zip with `Restore Progress` will restore the input data and previous caches. The time consuming operations like model selection, home range and occurrence can be finished instanly if the calculation is exactly same, i.e. same data and same selections. Note this cache only works when saved progress was using exactly same app package version.
- The saved work report is also opened for reference if app is running in local mode.

