
### Upload Data
- Data must be (multiple) (compressed) csv file in [Movebank format](https://www.movebank.org/node/13).
- The csv file can be compressed in `zip`, `gz`, `bzip` etc, but there can only be one file in a zip.
    Manually marked outliers in csv will be removed in importing. 
- You can drag and drop files into the file uploader. Multiple files upload is not supported by all browsers. Chrome is the recommended browser.

### Save Progress
- You can use `Save Progress` button in sidebar anytime to save input data, cached calculation results and work report as a zip:
  - `input_telemery.rds`: the telemetry objects of input data
  - `combined_data_table.csv`: the current data subset in csv format
  - `data.rds`: a list that can be imported into R later with `readRDS`
    - `tele_list`: the telemetry object list with modifications made in app, for example some outliers can be removed, new time subset may be added.
    - `merged`: list of
      - `data`: the combined `data.table` of all animals
      - `info`: the summary table
    - `all_removed_outliers`: all rows that were removed as outliers.
  - `cache.zip`: cached calculation results. Exactly the same calculations can be finished instantly.
  - `model_list_dt.rds`: a `data.table` of all fitted models with related information, and the ctmm model object can be accessed from `model_list_dt$model`, which is a list column.
  - `report.html`: work report.
  - `plot.zip`: all the plots saved as `png` or `pdf` files, or maps saved as html.
  - `error_log.txt` error messages if they are captured in the app instead of R console.
- `Plot DPI` control the dpi of saved plot pictures. You can resize the app to change the plot size.
- The zip can be imported later by `Restore Progess` button in `import` page. Thus you can restore previoius data and cache. Note the app data format may evolve and old saved data may not work with newer version app. You can always use the exactly same version app if needed.
- If you made some changes to the original input data like removed outliers, added time subsets, you can save the changes through `Save Progress`. Later you can restore the csv data through `combined_data_table.csv`, the more complete data through `data.rds`, or whole data zip through `Restore Progess`.

### Restore Progress
- Uploading the saved zip from `Restore Progress` will restore the input data and previous caches.
  - Note this will only restore input data, not the latter stages of workflow. Because the workflow depend on lots of user interactions (table row selection, plot selection) and often need to wait app update to finish before going to next step, it's not possible to reproduce every user interaction programmingly to reach the exact same stage when saving progress.
  - However, the consuming operations like model selection, home range and occurrence can be finished instanly because of cache, if the calculation is exactly same, i.e. same data and same selections. Note cache will only work with exactly same app package version.
- The saved work report is also opened for reference if app is running in local mode.

