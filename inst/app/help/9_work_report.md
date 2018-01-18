### Save Cache
- `Save Cache` will save input data as `saved.rds`, cached calculation results as `cache.zip` and work report into a zip. The zip can be uploaded by `Load Cache` button in `import` page.
- `saved.rds` is a list with item name `data`, which in turn include
  - `input_tele_list`: the original input telemetry object list
  - `tele_list`: the current telemetry object list, for example some outliers can be removed, new time subset may be added.
  - `merged`: list of 
    - `data`: the combined `data.table` of all animals 
    - `info`: the summary table
  - `all_removed_outliers`: all rows that were removed as outliers.

### Report
- If app is running locally, `Preview Report` will compile current recordings into a html report, and open it in a new browser window.
- If app is running in hosted server, report cannot be opened automatically because of security settings. It has to be downloaded with `Download Report` then you can open it.
- `Download Report as zip` will download report and other data files as a zip.
  - When `Save Telemetry Data` is selected, the current combined telemetry data.table will be exported as `combined_data_table.csv` and included in the zip. 
- The recording for report can be turned off in `Local Data Import` box of `Import Data` page.

### Known limitations
- Note the shinyapps.io server probably have a different timezone from your local time, so the timestamp inside report and the file names could have different time.
- RStudio Mac have a known bug that caused file download function executed twice, so there may be 2 entries of report generation/download in report.
