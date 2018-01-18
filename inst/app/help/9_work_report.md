### Save Data
- `Save Data` will save these data as a zip:
  - `combined_data_table.csv`: the current combined telemetry `data.table`. It can be imported into app later as csv.
  - `data.rds`: a list that can be imported into R later with `readRDS`
    - `input_tele_list`: the original input telemetry object list
    - `tele_list`: the current telemetry object list, for example some outliers can be removed, new time subset may be added.
    - `merged`: list of 
      - `data`: the combined `data.table` of all animals 
      - `info`: the summary table
    - `all_removed_outliers`: all rows that were removed as outliers.
  - `cache.zip`: cached calculation results. Exactly same calculation can be finished instantly
  - `report.html`: work report.
- The zip can be imported later by `Load Data` button in `import` page. Thus you can restore previoius data and cache.
- If you made some changes to the original input data like removed outliers, added time subsets, you can save the changes through `Save Data`. Later you can restore the csv data through `combined_data_table.csv`, the more complete data through `data.rds`, or whole data zip through `Load Data`.

### Report
- If app is running locally, `Preview Report` will compile current recordings into a html report, and open it in a new browser window.
- If app is running in hosted server, report cannot be opened automatically because of security settings. It has to be downloaded with `Download Report` then you can open it.
- `Download Report as zip` will download report and other data files as a zip.
- The recording for report can be turned off in `Local Data Import` box of `Import Data` page.

### Known limitations
- Note the shinyapps.io server probably have a different timezone from your local time, so the timestamp inside report and the file names could have different time.
- RStudio Mac have a known bug that caused file download function executed twice, so there may be 2 entries of report generation/download in report.
