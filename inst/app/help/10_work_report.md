### Record Actions
- Every meaningful actions by user can be recorded and compiled into a html work report in `Work Report` page. All plots and most tables are also saved. The option can be turned off to improve performance.

### Disable Parallel Mode
- Parallel mode can be tricky. Turning it off can help to verify if problems in app are related to parallel mode. You will also see model fitting progress messages in console, which is not visible in parallel mode because they are in cluster/forked process instead of current R process.

### Report
- If app is running locally, `Preview Report` will compile current recordings into a html report, and open it in a new browser window.
- If app is running in hosted server, report cannot be opened automatically because of security limits. It has to be downloaded with `Download Report` then you can open it.

### Save Progress
- `Save Progress` button in side bar will save these data as a zip:
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
  - `plot.zip`: the plots saved as `png` or `pdf` files.
- The zip can be imported later by `Load Data` button in `import` page. Thus you can restore previoius data and cache.
- If you made some changes to the original input data like removed outliers, added time subsets, you can save the changes through `Save Data`. Later you can restore the csv data through `combined_data_table.csv`, the more complete data through `data.rds`, or whole data zip through `Load Data`.
