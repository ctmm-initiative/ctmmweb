### Session
- `Save Session` will save input data, cached calculation results into a zip. The zip can be uploaded by `Load Session` button in `import` page.

### Report
- If app is running locally, `Preview Report` will compile current recordings into a html report, and open it in a new browser window.
- If app is running in hosted server, report cannot be opened automatically because of security settings. It has to be downloaded with `Download Report` then you can open it.
- `Download Report zip` will download report and other data files as a zip.
- The recording for report can be turned off in `Local Data Import` box of `Import Data` page.

### Known limitations
- Note the shinyapps.io server probably have a different timezone from your local time, so the timestamp inside report and the file names could have different time.
- RStudio Mac have a known bug that caused file download function executed twice, so there may be 2 entries of report generation/download in report.
