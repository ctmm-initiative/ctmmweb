### Record Actions
- Every meaningful actions by user can be recorded and compiled into a html work report. All plots and most tables are also saved. This can be turned off to improve performance.

### Collect Diagnostic Info
- If enabled, warning and error messages will be captured by app instead of showing up in R console. Click `Diagnostic Info` button on side bar to show them. This is mainly intended for use in hosted mode as the R console is not available to web users.

### Disable Parallel Mode
- Parallel mode can be tricky. Turning it off can help to isolate and locate the problems. When disabled, you will also see model fitting progress messages in console, which is not visible in parallel mode because they are in cluster/forked process instead of current R process.

### Report
- If app is running locally, `Preview Report` will compile current recordings into a html report, and open it in a new browser window.
- If app is running in hosted server, report cannot be opened automatically because of security limits. It has to be downloaded with `Download Report` then you can open it.
