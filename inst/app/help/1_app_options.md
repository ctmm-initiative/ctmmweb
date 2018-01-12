### Record Actions
- Every meaningful actions by user can be recorded and compiled into a html work report in `Work Report` page. All plots and most tables are also saved. The option can be turned off to improve performance.

### Capture Error Messages
- When app is running locally, error messages are printed in R console. They are not visible when app is running in a hosting server. Turn on this option can capture error messages in app and help reporting problems. 
- Once turned on, click `Error Messages` button will show the error messages captured so far and the R session information.

### Disable Parallel Mode
- Parallel mode can be tricky. Turning it off can help to verify if problems in app are related to parallel mode. You will also see model fitting progress messages in console, which is not visible in parallel mode because they are in cluster/forked process instead of current R process.
