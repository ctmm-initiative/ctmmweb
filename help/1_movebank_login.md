### Save login info in local R environment
If you are running the app locally (instead of using browser to access a website), you can save your Movebank login information in your local R environment, which reside in your local personal directory. 

1. Edit your environment file by running `file.edit('~/.Renviron')`
2. add your login information as follows

	```r
	movebank_user = 'put your user name here'
	movebank_pass = 'put your password here'
	```
3. Update the environment by running `readRenviron("~/.Renviron")`
4. Restart the app to read the login information.

### Security concerns
- This feature doesn't work when you are access hosted app from browser, since the local R environment will be in the server.
- Since the app is open source, it can be verified by everybody. You can run the app from the source directly which also ensure the app is the verified version.
