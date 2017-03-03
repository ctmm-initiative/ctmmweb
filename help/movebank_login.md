### Save login info in local R environment
- For you convenience, you can save your login information in your local R environment, which reside in your local personal directory. Every time when the app launches, it will try to read Movebank login info from it.
- This only applies if you have R installed, and you are running the app locally. For app running from cloud, i.e. a web server, this method will not work since the R environment will be in the server which is shared by all users.

1. Edit your environment file by running `file.edit('~/.Renviron')`
2. add your login information as follows

	```r
	movebank_user = 'put your user name here'
	movebank_pass = 'put your password here'
	```
3. Update the environment by running `readRenviron("~/.Renviron")`
4. Restart the app to read the login information.

### Security concerns
- The login info is never saved after the app is closed.
- All the code of the app are open source and ready to be verified. You can run the app from the source directly which ensured the app is the verified version.