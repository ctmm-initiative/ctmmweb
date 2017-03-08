# Movebank data download

1. In all Movebank studies, only a part of them have data visible to your account. Only these studies are listed.
2. Some studies don't have a valid number in `deployments`, `events` or `individuals`, but still have data available. 
3. After clicking `Download`: 
	- Success download will have a notification and have the data previewed.
	- Sometimes the response is a page saying once you agree the license term the data can be downloaded. You will need to go to www.movebank.org and search by Movebank ID, try downloading there. 
		+ There could be a pop up window asking to agree the license terms. After one time agreement you can download from Movebank or from the app later.
		+ It's also possible there is actually no data available for download. The Movebank API response was not accurate in this case.
4. The data downloaded with Movebank API are not as complete as csv downloaded from Movebank website. Only core columns are included by default. 

	You should use Movebank website download if you need more attributes or more filter options. It'll be too complex to include all the options possible in the app.
