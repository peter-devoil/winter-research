# Data sources

Data is located on qaafi-hss8hy2.instrument.net.uq.edu.au, and pushed up to nectar every night.

Data comes from two sources, hindcast data at NCI, operational forecasts from Bom's data servers.

Hindcast data is used to develop a climatology that the forecasts compare to. Data from NCI is downloaded by scp and cached locally (qaafi-hss8hy2.instrument.net.uq.edu.au). Access to NCI is requires permission from the ub7 [project administrator] (https://my.nci.org.au/mancini/project/ub7). After the data is downloaded, it's run through apsim and collated into an .RData file.

Data from the BoM's live forecast system is downloaded from their opendap server as raw netCDF files. Forecasts for each study location are extracted from the netcdf data as apsim formatted met files. Apsim is run for each site for each ensemble member to generate a forecast soil temperature timeseries. These are aggregated into .RDS  format and again uploaded to the nectar VM.