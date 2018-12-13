** EE_Method_paper

* Set wd 

	cd "C:\Users\jeri\Dropbox\Economics\Bachelor_Projekt\Metode_Paper\Analysis"
	
* Load data
	
	use "Data/Dat1.dta" if Year > 1980
	

* Set as panel data 
	
	 xtset idn Year
	 

* Test for unit root in real GDP per capita and its growth rate 

	xtunitroot ips growth
	xtunitroot ips growth, trend 
	xtunitroot ips growth, lags(1)
	xtunitroot ips growth, trend lags(1)
	
	xtunitroot ips rGDPc
	xtunitroot ips rGDPc, trend 
	xtunitroot ips rGDPc, lags(1)
	xtunitroot ips rGDPc, trend lags(1)
