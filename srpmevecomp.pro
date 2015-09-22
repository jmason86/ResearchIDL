; Program to compare EVE data to SRPM data
; At the time this code is written, have EVE data from mission start
; to 5/15/11. 
; Comparisons done for 
; 1.) select day, all wavelengths and 
; 2.) select wavelengths across time. 
; 
; James Paul Mason
; 5/24/11

PRO SRPMEVEcomp 

EVEdataloc='/Users/jmason86/Dropbox/Research/Data/SDO/EVE/'
SRPMdataloc='/Users/jmason86/Dropbox/Research/Data/SRPM/'

;read in data
EVE1A = readfits(EVEdataloc+'EVESpectraStartTo051511.fits',EVEhdr)
;EVE wavelength ranges from 6.41 to 36.97 or 64 nm
SRPM = readfits(SRPMdataloc+'FEUV6Cont.h5copt0.conv1a.fits',SRPMhdr)
;SRPM wavelength ranges from 1 to 199


;choose a specific date: May 8, 2011
EVEd1 = EVE1A(*,closest(2.45569E6,EVE1A(0,*)))
SRPMd1 = SRPM(*,closest(2.45569E6,SRPM(0,*)))

;bin EVE data down to 1nm res
;bineve,EVEd1,EVE1A(*,0),EVEbinflux,EVEbinwave ;Don't need this now that SRPM at 1Å

;produce plots for May 8, 2011
;p1 = plot(SRPMd1(*,0),/ylog,'r2',$
;          xrange=[6,65],yrange=[1E-6,1],$
;          title='SRPM vs EVE Irradiance for May 8, 2011',$
;          xtitle='Wavelength [nm]',$
;          ytitle='Irradiance',$
;          NAME='SRPM')
;p2 = plot(EVEbinflux,/ylog,'b2',/overplot,$
;          NAME='EVE')
;leg1 = legend(TARGET=[p1,p2], position=[0.7,0.8])



;choose a specific wavelength: 30.4 nm
;--------------------------------------------------------------------------------------------;
;comment out. this method is for comparing at 1Å
;EVEwave = EVE1A(*,0) & SRPMwave = SRPM(*,0)
;EVEw1 = EVE1A(closest(30.4,EVEwave),3:n_elements(EVE1A(0,*))-1)
;EVEw1 = EVEw1(1,*)
;;EVEw1 = mean(EVEw1,dimension=1) ;choose just one of those found close to 30.4 (30.410) b/c higher irrad
;EVENAN = where(EVEw1 LT 0) & EVEw1(EVENAN) = !VALUES.F_NAN
;SRPMw1 = SRPM(closest(30.4,SRPMwave),2:n_elements(SRPM(0,*))-1)

;New method to compare at 1nm: average
EVEwave = EVE1A(*,0) & SRPMwave = SRPM(*,0)
EVEw1 = EVE1A(where(EVEwave GE 17 AND EVEwave LE 18),2:n_elements(EVE1A(0,*))-1)
EVEw1 = mean(EVEw1, dimension=1) ; average points found between
EVENAN = where(EVEw1 LT 1E-5) & EVEw1(EVENAN) = !VALUES.F_NAN ;remove erroneous data
SRPMw1 = SRPM(where(SRPMwave GE 17 AND SRPMwave LE 18),2:n_elements(SRPM(0,*))-1)
SRPMw1 = mean(SRPMw1,dimension=1) ;average points found between
SRPMNAN = where(SRPMw1 GT 1.5E-4) & SRPMw1(SRPMNAN) = !VALUES.F_NAN
;NOTE might have to remove bad data in EVE before doing average since bad data is -1

;modify julian day
SRPMdays = SRPM(0,2:n_elements(SRPM(0,*))-1) - SRPM(0,2)
EVEdays = EVE1A(0,*) - SRPM(0,2)
;Match range of EVE time to range of SRPM time
EVEdaysRange_dex = where(EVEdays GT 0 AND EVEdays LE max(SRPMdays))
EVEdays = EVEdays(EVEdaysRange_dex)
EVEw1 = EVEw1(EVEdaysRange_dex)
STOP

;produce plots for 304Å, all time
p5 = plot(SRPMdays,SRPMw1*1.8,'r2',$
;          yrange=[3E-4,5E-4],$
          title='SRPM vs EVE Irradiance for 35-36nm Average',$
          xtitle='Time (days)',$
          ytitle='Irradiance',$
          NAME='SRPM*0.9')
p6 = plot(EVEdays,EVEw1,'b2',/overplot,$
          NAME='EVE')
leg3 = legend(TARGET=[p5,p6], position=[0.7,0.8])

STOP

;--------------------------------------------------------------------------------------------;

;choose another specific wavelength: 21.17Å
;--------------------------------------------------------------------------------------------;
EVEw1 = EVE1A(closest(21.17,EVE1A(*,0)),3:n_elements(EVE1A(0,*))-1)
EVEw1 = EVEw1(0,*) ;only one point close by
EVENAN = where(EVEw1 LT 0) & EVEw1(EVENAN) = !VALUES.F_NAN ;get rid of erroneous data
SRPMw1 = SRPM(closest(21.17,SRPM(*,0)),2:n_elements(SRPM(0,*))-1)
SRPMw1 = SRPMw1(0,*)

;Match range of EVE time to range of SRPM time
EVEw1 = EVEw1(EVEdaysRange_dex)

;produce plots for 584, all time
;p7 = plot(SRPMdays,SRPMw1,'r2',$
;;          yrange=[5E-5,1.5E-4],$
;          title='SRPM vs EVE Irradiance for 217Å',$
;          xtitle='Time (days)',$
;          ytitle='Irradiance',$
;          NAME='SRPM')
;p8 = plot(EVEdays,EVEw1,'b2',/overplot,$
;          NAME='EVE')
;leg4 = legend(TARGET=[p7,p8], position=[0.7,0.8])

;--------------------------------------------------------------------------------------------;

;choose another specific wavelength: 211Å
;--------------------------------------------------------------------------------------------;
EVEw1 = EVE1A(closest(21.1,EVE1A(*,0)),3:n_elements(EVE1A(0,*))-1)
EVEw1 = EVEw1(1,*) ;choose just one of those found close to 21.1 (21.1100)
EVENAN = where(EVEw1 LT 0) & EVEw1(EVENAN) = !VALUES.F_NAN ;get rid of erroneous data
SRPMw1 = SRPM(closest(21.1,SRPM(*,0)),2:n_elements(SRPM(0,*))-1)


;Match range of EVE time to range of SRPM time
EVEw1 = EVEw1(EVEdaysRange_dex)

;produce plots for 304Å, all time
;p9 = plot(SRPMdays,SRPMw1,'r2',$
;;          yrange=[0.0008,0.0014],$
;          title='SRPM vs EVE Irradiance for 171Å',$
;          xtitle='Time (days)',$
;          ytitle='Irradiance',$
;          NAME='SRPM')
;p10 = plot(EVEdays,EVEw1,'b2',/overplot,$
;          NAME='EVE')
;leg5 = legend(TARGET=[p9,p10], position=[0.7,0.7])

;--------------------------------------------------------------------------------------------;

END