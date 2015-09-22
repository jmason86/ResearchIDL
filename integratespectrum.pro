; Program to integrate spectral bands and plot a time series. 
; Comparison is done between SRPM and EVE. 
; 
; James Paul Mason
; 10/5/2011

PRO integrateSpectrum

;setup and read in data
saveloc = '/Users/jmason86/Desktop/MURIplots/'
EVEdataloc = '/Users/jmason86/Dropbox/Research/Data/SDO/EVE/'
SRPMdataloc = '/Users/jmason86/Dropbox/Research/Data/SRPM/SpectralTimeSeries/'

EVE = readfits(EVEdataloc+'EVESpectraStartTo092911.fits')
SRPM = readfits(SRPMdataloc+'SplitModelRatio.FEUV6Cont.hcopt0.conv1a.fits')

;grab vectors for wavelength and julian date
EVEwave = EVE(0:2929,0) & EVEjd = EVE(0,2:1038)
SRPMwave = SRPM(0:10084,0) & SRPMjd = SRPM(0,2:515)

;convert jd to be days since the first SRPM date
EVEjd = EVEjd-SRPMjd(0)
SRPMjd = SRPMjd-SRPMjd(0)

;redfine the fits files to contain only data, i.e. remove border information
EVE = EVE(1:2929,3:1038) & SRPM = SRPM(1:10084,2:515)

;Define the bands for integration
EVEband1 = where(EVEwave GE 10 and EVEwave LE 17) & SRPMband1 = where(SRPMwave GE 10 and SRPMwave LE 17)
EVEband2 = where(EVEwave GE 17 and EVEwave LE 23) & SRPMband2 = where(SRPMwave GE 17 and SRPMwave LE 23)
EVEband3 = where(EVEwave GE 23 and EVEwave LE 28) & SRPMband3 = where(SRPMwave GE 23 and SRPMwave LE 28)
EVEband4 = where(EVEwave GE 33 and EVEwave LE 36) & SRPMband4 = where(SRPMwave GE 33 and SRPMwave LE 36)

;Loop through all EVE rows (time) and integrate across columns (spectral ranges)
EVEintegral1=dblarr(1037) & EVEintegral2=EVEintegral1 & EVEintegral3=EVEintegral1 & EVEintegral4=EVEintegral1
FOR i=0,1035 DO BEGIN
  EVEintegral1(i) = int_tabulated(EVEwave(EVEband1),EVE(EVEband1,i))
  EVEintegral2(i) = int_tabulated(EVEwave(EVEband2),EVE(EVEband2,i))
  EVEintegral3(i) = int_tabulated(EVEwave(EVEband3),EVE(EVEband3,i))
  EVEintegral4(i) = int_tabulated(EVEwave(EVEband4),EVE(EVEband4,i))
ENDFOR

;Loop through all SRPM rows (time) and integrate across columns (spectral ranges)
SRPMintegral1=dblarr(514) & SRPMintegral2=dblarr(514) & SRPMintegral3=dblarr(514) & SRPMintegral4=dblarr(514)
FOR i=0,513 DO BEGIN
  SRPMintegral1(i) = int_tabulated(SRPMwave(SRPMband1),SRPM(SRPMband1,i))
  SRPMintegral2(i) = int_tabulated(SRPMwave(SRPMband2),SRPM(SRPMband2,i))
  SRPMintegral3(i) = int_tabulated(SRPMwave(SRPMband3),SRPM(SRPMband3,i))
  SRPMintegral4(i) = int_tabulated(SRPMwave(SRPMband4),SRPM(SRPMband4,i))
ENDFOR


;produce plots
p1=plot(EVEjd,EVEintegral1,'r +', yrange=[1E-4,2.5E-4],xrange=[0,500],$
       title='Timeseries of 10-17nm Spectral Band',$
       xtitle='Time [Days Since 2010 July 19]',$
       ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
       NAME='SDO EVE')
p2=plot(SRPMjd,0.9*SRPMintegral1,'b *',/overplot,$
       NAME='SRPM*0.9')
leg1 = legend(target=[p1,p2],POSITION=[0.7,0.9])
p1.Save, saveloc+"EVE_SRPM_10-17nmTimesSeries.png", BORDER=10, RESOLUTION=300
p1.close

p3=plot(EVEjd,EVEintegral2,'r +', yrange=[4E-4,1E-3],xrange=[0,500],$
       title='Timeseries of 17-23nm Spectral Band',$
       xtitle='Time [Days Since 2010 July 19]',$
       ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
       NAME='SDO EVE')
p4=plot(SRPMjd,1*SRPMintegral2,'b *',/overplot,$
       NAME='SRPM*1')
leg2 = legend(target=[p3,p4],POSITION=[0.75,0.9])
p3.Save, saveloc+"EVE_SRPM_17-23nmTimesSeries.png", BORDER=10, RESOLUTION=300
p3.close

p5=plot(EVEjd,EVEintegral3,'r +', yrange=[1.5E-4,4E-4],xrange=[0,500],$
       title='Timeseries of 23-28nm Spectral Band',$
       xtitle='Time [Days Since 2010 July 19]',$
       ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
       NAME='SDO EVE')
p6=plot(SRPMjd,0.55*SRPMintegral3,'b *',/overplot,$
       NAME='SRPM*0.55')
leg3 = legend(target=[p5,p6],POSITION=[0.7,0.9])
p5.Save, saveloc+"EVE_SRPM_23-28nmTimesSeries.png", BORDER=10, RESOLUTION=300
p5.close

p7=plot(EVEjd,EVEintegral4,'r +', yrange=[1E-4,2.5E-4],xrange=[0,500],$
       title='Timeseries of 33-36nm Spectral Band',$
       xtitle='Time [Days Since 2010 July 19]',$
       ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
       NAME='SDO EVE')
p8=plot(SRPMjd,1.1*SRPMintegral4,'b *',/overplot,$
       NAME='SRPM*1.1')
leg4 = legend(target=[p7,p8],POSITION=[0.7,0.9])
p7.Save, saveloc+"EVE_SRPM_33-36nmTimesSeries.png", BORDER=10, RESOLUTION=300
p7.close


END