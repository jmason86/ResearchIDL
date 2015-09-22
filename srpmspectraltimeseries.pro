; Program to plot the daily SRPM spectra through time. 
; 
; File is based on data from Rome. 
;
; James Paul Mason
; 9/27/2011

PRO SRPMSpectralTimeSeries

;1Å RESOLUTION
;read in data
saveloc = '/Users/jmason86/Desktop/MURIplots/'
EVEdataloc = '/Users/jmason86/Dropbox/Research/Data/SDO/EVE/'
SRPMdataloc = '/Users/jmason86/Dropbox/Research/Data/SRPM/SpectralTimeSeries/'
EVE = readfits(EVEdataloc+'EVESpectraStartTo092911.fits')
SRPM = readfits(SRPMdataloc+'SplitModelRatio.FEUV6Cont.hcopt0.conv1a.fits')

;grab vectors for wavelength and julian date
EVEwave = EVE(0:2929,0) & EVEjd = EVE(0,2:1037)
SRPMwave = SRPM(0:10084,0) & SRPMjd = SRPM(0,2:515)

;convert jd to be days since the first SRPM date
EVEjd = EVEjd-SRPMjd(0)
SRPMjd = SRPMjd-SRPMjd(0)

p1 = plot(EVEjd,EVE(1199,2:1037), 'r2 +',yrange=[4E-3,6E-3],xrange=[0,500],$
          title ='304 Å Time Series Comparison',$
          xtitle='Days since July 19, 2010',$
          ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
          NAME='SDO EVE')
p2 = plot(SRPMjd,0.78*SRPM(1509,2:515),'b2 *',/OVERPLOT,$
          NAME='SRPM 1Å *0.78')
leg=legend(TARGET=[p1,p2],POSITION=[0.75,0.9])
p1.Save, saveloc+"EVE_SRPM_304TimeSeries.png", BORDER=10, RESOLUTION=300
p1.close


;redfine the fits files to contain only data, i.e. remove border information
EVE = EVE(0:2929,3:1038) & SRPM = SRPM(0:10084,2:515)

;define the wavelengths of interest
EVEwave_sel1 = closest(30.4,EVEwave,/lower)
SRPMwave_sel1 = closest(30.4,SRPMwave,/lower) ; TODO: Figure out if the upper or lower closest val contains the actual He II line
;wave_sel2 = where(wave EQ 46.48) + 1
;wave_sel3 = where(wave EQ 46.56) + 1
EVEspec304 = EVE[EVEwave_sel1,0:n_elements(EVE[0,*])-1]
SRPMspec304 = SRPM[SRPMwave_sel1,0:n_elements(SRPM[0,*])-1]
;spec4648 = spec[wave_sel2,2:n_elements(spec[0,*])-1]
;spec4656 = spec[wave_sel3,2:n_elements(spec[0,*])-1]

;generate plots
;p1 = plot(EVEjd,EVEspec304, 'r2 +',yrange=[4E-3,6E-3],xrange=[0,500],$
;          title ='304 Å Time Series Comparison',$
;          xtitle='Days since July 19, 2010',$
;          ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
;          NAME='SDO EVE')
;
;p2 = plot(SRPMjd,1.25*SRPMspec304,'b2 *',/OVERPLOT,$
;          NAME='SRPM 1Å *1.1')
;
;;p3 = plot(mjd/360,spec4656,'r2',/OVERPLOT,$
;;          NAME='46.56nm')
;leg=legend(TARGET=[p1,p2],POSITION=[0.75,0.9])
;STOP
;p1.close


;PLOT ON THE SIDE 

EVEtime0 = closest(0,EVEjd)

p9 = plot(SRPMwave[1:10084],SRPM[1:10084,0],'b2',/ylog, xrange=[0,40],yrange=[1E-6,1E-2],$
          title='Spectrum from July 19,2010',$
          xtitle='Wavelength [nm]',$
          ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
          NAME='SRPM 1Å')
p10 = plot(EVEwave[1:2929],EVE[1:2929,EVEtime0],'r:3',/overplot, $
           NAME='SDO EVE')
leg9 = legend(target=[p9,p10],position=[0.75,0.9])
p9.Save, saveloc+"EVE_SRPM_07192011Spectrum.png", BORDER=10, RESOLUTION=300
p9.close

STOP












;1nm RESOLUTION
;read in data
spec = readfits('/Users/jmason86/Dropbox/Research/Data/SRPM/SpectralTimeSeries/Binned10aFEUV6Cont_h5copt0_conv1a_Rome12.fits')



;set up axes arrays
wave = spec[1:n_elements(spec[*,0])-1,0] ;[nm]
jd = spec[0,*] ;julian days
mjd = jd[2:n_elements(jd)-1] - 2451555.5 ;modified julian day - from January 12, 2000

;define the wavelengths of interest
wave_sel1 = where(wave EQ 46.0) + 1 ;centered at 46.5
wave_sel2 = where(wave EQ 47.0) + 1 ;centered at 47.5
wave_sel3 = where(wave EQ 45.0) + 1 ;centered at 45.5
spec465 = spec[wave_sel1,2:n_elements(spec[0,*])-1]
spec475 = spec[wave_sel2,2:n_elements(spec[0,*])-1]
spec455 = spec[wave_sel3,2:n_elements(spec[0,*])-1]

;generate plots
;p1 = plot(mjd/360,spec465, '2',color='orange',$
;          title ='Solar Cycle Variation of Spectral Lines at 1nm Resolution',$
;          xtitle='Years since January 12, 2000',$
;          ytitle='Flux [photons cm!e-2!n s!e-1!n]',$
;          NAME='46.5nm')
;
;p2 = plot(mjd/360,6*spec475,'r2',/OVERPLOT,$
;          NAME='47.5nm *6')
;
;p3 = plot(mjd/360,6*spec455,'b2',/OVERPLOT,$
;          NAME='45.5nm *6')
;leg=legend(TARGET=[p1,p2,p3],POSITION=[0.75,0.9])







STOP



END