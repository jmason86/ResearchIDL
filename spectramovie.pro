; Program to create a movie of spectra
; 
; James Paul Mason
; 5/26/11


PRO spectramovie 

;setup
EVEdataloc='/Users/jmason86/Dropbox/Research/Data/SDO/EVE/'
SRPMdataloc='/Users/jmason86/Dropbox/Research/Data/SRPM/'
movieloc=EVEdataloc+'EVEvsSRPMplots/'


;read in data
EVE1A = readfits(EVEdataloc+'EVESpectraStartTo051511.fits',EVEhdr)
;EVE wavelength ranges from 6.41 to 36.97 nm
SRPM = readfits(SRPmdataloc+'FEUV6Cont.h5copt0.conv1a.fits',SRPMhdr)
;SRPM wavelength ranges from 1 to 199

;find date range of SRPM
SRPMmjd = SRPM(0,*) - SRPM(0,2)
EVEmjd = EVE1A(0,*) - SRPM(0,2)
EVErange_dex = where(EVEmjd GE 0 AND EVEmjd LE max(SRPMmjd))
EVEmjd = EVEmjd(EVErange_dex)

;loop through all the days
FOR i=0,n_elements(SRPMmjd)-1 DO BEGIN

  ;choose the specific date
  EVEd1 = EVE1A(*,closest(SRPMmjd(i),EVEmjd)) ;irradiance
  SRPMd1 = SRPM(*,where(SRPMmjd EQ SRPMmjd(i))) ;irradiance
  
  ;convert jd to Gregorian
  DAYCNV,SRPM(0,i),yr,mn,day,hr
  datestr = strcompress(string(yr),/remove_all)+'/'+strcompress(string(mn),/remove_all)+'/'+$
            strcompress(string(day),/remove_all)

  ;bin EVE and SRPM data down to 1nm res
  bineve,EVEd1,EVE1A(*,0),EVEbinflux,EVEbinwave
  bineve,SRPMd1,SRPM(*,0),SRPMbinflux,SRPMbinwave

  ;produce plots
  p1 = plot(SRPMbinwave,SRPMbinflux,/ylog,'r2',$
            xrange=[6,65],yrange=[1E-8,1E-1],$
            title='SRPM vs EVE Irradiance for '+datestr,$
            xtitle='Wavelength [nm]',$
            ytitle='Irradiance',$
            NAME='SRPM',$
            BUFFER=1)
  p2 = plot(EVEbinwave,EVEbinflux,/ylog,'b2',/overplot,$
            NAME='EVE',$
            BUFFER=1)
  leg1 = legend(TARGET=[p1,p2], position=[0.7,0.8])
  
  name=movieloc+'movieframes/frame_' + STRING(i, FORMAT="(I4.4)") + '.png'
  p2.save,name,resolution=200
  p2.delete
  p1.delete



ENDFOR ;i loop
END