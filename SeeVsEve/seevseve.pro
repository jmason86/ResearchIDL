; Program to compare SDO/EVE and TIMED/SEE data
;
; James Paul Mason
; 12/3/10

PRO SeeVsEve

read_netCDF, 'see__L3_2010302_010_03.ncdf', seedata, attributes, status
eveave, eveflux, evewave
bineve, eveflux, evewave, evebinflux, evebinwave



p1 = plot(seedata.sp.wave, seedata.sp.flux*1E6, 'b2',$
          xrange = [0,40], /ylog, $
          xtitle='Wavelength [nm]', ytitle='Log Irradiance [!9m!1W m!e-2!n nm!e-1!n]', $
          title='Solar Spectral Irradiance: EVE vs SEE Daily Average for Oct 29, 2010', $
          NAME='SEE SSI')
p2 = plot(evebinwave[6:103]+0.5, evebinflux[6:103]*1E6, yrange=[1,4E2], 'r2',/ylog,$
          /OVERPLOT, NAME='EVE SSI')
leg = legend(TARGET=[p1,p2], LOCATION=[-0.8,0.6])

STOP
END 
