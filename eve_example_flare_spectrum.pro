PRO eve_example_flare_spectrum 

eve = eve_read_whole_fits('/Users/' + getenv('username') + '/Dropbox/Research/Data/SDO/EVE/EVS_L2_2011307_20_006_02.fit.gz')
eve = eve_read_whole_fits('/Users/masonjp2/Downloads/EVS_L2_2013136_09_007_02.fit.gz')


spectra = eve.spectrum
wave = eve.spectrummeta.wavelength
jd = jpmtai2jd(spectra.tai)
iso = jpmjd2iso(jd, /NO_T_OR_Z)

irrs = spectra.irradiance

fe131 = where(wave GE 13.0 and wave LE 13.2)

sum = total(irrs[fe131, *], 1)
;sum = total(irrs, 1)

p = plot(jd, sum, xtickunits=['minute', 'hour'])

m = max(sum, loc)

p2 = plot(wave * 10, irrs[*, loc], $
          title='X1.9 flare at ' + iso[loc], $
          xtitle='Wavelength [Å]', $
          ytitle='irradiance [W m$^{-2}$ nm$^{-1}$]', /YLOG)

wave_A = wave*10
flare_irradiance = irrs[*, loc]
irradiance_units = 'W m$^{-2}$ nm$^{-1}$'
time_iso = iso[loc]

save, wave_A, flare_irradiance, irradiance_units, time_iso, filename='EVE Example Flare Spectrum (X1.9 2011-11-03 20:30:10).sav', /compress

STOP

END 