;+
; NAME:
;   PlotBlackbodyAndFraunhofer
;
; PURPOSE:
;   Dissertation plot. 
;   Create plot showing blackbody spectrum (Planck's equation) at various representative solar temperatures 
;   and also the Fraunhofer spectrum showing the true solar photosphereic spectrum in the visible. 
;   Uses the AM0 spectrum from http://www.superstrate.net/pv/illumination/spectrum.html
;   that I datathiefed. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   Plot of spectral radiance versus wavelength for blackbody at the core, mean radiative zone, mean convection zone, photosphere,
;   and the AM0 spectrum (Fraunhofer) for comparison to the photosphere blackbody. 
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires JPMrange.pro
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2016/03/16: James Paul Mason: Wrote script.
;-
PRO PlotBlackbodyAndFraunhofer

; Setup
dataloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Data/Fraunhofer/'
saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'

; Constants
c = 3e8             ; [m/s]
h = 6.62607004e-34  ; [m^2 kg / s]
kb = 1.38064852e-23 ; [m^2 kg s^-2 K^-1]

; Read in Fraunhofer data that was datathiefed from http://www.superstrate.net/pv/illumination/spectrum.html
readcol, dataloc + 'FraunhoferSpectrum.txt', fraunhoferWavelengthNm, fraunhoferSpectralIrradiance, /SILENT ; irradiance in W/m^2 nm

; Convert fraunhofer wavelength to meters for consistency
fraunhoferWavelength = fraunhoferWavelengthNm * 1e-9

; Setup wavelength range
wavelengths = JPMrange(0, 3e-6, INC = 1e-10) ; [m] ranging from 0 (HXR) to 800 nm (visible) with 1 Å resolution

; Selected temepratures
coreTemperature = 15e6        ; [K] (wikipedia)
radiativeTemperature = 3.4e6  ; [K] (mean value determined with interpolation across radiative zone in PlotSunTemperatureDensity.pro)
convectionTemperature = 3.2e5 ; [K] (mean value determined with convection zone indices in PlotSunTemperatureDensity.pro)
photosphereTemperature = 5777 ; [K] (wikipedia)

; Planck's equation
coreSpectrum = (2 * h * c^2 / wavelengths^5) * 1 / (exp(h * c / (wavelengths * kb * coreTemperature)) - 1)
radiativeSpectrum = (2 * h * c^2 / wavelengths^5) * 1 / (exp(h * c / (wavelengths * kb * radiativeTemperature)) - 1)
convectionSpectrum = (2 * h * c^2 / wavelengths^5) * 1 / (exp(h * c / (wavelengths * kb * convectionTemperature)) - 1)
photosphereSpectrum = (2 * h * c^2 / wavelengths^5) * 1 / (exp(h * c / (wavelengths * kb * photosphereTemperature)) - 1)

; Scale Fraunhofer irradiance to the photosphere to get it to look right (irradiance -> radiance)
fraunhoferSpectralRadiance = fraunhoferSpectralIrradiance * (max(photosphereSpectrum, /NAN) / max(fraunhoferSpectralIrradiance, /NAN))

; Plot it up
w = window(DIMENSIONS = [1400, 1000])
p1 = plot(wavelengths * 1e10, coreSpectrum, '4', COLOR = 'yellow', /CURRENT, POSITION = [0.1, 0.55, 0.9, 0.95], FONT_SIZE = 16, $
          XRANGE = [0, 8000], XSHOWTEXT = 0, $
          YRANGE = [1e14, 1e30], /YLOG, $
          NAME = 'Core (15 MK)')
p1b = plot(wavelengths * 1e10, coreSpectrum, '3--', COLOR = 'black', /OVERPLOT)
p2 = plot(wavelengths * 1e10, radiativeSpectrum, '4', COLOR = 'gold', /OVERPLOT, $
          NAME = 'Radiative Zone (3.4 MK)')
p3 = plot(wavelengths * 1e10, convectionSpectrum, '4', COLOR = 'orange', /OVERPLOT, $
          NAME = 'Convection Zone (0.32 MK)')
p4 = plot(wavelengths * 1e10, photosphereSpectrum, '4', COLOR = 'green', /CURRENT, POSITION = [0.1, 0.1, 0.9, 0.5], FONT_SIZE = 16, $
          YMAJOR = 4, $
          XTITLE = 'Wavelength [Å]', XRANGE = [0, 8000], $
          NAME = 'Photosphere (5777 K)')
p5 = plot(fraunhoferWavelength * 1e10, fraunhoferSpectralRadiance, '4', /OVERPLOT, $
          NAME = 'AM0 Spectrum')
t1 = text(0.0, 0.525, 'Spectral Radiance [$W sr^{−1} m^{−3}$]', ALIGNMENT = 0.5, VERTICAL_ALIGNMENT = 1, FONT_SIZE = 16, ORIENTATION = 90)
l1 = legend(TARGET = [p1, p2, p3], POSITION = [0.9, 0.93])
l2 = legend(TARGET = [p4, p5], POSITION = [0.9, 0.2])

; Save plot and data
p1.save, saveloc + 'BlackbodyAndFraunhofer.png'
save, FILENAME = saveloc + 'IDLSavesets/BlackbodyAndFraunhofer.sav'

END