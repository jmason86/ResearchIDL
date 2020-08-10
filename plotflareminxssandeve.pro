;+
; NAME:
;   PlotFlareMinXSSAndEve
;
; PURPOSE:
;   Plot a series of flare spectra from MinXSS and EVE/MEGS-B with pre-flare spectrum overplotted
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   MAKE_MOVIE: Set this to produce a movie rather than a series of individual plots
;
; OUTPUTS:
;   Plots (.png) saved to disk in Dropbox/Research/Postdoc_LASP/Analysis/MinXSS-EVE Big Flare/
;   Also a movie (.mov) in the same directory if MAKE_MOVIE keyword is set. 
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires that GetMinxssMegsbFlareData has run
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2016-11-29: James Paul Mason: Wrote script.
;-
PRO PlotFlareMinXSSAndEve, MAKE_MOVIE = MAKE_MOVIE

; Setup
dataloc = '/Users/' + getenv('username') + '/Dropbox/Research/Data/EVE-GOES-MinXSS/'
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Analysis/MinXSS-EVE Big Flare/'

; Restore data
restore, dataloc + 'FlareAnalysis.sav'

; Interpolate the EVE spectra to the MinXSS times -- EVE has better coverage so interpolation should be alright 
eveSpectraInterpolated = fltarr(n_elements(minxss), n_elements(eveSpectra[0].irradiance))
FOR binIndex = 0, n_elements(eveSpectra[0].irradiance) - 1 DO BEGIN
  eveSpectraInterpolatedTemp = spline(eveSpectra.timeJd, eveSpectra.irradiance[binIndex], minxss.time.jd)
  eveSpectraInterpolated[*, binIndex] = temporary(eveSpectraInterpolatedTemp)
ENDFOR

; Convert MinXSS units to same as EVE (photons / s / cm2 / keV -> W / m2 / nm)
;minxss.irradiance = minxss_convert_irradiance_units(minxss.irradiance, minxss.energy)

; Setup movie
IF keyword_set(MAKE_MOVIE) THEN BEGIN
  movieObject = IDLffVideoWrite(saveloc + 'Spectrum Movie.mp4')
  xsize = 800
  ysize = 600
  fps = 3
  bitrate = 1e7
  vidStream = movieObject.AddVideoStream(xsize, ysize, fps, BIT_RATE = bitrate)
ENDIF

;
; Loop through the MinXSS spectra and plot
; 

FOR spectrumIndex = 1, n_elements(minxss) - 1 DO BEGIN
 
  ; Change EVE bad data flag from -1 to NAN
  badDataIndices = where(eveSpectraInterpolated[spectrumIndex, *] EQ -1)
  eveSpectraInterpolated[spectrumIndex, badDataIndices] = !VALUES.F_NAN
  
  ; Normalize everything by pre-flare so that the irradiance units are comparable
  minxssIrradiance = minxss[spectrumIndex].irradiance - minxss[0].irradiance
  minxssIrradiance = minxss_convert_irradiance_units(minxssIrradiance, minxss[spectrumIndex].energy)
  eveIrradiance = eveSpectraInterpolated[spectrumIndex, *] - eveSpectraInterpolated[0, *]
  
  ; Present plot
  !EXCEPT = 0 ; Disable annoying error messages
  w = window(DIMENSIONS = [800, 600], /BUFFER)
  p1a = plot(minxss[spectrumIndex].energy, minxssIrradiance, '2', COLOR = 'dodger blue', /CURRENT, MARGIN = 0.15, $ 
             AXIS_STYLE = 1, $ 
             XTITLE = 'Energy [keV]', XRANGE = [0.01, 10], /XLOG, $ 
             YTITLE = 'Irradiance [W m$^{-2}$ nm$^{-1}$]', /YLOG, YRANGE = [1e-20, 1e0], $
             NAME = 'MinXSS-1 X123')
  p2a = plot(JPMAngstrom2keV(eveSpectra[spectrumIndex].wavelength * 10.), eveIrradiance, '2', COLOR = 'tomato', /OVERPLOT, $
             NAME = 'SDO EVE MEGS-B')
  l1 = legend(TARGET = [p1a, p2a], POSITION = [0.8, 0.78])
  pHide = plot(JPMkev2Angstrom(minxss[spectrumIndex].energy) / 10., minxssIrradiance, /CURRENT, MARGIN = 0.15, LINESTYLE = 'none', AXIS_STYLE = 4, $
               XRANGE = [124., 0.124], /XLOG)
  pHide2 = plot(eveSpectra[spectrumIndex].wavelength, eveIrradiance, /CURRENT, MARGIN = 0.15, LINESTYLE = 'none', AXIS_STYLE = 4, $
                /XLOG)
  axisTop = axis('X', LOCATION = 'top', TARGET = [pHide], TITLE = 'Wavelength [nm]')
  axisRight = axis('Y', LOCATION = 'right', TARGET = [p1a], SHOWTEXT = 0)
  t1 = text(0.16, 0.79, strtrim(goesEvent.st$class, 2) + ' flare peak at ' + goesEvent.eventPeakTimeHuman, TARGET = p1a)
  t2 = text(0.16, 0.19, minxss[spectrumIndex].time.human, TARGET = p1a)
  
  p1a.font_size = 18
  p2a.font_size = 18
  l1.font_size = 14
  axisTop.tickfont_size = 18
  t1.font_size = 16
  t2.font_size = 16
  p1a.save, saveloc + 'Spectrum ' + minxss[spectrumIndex].time.human + '.png', /TRANSPARENT
  !EXCEPT = 1
  ; Insert frame into movie
  IF keyword_set(MAKE_MOVIE) THEN BEGIN
    timeInMovie = movieObject.Put(vidStream, w.CopyWindow()) ; time returned in seconds
  ENDIF 
  
  w.close
  
ENDFOR

IF keyword_set(MAKE_MOVIE) THEN BEGIN
  movieObject.Cleanup
ENDIF

END