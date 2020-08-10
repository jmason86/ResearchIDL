;+
; NAME:
;   PlotEveExampleSpectraAnnotated
;
; PURPOSE:
;   Make a plot showing example SDO/EVE spectra during quiet vs flaring times. Annotate emission lines. Optionally annotate EVE instrument bandpasses.
;
; INPUTS:
;   None, but requires access to /Users/jmason86/Dropbox/Research/Data/EVE/EVE Example Spectra For Annotation.sav
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   ANNOTATE_INSTRUMENTS: Set this to include annotations for the EVE instrument bandpasses
;   QUIET_SPECTRUM_ONLY:  Set this to plot only the quiet spectrum, i.e., skip overplotting a flare spectrum
;   FLARE_SPECTRUM_ONLY:  Set this to plot only the flare spectrum, i.e., skip overplotting a quiet sun spectrum
;
; OUTPUTS:
;   Annotated plot of example solar EUV spectrum as observed by EVE
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires the file EVE Example Spectra For Annotation.sav
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2018-04-14: James Paul Mason: Wrote script.
;-
PRO PlotEveExampleSpectraAnnotated, ANNOTATE_INSTRUMENTS = ANNOTATE_INSTRUMENTS, QUIET_SPECTRUM_ONLY = QUIET_SPECTRUM_ONLY, $
                                    DARK_BACKGROUND = DARK_BACKGROUND, FLARE_SPECTRUM_ONLY = FLARE_SPECTRUM_ONLY

; Defaults
fontSize = 16
dimensions = [1800, 600]
xtitle = 'wavelength [nm]'
xrange = [0, 110]
;xrange = [8, 45] ; ESCAPE proposed range
ytitle = 'irradiance [W m$^{-2}$ nm$^{-1}$]'
yrange = [1e-8, 1e-1]
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  backgroundColor = 'black'
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  backgroundColor = 'white'
ENDELSE

; Load data
restore, '/Users/jmason86/Dropbox/Research/Data/EVE/EVE Example Spectra For Annotation.sav'

; Filter out bad data
quietIrradiance[where(quietIrradiance LT 1e-7)] = !VALUES.F_NAN
flareIrradiance[where(flareIrradiance LT 1e-7)] = !VALUES.F_NAN

; Sort temperatures for later use
logTSortIndices = sort(logT)

; Main plot
w = window(BACKGROUND_COLOR = backgroundColor, DIMENSIONS = dimensions)
IF keyword_set(QUIET_SPECTRUM_ONLY) THEN BEGIN
  p1 = plot(wavelength, flareIrradiance, 'dodger blue', /CURRENT, $
            TITLE = 'SDO/EVE Example Solar EUV Flare Spectrum')
ENDIF ELSE IF keyword_set(FLARE_SPECTRUM_ONLY) THEN BEGIN
  p1 = plot(wavelength, quietIrradiance, 'tomato', /CURRENT, $
            TITLE = 'SDO/EVE Example Solar EUV Quiet Sun Spectrum')
ENDIF ELSE BEGIN
  p1 = plot(wavelength, quietIrradiance, 'tomato', /CURRENT, $
            TITLE = 'SDO/EVE Example Solar EUV Spectra', $
            NAME = 'quiet')
  p2 = plot(wavelength, flareIrradiance, 'dodger blue', /OVERPLOT, $
            NAME = flareClass + ' flare')
  l = legend(TARGET = [p1, p2], POSITION = [0.91, 0.28], FONT_SIZE = fontSize - 2, TRANSPARENCY = 100, TEXT_COLOR = foregroundBlackOrWhite)
ENDELSE

; Plot formatting similar for all
p1.font_size = fontSize
p1.font_color = foregroundBlackOrWhite
p1.xcolor = foregroundBlackOrWhite
p1.ycolor = foregroundBlackOrWhite
p1.xtitle = xtitle
p1.xrange = xrange
p1.ytitle = ytitle
p1.yrange = yrange
p1.ylog = 1

; Ion annotations
FOR i = 0, n_elements(ion) - 1 DO BEGIN
  t = text(lineCenter[i], 1e-1, strtrim(ion[i], 2) + ' ' + JPMPrintNumber(lineCenter[i]), /DATA, COLOR = JPMColors(logTSortIndices[i], TOTALPOINTSFORGRADIENT=39), $
           ALIGNMENT = 1, VERTICAL_ALIGNMENT = 0.5, ORIENTATION = 90, FONT_SIZE = fontSize - 6)
  p = plot([lineCenter[i], lineCenter[i]], [flareIrradiance[closest(lineCenter[i], wavelength)], 4e-3], '--', COLOR = 'light grey', /OVERPLOT)
ENDFOR

; Color bar for ion temperature
rgb = JPMColors(0, TOTALPOINTSFORGRADIENT = 39, /RETURN_COLORTABLE)
cb = colorbar(POSITION = [0.93, 0.18 ,0.95, 0.86], ORIENTATION = 1, TEXTPOS = 1, $
              RGB_TABLE = rgb, RANGE = minmax(logt), FONT_SIZE = fontSize - 2, COLOR = foregroundBlackOrWhite, $
              TITLE = 'log(temperature [K])')

; Instrument annotations
IF keyword_set(ANNOTATE_INSTRUMENTS) THEN BEGIN
  ; ESP
  FOR i = 0, 4 DO BEGIN
    ar = arrow(espBands[*, i], [3e-8, 3e-8], ARROW_STYLE = 3, COLOR = 'dark grey', /DATA, /CURRENT, THICK = 2, HEAD_SIZE = 0.5)
    t = text(total(espBands[*, i]) / 2.0, 3e-8, espNames[i], /DATA, ALIGNMENT = 0.5, COLOR = 'dark grey', FONT_SIZE = fontSize - 6)
  ENDFOR
  
  ; MEGS
  ar = arrow(megsA1Band, [5e-8, 5e-8], ARROW_STYLE = 3, COLOR = 'dark grey', /DATA, /CURRENT, THICK = 2, HEAD_SIZE = 0.5)
  t = text(total(megsA1Band) / 2.0, 5e-8, 'MEGS A1', /DATA, ALIGNMENT = 0.5, COLOR = 'dark grey', FONT_SIZE = fontSize - 6)
  ar = arrow(megsA2Band, [5e-8, 5e-8], ARROW_STYLE = 3, COLOR = 'dark grey', /DATA, /CURRENT, THICK = 2, HEAD_SIZE = 0.5)
  t = text(total(megsA2Band) / 2.0, 5e-8, 'MEGS A2', /DATA, ALIGNMENT = 0.5, COLOR = 'dark grey', FONT_SIZE = fontSize - 6)
  ar = arrow(megsBBand, [5e-8, 5e-8], ARROW_STYLE = 3, COLOR = 'dark grey', /DATA, /CURRENT, THICK = 2, HEAD_SIZE = 0.5)
  t = text(total(megsBBand) / 2.0, 5e-8, 'MEGS B', /DATA, ALIGNMENT = 0.5, COLOR = 'dark grey', FONT_SIZE = fontSize - 6)
  
  ; Rearrange legend
  l.position = [0.25, 0.4]
  
  p1.save, 'EVE Example Spectra Annotated With Instruments.png'
ENDIF ELSE BEGIN

  ; Save plot
  p1.save, 'EVE Example Spectra Annotated.png', /TRANSPARENT

ENDELSE

END