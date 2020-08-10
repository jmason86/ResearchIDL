;+
; NAME:
;   PlotFeIonizationFraction
;
; PURPOSE:
;   Create plot showing iron (Fe) ionization fraction as a function of temperature (logT). 
;   Dissertation plot. 
;
; INPUTS:
;   None, though code uses data from '/Users/jmason86/Dropbox/Research/Woods_LASP/Data/MazzottaIonizationFractions.sav' that was saved from 
;   CHIANTI plot_ioneq procedure. 
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   DARK_BACKGROUND: Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;
; OUTPUTS:
;   Plot of Fe ionization fraction as a function of temperature
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires the MazzottaIonizationFractions.sav file that was manually exported from CHIANTI plot_ioneq procedure
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2016/03/19: James Paul Mason: Wrote script.
;-
PRO PlotFeIonizationFraction, DARK_BACKGROUND = DARK_BACKGROUND

; Defaults
dark_background = 1
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  backgroundColor = 'black' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  backgroundColor = 'white'
ENDELSE

; Setup
dataloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Data/'
saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'
totalPointsForGradient = 40
legendStartY = 1.1 ; [ion fraction] data units
legendStartX = 10^6.75  ; [K] temperature data units

; Restore data
restore, dataloc + 'MazzottaIonizationFractions.sav'

; Rename variables
temperature = temporary(ioneq_t)
feFraction = temporary(ioneq)
atomicNumber = temporary(iz) ; actually atomic number - 1, probably an indexing thing where hydrogen would be z = 0 instead of 1. clunky. 
maxIonization = 26

; Create plot
w = window(DIMENSIONS = [2000, 800], BACKGROUND_COLOR = backgroundColor)
p1 = plot(10^temperature, feFraction[*, atomicNumber, 0], '3', FONT_SIZE = 24, COLOR = JPMColors(40, totalPointsForGradient = totalPointsForGradient), MARGIN = 0.1, /CURRENT, $
          XTITLE = 'Temperature [K]', /XLOG, XCOLOR = foregroundBlackOrWhite, $
          YTITLE = 'Ion Fraction', YRANGE = [0, 1.2], YCOLOR = foregroundBlackOrWhite)
;t1 = text(legendStartX, legendStartY, 'Fe ' + ionStage[0], COLOR = JPMColors(40, totalPointsForGradient = totalPointsForGradient), /DATA, FONT_SIZE = 20)

ionizationReverseIndex = 11
FOR ionizationIndex = maxIonization, 1, -1 DO BEGIN
  p = plot(10^temperature, feFraction[*, atomicNumber, ionizationIndex], '3', COLOR = JPMColors(ionizationReverseIndex, totalPointsForGradient = totalPointsForGradient), /OVERPLOT)
  peakValue = max(feFraction[*, atomicNumber, ionizationIndex], peakIndex)
  IF ionizationIndex GT 24 THEN CONTINUE ; Don't label the last two ionization states in alternate legend style
  IF ionizationIndex mod 2 EQ 1 THEN $
  ;IF ionizationIndex LT 14 THEN $ 
    ;t = text(legendStartX, legendStartY - (ionizationIndex * 0.04), 'Fe ' + ionStage[ionizationIndex], /DATA, COLOR = JPMColors(ionizationReverseIndex, totalPointsForGradient = totalPointsForGradient), FONT_SIZE = 20) $
    t = text(10^temperature[peakIndex], 1.1, ionStage[ionizationIndex], /DATA, COLOR = JPMColors(ionizationReverseIndex, totalPointsForGradient = totalPointsForGradient), FONT_SIZE = 20, ALIGNMENT = 0.5) $
  ELSE $
    ;t = text(legendStartX + 0.6e7, legendStartY - ((ionizationIndex - 14) * 0.04), 'Fe ' + ionStage[ionizationIndex], /DATA, COLOR = JPMColors(ionizationReverseIndex, totalPointsForGradient = totalPointsForGradient), FONT_SIZE = 20)
    t = text(10^temperature[peakIndex], 1.05, ionStage[ionizationIndex], /DATA, COLOR = JPMColors(ionizationReverseIndex, totalPointsForGradient = totalPointsForGradient), FONT_SIZE = 20, ALIGNMENT = 0.5)
    ionizationReverseIndex++
ENDFOR
STOP
; Save plot and data
IF keyword_set(DARK_BACKGROUND) THEN saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/'
p.save, saveloc + 'FeIonizationFraction.png', /TRANSPARENT
save, FILENAME = saveloc + 'IDLSavesets/FeIonizationFraction.sav'

END