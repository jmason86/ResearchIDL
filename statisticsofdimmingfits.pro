;+
; NAME:
;   StatisticsOfDimmingFits
;
; PURPOSE:
;   Compute and create plot of statistics for the fits on the EVE dimming light curves in the 2-2 week period 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   INCLUDE_BAD_EVENTS: Set this to skip events that are effective or literal duplicates, or flagged as horrible in EVE
;   REPROCESS_FITS:     Set this to call FitCoronalDimmingLightCurve.pro for each event. If not set, then will just restore
;                       a save file that is created at the end of this code when this keyword is set. 
;   
; OUTPUTS:
;   Plot of histogram for fit types and overlaid with their chi squareds
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires FitCoronalDimmingLightCurve
;   Requires JPMPrintNumber
;
; EXAMPLE:
;   StatisticsOfDimmingFits, /SKIP_BAD_EVENTS
;
; MODIFICATION HISTORY:
;   2015/02/12: James Paul Mason: Wrote script.
;   2015/02/13: James Paul Mason: Added SKIP_BAD_EVENTS keyword and supporting code. 
;   2015/10/02: James Paul Mason: Changed SKIP_BAD_EVENTS to INCLUDE_BAD_EVENTS since that's the unusual case.
;                                 Added REPROCESS_FITS keyword. 
;                                 Also updated plot formatting. 
;-
PRO StatisticsOfDimmingFits, INCLUDE_BAD_EVENTS = INCLUDE_BAD_EVENTS, REPROCESS_FITS = REPROCESS_FITS

; Defaults
IF keyword_set(INCLUDE_BAD_EVENTS) THEN BEGIN
  SKIP_BAD_EVENTS = 0
  numberOfEvents = 36
ENDIF ELSE BEGIN
  SKIP_BAD_EVENTS = 1
  numberOfEvents = 36 - 7
ENDELSE

; Setup
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/'

IF keyword_set(REPROCESS_FITS) THEN BEGIN
  ; Create storage arrays
  allBestFits = strarr(numberOfEvents)
  allBestFitChis = fltarr(numberOfEvents)
  
  ; Loop through all events and store best fit information 
  storageIndex = 0
  FOR eventNumber = 1, 36 DO BEGIN
    bestFit = !NULL
    FitCoronalDimmingLightCurve, eventNumber = eventNumber, bestFitOut = bestFit, bestFitChiOut = bestFitChi, SKIP_BAD_EVENTS = SKIP_BAD_EVENTS
    IF bestFit EQ !NULL THEN CONTINUE
    IF eventNumber EQ 32 THEN print, eventNumber
    allBestFits[storageIndex] = bestFit
    allBestFitChis[storageIndex] = bestFitChi
    storageIndex++
  ENDFOR
  
  ; Calculate statistics 
  parabolaIndices = where(allBestFits EQ 'Parabola', numberOfParabolas)
  poly3Indices = where(allBestFits EQ '3rd Order Poly', numberOf3Polys)
  poly4Indices = where(allBestFits EQ '4th Order Poly', numberOf4Polys)
  poly5Indices = where(allBestFits EQ '5th Order Poly', numberOf5Polys)
  
  ; Collate data for histogram
  histogramData = [numberOfParabolas, numberOf3Polys, numberOf4Polys, numberOf5Polys]
  histogramPlaceHolderX = findgen(4)
  names = [' ', 'Parabola', 'Poly-3', 'Poly-4', 'Poly-5', ' ']
  
  ; Manipulate data for chi squareds
  parabolaChis = allBestFitChis[parabolaIndices]
  poly3Chis = allBestFitChis[poly3Indices]
  poly4Chis = allBestFitChis[poly4Indices]
  poly5Chis = allBestFitChis[poly5Indices]
ENDIF $ ; REPROCESS_FITS set
ELSE restore, saveloc + 'StatisticsOfDimmingFits.sav'

; Manipulate chi squared data for plotting of histogram (square brackets ensure that if npts = 1, still have an array for barplot)
parabolaChiXValues = [range(-0.15, 0.15, npts = n_elements(parabolaChis))]
poly3ChiXValues = [range(0.7, 1.3, npts = n_elements(poly3Chis))]
poly4ChiXValues = [range(1.85, 2.15, npts = n_elements(poly4Chis))]
poly5ChiXValues = [range(2.65, 3.35, npts = n_elements(poly5Chis))]

; Deal with edge case for npts = 1 to still have sensible plot
IF n_elements(parabolaChis) EQ 1 THEN parabolaHistogramWidth = 0.5 ELSE parabolaHistogramWidth = 1.0
IF n_elements(poly3Chis) EQ 1 THEN poly3HistogramWidth = 0.5 ELSE poly3HistogramWidth = 1.0
IF n_elements(pol43Chis) EQ 1 THEN poly4HistogramWidth = 0.5 ELSE poly4HistogramWidth = 1.0
IF n_elements(pol53Chis) EQ 1 THEN poly5HistogramWidth = 0.5 ELSE poly5HistogramWidth = 1.0

; Plot histogram
w = window(DIMENSIONS = [700, 800])
b1 = barplot(histogramPlaceHolderX, histogramData, TITLE = '"Best Fit" Histogram', /CURRENT, MARGIN = 0.1, AXIS_STYLE = 1, $
             XTEXT_ORIENTATION = 60, $
             YTITLE = 'Number of ' + JPMPrintNumber(numberOfEvents) + ' Events With Best Fit', YRANGE = [0, 20])
b1.XTICKNAME = names

IF numberOfParabolas NE 0 THEN $
b2 = barplot(parabolaChiXValues, parabolaChis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = parabolaHistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
IF numberOf3Polys NE 0 THEN $
b3 = barplot(poly3ChiXValues, poly3Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = poly3HistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
IF numberOf4Polys NE 0 THEN $
b4 = barplot(poly4ChiXValues, poly4Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = poly4HistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
IF numberOf5Polys NE 0 THEN $             
b5 = barplot(poly5ChiXValues, poly5Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = poly5HistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
ax1 = axis('Y', LOCATION = 'right', TARGET = [b5], TITLE = 'Reduced $\chi^2$', COLOR = 'red')
b1.save, saveloc + 'Best Fit Histogram.png'

; Save everything so its not necessary to reprocess every time
IF keyword_set(REPROCESS_FITS) THEN save, FILENAME = saveloc + 'StatisticsOfDimmingFits.sav'

END