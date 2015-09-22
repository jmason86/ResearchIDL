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
;   SKIP_BAD_EVENTS: Set this to skip events that are effective or literal duplicates, or flagged as horrible in EVE
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
;-
PRO StatisticsOfDimmingFits, SKIP_BAD_EVENTS = SKIP_BAD_EVENTS

; TODO: Remove this
SKIP_BAD_EVENTS = 1

IF keyword_set(SKIP_BAD_EVENTS) THEN numberOfEvents = 36 - 7 ELSE numberOfEvents = 36

; Create storage arrays
allBestFits = strarr(numberOfEvents)
allBestFitChis = fltarr(numberOfEvents)

; Loop through all events and store best fit information 
storageIndex = 0
FOR eventNumber = 1, 36 DO BEGIN
  bestFit = !NULL
  FitCoronalDimmingLightCurve, eventNumber = eventNumber, bestFitOut = bestFit, bestFitChiOut = bestFitChi, SKIP_BAD_EVENTS = SKIP_BAD_EVENTS
  IF bestFit EQ !NULL THEN CONTINUE
  allBestFits[storageIndex] = bestFit
  allBestFitChis[storageIndex] = bestFitChi
  storageIndex++
ENDFOR

; Calculate statistics 
parabolaIndices = where(allBestFits EQ 'Parabola', numberOfParabolas)
poly3Indices = where(allBestFits EQ '3rd Order Polynomial', numberOf3Polys)
poly4Indices = where(allBestFits EQ '4th Order Polynomial', numberOf4Polys)
poly5Indices = where(allBestFits EQ '5th Order Polynomial', numberOf5Polys)

; Collate data for histogram
histogramData = [numberOfParabolas, numberOf3Polys, numberOf4Polys, numberOf5Polys]
histogramPlaceHolderX = findgen(4)
names = [' ', 'Parabola', 'Poly-3', 'Poly-4', 'Poly-5', ' ']

; Manipulate data for chi squareds
parabolaChis = allBestFitChis[parabolaIndices]
poly3Chis = allBestFitChis[poly3Indices]
poly4Chis = allBestFitChis[poly4Indices]
poly5Chis = allBestFitChis[poly5Indices]
parabolaChiXValues = range(-0.15, 0.15, npts = n_elements(parabolaChis))
poly3ChiXValues = range(0.7, 1.3, npts = n_elements(poly3Chis))
poly4ChiXValues = range(1.65, 2.35, npts = n_elements(poly4Chis))
poly5ChiXValues = range(2.65, 3.35, npts = n_elements(poly5Chis))

; Plot histogram
w = window(DIMENSIONS = [700, 800])
b1 = barplot(histogramPlaceHolderX, histogramData, TITLE = '"Best Fit" Histogram', /CURRENT, MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 1, $
             XTEXT_ORIENTATION = 60, $
             YTITLE = 'Number of ' + JPMPrintNumber(numberOfEvents) + ' Events With Best Fit', YRANGE = [0, 20])
b1.XTICKNAME = names

IF numberOfParabolas NE 0 THEN $
b2 = barplot(parabolaChiXValues, parabolaChis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = 1, /CURRENT, MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
IF numberOf3Polys NE 0 THEN $
b3 = barplot(poly3ChiXValues, poly3Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = 1, /CURRENT, MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
IF numberOf4Polys NE 0 THEN $
b4 = barplot(poly4ChiXValues, poly4Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = 1, /CURRENT, MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
IF numberOf5Polys NE 0 THEN $             
b5 = barplot(poly5ChiXValues, poly5Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = 1, /CURRENT, MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 4, $
             XRANGE = b1.XRANGE, $
             YRANGE = [0, 10])
ax1 = axis('Y', LOCATION = 'right', TARGET = [b5], TITLE = 'Reduced $\chi^2$', COLOR = 'red')
STOP
END