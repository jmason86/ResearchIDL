;+
; NAME:
;   PlotDimmingPaper2Figure2BestFitHistogram
;
; PURPOSE:
;   Create plot of statistics for the fits on the EVE dimming light curves in the 2-2 week period
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
;   PNG and EPS versions of plot in 2 directories: 
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/
;   2. Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/EPSs/ and PNGs/
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires JPMPrintNumber
;   Requires the StatisticsOfDimmingFits.sav file output from StatisticsOfDimming.pro
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2015/10/05: James Paul Mason: Copied and modified from StatisticsOfDimmingFits.pro
;-
PRO PlotDimmingPaper2Figure2BestFitHistogram

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

restore, saveloc1 + 'StatisticsOfDimmingFits.sav'

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

p1.save, saveloc1 + 'BestFitHistogram.png'
p1.save, saveloc1 + 'BestFitHistogram.eps'
p1.save, saveloc2 + 'PNGs/BestFitHistogram.png'
p1.save, saveloc2 + 'EPSs/BestFitHistogram.eps'
save, FILENAME = saveloc2 + 'IDLSavesets/Figure2Saveset.sav', /COMPRESS

END