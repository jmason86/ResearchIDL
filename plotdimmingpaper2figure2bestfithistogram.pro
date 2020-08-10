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
;   DARK_BACKGROUND: Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;
; OUTPUTS:
;   PNG and EPS versions of plot in 2 directories: 
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/
;   2. Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/EPSs/ and PNGs/
;   Also produces a .sav of everything in
;   Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/IDLSavesets/
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
PRO PlotDimmingPaper2Figure2BestFitHistogram, DARK_BACKGROUND = DARK_BACKGROUND

; Defaults
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  barFillColor = 'Azure'
  blueDarkOrLight = 'deep sky blue'
  backgroundColor = 'black' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  barFillColor = 'dark slate grey'
  blueDarkOrLight = 'blue'
  backgroundColor = 'white'
ENDELSE

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Fitting/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

restore, saveloc1 + 'StatisticsOfDimmingFits.sav'

; Manipulate chi squared data for plotting of histogram (square brackets ensure that if npts = 1, still have an array for barplot)
parabolaChiXValues = [JPMrange(-0.15, 0.15, npts = n_elements(parabolaChis))]
poly3ChiXValues = [JPMrange(0.7, 1.3, npts = n_elements(poly3Chis))]
poly4ChiXValues = [JPMrange(1.85, 2.15, npts = n_elements(poly4Chis))]
poly5ChiXValues = [JPMrange(2.65, 3.35, npts = n_elements(poly5Chis))]

; Deal with edge case for npts = 1 to still have sensible plot
IF n_elements(parabolaChis) EQ 1 THEN parabolaHistogramWidth = 0.5 ELSE parabolaHistogramWidth = 1.0
IF n_elements(poly3Chis) EQ 1 THEN poly3HistogramWidth = 0.5 ELSE poly3HistogramWidth = 1.0
IF n_elements(pol43Chis) EQ 1 THEN poly4HistogramWidth = 0.5 ELSE poly4HistogramWidth = 1.0
IF n_elements(pol53Chis) EQ 1 THEN poly5HistogramWidth = 0.5 ELSE poly5HistogramWidth = 1.0

; Plot histogram
w = window(DIMENSIONS = [700, 800], BACKGROUND_COLOR = backgroundColor)
b1 = barplot(histogramPlaceHolderX, histogramData, TITLE = '"Best Fit" Histogram', FONT_COLOR = foregroundBlackOrWhite, FILL_COLOR = barFillColor, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 1, $
             XTEXT_ORIENTATION = 60, XMINOR = 0, XCOLOR = foregroundBlackOrWhite, $
             YTITLE = 'Number of ' + JPMPrintNumber(numberOfEvents, /NO_DECIMALS) + ' Events With Best Fit', YRANGE = [0, 20], YCOLOR = foregroundBlackOrWhite)
b1.XTICKNAME = names

IF numberOfParabolas NE 0 THEN $
  b2 = barplot(parabolaChiXValues, parabolaChis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = parabolaHistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
               XRANGE = b1.XRANGE, $
               YRANGE = [0, 30])
IF numberOf3Polys NE 0 THEN $
  b3 = barplot(poly3ChiXValues, poly3Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = poly3HistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
               XRANGE = b1.XRANGE, $
               YRANGE = [0, 30])
IF numberOf4Polys NE 0 THEN $
  b4 = barplot(poly4ChiXValues, poly4Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = poly4HistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
               XRANGE = b1.XRANGE, $
               YRANGE = [0, 30])
IF numberOf5Polys NE 0 THEN $
  b5 = barplot(poly5ChiXValues, poly5Chis, FILL_COLOR = 'red', TRANSPARENCY = 20, WIDTH = poly5HistogramWidth, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
               XRANGE = b1.XRANGE, $
               YRANGE = [0, 30])
ax1 = axis('X', LOCATION = 'top', TARGET = [b5], COLOR = foregroundBlackOrWhite, SHOWTEXT = 0, MINOR = 0)
ax2 = axis('Y', LOCATION = 'right', TARGET = [b5], TITLE = 'Reduced $\chi^2$', COLOR = 'red')

STOP
IF keyword_set(DARK_BACKGROUND) THEN b1.save, '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/BestFitHistogram.png', /TRANSPARENT $
ELSE BEGIN
  b1.save, saveloc1 + 'Best Fit Histogram.png'
  b1.save, saveloc2 + 'PNGs/BestFitHistogram.png'
  b1.save, saveloc2 + 'EPSs/BestFitHistogram.eps'
  save, FILENAME = saveloc2 + 'IDLSavesets/Figure2Saveset.sav', /COMPRESS
ENDELSE

END