;+
; NAME:
;   PlotCmeVelocityHistogram
;
; PURPOSE:
;   Make a plot showing the distribution of CME velocities from CDAW
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   REPROCESS_CMES: Set this to reprocess the CME data insted of loeading from the saveset that already ran this code.
;
; OUTPUTS:
;   PNG of plot in 2 directories:
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/
;   2. Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images
;   Also produces a .sav of everything in
;   Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/IDLSavesets/
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires access to EVE lines data and EVE ssw code
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2016/01/22: James Paul Mason: Wrote script.
;-
PRO PlotCmeVelocityHistogram, REPROCESS_CMES = REPROCESS_CMES

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'

IF keyword_set(REPROCESS_CMES) THEN BEGIN
  readcol, saveloc1 + 'Historical CME Data Velocity.csv', cmeDate, cmeUtc, CentralPA, width, linearSpeed, format = 'a, a, a, a, a', DELIMITER = ',', SKIPLINE = 1, /SILENT
  
  speeds = float(linearSpeed[where(linearSpeed NE '----')])
  pdf = histogram(speeds, LOCATIONS = speedBins)
  
  save, speeds, pdf, speedBins, FILENAME = saveloc1 + 'Historical CME Velocity.sav'
ENDIF ELSE restore,                        saveloc1 + 'Historical CME Velocity.sav'

speedAbove1315kmpsIndices = where(speeds GT 1315, numberAbove1315kmps)
percentAbove1315kmps = double(numberAbove1315kmps) / n_elements(speeds) * 100d

speedBinsAbove1315kmpsIndices = where(speedBins GT 1315)

p1 = plot(speedBins, pdf, $
          TITLE = 'Histogram of CME Speed (1996/01/11 - 2015/02/28)', $
          XTITLE = 'Speed [$km s^{-1}$]', XRANGE = [0, 3500], $
          YTITLE = 'Number of CMEs (of 25,053)')
p2 = plot(speedBins[speedBinsAbove1315kmpsIndices], pdf[speedBinsAbove1315kmpsIndices], 'r', /OVERPLOT)
p3 = plot([1315, 1315], p1.yrange, 'r--', /OVERPLOT)
t1 = text(1280, 59, JPMPrintNumber(100.01 - percentAbove1315kmps) + '% of CMEs', /DATA, ORIENTATION = 90, FONT_STYLE = 'bold') ; 100.01 to deal with rounding
t2 = text(1350, 60, JPMPrintNumber(percentAbove1315kmps) + '% of CMEs', /DATA, COLOR = 'red', ORIENTATION = 90, VERTICAL_ALIGNMENT = 1, FONT_STYLE = 'bold')

p1.save, saveloc1 + 'HistoricalCmeVelocity.png'
p1.save, saveloc2 + 'HistoricalCmeVelocity.png'
save, FILENAME = saveloc2 + 'IDLSavesets/HistoricalCmeVelocity.sav', /COMPRESS

END