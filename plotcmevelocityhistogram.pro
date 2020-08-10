;+
; NAME:
;   PlotCmeVelocityHistogram
;
; PURPOSE:
;   Dissertation plot. 
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
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Data/CDAW/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'
speedThreshold = 1315 ; [km/s]

IF keyword_set(REPROCESS_CMES) THEN BEGIN
  restore, saveloc1 + 'Historical CME Data CSV Template.sav'
  cdaw = read_ascii(saveloc1 + 'Historical CME Data.csv', template = myTemplate)
  ;readcol, saveloc1 + 'Historical CME Data.csv', cmeDate, cmeUtc, CentralPA, width, speed, mass, kineticEnergy, format = 'a, a, a, a, a, a, a', DELIMITER = ',', SKIPLINE = 1
  time_iso = cdaw.date + 'T' + cdaw.time + 'Z'
  STOP
  speed = cdaw.speed
  speed = speed[where(speed GT 0)]
  
  mass = cdaw.mass
  mass = float(mass[where(mass NE '----')])
  
  pdfSpeed = histogram(speed, LOCATIONS = speedBins)
  pdfMass = histogram(mass, LOCATIONS = massBins, nbins = n_elements(speedbins), /NAN)
  
  save, time_iso, speed, mass, pdfSpeed, speedBins, pdfMass, massBins, FILENAME = saveloc1 + 'Historical CME Mass And Speed.sav'
ENDIF ELSE restore, saveloc1 + 'Historical CME Mass And Speed.sav'

speedAboveThresholdkmpsIndices = where(speed GT speedThreshold, numberAboveThresholdkmps)
percentAboveThresholdkmps = double(numberAboveThresholdkmps) / n_elements(speed) * 100d

speedBinsAboveThresholdkmpsIndices = where(speedBins GT speedThreshold)

p1 = plot(speedBins, pdfSpeed, $
          TITLE = 'Histogram of CME Speed (' + strmid(time_iso[0], 0, 10) + ' to ' + strmid(time_iso[-1], 0, 10) + ')', $
          XTITLE = 'Speed [$km s^{-1}$]', XRANGE = [0, 3500], $
          YTITLE = 'Number of CMEs (of ' + JPMPrintNumber(n_elements(speed), /NO_DECIMALS) + ')')
p2 = plot(speedBins[speedBinsAboveThresholdkmpsIndices], pdfSpeed[speedBinsAboveThresholdkmpsIndices], 'r', /OVERPLOT)
p3 = plot([speedThreshold, speedThreshold], p1.yrange, 'r--', /OVERPLOT)
t1 = text(speedThreshold - 35, 59, JPMPrintNumber(100.01 - percentAboveThresholdkmps) + '% of CMEs', /DATA, ORIENTATION = 90, FONT_STYLE = 'bold') ; 100.01 to deal with rounding
t2 = text(speedThreshold + 35, 60, JPMPrintNumber(percentAboveThresholdkmps) + '% of CMEs', /DATA, COLOR = 'red', ORIENTATION = 90, VERTICAL_ALIGNMENT = 1, FONT_STYLE = 'bold')

p1.save, saveloc1 + 'HistoricalCmeVelocity.png'
p1.save, saveloc2 + 'HistoricalCmeVelocity.png'
save, FILENAME = saveloc2 + 'IDLSavesets/HistoricalCmeVelocity.sav', /COMPRESS

END