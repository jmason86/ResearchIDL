;+
; NAME:
;   PlotCmeMassHistogram
;
; PURPOSE:
;   Make a plot showing the distribution of CME mass and kinetic energy from CDAW
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
;   Plot in directory Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/
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
;   2016-03-30: James Paul Mason: Wrote script.
;   2016-10-11: James Paul Mason: Now stores speed as well and changed filtering to include null values and set them to NAN
;   2020-04-30: James Paul Mason: If reprocessing the CMEs, call the PlotVelocityHistogram function to do it, to keep that code in one place
;-
PRO PlotCmeMassHistogram, REPROCESS_CMES = REPROCESS_CMES

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Data/CDAW/'

IF keyword_set(REPROCESS_CMES) THEN BEGIN
  PlotCmeVelocityHistogram, /REPROCESS_CMES
ENDIF ELSE restore, saveloc1 + 'Historical CME Mass And Speed.sav'

p1 = plot(massBins, pdfMass, '2', /FILL_BACKGROUND, FILL_COLOR = 'dark grey', FONT_SIZE = 14, $
          TITLE = 'Histogram of CME Mass (' + strmid(time_iso[0], 0, 10) + ' to ' + strmid(time_iso[-1], 0, 10) + ')', $
          XTITLE = 'Mass [g]', /XLOG, XRANGE = [1e10, 1e17], $
          YTITLE = 'Number of CMEs (of ' + JPMPrintNumber(n_elements(mass), /NO_DECIMALS) +')')
p1.save, saveloc1 + 'HistoricalCmeMass.png'
STOP
END