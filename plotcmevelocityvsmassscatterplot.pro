;+
; NAME:
;   PlotCMEVelocityVsMassScatterplot
;
; PURPOSE:
;   Make a scatter plot comparing CME speed vs mass
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
;   PNG of plot in the save location
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires the CDAW catalog file be in the expected directory
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2020-04-30: James Paul Mason: Wrote script.
;-
PRO PlotCMEVelocityVsMassScatterplot

  ; Setup
  saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Data/CDAW/'
  
  restore, saveloc + 'Historical CME Data CSV Template.sav'
  cdaw = read_ascii(saveloc + 'Historical CME Data.csv', template = myTemplate)
  ;readcol, saveloc1 + 'Historical CME Data.csv', cmeDate, cmeUtc, CentralPA, width, speed, mass, kineticEnergy, format = 'a, a, a, a, a, a, a', DELIMITER = ',', SKIPLINE = 1
  time_iso = cdaw.date + 'T' + cdaw.time + 'Z'
  
  speed = cdaw.speed
  mass = cdaw.mass
  goodIndices = where(speed GT 0 and mass NE '----')
  speed = speed[goodIndices]
  mass = mass[goodIndices]
  
  p1 = scatterplot(mass, speed, FONT_SIZE = 13, $
                   TITLE = 'CME Speed vs Mass (' + strmid(time_iso[0], 0, 10) + ' to ' + strmid(time_iso[-1], 0, 10) + '): ' + JPMPrintNumber(n_elements(speed), /NO_DECIMALS) + ' CMEs', $
                   XTITLE = 'Mass [g]', /XLOG, $
                   YTITLE = 'Speed [$km s^{-1}$]')
  STOP
  p1.save, saveloc + 'HistoricalCmeSpeedVsMass.png'

END