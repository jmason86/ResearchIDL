;+
; NAME:
;   Explore2017SeptemberFlareDimmings
;
; PURPOSE:
;   There were a bunch of surprise large flares in 2017 September from the same AR. 
;   This code just loads up the EVE / MEGS-B and ESP data to generate plots for exploration.
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
;   Plots to screen
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires the EVE data be downloaded and in expected directory structure
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2017-10-26: James Paul Mason: Wrote script.
;-
PRO Explore2017SeptemberFlareDimmings

reloadData = 0
startYyyymmdd = 20170904
endYyyymmdd = 20170910
saveloc = '/Users/jmason86/Dropbox/Research/Data/EVE/'
filename = saveloc + 'eve_nevii_mgx_esp_feix_' + strtrim(startYyyymmdd, 2) + '-' + strtrim(endYyyymmdd, 2) + ' Big Flares.sav'

; Load the data the first time and generate a saveset to be loaded in subsequent runs to reduce run time
IF reloadData THEN BEGIN

; MEGS-B, 2 dimming sensitive lines
GetEVEInPercentChange, JPMyyyymmdd2yyyydoy(startYyyymmdd), JPMyyyymmdd2yyyydoy(endYyyymmdd), PERCENTCHANGEOUT=irradiance, jdout=jdMegs, EVEMETAOUT=eveMeta
lines = evemeta.LINESMETA.wave_center
neviiIndex = closest(46.5, lines)
mgxIndex = closest(62.5, lines)
nevii = irradiance[neviiIndex, *]
mgx = irradiance[mgxIndex, *]

; ESP, 180 Å channel that includes 171 Å 
files = file_search('Downloads/ESP/*.fit')
esp171 = JPMReadEveEsp(files, 18)
jdEsp = esp171.jd
feix = esp171.espData

; Convert ESP irradiance to % just like already done for MEGS-B
feix = perdiff(feix[0], feix)

save, jdMegs, nevii, mgx, jdEsp, feix, FILENAME = filename
ENDIF ELSE BEGIN
  restore, filename
ENDELSE

; Mask out the eclipse (or something) data in ESP
goodIndices = where(feix GT 4.5e-4)
jdEsp = jdEsp[goodIndices]
feix = feix[goodIndices]

; Plot ESP 171 Å 
labelDate = label_date(DATE_FORMAT = ['%D', '%M'])
p1 = plot(jdEsp, feix, $
          TITLE = 'Fe IX 171 Å Band (ESP)', $
          XTITLE = '2017', XTICKFORMAT = ['LABEL_DATE', 'LABEL_DATE'], XTICKUNITS = ['Day', 'Month'], $
          YTITLE = 'Irradiance [%]')

STOP
END