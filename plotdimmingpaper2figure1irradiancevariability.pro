;+
; NAME:
;   PlotDimmingPaper2Figure1IrradianceVariability
;
; PURPOSE:
;   Make a plot showing the variability of irradiance over EVE lifetime, with CME occurrences overlaid, 
;   and highlighted two separate two week periods for reference
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   REPROCESS_EVE_LEVEL3: Set this to reprocess from EVE level 3 data product instead of loading from the saveset that 
;                         already ran this code. 
;   REPROCESS_CMES:       Set this to reprocess the CME data insted of loeading from the saveset that already ran this code. 
;
; OUTPUTS:
;   PNG and EPS versions of plot in 2 directories: 
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/
;   2. Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/ (for EPS and PNGs/ for the PNG)
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
;   2015/07/14: James Paul Mason: Wrote script.
;-
PRO PlotDimmingPaper2Figure1IrradianceVariability, REPROCESS_EVE_LEVEL3 = REPROCESS_EVE_LEVEL3, REPROCESS_CMES = REPROCESS_CMES
; Test comment
; Setup
saveloc1 = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/jama6159/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

; Grab EVE data and process
IF keyword_set(REPROCESS_EVE_LEVEL3) THEN BEGIN
  eveLevel3Files = file_search(getenv('EVE_DATA') + '/level3/*/EVE_L3_*.fit')
  eve171 = !NULL & eveJD = !NULL
  ticObject = TIC()
  FOR eveFilesLoop = 0, n_elements(eveLevel3Files) - 1 DO BEGIN
    eveDailyData = eve_read_whole_fits(eveLevel3Files[eveFilesLoop])
    eve171 = [eve171, eveDailyData.data.line_irradiance[3]]
    eveJD = [eveJD, JPMyyyyDoy2JD(eveDailyData.data.yyyydoy)]
    
    progressBar = JPMProgressBar(100. * (eveFilesLoop + 1) / n_elements(eveLevel3Files), progressBar = progressBar, NAME = 'EVE Daily Progress', $
                                 ticObject = ticObject, runTimeText = runTimeText, etaText = etaText)
  ENDFOR
  
  ; Get rid of bad data
  eve171[where(eve171 EQ -1 OR eve171 EQ 0)] = !VALUES.F_NAN
  
  ; Convert to percent change from average
  evePercent171 = perdiff(mean(eve171, /NAN), eve171)
  
  ; Save data to IDL saveset
  save, eveJD, eve171, evePercent171, FILENAME = saveloc1 + 'Historical EVE Data 171 Å Daily Average.sav'
ENDIF ELSE restore,                              saveloc1 + 'Historical EVE Data 171 Å Daily Average.sav'

; Grab CME data and process
IF keyword_set(REPROCESS_CMES) THEN BEGIN
  readcol, saveloc1 + 'Historical CME Data During EVE Era.csv', cmeDate, cmeUtc, format = 'a, a', DELIMITER = ',', /SILENT
  
  ; Loop through all CME occurrences and sum them up per day, store in arrays cmeJD and cmesPerDay
  previousDay = '01' & numberOfCmesInDay = 0 & cmeJD = !NULL & cmesPerDay = !NULL
  FOR cmeLoop = 0, n_elements(cmeDate) - 1 DO BEGIN
    day = strmid(cmeDate[cmeLoop], 1, 2, /REVERSE_OFFSET)
    IF day EQ previousDay THEN BEGIN
      numberOfCmesInDay++
    ENDIF ELSE BEGIN
      jdStructure = anytim2jd(cmeDate[cmeLoop - 1])
      cmeJD = [cmeJD, double(jdStructure.int + jdStructure.frac)]
      cmesPerDay = [cmesPerDay, numberOfCmesInDay]
      numberOfCmesInDay = 1
    ENDELSE
    previousDay = day
  ENDFOR
  
  ; Save data to IDL saveset
  save, cmeJD, cmesPerDay, FILENAME = saveloc1 + 'Historical CME Data Daily Total Occurrence.sav'
ENDIF ELSE restore,                   saveloc1 + 'Historical CME Data Daily Total Occurrence.sav'

; Convert 2-2 week period times to jd
periodStart1JD = JPMyyyydoy2jd(2011041)
periodStop1JD = JPMyyyydoy2jd(2011055)
periodStart2JD = JPMyyyydoy2jd(2011213)
periodStop2JD = JPMyyyydoy2jd(2011226)

; Produce plot
p1 = plot(eveJD, evePercent171, '3', TITLE = 'Historical Solar Variability', MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 1, $
          XTITLE = 'Time [Year]', XTICKUNITS = 'Years', $
          YTITLE = 'SDO/EVE Daily Average 171 Å [%]', $
          NAME = 'EVE 171 Å')
p3 = plot(p1.xrange, [0, 0], '--',  /CURRENT, MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 4,$
          XRANGE = p1.xrange, $
          YRANGE = p1.yrange)
p2 = plot(cmeJD, cmesPerDay, 'b2', /CURRENT, MARGIN = [0.1, 0.1, 0.1, 0.1], AXIS_STYLE = 4, $
          XRANGE = p1.xrange, $
          YRANGE = [0, 50], $
          NAME = 'CMEs Per Day')

ax1 = axis('Y', LOCATION = 'right', TARGET = [p2], TITLE = 'Number of CMEs per Day', COLOR = 'blue')
ax2 = axis('X', LOCATION = 'top', TARGET = [p2], TICKUNITS = 'Years', TEXT_COLOR = 'white')
t1 = text(0.1, 0.5, 'Historical Mean')
poly1 = polygon([[periodStart1JD, p2.yrange[0]], [periodStop1JD, p2.yrange[0]], [periodStop1JD, p2.yrange[1]], [periodStart1JD, p2.yrange[1]]], /DATA, TARGET = [p2], $
                /FILL_BACKGROUND, FILL_COLOR = 'lime green', FILL_TRANSPARENCY = 20)
poly2 = polygon([[periodStart2JD, p2.yrange[0]], [periodStop2JD, p2.yrange[0]], [periodStop2JD, p2.yrange[1]], [periodStart2JD, p2.yrange[1]]], /DATA, TARGET = [p2], $
                /FILL_BACKGROUND, FILL_COLOR = 'lime green', FILL_TRANSPARENCY = 20)

p1.save, saveloc1 + 'TwoTwoWeekInHistoricalContext.png'
p1.save, saveloc1 + 'TwoTwoWeekInHistoricalContext.eps'
p1.save, saveloc2 + 'PNGs/TwoTwoWeekInHistoricalContext.png'
p1.save, saveloc2 + 'EPSs/TwoTwoWeekInHistoricalContext.eps'
save, FILENAME = saveloc2 + 'IDLSavesets/Figure1Saveset.sav', /COMPRESS

END