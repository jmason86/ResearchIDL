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
;   DARK_BACKGROUND: Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;   
; OUTPUTS:
;   PNG and EPS versions of plot in 2 directories: 
;   1. Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/
;   2. Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/EPSs/ and PNGs/
;   Also produces a .sav of everything in
;   Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/IDLSavesets/
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
PRO PlotDimmingPaper2Figure1IrradianceVariability, REPROCESS_EVE_LEVEL3 = REPROCESS_EVE_LEVEL3, REPROCESS_CMES = REPROCESS_CMES, DARK_BACKGROUND = DARK_BACKGROUND

; Defaults
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  blueDarkOrLight = 'deep sky blue'
  backgroundColor = 'purple' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  blueDarkOrLight = 'blue'
  backgroundColor = 'white'
ENDELSE

; Setup
saveloc1 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc2 = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Papers/2015 Mason 2-2 Week Period/Preparation/Figures/'

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
  readcol, saveloc1 + 'Historical CME Data All.csv', cmeDate, cmeUtc, format = 'a, a', DELIMITER = ',', SKIPLINE = 1, /SILENT
  
  ; Loop through all CME occurrences and sum them up per day, store in arrays cmeJD and cmesPerDay
  previousDay = '01' & numberOfCmesInDay = 0 & cmeJD = !NULL & cmesPerDay = !NULL
  FOR cmeLoop = 0, n_elements(cmeDate) - 1 DO BEGIN
    year = strmid(cmeDate[cmeLoop], 0, 4)
    month = strmid(cmeDate[cmeLoop], 5, 2)
    day = strmid(cmeDate[cmeLoop], 1, 2, /REVERSE_OFFSET)
    
    ; Only start storing data for 2008 and to the end of EVE MEGS-A
    IF year LT 2008 THEN CONTINUE
    IF year EQ 2014 AND month EQ '06' THEN BREAK
    
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

w = window(BACKGROUND_COLOR = backgroundColor)

; Produce plot
p1 = plot(eveJD, evePercent171, '3', COLOR = foregroundblackorwhite, TITLE = 'Historical Solar Variability', MARGIN = 0.1, AXIS_STYLE = 1, /CURRENT, $
          XTITLE = 'Time: 2011', XRANGE = [2455562.5, 2455927.5], XTICKUNITS = 'Months', XCOLOR = foregroundBlackOrWhite, $
          YTITLE = 'SDO/EVE Daily Average 171 Å [%]', YCOLOR = foregroundBlackOrWhite, $
          NAME = 'EVE 171 Å')
p3 = plot(p1.xrange, [0, 0], '--',  COLOR = foregroundBlackOrWhite, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
          XRANGE = p1.xrange, $
          YRANGE = p1.yrange)
p2 = plot(cmeJD, cmesPerDay, '2', COLOR = blueDarkOrLight, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
          XRANGE = p1.xrange, $
          YRANGE = [0, 50], $
          NAME = 'CMEs Per Day')
p4 = plot(p2.xrange, [mean(cmesPerDay), mean(cmesPerDay)], '--', COLOR = blueDarkOrLight, /CURRENT, MARGIN = 0.1, AXIS_STYLE = 4, $
          XRANGE = p2.xrange, $
          YRANGE = p2.yrange)

ax1 = axis('Y', LOCATION = 'right', TARGET = [p2], TITLE = 'Number of CMEs per Day', COLOR = blueDarkOrLight)
ax2 = axis('X', LOCATION = 'top', TARGET = [p2], TICKUNITS = 'Months', SHOWTEXT = 0, COLOR = foregroundBlackOrWhite)
t1 = text(0.7, 0.5, '2010-2014 Mean', COLOR = foregroundBlackOrWhite)
t2 = text(0.67, 0.13, '2008-2015 Mean', COLOR = blueDarkOrLight)
poly1 = polygon([[periodStart1JD, p2.yrange[0]], [periodStop1JD, p2.yrange[0]], [periodStop1JD, p2.yrange[1]], [periodStart1JD, p2.yrange[1]]], /DATA, TARGET = [p2], $
                /FILL_BACKGROUND, FILL_COLOR = 'lime green', FILL_TRANSPARENCY = 20)
poly2 = polygon([[periodStart2JD, p2.yrange[0]], [periodStop2JD, p2.yrange[0]], [periodStop2JD, p2.yrange[1]], [periodStart2JD, p2.yrange[1]]], /DATA, TARGET = [p2], $
                /FILL_BACKGROUND, FILL_COLOR = 'lime green', FILL_TRANSPARENCY = 20)

STOP
IF keyword_set(DARK_BACKGROUND) THEN p1.save, '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/FourWeekContext.png', /TRANSPARENT $
ELSE BEGIN
  p1.save, saveloc1 + 'TwoTwoWeekInHistoricalContext.png'
  p1.save, saveloc2 + 'PNGs/TwoTwoWeekInHistoricalContext.png'
  p1.save, saveloc2 + 'EPSs/TwoTwoWeekInHistoricalContext.eps'
  save, FILENAME = saveloc2 + 'IDLSavesets/Figure1Saveset.sav', /COMPRESS
ENDELSE

END