;+
; NAME:
;   minxss_abundance_exploration
;
; PURPOSE:
;   Explore the abundance values over the mission with a focus on flares. 
;   Does anything about the flares distinguish what the abundance does? 
;   Confined vs eruptive? 
;   Timing?
;
; INPUTS:
;   None, but requires getenv('minxss_data') + '/merged/minxss1_L1_2temp_fits.sav' (here temp refers to temperature)
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   A variety of plots
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires MinXSS code package
;
; EXAMPLE:
;   Just run it!
;
; MODIFICATION HISTORY:
;   2016-12-05: James Paul Mason: Wrote script.
;-
PRO minxss_abundance_exploration

; Defaults
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Analysis/MinXSS Flare Abundances/'
fexxvFluxThreshold = 1000.

; Restore data
restore, getenv('minxss_data') + '/merged/minxss1_L1_2temp_fits.sav'
restore, '/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/GOES_events_MinXSS_era.sav'

; Load flare classification for eruptive or confined and rename fields to be less generic
flareClassification = read_csv('/Users/' + getenv('username') + '/Dropbox/Research/Data/GOES/GOES_events_confined_or_eruptive_MinXSS_era.csv', RECORD_START = 1)
struct_replace_field, flareClassification, 'FIELD1', flareClassification.field1, NEWTAG='flarePeakTimeHuman'
struct_replace_field, flareClassification, 'FIELD2', flareClassification.field2, NEWTAG='flareClass'
struct_replace_field, flareClassification, 'FIELD3', flareClassification.field3, NEWTAG='eruptiveOrConfined'
struct_replace_field, flareClassification, 'FIELD4', flareClassification.field4, NEWTAG='notes'

; Hard code coronal / photosheric ratio of low-FIP abundance
fipPhotosphere = 1.0
fipCorona = 2.138

; Mask out bad data (are 0's bad?)
goodData = where(x123_abundance NE 0)
x123_jd = x123_jd[goodData]
x123_abundance = x123_abundance[goodData]

; Plot abundance over the MinXSS-1 mission
p1 = plot(x123_jd, x123_abundance, SYMBOL = 'diamond', LINESTYLE = 'none', POSITION = [0.12, 0.35, 0.92, 0.92], /BUFFER, $ 
          TITLE = 'Solar Abundance over MinXSS-1 Mission (to date)', $ 
          XTICKUNITS = 'Months', XMAJOR = 8, XSHOWTEXT = 0, $
          YTITLE = 'Abundance Factor', YRANGE = [0, 5])
p2 = plot(x123_jd, JPMrange(fipPhotosphere, fipPhotosphere, NPTS = n_elements(x123_jd)), '2--', COLOR = 'tomato', /OVERPLOT)
p3 = plot(x123_jd, JPMrange(fipCorona, fipCorona, NPTS = n_elements(x123_jd)), '2--', COLOR = 'dodger blue', /OVERPLOT)
t2 = text(p1.xrange[1], fipPhotosphere, 'Photospheric', /DATA, ALIGNMENT = 1, COLOR = 'tomato')
t3 = text(p1.xrange[1], fipCorona, 'Coronal', /DATA, ALIGNMENT = 1, COLOR = 'dodger blue')

; In the same window, add plot of the GOES long channel
p5 = plot(goes_jd, goes_xrsb, /CURRENT, POSITION = [0.12, 0.1, 0.92, 0.28], $
          TITLE = 'GOES XRS-B (1-8 Å, long)', $
          XTITLE = '2016', XRANGE = p1.xrange, XTICKUNITS = 'Months', XMAJOR = 8, $
          YTITLE = 'Irradiance [W m$^-2$]', YRANGE = [1e-8, 1e-3], /YLOG)
axisRight = axis('Y', LOCATION = 'right', TARGET = p5, TICKNAME = ['A', 'B', 'C', 'M', 'X', ''])
p1.save, saveloc + 'X123 Abundance Mission Length.png'

; Loop through every GOES flare and produce a plot of the MinXSS abundance with GOES 1-8 Å (XRS-B) light curve on a small lower plot
ticObject = tic()
buffer = 1
FOR flareIndex = 0, n_elements(goesEvents) - 1 DO BEGIN
  goesEvent = goesEvents[flareIndex]
  
  ; Identify the X123 data for the flare and skip if none available (possible because level 1 data didn't start until 2016 June 9)
  x123InRangeIndices = where(x123_jd GE goesEvent.eventStartTimeJd AND x123_jd LE (goesEvent.eventStartTimeJd + 1. / 24.), abundanceCount)
  IF abundanceCount LT 8 THEN CONTINUE ; Require having at least 8 points
  
  ; Identify "good" X123 abundances by the strength of the Fe XXV line they are derived from 
  goodFluxIndices = where(x123_fe_xxv[x123InRangeIndices] GT fexxvFluxThreshold) 
  
  ; Identify the XRS-B data for the flare
  goesInRangeIndices = where(goes_jd GE goesEvent.eventStartTimeJd AND goes_jd LE (goesEvent.eventStartTimeJd + 1. / 24.))
  IF goesInRangeIndices EQ [-1] THEN CONTINUE 
  
  ; Identify whether this is a confined or eruptive flare (if any classification was possible)
  flareClassificationIndex = where(flareClassification.flarePeakTimeHuman EQ goesEvent.eventPeakTimeHuman)
  IF flareClassificationIndex NE [-1] THEN BEGIN
    eruptiveOrConfined = flareClassification.eruptiveOrConfined[flareClassificationIndex]
    IF eruptiveOrConfined EQ 'NA' THEN eruptiveOrConfined = '' 
  ENDIF ELSE BEGIN
    erupiveOrConfined = ''
  ENDELSE
  
  ; Plot abundance for the flare
  !EXCEPT = 0 ; Turn off annoying overflow / illegal operand warnings
  p1 = plot(x123_jd[x123InRangeIndices], x123_abundance[x123InRangeIndices], SYMBOL = 'diamond', /SYM_FILLED, LINESTYLE = 'none', POSITION = [0.12, 0.45, 0.92, 0.92], $ 
            BUFFER = buffer, $
            TITLE = strtrim(goesEvent.st$class, 2) + ' ' + eruptiveOrConfined + ' Flare', $ 
            XTICKUNITS = 'Minutes', XMAJOR = 9, XSHOWTEXT = 0, $
            YTITLE = 'Abundance Factor', YRANGE = [0, 10], $ 
            NAME = 'MinXSS abundance')
  
  ; Highlight the "good" abundances
  IF goodFluxIndices NE [-1] THEN BEGIN 
    p1a = plot(x123_jd[x123InRangeIndices[goodFluxIndices]], x123_abundance[x123InRangeIndices[goodFluxIndices]], $
               SYMBOL = 'diamond', /SYM_FILLED, LINESTYLE = 'none', SYM_COLOR = 'lime green', /OVERPLOT, $
               NAME = 'MinXSS abundance (Fe XXV > ' + JPMPrintNumber(fexxvFluxThreshold, /NO_DECIMALS) + ' ph s$^{-1}$ cm$^{-2}$ keV$^{-1}$)')
  ENDIF 
  
  ; In the same window, add plot of the GOES long channel
  p5 = plot(goes_jd[goesInRangeIndices], goes_xrsb[goesInRangeIndices], /CURRENT, POSITION = [0.12, 0.15, 0.92, 0.37], $
            TITLE = '', $
            XTITLE = 'Time around flare peak at ' + goesEvent.eventPeakTimeHuman, XTICKUNITS = ['Minutes', 'Hours'], $
            YTITLE = 'Irradiance [W m$^-2$]', YRANGE = [1e-8, 1e-3], /YLOG, $
            NAME = 'GOES XRS-B (1-8 Å, long)')
  axisRight = axis('Y', LOCATION = 'right', TARGET = p5, TICKNAME = ['A', 'B', 'C', 'M', 'X', ''])
  
  ; Resize the X123 XRANGE to match the GOES one
  p1.xrange = p5.xrange
  
  ; Add annotations to plot
  IF goodFluxIndices NE [-1] THEN BEGIN
    legendTarget = [p1, p1a, p5]
  ENDIF ELSE BEGIN
      legendTarget = [p1, p5] 
  ENDELSE
  l1 = legend(TARGET = legendTarget, POSITION = [p1.xrange[1], 10], HORIZONTAL_ALIGNMENT = 1, /DATA)
  p2 = plot(p1.xrange, [fipPhotosphere, fipPhotosphere], '2--', COLOR = 'tomato', OVERPLOT = p1)
  p3 = plot(p1.xrange, [fipCorona, fipCorona], '2--', COLOR = 'dodger blue', OVERPLOT = p1)
  t2 = text(p1.xrange[1], fipPhotosphere, 'Photospheric', /DATA, TARGET = p1, ALIGNMENT = 1, COLOR = 'tomato')
  t3 = text(p1.xrange[1], fipCorona, 'Coronal', /DATA, TARGET = p1, ALIGNMENT = 1, COLOR = 'dodger blue')
  p1.save, saveloc + 'X123 Abundance ' + goesEvent.eventPeakTimeHuman + ' ' + strtrim(goesEvent.st$class, 2) + ' ' + eruptiveOrConfined + ' Flare.png', /TRANSPARENT
  p1.close
  !EXCEPT = 1
  
  ; Show progress bar because this will take awhile
  percentComplete = (flareIndex + 1.) / n_elements(goesEvents) * 100.
  progressBar = JPMProgressBar(percentComplete, progressBar = progressBar, ticObject = ticObject, runTimeText = runTimeText, etaText = etaText)
  
ENDFOR

END