;+
; NAME:
;   EVEMultipleLightCurves
;
; PURPOSE:
;   Create EVE light curve plots for multiple wavelengths either overplotted or stacked
;
; INPUTS:
;   startYYYYDOY [int]: The start date to be used for getting EVE data. Can later specify a specific time window to plot.
;   endYYYYDOY   [int]: The end date to be used for getting EVE data. Can later specify a specific time window to plot. ;   
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   PERCENT               : Set this keyword to compute and plot a percent change instead of irradiance. 
;   REFERENCE_SECOND [int]: The second of day to use as the reference point when calculating a percent change. Default value = 0. 
;   PUBLICATION           : Set this keyword to increase font size on the plot. Also exports a .eps file in addition to the .png. 
;   STACKED_PLOT          : This is the default. Creates a vertical stack of plots, one for each Fe line in MEGS-A. 
;   OVERPLOT              : Alternative to stacked plot - instead creates one plot with all Fe lines in MEGS-A overplotted in different colors. 
;   
; OUTPUTS:
;   PNG plot of EVE light curves for Fe lines in MEGS-A. 
;
; OPTIONAL OUTPUTS:
;   If /PUBLICATION, a .eps version of the plot is also outputted. 
;
; RESTRICTIONS:
;   getgoes1m.pro from Tom Woods
;   closest.pro from James Paul Mason
;   perdiff.pro from James Paul Mason
;   solarsoft for GOES events
;
; EXAMPLE:
;   EVEMultipleLightCurves, 2010219, 2010219, /PERCENT, REFERENCE_SECOND = 61200
;   EVEMultipleLightCurves, 2011158, 2011158, /PERCENT, REFERENCE_SECOND = 18000
;   EVEMultipleLightCurves, 2011216, 2011216, /PERCENT, REFERENCE_SECOND = 10800, /PUBLICATION, /STACKED_PLOT
; 
; MODIFICATION HISTORY:
;   Written by:
;     James Paul Mason
;     2014/02/17
;-
PRO EVEMultipleLightCurves, startYYYYDOY, endYYYYDOY, PERCENT = PERCENT, REFERENCE_SECOND = REFERENCE_SECOND, PUBLICATION = PUBLICATION, STACKED_PLOT = STACKED_PLOT, OVERPLOT = OVERPLOT

; Setup
IF ~keyword_set(STACKED_PLOT) AND ~keyword_set(OVERPLOT) THEN STACKED_PLOT = 1
IF keyword_set(PERCENT) AND ~keyword_set(REFERENCE_SECOND) THEN REFERENCE_SECOND = 0
IF ~keyword_set(PERCENT) THEN xRangeStart = 0 ELSE xRangeStart = REFERENCE_SECOND
saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/2010219_07AUG_1824_M1.0/'
;saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/2011083_24MAR_1207_M1.0/'
;saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Case Studies/2011216_04AUG_0357_M9.3/'

IF keyword_set(PERCENT) THEN BEGIN
  GetEVEInPercentChange, startYYYYDOY, endYYYYDOY, REFERENCE_TIME = REFERENCE_SECOND, percentChangeOut=eveLines, jdOut=jd
  ; Get rid of the second day since I didn't really want it
  eveLines = eveLines[*,0:n_elements(eveLines[0, *]) / 2]
  sod = sod[0:n_elements(sod) / 2]
  eve94  = reform(eveLines[0, *]) ; brighten
  eve132 = reform(eveLines[2, *]) ; brighten
  eve171 = reform(eveLines[3, *]) ; dimm
  eve177 = reform(eveLines[4, *]) ; dimm
  eve180 = reform(eveLines[5, *]) ; dimm
  eve195 = reform(eveLines[6, *]) ; dimm
  eve202 = reform(eveLines[7, *]) ; dimm
  eve211 = reform(eveLines[8, *]) ; sometimes dim, sometimes brighten
  eve284 = reform(eveLines[10, *]) ; brighten
  eve335 = reform(eveLines[12, *]) ; brighten
  eve256 = reform(eveLines[9, *]) ; He II for obscuration
  eve304 = reform(eveLines[11, *]) ; He II for obscuration
ENDIF ELSE BEGIN
  ; Get EVE data
  eveData = eve_merge_evl(startYYYYDOY, endYYYYDOY, N_AVERAGE = 6, META = eveMeta)
  eveLines = eveData.line_irradiance
  sod = eveData.sod
  
  ; Remove bad EVE data
  eveLines[where(eveLines EQ -1)] = !VALUES.F_NAN
  
  ; Store specific lines
  eve94  = 1e6 * eveLines[0, *] ; brighten
  eve132 = 1e6 * eveLines[2, *] ; brighten
  eve171 = 1e6 * eveLines[3, *] ; dimm
  eve177 = 1e6 * eveLines[4, *] ; dimm
  eve180 = 1e6 * eveLines[5, *] ; dimm
  eve195 = 1e6 * eveLines[6, *] ; dimm
  eve202 = 1e6 * eveLines[7, *] ; dimm
  eve211 = 1e6 * eveLines[8, *] ; sometimes dim, sometimes brighten
  eve284 = 1e6 * eveLines[10, *] ; brighten
  eve335 = 1e6 * eveLines[12, *] ; brighten
ENDELSE

; Convert second of day (sod) into Julian fraction (jd)
jd = (sod + 43200.)/86400.

; Get GOES Data
;doy2date, strmid(strtrim(startYYYYDOY, 2), 3, /REVERSE_OFFSET), strmid(strtrim(startYYYYDOY, 2), 0, 4), startMonth, startDay, startYYMMDD
;doy2date, strmid(strtrim(endYYYYDOY,2 ), 3, /REVERSE_OFFSET), strmid(strtrim(endYYYYDOY, 2), 0, 4), endMonth, endDay, endYYMMDD
;formattedStartDate = strmid(strtrim(startYYYYDOY, 2), 0, 4) + '/' + strtrim(startMonth, 2) + '/' + strtrim(startDay, 2) + ' 0000:00.000'
;formattedEndDate = strmid(strtrim(endYYYYDOY, 2), 0, 4) + '/' + strtrim(endMonth, 2) + '/' + strtrim(endDay + 1, 2) + ' 0000:00.000'
;rd_gxd, formattedStartDate, formattedEndDate, goesData, /ONE_MINUTE
;goesTime = goesData.time / 1000 ; Output from rd_gxd is in milliseconds of day
;goesData = goesData.lo ; Only care about the low energy channel

; Alternative method using Tom's code
goesData = getgoes1m(startYYYYDOY, 'L', /SECTIME)
goesTime = reform(goesData[0, *]) ; [Seconds of day]
goesData = reform(goesData[1, *]) ; 

; Ignore GOES
;goesData = 0
;goesTime = 0 

; Convert goes data to percent change
IF keyword_set(PERCENT) THEN BEGIN
  goesReferenceSecondIndex = closest(REFERENCE_SECOND, goesTime)
  goesData = perdiff(goesData[goesReferenceSecondIndex], goesData)
ENDIF

; GOES events
IF endYYYYDOY EQ startYYYYDOY THEN endYYYYDOYgoes = endYYYYDOY + 1 ELSE endYYYYDOYgoes = endYYYYDOY
rd_gev, JPMyyydoy2dd_mon_yy(startYYYYDOY), JPMyyydoy2dd_mon_yy(endYYYYDOYgoes), goesEvents
selectFlares = goesEvents[where(string(goesEvents.ST$CLASS) GE 'M', numFlares)]
goesEventStartTime = selectFlares.TIME / 1000. ; [Seconds of day]   
goesEventPeakTime = goesEventStartTime + selectFlares.PEAK ; [Seconds of day]

; Set font size for plots
IF keyword_set(PUBLICATION) THEN fontSize = 24 ELSE fontSize = 16 

; -= STACKED PLOT =- ;
IF keyword_set(STACKED_PLOT) THEN BEGIN
  
  ; Percentage change or not
  IF keyword_set(PERCENT) THEN BEGIN
    yTitle = '% Change'
    savePercentOrNot = 'Percent Change'
    yRange171 = [-8, 4]
    yRange177 = [-8, 4]
    yRange180 = [-8, 4]
    yRange195 = [-8, 4]
    yRange202 = [-8, 4]
  ENDIF ELSE BEGIN
    yTitle = 'Irradiance [µW/m!U2!N/nm]'
    savePercentOrNot = 'Irradiance'
    yRange171 = minmax(eve171)
    yRange177 = minmax(eve177)
    yRange180 = minmax(eve180)
    yRange195 = minmax(eve195)
    yRange202 = minmax(eve202)
  ENDELSE
  xRange = [xRangeStart, max(sod)]
  xRange = [xRangeStart, 43200] ; Override with manual end time
  
  ; Publication styling
  IF keyword_set(PUBLICATION) THEN BEGIN
    topMargin = 0.02
    bottomMargin = 0.06
    plotSpacing = 0.03
    numberOfYTickLabels = 3
  ENDIF ELSE BEGIN
    topMargin = 0.02
    bottomMargin = 0.05
    plotSpacing = 0.02
    numberOfYTickLabels = 5
  ENDELSE
  
  numberOfPlots = 10
  plotHeight = (1 - topMargin - bottomMargin - (plotSpacing * numberOfPlots))/numberOfPlots
  
  w = window(DIMENSIONS = [1200, 3000])
  p1 = plot(sod, eve171, '2', /CURRENT, POSITION = [0.13, (9 * plotHeight) + (9 * plotSpacing) + bottomMargin, 0.99, (10 * plotHeight) + (9 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YRANGE = yRange171, YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'Fe IX 171 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], yRange171, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], yRange171, '--', /OVERPLOT)
  p2 = plot(sod, eve177, '2', /CURRENT, POSITION = [0.13, (8 * plotHeight) + (8 * plotSpacing) + bottomMargin, 0.99, (9 * plotHeight) + (8 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YRANGE = yRange177, YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'Fe X 177 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], yRange177, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], yRange177, '--', /OVERPLOT)
  p3 = plot(sod, eve180, '2', /CURRENT, POSITION = [0.13, (7 * plotHeight) + (7 * plotSpacing) + bottomMargin, 0.99, (8 * plotHeight) + (7 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YRANGE = yRange180, YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'Fe XI 180 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], yRange180, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], yRange180, '--', /OVERPLOT)
  p4 = plot(sod, eve195, '2', /CURRENT, POSITION = [0.13, (6 * plotHeight) + (6 * plotSpacing) + bottomMargin, 0.99, (7 * plotHeight) + (6 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YRANGE = yRange195, YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'Fe XII 195 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], yRange195, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], yRange195, '--', /OVERPLOT)
  p5 = plot(sod, eve202, '2', /CURRENT, POSITION = [0.13, (5 * plotHeight) + (5 * plotSpacing) + bottomMargin, 0.99, (6 * plotHeight) + (5 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YRANGE = yRange202, YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'Fe XIII 202 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], yRange202, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], yRange202, '--', /OVERPLOT)
  p6 = plot(sod, eve211, '2', /CURRENT, POSITION = [0.13, (4 * plotHeight) + (4 * plotSpacing) + bottomMargin, 0.99, (5 * plotHeight) + (4 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'Fe XIV 211 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], p6.YRANGE, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], p6.YRANGE, '--', /OVERPLOT)
  p7 = plot(sod, eve284, '2', /CURRENT, POSITION = [0.13, (3 * plotHeight) + (3 * plotSpacing) + bottomMargin, 0.99, (4 * plotHeight) + (3 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'Fe XV 284 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], p7.YRANGE, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], p7.YRANGE, '--', /OVERPLOT)
;  p8 = plot(sod, eve335, '2', /CURRENT, POSITION = [0.13, (2 * plotHeight) + (2 * plotSpacing) + bottomMargin, 0.99, (3 * plotHeight) + (2 * plotSpacing) + bottomMargin], $
;            XRANGE = xRange, XSHOWTEXT = 0, $
;            YMAJOR = numberOfYTickLabels, $
;            FONT_SIZE = fontSize, $
;            TITLE = 'Fe XVI 335 Å')
p8 = plot(sod, eve256, '2', /CURRENT, POSITION = [0.13, (2 * plotHeight) + (2 * plotSpacing) + bottomMargin, 0.99, (3 * plotHeight) + (2 * plotSpacing) + bottomMargin], $
          XRANGE = xRange, XSHOWTEXT = 0, $
          YMAJOR = numberOfYTickLabels, $
          FONT_SIZE = fontSize, $
          TITLE = 'He II 256 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], p8.YRANGE, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], p8.YRANGE, '--', /OVERPLOT)
  ;p9 = plot(sod, eve94, '2', /CURRENT, POSITION = [0.13, (1 * plotHeight) + (1 * plotSpacing) + bottomMargin, 0.99, (2 * plotHeight) + (1 * plotSpacing) + bottomMargin], $
;            XRANGE = xRange, XSHOWTEXT = 0, $
;            YMAJOR = numberOfYTickLabels, $
;            FONT_SIZE = fontSize, $
;            TITLE = 'Fe XVIII 94 Å')
  p9 = plot(sod, eve304, '2', /CURRENT, POSITION = [0.13, (1 * plotHeight) + (1 * plotSpacing) + bottomMargin, 0.99, (2 * plotHeight) + (1 * plotSpacing) + bottomMargin], $
            XRANGE = xRange, XSHOWTEXT = 0, $
            YMAJOR = numberOfYTickLabels, $
            FONT_SIZE = fontSize, $
            TITLE = 'He II 304 Å')
  d = plot(sod, intarr(n_elements(sod)), '--', /OVERPLOT)
  g1 = plot([goesEventStartTime, goesEventStartTime], p9.YRANGE, '--', /OVERPLOT)
  g2 = plot([goesEventPeakTime, goesEventPeakTime], p9.YRANGE, '--', /OVERPLOT)
  p10 = plot(jd, eve132, '2', /CURRENT, POSITION = [0.13, bottomMargin, 0.99, plotHeight + bottomMargin], $
             XTITLE = 'Time [UTC Hours]', XRANGE = [(xRange[0] + 43200) / 86400., (xRange[1] + 43200) / 86400.], XTICKUNITS = 'Hours', $
             YMAJOR = numberOfYTickLabels, $
             FONT_SIZE = fontSize, $
             TITLE = 'Fe XX 131 Å (Solid) GOES 1-8 Å Divided by 25 (Long Dash)')
  p11 = plot((goesTime + 43200.)/86400., goesData/25., '2 __', /OVERPLOT)
  d = plot(jd, intarr(n_elements(jd)), '--', /OVERPLOT)
  g1 = plot([(goesEventStartTime + 43200.)/86400, (goesEventStartTime + 43200.)/86400], p10.YRANGE, '--', /OVERPLOT)
  g2 = plot([(goesEventPeakTime + 43200.)/86400, (goesEventPeakTime + 43200.)/86400], p10.YRANGE, '--', /OVERPLOT)
  t = text(0.03, 0.5, '% Change', ALIGNMENT = 0.5, ORIENTATION = 90, FONT_SIZE = 30)
  STOP
  p1.save, saveloc + strtrim(startYYYYDOY, 2) + 'EVE Light Curves Stacked ' + savePercentOrNot + '.png'
  IF keyword_set(PUBLICATION) THEN p1.save, saveloc + strtrim(startYYYYDOY, 2) + 'Eve Light Curves Stacked ' + savePercentOrNot + '.eps'
ENDIF
; -= END STACKED PLOT =- ;

; -= OVERPLOT =- ;
IF keyword_set(OVERPLOT) THEN BEGIN
  IF keyword_set(PERCENT) THEN BEGIN
    yTitle = '% Change'
    savePercentOrNot = 'Percent Change'
  ENDIF ELSE BEGIN
    yTitle = 'Irradiance [µW/m!U2!N/nm]'
    savePercentOrNot = 'Irradiance'
  ENDELSE

  p1 = plot(sod, eve171, COLOR = JPMColors(0, /SIMPLE), '2', $
            TITLE = 'EVE Light Curves', $
            XTITLE = 'Time []', $ ;XTICKUNITS = 'Hours', XRANGE = timeRange ; Needs to be in JD
            YTITLE = yTitle, $
            FONT_SIZE = fontSize, $
            NAME = 'Fe IX 171 Å')
  p2 = plot(sod, eve177, COLOR = JPMColors(1, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe X 177 Å')
  p3 = plot(sod, eve180, COLOR = JPMColors(2, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe XI 180 Å')
  p4 = plot(sod, eve195, COLOR = JPMColors(3, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe XII 195 Å')
  p5 = plot(sod, eve202, COLOR = JPMColors(4, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe XIII 202 Å')
  p6 = plot(sod, eve211, COLOR = JPMColors(5, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe XIV 211 Å')
  p7 = plot(sod, eve284, COLOR = JPMColors(6, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe XV 284 Å')
  p8 = plot(sod, eve335, COLOR = JPMColors(7, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe XVI 335 Å')
  p9 = plot(sod, eve94, COLOR = JPMColors(8, /SIMPLE), '2', /OVERPLOT, $
            NAME = 'Fe XVIII 94 Å')
  p10 = plot(sod, eve132, COLOR = JPMColors(9, /SIMPLE), '2', /OVERPLOT, $
             XRANGE = xRange, $
             YRANGE = [-5, 5], $
             NAME = 'Fe XX 131 Å')
  leg = legend(TARGET = [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10], POSITION = [0.92, 0.88])
  p1.save, saveloc + strtrim(startYYYYDOY, 2) + 'EVE Light Curves Overplot' + savePercentOrNot + '.png'
  IF keyword_set(PUBLICATION) THEN p2.save, saveloc + startYYYYDOY + 'Eve Light Curves Overplot' + savePercentOrNot + '.eps'
ENDIF
; -= END OVERPLOT =- ;

save, goesEventStartTime, goesEventPeakTime, FILENAME = 'goesStartPeak.sav' 
END