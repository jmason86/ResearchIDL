;+
; NAME:
;   Two2WeekPeriodsInEVE
;
; PURPOSE:
;   Plot EVE light curves in %change from coronal dimming event times identified in SDO/AIA by James Mason and David Webb. 
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
;   Plots [.png] Light curves of 171, 177, 180, 195, and 202Å for 58 identified events
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires readcol, GetEVEInPercentChange, JPMColors, closest
;
; EXAMPLE:
;   Two2WeekPeriodsInEVE
;
; MODIFICATION HISTORY:
;     2013/09/24: James Paul Mason: Wrote procedure
;-
PRO Two2WeekPeriodsInEVE

; Hard-coded options
useEVECorrection = 1 ; 0 = false, 1 = true

; Setup 
dataloc = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/'
saveloc = dataloc + 'EVEPlots/'
wavelengthNames = ['Fe IX 171', 'Fe X 177', 'Fe XI 180', 'Fe XII 195', 'Fe XIII 202']
textVerticalSpacing = [0.85, 0.82, 0.79, 0.76, 0.73]

; Read in dates and times
readcol, dataloc + 'AngelosDaveJamesEvents_21Aug2014_YYYYDOYSOD_ManualPreFlareTimes.txt', allYYYYDOY, allPreFlareSods, /SILENT
readcol, dataloc + 'AngelosDaveJamesEvents_21Aug2014_YYYYDOYSOD_PeakMatchCenters.txt', allYYYYDOY, allEventSelectionSods, /SILENT
readcol, dataloc + 'AngelosDaveJamesEvents_21Aug2014_YYYYDOYSOD_PeakMatchCenters.txt', allYYYYDOYstring, allEventSelectionSodsString, FORMAT = 'a, a', /SILENT

IF useEVECorrection EQ 0 THEN BEGIN
  ticObject22Week = TIC()
  
  ; Loop through all events
  FOR eventIndex = 0, n_elements(allPreFlareSods) - 1 DO BEGIN
        
    ; Get EVE in percent change
    GetEVEInPercentChange, allYYYYDOY[eventIndex], allYYYYDOY[eventIndex], REFERENCE_TIME = allPreFlareSods[eventIndex], NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE = 12, evePercentChange, sod
    
    ; Find minimum within 4 hours 
    minWithin4Hours = fltarr(5)
    minWithin4HoursTime = fltarr(5)
    j = 0
    FOR i = 3, 7 DO BEGIN 
      closestEventTime = closest(allPreFlareSods[eventIndex], sod, /UPPER)
      closestTo4Hours = closest(allPreFlareSods[eventIndex] + 4 * 3600, sod, /UPPER)
      minWithin4Hours[j] = min(evePercentChange[i, closestEventTime: closestTo4Hours])
      minWithin4HoursTime[j] = sod[where(evePercentChange[i,*] EQ minwithin4hours[j])]
      j++
    ENDFOR
    
    ; Create single plot showing 171, 177, 180, 195, 202
    p1 = plot(sod / 3600., evePercentChange[3, *], COLOR = JPMColors(0, /SIMPLE), /BUFFER, $
              TITLE = 'Event #' + strtrim(string(eventIndex + 1), 2) + ', YYYYDOY: ' + allYYYYDOYstring[eventIndex] + ', SOD: ' + allEventSelectionSodsString[eventIndex], $
              XTITLE = 'Hour', $
              YTITLE = '% Change from reference time (red line)', $
              NAME = 'Fe IX 171 Å')
    p2 = plot(sod / 3600., evePercentChange[4, *], COLOR = JPMColors(1, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe X 177 Å')
    p3 = plot(sod / 3600., evePercentChange[5, *], COLOR = JPMColors(2, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe XI 180 Å')
    p4 = plot(sod / 3600., evePercentChange[6, *], COLOR = JPMColors(3, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe XII 195 Å')
    p5 = plot(sod / 3600., evePercentChange[7, *], COLOR = JPMColors(4, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe XIII 202 Å')
    pl1 = plot([allPreFlareSods[eventIndex], allPreFlareSods[eventIndex]] / 3600., p5.YRANGE, COLOR = JPMColors(5, /SIMPLE), /OVERPLOT, $
               YRANGE = p5.YRANGE)
    FOR i = 0, 4 DO BEGIN
      pl2 = plot([minWithin4HoursTime[i], minWithin4HoursTime[i]] / 3600., p5.YRANGE, COLOR = JPMColors(i, /SIMPLE), /OVERPLOT, $
                 YRANGE = p5.YRANGE)
      t1 = text(0.2, textVerticalSpacing[i], wavelengthNames[i] + ' Min: ' + strtrim(string(minWithin4Hours[i]), 2), FONT_COLOR = JPMColors(i, /SIMPLE))
    ENDFOR
    t2 = text(0.2, textVerticalSpacing[-1] - 0.03, 'Reference Time', JPMColors(5, /SIMPLE))
    p1.save, saveloc + 'Event' + strtrim(string(eventIndex + 1), 2) + '.png'
    
    progressBar22Week = JPMProgressBar(100. * (eventIndex + 1) / n_elements(allPreFlareSods), progressBar = progressBar22Week, NAME = '2-2 Week Period Progress', $
                                       ticObject = ticObject22Week, runTimeText = runTimeText22Week, etaText = etaText22Week)
    
  ENDFOR ; Loop through all events
  
ENDIF ELSE BEGIN ; End no EVE correction, begin using EVE correction
  ticObject22Week = TIC()
  
  ; Loop through all events
  FOR eventIndex = 17, n_elements(allPreFlareSods) - 1 DO BEGIN 
    eventName = 'EVEPlots/Corrected/' + 'Event' + strtrim(string(eventIndex + 1), 2)
    correctedSaveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/' + eventName
    
    ; Prevent YRANGE blowout for certain events
    IF eventIndex EQ 27 OR eventIndex EQ 28 THEN yRange = [-5, 5] ELSE yRange = -1
    
    ; Call correction code
    EVECoreDimmingCorrection, allYYYYDOY[eventIndex], allYYYYDOY[eventIndex], allEventSelectionSods[eventIndex], REFERENCE_TIME = allPreFlareSods[eventIndex], $
                              eventName, NUMBER_OF_10s_INTEGRATIONS_TO_AVERAGE = 12, /EXTRAPOLATE_PRE_FLARE, YRANGE = yRange
    restore, correctedSaveloc + '/Warm correction/EVEScaledIrradiances.sav'
    
    ; Convert time to SOD
    sod = (eveTimeJD - floor(eveTimeJD[0]) - 0.5) * 86400.
    
    ; Identify the indices of 171, 177, 180, 195, and 202 all corrected by 284
    indicesOfInterest = [3, 8, 13, 18, 23]
    
    ; Find minimum within 4 hours
    minWithin4Hours = fltarr(5)
    minWithin4HoursTime = fltarr(5)
    FOR i = 0, 4 DO BEGIN
      closestEventTime = closest(allPreFlareSods[eventIndex], sod)
      closestTo4Hours = closest(allPreFlareSods[eventIndex] + 4 * 3600, sod)
      minWithin4Hours[i] = min(correctedEVEDimmingCurves[closestEventTime: closestTo4Hours, indicesOfInterest[i]])
      minWithin4HoursTime[i] = sod[where(correctedEVEDimmingCurves[*,indicesOfInterest[i]] EQ minwithin4hours[i])]
    ENDFOR
    
    ; Create a single plot showing 171, 177, 180, 195, 202
    p1 = plot(sod / 3600., correctedEVEDimmingCurves[*, indicesOfInterest[0]], COLOR = JPMColors(0, /SIMPLE), /BUFFER, $
              TITLE = 'Event #' + strtrim(string(eventIndex + 1), 2) + ', YYYYDOY: ' + allYYYYDOYstring[eventIndex] + ', SOD: ' + allEventSelectionSodsString[eventIndex], $
              XTITLE = 'Hour', $
              YTITLE = '% Change from reference time (orange line)', $
              NAME = 'Fe IX 171 Å')
    p2 = plot(sod / 3600., correctedEVEDimmingCurves[*, indicesOfInterest[1]], COLOR = JPMColors(1, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe X 177 Å')
    p3 = plot(sod / 3600., correctedEVEDimmingCurves[*, indicesOfInterest[2]], COLOR = JPMColors(2, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe XI 180 Å') 
    p4 = plot(sod / 3600., correctedEVEDimmingCurves[*, indicesOfInterest[3]], COLOR = JPMColors(3, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe XII 195 Å')
    p5 = plot(sod / 3600., correctedEVEDimmingCurves[*, indicesOfInterest[4]], COLOR = JPMColors(4, /SIMPLE), /OVERPLOT, $
              NAME = 'Fe XIII 202 Å')
    pl1 = plot([allPreFlareSods[eventIndex], allPreFlareSods[eventIndex]] / 3600., p5.YRANGE, COLOR = JPMColors(5, /SIMPLE), /OVERPLOT, $
              YRANGE = p5.YRANGE)
    FOR i = 0, 4 DO BEGIN
      pl2 = plot([minWithin4HoursTime[i], minWithin4HoursTime[i]] / 3600., p5.YRANGE, COLOR = JPMColors(i, /SIMPLE), /OVERPLOT, $
                 YRANGE = p5.YRANGE)
      t1 = text(0.2, textVerticalSpacing[i], wavelengthNames[i] + ' Min: ' + strtrim(string(minWithin4Hours[i]), 2), FONT_COLOR = JPMColors(i, /SIMPLE))
    ENDFOR
    t2 = text(0.2, textVerticalSpacing[-1] - 0.03, 'Reference Time', JPMColors(5, /SIMPLE))
    t3 = text(0.55, 0.85, 'Fe XV 284 Å Correction Applied')
    p1.save, saveloc + 'Corrected/Event' + strtrim(string(eventIndex + 1), 2) + 'Corrected284.png'
    
    ; Remove %compleition of correction method
    progressBarCorrection = getwindows('Dimming Correction Progress')
    progressBarCorrection.close
    
    ; Show %completion
    progressBar22Week = JPMProgressBar(100. * (eventIndex + 1) / n_elements(allPreFlareSods), progressBar = progressBar22Week, NAME = '2-2 Week Period Progress', $
                                       ticObject = ticObject22Week, runTimeText = runTimeText22Week, etaText = etaText22Week)
    
  ENDFOR ; Loop through all events
  
ENDELSE ; End EVE correction
END