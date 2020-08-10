;+
; NAME:
;   ComputeTotalFlareEnergyTwoTwoWeek
;
; PURPOSE:
;   Dissertation plot. 
;   Compute the total flare energy for the flare that correspond to dimmings in the 2-2 week period paper. 
;   The periods are 2011/02/10-25 and 2011/08/01-14. 17 of the identified events have flares. 
;   From Tom Woods:
;     energy = total(flux_time_series[i_start:i_end] - flux_time_series[i_start])*60.*factor1*factor2
;     Where Pre-flare level is first subtracted before doing the integration (total function),
;     60 is seconds in a minute (assuming your flux_time_series is the 1-min cadence data),
;     Factor1 is 2 * !pi * distance_1au_in_meters^2.   To account for flux measured at earth and radiated into 2-PI steradians from the sun's surface (good assumption for optically thin X-rays), and
;     Factor2 = 150. As based on Woods et al. Result for scaling X-Ray energy to TSI flare energy.
;     The energy units will then be in J. (Starting with flux in W/m^2)
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   REPROCESS_FLARE_ENERGY: Set this to rerun the flare energy calculations
;   DARK_BACKGROUND: Set this to make the plot background color transparent and flip the dark colors in the plot to light colors (e.g., black -> white text)
;   
; OUTPUTS:
;   flareEnergy [fltarr]: Flare energy in Joules for the 37 events of the four week period, plus the case study event (38 in total)
;
; OPTIONAL OUTPUTS:
;   Plot of CME KE versus flare energy?
;
; RESTRICTIONS:
;   Requires Tom's getgoes1m and LASP GOES files
;
; EXAMPLE:
;   Just run it! 
;
; MODIFICATION HISTORY:
;   2016/04/08: James Paul Mason: Wrote script.
;-
PRO ComputeTotalFlareEnergyTwoTwoWeek, REPROCESS_FLARE_ENERGY = REPROCESS_FLARE_ENERGY, DARK_BACKGROUND = DARK_BACKGROUND

; Defaults
IF keyword_set(DARK_BACKGROUND) THEN BEGIN
  foregroundBlackOrWhite = 'white'
  backgroundColor = 'white smoke' ; Will be used as the transparency mask for the png
ENDIF ELSE BEGIN
  foregroundBlackOrWhite = 'black'
  backgroundColor = 'white'
ENDELSE

; Setup
saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Two Two Week Period/Flare-CME Energy Partition/'
saveloc2 = '/Users/jmason86/Dropbox/Research/Woods_LASP/Papers/20160501 Dissertation/PhD_Dissertation/LaTeX/Images/'
saveloc3 = '/Users/jmason86/Dropbox/Research/Woods_LASP/Papers/20160115 Mason 2-2 Week Period/Preparation/Figures/EPSs/'

IF keyword_set(REPROCESS_FLARE_ENERGY) THEN BEGIN
  
  ; Parameters
  energyScalingFactor = 150. ; Based on Woods paper 
  lightSphereFactor = 2. * !PI * (1.496d11)^2 ; Account for 1/r^2  spreading of light at 1 AU
  period1StartDate = '7-aug-2010 00:00:00.000'
  period1EndDate = '7-aug-2010 24:00:00.000'
  period2StartDate = '10-feb-2011 00:00:00.000'
  period2EndDate = '25-feb-2011 24:00:00.000'
  period3StartDate = '1-aug-2011 00:00:00.000'
  period3EndDate = '14-aug-2011 24:00:00.000'
  
  event3StartTime = 7.707
  event3EndTime = 9.992
  event7StartTime = 17.3947
  event7EndTime = 19.3818
  event8StartTime = 6.76283
  event8EndTime = 8.05476
  event9StartTime = 17.0965
  event9EndTime = 18.6866
  event10StartTime = 1.64568
  event10EndTime = 4.03043
  event11StartTime = 14.1160
  event11EndTime = 15.4574
  event15StartTime = 18.9348
  event15EndTime = 19.7796
  event17StartTime = 4.42800
  event17EndTime = 10.8865
  event19StartTime = 13.1716
  event19EndTime = 16.6993
  event20StartTime = 3.58313
  event20EndTime = 6.51458
  event23StartTime = 11.3835
  event23EndTime = 12.1783
  event25StartTime = 16.7987
  event25EndTime = 19.3820
  event26StartTime = 22.4124
  event26EndTime = 23.5553
  event29StartTime = 3.03684
  event29EndTime = 3.53347
  event31StartTime = 17.9910
  event31EndTime = 19.2329
  event32StartTime = 7.85578
  event32EndTime = 9.34625
  event35StartTime = 9.44571
  event35EndTime = 13.0
  event38StartTime = 17.8
  event38EndTime = 22.63
  
  allEventStartTimes = [0, 0, event3StartTime, 0, 0, 0, event7StartTime, event8StartTime, event9StartTime, event10StartTime, event11StartTime, 0, 0, 0, event15StartTime, 0, $
                        event17StartTime, 0, event19StartTime, event20StartTime, 0, 0, event23StartTime, 0, event25StartTime, event26StartTime, 0, 0, event29StartTime, 0, event31StartTime, $
                        event32StartTime, 0, 0, event35StartTime, 0, 0, event38StartTime]
  allEventEndTimes = [0, 0, event3EndTime, 0, 0, 0, event7EndTime, event8EndTime, event9EndTime, event10EndTime, event11EndTime, 0, 0, 0, event15EndTime, 0, $
                      event17EndTime, 0, event19EndTime, event20EndTime, 0, 0, event23EndTime, 0, event25EndTime, event26EndTime, 0, 0, event29EndTime, 0, event31EndTime, $
                      event32EndTime, 0, 0, event35EndTime, 0, 0, event38EndTime]
  allEventYyyyDoys = [0, 0, 2011042L, 0, 0, 0, 2011044L, 2011045L, 2011045L, 2011046L, 2011047L, 0, 0, 0, 2011055L, 0, 2011214L, 0, 2011215L, 2011216L, 0, 0, 2011218L, 0, 2011218L, 2011218L, $
                      0, 0, 2011220L, 0, 2011220L, 2011221L, 0, 0, 2011223L, 0, 0, 2010219]

  ; Loop through all events
  allEventEnergies = !NULL
  eventNumber = 1
  FOR dayIndex = 0, n_elements(allEventYyyyDoys) - 1 DO BEGIN
    currentYyyyDoy = allEventYyyyDoys[dayIndex]
    IF currentYyyyDoy EQ 0 THEN BEGIN
      allEventEnergies = [allEventEnergies, 0]
      eventNumber++
      CONTINUE
    ENDIF
    currentEventStartTime = allEventStartTimes[dayIndex]
    currentEventEndTime = allEventEndTimes[dayIndex]
    
    goesData = getgoes1m(currentYyyyDoy, /HOUR)
    goesFlux = reform(goesdata[1,*])  ; XRS-B [W/m2]
    goesTime = reform(goesdata[0,*])  ; [hour of day]
  
    ; Figure out which indices correspond to event start and end time
    eventStartIndex = closest(currentEventStartTime, goesTime)
    eventEndIndex = closest(currentEventEndTime, goesTime)
    
    yyyymmdd = JPMyyyydoy2yyyymmdd(currentYyyyDoy, /RETURN_STRING)
    
    IF eventNumber EQ 16 THEN STOP
    eventEnergy = total(goesFlux[eventStartIndex:eventEndIndex] - min(goesFlux[eventStartIndex:eventEndIndex])) * 60. * energyScalingFactor * lightSphereFactor ; [J]
    allEventEnergies = [allEventEnergies, eventEnergy]
    IF eventEnergy LT 2e23 THEN STOP
    
    p1 = plot(goesTime, goesFlux, '2', DIMENSIONS = [1000, 700], /BUFFER, $
              TITLE = 'GOES/XRS-B Channel - ' + yyyymmdd,  $
              XTITLE = 'Time [Hour]', $
              YTITLE = 'Flux [W m$^{-2}$]')
    p2 = plot(goesTime[eventStartIndex:eventEndIndex], goesFlux[eventStartIndex:eventEndIndex], 'r2', /OVERPLOT)
    t1 = text(0.2, 0.8, 'Total Energy = ' + strtrim(eventEnergy, 2) + ' J')
    p1.save, saveloc + 'Event' + strtrim(eventNumber, 2) + '.png'
    
    eventNumber++
  ENDFOR
  
  ; Open csv file and write the data
  ;file_delete, saveloc + 'TwoTwoWeekFlareEnergy.csv', /QUIET
  close, 1 & openw, 1, saveloc + 'TwoTwoWeekFlareEnergy.csv'
  printf, 1, 'Event#, Energy[J]'
  FOR i = 0, n_elements(allEventEnergies) - 1 DO printf, 1, JPMPrintNumber(i + 1, /NO_DECIMALS), ',', strtrim(allEventEnergies[i], 2)
  close, 1

ENDIF ; REPROCESS_FLARE_ENERGY

; Read data and produce plot
readcol, saveloc + 'Two Two Week Period Event List - Clean Event List.csv', eventNumber, dateEvent, timeEvent, aialocation, goesClass, peakTime, flareLocation, flareEnergy, $
                                                                            cmeOnset, positionAngle, span, speed, mass, cmeKineticEnergy, depth, depthError, slope, slopeError, $ 
                                                                            format = 'a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a', $
                                                                            SKIPLINE = 1, DELIMITER = ',', /SILENT

; Filter for events with both flare and CME energy computed
cmeAndFlareIndices = where(flareEnergy NE '--' AND cmeKineticEnergy NE '--')

; Dashed lines for comparison
xline = JPMrange(1d20, 1e28, NPTS = 1000)
yline = JPMrange(1d20, 1e28, NPTS = 1000)
emslieYline = 0.35 * xline

; Low mass identification
lowMassIndices = where(mass[cmeAndFlareIndices] LT 1e15)
lowMassCmeKineticEnergy = float(cmeKineticEnergy[cmeAndFlareIndices]) & lowMassCmeKineticEnergy = lowMassCmeKineticEnergy[lowMassIndices]
lowMassFlareEnergy = float(flareEnergy[cmeAndFlareIndices]) & lowMassFlareEnergy = lowMassFlareEnergy[lowMassIndices]

w = window(BACKGROUND_COLOR = backgroundColor)
p1 = scatterplot(float(cmeKineticEnergy[cmeAndFlareIndices]), float(flareEnergy[cmeAndFlareIndices]), SYM_SIZE = 2, SYM_THICK = 2, /SYM_FILLED, SYM_FILL_COLOR = foregroundBlackOrWhite, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
                 TITLE = 'Solar Eruptive Event Energy Partition', $
                 XTITLE = 'CME Kinetic Energy [J]', /XLOG, XRANGE = [1e20, 1e26], XCOLOR = foregroundBlackOrWhite, $
                 YTITLE = 'Flare Energy [J]', /YLOG, YRANGE = [1e20, 1e26], YCOLOR = foregroundBlackOrWhite)
p2 = scatterplot(lowMassCmeKineticEnergy, lowMassFlareEnergy, SYM_SIZE = 2, SYM_THICK = 2, /SYM_FILLED, SYM_FILL_COLOR = 'grey', /OVERPLOT)
p1a = plot(xline, yline, 'r2--', /OVERPLOT, NAME = '1-1')
p1b = plot(xline, yline * 10, 'b2--', /OVERPLOT, NAME = '10-1')
p1c = plot(xline, emslieyline, '2--', COLOR = 'orange', /OVERPLOT, NAME = 'Emslie')
l1 = legend(TARGET = [p1a, p1b, p1c], POSITION = [0.3, 0.84], TRANSPARENCY = 100, TEXT_COLOR = foregroundBlackOrWhite)

IF keyword_set(DARK_BACKGROUND) THEN saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/'
p1.save, saveloc + 'Two Two Week Flare-CME Energy Partition.png', /TRANSPARENT
p1.save, saveloc2 + 'TwoTwoWeekFlare-CMEEnergyPartition.png'
p1.save, saveloc3 + 'TwoTwoWeekFlare-CMEEnergyPartition.eps'
;
; Predicted CME total energy based on flare and based on dimming
;
 
; Filter for events with both flare energy and dimming slope/depth
dimmingAndFlareIndices = where(flareEnergy NE '--' AND depth NE '--' AND slope NE '--')

; Compute CME energy based on flare and based on dimming
predictedCmeEnergyFromFlare = flareEnergy[dimmingAndFlareIndices] / 0.35
predictedCmeMassFromDimming = 2.59E15 * sqrt(depth[dimmingAndFlareIndices])
predictedCmeSpeedFromDimming = 2.36e6 * slope[dimmingAndFlareIndices]
predictedCmeEnergyFromDimming = 0.5 * (predictedCmeMassFromDimming / 1000.) * (predictedCmeSpeedFromDimming * 1000.)^2

; Low mass identification
lowMassIndices = where(mass[dimmingAndFlareIndices] LT 1e15 AND mass[dimmingAndFlareIndices] NE '--')
lowMassCmeEnergyFromDimming = predictedCmeEnergyFromDimming[lowMassIndices]
lowMassCmeEnergyFromFlare = predictedCmeEnergyFromFlare[lowMassIndices]

w = window(BACKGROUND_COLOR = backgroundColor)
p1 = scatterplot(predictedCmeEnergyFromDimming, predictedCmeEnergyFromFlare, SYM_SIZE = 2, SYM_THICK = 2, /SYM_FILLED, SYM_FILL_COLOR = foregroundBlackOrWhite, FONT_COLOR = foregroundBlackOrWhite, /CURRENT, $
                 XTITLE = 'CME Predicted Kinetic Energy from Dimming [J]', /XLOG, XRANGE = [1e22, 1e26], XCOLOR = foregroundBlackOrWhite, $
                 YTITLE = 'CME Predicted Total Energy from Flare [J]', /YLOG, YRANGE = [1e22, 1e26], YCOLOR = foregroundBlackOrWhite)
p2 = scatterplot(lowMassCmeEnergyFromDimming, lowMassCmeEnergyFromFlare, SYM_SIZE = 2, SYM_THICK = 2, /SYM_FILLED, SYM_FILL_COLOR = 'grey', /OVERPLOT)
p1a = plot(xline, yline, 'r2--', /OVERPLOT, NAME = '1-1')
IF keyword_set(DARK_BACKGROUND) THEN saveloc = '/Users/jmason86/Dropbox/Research/Woods_LASP/Presentations/20160425 PhD Defense/Images/'
p1.save, saveloc + 'Two Two Week CME Predicted vs Computed Energy.png', /TRANSPARENT
IF ~keyword_set(DARK_BACKGROUND) THEN p1.save, saveloc2 + 'TwoTwoWeekCmePredictedVsComputedEnergy.png'
p1.save, saveloc3 + 'TwoTwoWeekCmePredictedVsComputedEnergy.eps'

END
