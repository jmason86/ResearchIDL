;+
; NAME:
;   ComputeWorldEnergyConsumptionForNYears
;
; PURPOSE:
;   Dissertation numbers. 
;   A solar flare releases up to 6e25 J of energy. How many years of world energy consumption is that? 
;   Data on energy consumption from international energy agency 2015 key world statistics report: 
;   http://www.iea.org/publications/freepublications/publication/KeyWorld_Statistics_2015.pdf
;   Only have 4 data points. 
;
; INPUTS:
;   None
;
; OPTIONAL INPUTS:
;   numberOfYears [integer]: Provide this value if you want the return value to be the amount of power consumed in that number of years
;                            (assume going backwards from 2013). 
;   powerConsumed [float]:   Provide this in Joules if instead you want to know how many years (going back from 2013) it would take to 
;                            generate the specified powerConsumed. 
;
; KEYWORD PARAMETERS:
;   ANIMATE_BAR_CHART: Set this to produce a movie of the two energies scaling up in a bar chart
;
; OUTPUTS:
;   If numberOfYears optional input supplied
;     totalPowerConsumed [float]: The amount of power consumed in Joules in the specified time
;   If powerConsumed optional input supplied instead
;     numberOfYears [integer]: The number of years required to sum to the input powerConsumed, going backwards from 2013. 
;
; OPTIONAL OUTPUTS:
;   None 
;
; RESTRICTIONS:
;   None
;
; EXAMPLE:
;   numberOfYears = ComputeWorldEnergyConsumptionForNYears(powerConsumed = 6e25) ; the number of years from 2013 backwards required to produce the power of a solar flare
;
; MODIFICATION HISTORY:
;   2016-03-03: James Paul Mason: Wrote script.
;   2017-05-14: James Paul Mason: Added ANIMATE_BAR_CHART keyword and code
;-
FUNCTION ComputeWorldEnergyConsumptionForNYears, numberOfYears = numberOfYears, powerConsumed = powerConsumed, $
                                                 ANIMATE_BAR_CHART = ANIMATE_BAR_CHART

; Setup
dataloc = '/Users/jmason86/Dropbox/Research/Data/World Energy Consumption/'
mtoe2Joules = 4.187e16

; Read data (that I datathiefed) from the IEC
readcol, dataloc + 'Power Consumption (Mtoe).txt', year, powerMtoe, /SILENT

; Truncate dates to integers (that's as good as the data really are)
year = fix(year)

; Convert power to Joules
powerJ = powerMtoe * mtoe2Joules

;;
; Create optional animation
;;
IF keyword_set(ANIMATE_BAR_CHART) THEN BEGIN
  ; Setup movie
  movieObject = IDLffVideoWrite('World Energy vs Flare.mp4')
  vidStream = movieObject.AddVideoStream(900, 700, 30, BIT_RATE = 2e3)
  
  ; Compute total power
  powerTotalWorld = total(powerJ)
  
  ; Solar flare power
  powerFlare = 1e25 ; [J]
  
  ; Create first frame
  w = window(DIMENSIONS = [900, 700], BACKGROUND_COLOR = 'black', /BUFFER)
  xPlaceholder = [1, 2]
  p1 = barplot(xPlaceholder, [0, 0], FILL_COLOR = 'lime green', FONT_SIZE = 13, FONT_COLOR = 'white', /CURRENT, $ 
              XTICKVALUES = [1, 2], XTICKNAME = ['World total', 'Single solar flare'], XMINOR = 0, XCOLOR = 'white', $
              YTITLE = 'Energy [J]', YCOLOR = 'white')
              
  ; Loop through and create each frame
  subPowerTotalWorld = 0
  subPowerFlare = 0
  stepSize = 1e21
  ticker = tic()
  WHILE subPowerFlare LT powerFlare DO BEGIN
    
    IF subPowerTotalWorld + stepSize LT powerTotalWorld THEN BEGIN
      subPowerTotalWorld += stepSize
    ENDIF ELSE BEGIN 
      subPowerTotalWorld = powerTotalWorld
      stepSize = 1e22
    ENDELSE
    
    IF subPowerFlare + stepSize LT powerFlare THEN BEGIN
      subPowerFlare += stepSize
    ENDIF ELSE BEGIN
      subPowerFlare = powerFlare
    ENDELSE
    
    p1.SetData, xplaceholder, [subPowerTotalWorld, subPowerFlare]
    
    ; Insert frame into movie
    timeInMovie = movieObject.Put(vidStream, p1.CopyWindow()) ; time returned in seconds
    
    progressBar = JPMProgressBar(100. * subPowerFlare / powerFlare, progressBar = progressBar, NAME = 'Movie progress', $
                                 ticObject = ticker, runTimeText = runTimeText, etaText = etaText)

    
  ENDWHILE
  movieObject.Cleanup
  
ENDIF


; If optional input was the number of years, then sum up those years and return the total power
IF numberOfYears NE !NULL THEN BEGIN
  relevantYearIndices = where(year GE 2013 - numberOfYears)
  print, 'From ' + JPMPrintNumber(2013 - numberOfYears, /NO_DECIMALS) + ' to 2013 (' + JPMPrintNumber(numberOfYears, /NO_DECIMALS) + ' Years), ' + $
          strtrim(total(powerJ[relevantYearIndices]), 2) + ' Joules of energy were consumed worldwide'
  return, total(powerJ[relevantYearIndices])
ENDIF

; If optional input was the amount of power, then determine the number of years required to generate that
IF powerConsumed NE !NULL THEN BEGIN
  
  ; Need to lienar fit the data in order to go back in time enough for a flare
  linfitParameters = linfit(year, powerJ)
  yearFit = JPMrange(1932, 2013, NPTS = 81)
  powerFit = linfitParameters[0] + linfitParameters[1] * yearFit
  
  i = n_elements(powerFit) - 1
  powerSum = 0
  yearSum = 0
  WHILE powerSum LT powerConsumed DO BEGIN
    IF i EQ 0 THEN BEGIN
      print, 'That is ' + JPMPrintNumber(round(powerConsumed / total(powerJ)), /NO_DECIMALS) + ' more power than has been consumed in the entire recorded history (1973-2013)'
      return, -1
    ENDIF
    powerSum+=powerFit[i]
    i--
    yearSum++
  ENDWHILE
  print, 'It took ' + JPMPrintNumber(yearSum, /NO_DECIMALS) + ' years to generate ' + JPMPrintNumber(powerConsumed) + ' Joules of energy worldwide'
  return, powerSum
ENDIF

END