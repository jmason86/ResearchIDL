; Program to produce plots comparing coronal loop lengths to cooling times. 
; 
; Coronal loop lengths are calculated using AIA images and ComputeAIALoopLength.pro
; Cooling times are computed from the peak times of EVE spectral light curves
; with ranged formation temperatures. 
;
; INPUT: 
; 
; OUTPUT: 
;   
; James Paul Mason
; 2012/3/20

PRO PlotLoopLengthVsCoolingRate

; Setup
dataloc = '/Users/jmason86/Dropbox/Research/Data/Cooling Rate Flare Loop Length Analysis/Late Phase/AIA Selected Loops/' ; Late Phase
;dataloc = '/Users/jmason86/Dropbox/Research/Data/Cooling Rate Flare Loop Length Analysis/Compact And Eruptive/AIA Selected Loops/' ; Compact and Eruptive
;dataloc = '/Users/jama6159/Dropbox/Research/Data/Cooling Rate Flare Loop Length Analysis/Late Phase/AIA Selected Loops/' ; Late Phase
;dataloc = '/Users/jmason86/Dropbox/Research/Data/Cooling Rate Flare Loop Length Analysis/Compact And Eruptive/AIA Selected Loops/' ; Compact and Eruptive

; Read in loop length and EVE spectral light curve peak data (must be prepared prior to running this program)
readcol, dataloc+'LoopLengths.txt', imageFlareID, loopLength, format='a,f', /silent
readcol, dataloc+'SpectralPeaks.txt', spectrumFlareID, atomicSpecies, spectralLine, formationTemperature, timeOfPeak, format='a,a,f,f,a', /silent

; Convert temperatures out of log
formationTemperature = double(10.^formationTemperature)

; Convert times to MJD to avoid issues
timeOfPeakMJD = dblarr(n_elements(timeOfPeak))
FOR i=0,n_elements(timeOfPeak)-1 DO BEGIN 
  converter = anytim2jd(timeOfPeak(i)) 
  timeOfPeakMJD(i) = converter.int + converter.frac
ENDFOR ;i loop

; Simple computation of cooling rate based on time of peak at different wavelengths
coolingRate = fltarr(n_elements(loopLength))
i=0 & j=0
WHILE j LE n_elements(spectrumFlareID)-1 DO BEGIN
  ; Group the spectral peak data for the same flare together 
  flareIDGroup = where(spectrumFlareID EQ spectrumFlareID(j))
  seriesOfPeakTimes = (timeOfPeakMJD(flareIDGroup) - timeOfPeakMJD(flareIDGroup(0)))*24*60 ; Modify JD to first spectral peak time and convert to minutes
  seriesOfFormationTemperatures = formationTemperature(flareIDGroup)
  
  ; Linearly interpolate to get the slope along the spectral peaks at different times and temperatures
  linearInterpolationResults = linfit(seriesOfPeakTimes, seriesOfFormationTemperatures)
  coolingRate(i) = linearInterpolationResults(1) ; slope value from linear interpolation
  
  ; Increment indices
  i=i+1
  j = j + n_elements(flareIDGroup)
ENDWHILE ; j < n_elements(spectrumFlareID)

STOP

; Produce plots
p1 = plot(coolingRate, loopLength, LINESTYLE='none',SYMBOL='Diamond', SYM_SIZE=2, SYM_COLOR='r', /SYM_FILLED, $
          TITLE = 'Coronal Loop Length vs. Cooling Rate from EVE Spectral Peak Times',$
          XTITLE = 'Cooling Rate [K/hour]',$
          YTITLE = 'Coronal Loop Length [km]')

END