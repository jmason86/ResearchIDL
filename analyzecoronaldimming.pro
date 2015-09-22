; Exploratory messy code.
;
; James Paul Mason
; 2012/11/20

PRO AnalyzeCoronalDimming

savedir = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Computed On16-Oct-2012 17:45:48/Analysis Plots/'

restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Computed On16-Oct-2012 17:45:48/CoronalDimmingParametersComputedOn_26-Oct-2012 19:00:20.sav'
coronalDimmingFlareArray2 = coronalDimmingFlareArray
restore, '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/Computed On16-Oct-2012 17:45:48/CoronalDimmingParametersComputedOn_16-Oct-2012 18:54:44.sav'
restore,  '/Users/jama6159/Dropbox/Research/Woods_LASP/Data/Coronal Dimming/merged_flare_catalog.sav'

; TEMPORARY: manually change formation temperatures
coronalDimmingFlareArray[where(coronalDimmingFlareArray.wavelength EQ 171)].formationTemperature = 6.31E5
coronalDimmingFlareArray[where(coronalDimmingFlareArray.wavelength EQ 177)].formationTemperature = 933254.
coronalDimmingFlareArray[where(coronalDimmingFlareArray.wavelength EQ 180)].formationTemperature = 1.1481E6
coronalDimmingFlareArray[where(coronalDimmingFlareArray.wavelength EQ 195)].formationTemperature = 1.26E6
coronalDimmingFlareArray[where(coronalDimmingFlareArray.wavelength EQ 202)].formationTemperature = 1.58E6
coronalDimmingFlareArray[where(coronalDimmingFlareArray.wavelength EQ 211)].formationTemperature = 1.8621E6
coronalDimmingFlareArray[where(coronalDimmingFlareArray.wavelength EQ 284)].formationTemperature = 2.1878E6

; TEMPORARY: manually fix the slope units back to W from µW
coronalDimmingFlareArray.Slope *= 1E-6 

; TEMPORARY: manually add flare class field to stucture
struct_add_field, coronalDimmingFlareArray, 'FlareClass', strarr(245)

; TEMPORARY: Create array of flags for good fit (1) or bad fit (0) of the slope/depth, brightening (2)
slopeFlag = [1,0,0,1,1,1,0,$
             1,0,0,0,0,1,1,$
             1,1,1,1,1,0,0,$
             0,0,0,0,0,0,0,$ ; TEMPORARY
             ;0,0,0,0,1,0,0,$ ; only 1 good
             1,1,1,1,0,2,2,$
             1,1,1,1,1,0,0,$
             1,1,1,1,1,2,2,$ ; Light curve shows 2 distinct slopes
             0,0,0,0,0,0,0,$ ; TEMPORARY
             ;0,0,0,0,1,2,2,$ ; only 1 good
             1,1,1,1,1,1,0,$
             1,1,1,1,1,1,2,$
             1,1,1,1,0,1,1,$
             1,0,1,1,0,0,0,$
             1,1,1,1,1,0,1,$
             1,0,2,2,2,2,2,$
             1,1,0,0,1,1,1,$ ; Light curve shows 2 distinct slopes at cool lines, two dimmings at middle temperatures
             1,1,1,0,0,0,2,$
             0,0,0,0,0,0,0,$ ; TEMPORARY
             ;1,1,0,0,0,0,0,$ ; only 2 good Mid temperature's irradiance above preflare
             0,0,0,0,0,0,0,$ ; TEMPORARY
             ;1,0,0,0,0,1,0,$ ; only 2 good Extremely small dimming event
             1,1,1,1,1,0,0,$ ; Light curve shows 2 distinct slopes
             1,1,1,1,1,0,0,$
             0,0,0,0,0,0,0,$ 
             1,1,1,1,0,0,0,$ ; 284Å shows 2 distinct slopes but slope doesn't fit either well
             0,0,0,0,0,0,0,$ 
             1,1,1,1,1,0,0,$ ; Light curve shows 2 distinct slopes
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,0,0,1,0,1,0,$ ; Data dropout ruins slope in some wavelengths
             0,1,1,1,1,1,0,$
             1,1,1,1,0,0,0,$
             1,1,0,0,1,1,1,$ ; Whether slopes were good or not is debatable
             1,1,1,1,1,0,0,$
             1,1,1,1,1,0,0,$
             1,1,1,1,1,1,2,$ ; Light curve shows 2 distinct slopes
             1,1,1,1,1,1,1] ; Light curve shows 2 distinct slopes
 
depthFlag = [1,1,1,1,1,1,1,$
             1,0,0,0,1,1,1,$
             1,1,1,1,1,0,0,$
             1,1,1,1,1,1,0,$
             1,1,1,1,0,2,2,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,2,2,$
             1,1,1,1,1,2,2,$
             1,1,1,1,1,1,2,$
             1,1,1,1,1,1,2,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,0,2,2,2,2,2,$
             1,1,1,1,1,1,1,$
             1,1,1,0,1,0,2,$
             1,1,1,0,0,0,0,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,0,0,$
             1,1,1,1,1,1,1,$
             1,1,1,0,0,0,0,$
             1,1,1,1,0,1,1,$
             1,1,1,1,1,1,0,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,0,$
             1,1,1,1,1,1,0,$
             1,1,1,1,1,0,0,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,1,$
             1,1,1,1,1,1,2,$
             1,1,1,1,1,1,1]
struct_add_field, coronalDimmingFlareArray, 'SlopeFlag', slopeFlag
struct_add_field, coronalDimmingFlareArray, 'DepthFlag', depthFlag

; NAN out the bad slopes and depths
FOR i = 0, 244 DO BEGIN
  currentSlopeFlag = slopeFlag(i)
  currentDepthFlag = depthFlag(i)
  IF currentSlopeFlag NE 1 THEN coronalDimmingFlareArray[i].slope = !VALUES.F_NAN
  IF currentDepthFlag NE 1 THEN coronalDimmingFlareArray[i].depth = !VALUES.F_NAN
ENDFOR ; i loop

; Reference
wavelengths = [171, 177, 180, 195, 202, 211, 284]

; Get GOES flare class from FlareIDs
FOR i = 0, 244, 7 DO BEGIN
  flareID = coronalDimmingFlareArray(i).flareID
  flareClass = strmid(flareID, 3, /REVERSE_OFFSET)
  coronalDimmingFlareArray[i:i+6].FlareClass = flareClass
ENDFOR ; i loop

; Get preflare irradiance from catalog TODO: the preflare irradiance calculated in GenerateCoronalDimmingParameters is different than the value in the flare catalog
catalogSpectralIndices = [3, 4, 5, 6, 7, 8, 10]
catalogFlareIDs = flare_catalog.flare_id
dimmingFlareIDs = coronalDimmingFlareArray.FlareID
preflareIrradiance = dblarr(35, 7)
FOR i = 0, 34 DO BEGIN
  catalogFlareIDIndices = where(catalogFlareIDs EQ dimmingFlareIDs(i))
  tmp = flare_catalog[catalogFlareIDIndices[0]].evl.evl_lines.preflare_irrad
  preflareIrradiance(i, *) = tmp[catalogSpectralIndices]
ENDFOR ; i loop

; Sort catalog by ascending formation temperature
FOR i = 0, 244, 7 DO BEGIN
  tmpCoronalDimmingFlareArray = coronalDimmingFlareArray[i:i+6]
  sortedIndices = sort(tmpCoronalDimmingFlareArray.formationTemperature)
  coronalDimmingFlareArray[i:i+6] = tmpCoronalDimmingFlareArray(sortedIndices)
ENDFOR ; i loop

; Normalize slopes and depths in dimming structure
j = 0
FOR i = 0, 34 DO BEGIN
  currentFlare = coronalDimmingFlareArray[j].flareID
  currentSlopes = coronalDimmingFlareArray[j:j+6].slope
  slopeOf171 = currentSlopes[where(coronalDimmingFlareArray[j:j+6].wavelength EQ 171)]
  normalizedSlopes = currentSlopes / slopeOf171[0]
  ;coronalDimmingFlareArray[j:j+6].slope = normalizedSlopes
  
  normalizedDepths = dblarr(7)
  FOR k = 0, 6 DO BEGIN
    currentDepths = coronalDimmingFlareArray[j:j+6].depth
    normalizedDepths(k) = currentDepths(k) / preflareIrradiance(i, k) * 100 ; Convert to percentage
  ENDFOR ; k loop
  ;coronalDimmingFlareArray[j:j+6].depth = normalizedDepths
  
  j = j + 7 
ENDFOR ; i loop

; Compute statistics 
medianSlope = fltarr(7)
medianDepth = fltarr(7)
momentsSlope = fltarr(7, 4)
momentsDepth = fltarr(7, 4)
sDevSlope = fltarr(7)
sDevDepth = fltarr(7)
FOR i = 0, 6 DO BEGIN ; loop through temperatures
  currentTemperature = coronalDimmingFlareArray[i].formationTemperature
  tIndices = where(coronalDimmingFlareArray.formationTemperature EQ currentTemperature)
  medianSlope(i) = median(coronalDimmingFlareArray[tIndices].slope)
  momentsSlope(i, *) = moment(coronalDimmingFlareArray[tIndices].slope, SDEV = sDevTmp, /NAN)
  sDevSlope(i) = sDevTmp
  medianDepth(i) = median(coronalDimmingFlareArray[tIndices].depth)
  momentsDepth(i, *) = moment(coronalDimmingFlareArray[tIndices].depth, SDEV = sDevTmp)
  sDevDepth(i) = sDevTmp
ENDFOR ; i loop

; Define color array in order of energy ROYGBIV
colors = ['Dark Red', 'Crimson', 'Red', 'Orange Red', 'Dark Orange', 'Orange', 'Dark Goldenrod', 'Goldenrod', 'Gold', 'Yellow', $
          'Green Yellow', 'Lime Green', 'Spring Green', 'Light Green', 'Green', 'Sea Green', 'Light Sea Green', 'Teal', 'Dark Cyan', 'Dark Turquoise', $
          'Aqua', 'Cyan', 'Light Blue', 'Light Sky Blue', 'Deep Sky Blue', 'Dodger Blue', 'Royal Blue', 'blue', 'Dark Blue', 'Navy', $
          'Indigo', 'Dark Violet', 'Blue Violet', 'Dark Orchid', 'Purple']

;p1 = plot(coronalDimmingFlareArray[0:6].formationTemperature, medianDepth, THICK = 2, $
;          title = 'Coronal Dimming Percentage Depth Versus Formation Temperature for 35 Flares', $
;          xtitle = 'Formation Temperature [K]', $
;          ytitle = 'Percentage Depth From Preflare Irradiance')
;          ;ytitle = "Depth Normalized to Depth of 171 Å's")
;p1 = plot(coronalDimmingFlareArray[0:6].formationTemperature, medianDepth + sDevDepth, THICK = 2, /OVERPLOT)
;p1 = plot(coronalDimmingFlareArray[0:6].formationTemperature, medianDepth - sDevDepth, THICK = 2, /OVERPLOT)
;
;j=0
;FOR i = 0, 244, 7 DO BEGIN
;  p2 = plot(coronalDimmingFlareArray[i:i+6].formationTemperature, coronalDimmingFlareArray[i:i+6].Depth, /OVERPLOT, LINESTYLE = 'none', SYMBOL = 'Square',  SYM_COLOR = colors(j), /SYM_FILLED, $
;            NAME = '')            
;  j ++
;ENDFOR ; i loop

; Break up plots into flare classes
;leC5Indices = where(coronaldimmingflarearray.flareclass LE 'C5.0')
;p3 = plot(coronalDimmingFlareArray[leC5Indices].formationTemperature, coronalDimmingFlareArray[leC5Indices].Slope, LINESTYLE = 'none', SYMBOL = 'Square',  SYM_COLOR = colors(0), /SYM_FILLED)
;p3.title = 'Coronal Dimming Normalized Slope Versus Formation Temperature for 20 Flares < C5.0'
;p3.xtitle = 'Formation Temperature [K]'
;p3.ytitle = "Slope Normalized to Slope of 171 Å's"
;geC5Indices = where(coronaldimmingflarearray.flareclass GE 'C5.0')
;p4 = plot(coronalDimmingFlareArray[geC5Indices].formationTemperature, coronalDimmingFlareArray[geC5Indices].Slope, LINESTYLE = 'none', SYMBOL = 'Square',  SYM_COLOR = colors(20), /SYM_FILLED)
;p4.title = 'Coronal Dimming Normalized Slope Versus Formation Temperature for 15 Flares > C5.0'
;p4.xtitle = 'Formation Temperature [K]'
;p4.ytitle = "Slope Normalized to Slope of 171 Å's"

; Get CME parameters
flareIDsCMEs = flare_catalog.CME.FLARE_ID
velocity_CDAW = flare_catalog.CME.CME_LASCO_CDAW.VELOCITY
velocity_CACTUS = flare_catalog.CME.CME_LASCO_CACTUS.VELOCITY
mass_CDAW = flare_catalog.CME.CME_LASCO_CDAW.MASS
mass_CACTUS = flare_catalog.CME.CME_LASCO_CACTUS.MASS
matchedCDAWVelocities = fltarr(245)
matchedCDAWMass = fltarr(245)
matchedCACTUSVelocities = fltarr(245)
matchedCACTUSMass = fltarr(245)
FOR i = 0, 244 DO BEGIN
  currentCMEForFlareIndices = where(flareIDsCMEs EQ coronalDimmingFlareArray[i].flareID)
  matchedCDAWVelocities(i) = velocity_CDAW[currentCMEForFlareIndices] ; 134 non -999 values stored
  matchedCDAWMass(i) = mass_CDAW[currentCMEForFlareIndices] ; 35 values stored
  matchedCACTUSVelocities(i) = velocity_CACTUS[currentCMEForFlareIndices] ; 99 values stored
  matchedCACTUSMass(i) = mass_CACTUS[currentCMEForFlareIndices] ; No values stored as of 2012/10/24
ENDFOR ; i loop

; Get where the CME catalogs have data
existentCDAWVelocityIndices = where(matchedCDAWVelocities NE -999)
existentCDAWMassIndices = where(matchedCDAWMass NE -999)
existentCACTUSVelocityIndices = where(matchedCACTUSVelocities NE -999)
existentCACTUSMassIndices = where(matchedCACTUSMass NE -999)

; Get the flare IDs for the events that have CME data
existentCDAWVelocityFlareIDs = coronalDimmingFlareArray[existentCDAWVelocityIndices].flareID
existentCDAWVelocityFlareIDs_short = strtrim(string(indgen(n_elements(existentCDAWVelocityFlareIDs)) + 1),2)

; Produce plots of slope vs CME velocity
FOR i = 0, 6 DO BEGIN
  ; Slopes
  existentVelocityAndSingleWavelengthIndices = where(coronalDimmingFlareArray[existentCDAWVelocityIndices].wavelength EQ wavelengths(i))
  existentCDAWVelocity = matchedCDAWVelocities[existentCDAWVelocityIndices]
  slopes = coronalDimmingFlareArray[existentCDAWVelocityIndices].Slope
  slopesExistentAndSingleWavelength = slopes[existentVelocityAndSingleWavelengthIndices]
  CDAWVelocityExistentAndSingleWavelength = existentCDAWVelocity[existentVelocityAndSingleWavelengthIndices]
  
  ; Compute correlation coefficients
  finiteSlope = where(finite(slopesExistentAndSingleWavelength) EQ 1, count)
  spearmanCoefficient = r_correlate(CDAWVelocityExistentAndSingleWavelength(finiteSlope), slopesExistentAndSingleWavelength(finiteSlope))
  pearsonCoefficient = correlate(CDAWVelocityExistentAndSingleWavelength(finiteSlope), slopesExistentAndSingleWavelength(finiteSlope)) 
  spearmanSignificance = PCORRE(spearmanCoefficient[0], count)
  pearsonSignificance = PCORRE(pearsonCoefficient[0], count)
    
  ; Plot slope versus CME velocity
  p5 = plot(CDAWVelocityExistentAndSingleWavelength, slopesExistentAndSingleWavelength*1E6, /CURRENT, LAYOUT = [3, 3, i+1], $
            linestyle='none', SYMBOL = 'Square', SYM_COLOR = 'Red', /SYM_FILLED, $
            FONT_SIZE = 16, $
            title = 'Dimming Slope of ' + strtrim(wavelengths(i),2) + 'Å, S=' + number_formatter(spearmanCoefficient[0], decimals=2) + '/' + number_formatter(spearmanSignificance, decimals=2) + ' P=' + number_formatter(pearsonCoefficient, decimals=2) + '/' + number_formatter(pearsonSignificance, decimals=2), $
            xtitle = 'CDAW CME Velocity [km/s]', $
            ytitle = 'Slope of Dimming [µW/m!U2!N/s]', $
            DIMENSIONS = [2560,1440], /BUFFER)
            ;ytitle = "Dimming Slope Normalized to Slope of 171 Å's", yrange = [0,5])
  


ENDFOR ; i loop
p5.save, savedir + 'SlopeVsVelocity.png'
p5.close

; Produce plot of depth vs. CME mass
FOR i = 0, 6 DO BEGIN
  ; Depths
  existentMassAndSingleWavelengthIndices = where(coronalDimmingFlareArray[existentCDAWMassIndices].wavelength EQ wavelengths(i))
  existentCDAWMass = matchedCDAWMass[existentCDAWMassIndices]
  depths = coronalDimmingFlareArray[existentCDAWMassIndices].Depth
  depthsExistentAndSingleWavelength = depths[existentMassAndSingleWavelengthIndices]
  CDAWMassExistentAndSingleWavelength = existentCDAWMass[existentMassAndSingleWavelengthIndices]
  flareIDs = coronalDimmingFlareArray[existentMassAndSingleWavelengthIndices].flareID
  
  ; Compute correlation coefficients and significance 
  finiteSlope = where(finite(depthsExistentAndSingleWavelength) EQ 1, count)
  spearmanCoefficient = r_correlate(CDAWMassExistentAndSingleWavelength(finiteSlope), depthsExistentAndSingleWavelength(finiteSlope))
  pearsonCoefficient = correlate(CDAWMassExistentAndSingleWavelength(finiteSlope), depthsExistentAndSingleWavelength(finiteSlope)) 
  spearmanSignificance = PCORRE(spearmanCoefficient[0], count)
  pearsonSignificance = PCORRE(pearsonCoefficient[0], count)
  
  ; Plot depth versus CME velocity (should be mass but too few mass points for now)
  p6 = plot(CDAWMassExistentAndSingleWavelength, depthsExistentAndSingleWavelength*1E6, /CURRENT, LAYOUT = [3, 3, i+1], $
            linestyle='none', SYMBOL = 'Square', SYM_COLOR = 'red', /SYM_FILLED, $
            FONT_SIZE = 16, $
            title = 'Dimming Depth of ' + strtrim(wavelengths(i),2) + 'Å, S=' + number_formatter(spearmanCoefficient[0], decimals=2) + '/' + number_formatter(spearmanSignificance, decimals=2) + ' P=' + number_formatter(pearsonCoefficient, decimals=2) + '/' + number_formatter(pearsonSignificance, decimals=2), $
            xtitle = 'CDAW CME Mass [g]', $
            ytitle = 'Dimming Depth [µW/m!U2!N]', $
            DIMENSIONS = [2560,1440], /BUFFER)
;  FOR j = 0, n_elements(flareIDs) - 1 DO BEGIN
;    matchedFlareIDIndices = where(existentCDAWVelocityFlareIDs EQ flareIDs(j))
;    STOP
;    t1 = text(CDAWVelocityExistentAndSingleWavelength(j), depthsExistentAndSingleWavelength(j), existentCDAWVelocityFlareIDs_short(matchedFlareIDIndices(0)), /DATA)
;  ENDFOR ; j loop
ENDFOR ; i loop
p6.save, savedir + 'DepthVsMass.png'
p6.close

; Plot slope versus depth
FOR i = 0, 6 DO BEGIN
  singleWavelengthIndices = where(coronalDimmingFlareArray.wavelength EQ wavelengths(i))
  depths = coronalDimmingFlareArray[singleWavelengthIndices].depth
  slopes = coronalDimmingFlareArray[singleWavelengthIndices].slope
  
  ; Compute correlation coefficients
  finiteSlopeAndDepth = where(finite(slopes) EQ 1 AND finite(depths) EQ 1, count)
  spearmanCoefficient = r_correlate(depths(finiteSlopeAndDepth), slopes(finiteSlopeAndDepth))
  pearsonCoefficient = correlate(depths(finiteSlopeAndDepth), slopes(finiteSlopeAndDepth)) 
  spearmanSignificance = PCORRE(spearmanCoefficient[0], count)
  pearsonSignificance = PCORRE(pearsonCoefficient[0], count)
    
  p6 = plot(depths*1E6, slopes*1E6, /CURRENT, LAYOUT = [3, 3, i+1], $
            LINESTYLE = 'none', SYMBOL = 'square', SYM_COLOR = 'Blue', /SYM_FILLED, $
            FONT_SIZE = 16, $
            title = 'Dimming Slope Vs Depth for ' + strtrim(wavelengths(i), 2) + 'Å, S=' + number_formatter(spearmanCoefficient[0], decimals=2) + '/' + number_formatter(spearmanSignificance, decimals=2) + ' P=' + number_formatter(pearsonCoefficient, decimals=2) + '/' + number_formatter(pearsonSignificance, decimals=2), $
            xtitle = 'Depth of Dimming [µW/m!U2!N]', $
            ytitle = 'Slope of Dimming [µW/m!U2!N/s]', $
            DIMENSIONS = [2560,1440], /BUFFER)
  
  
  ; TEMPORARY: 202 Å seems to show two distinct groupings of points. Plot them using a different symbol in all wavelengths
  ;group2Indices = [3,9,28,33]
  ;p7 = plot(depths[group2Indices], slopes[group2Indices], /OVERPLOT, $ /CURRENT, LAYOUT = [3, 3, i+1], $
  ;          LINESTYLE = 'none', SYMBOL = 'triangle', SYM_COLOR = 'Blue', /SYM_FILLED, SYM_SIZE = 2, $
  ;          FONT_SIZE = 16)
           
ENDFOR ; i loop
p6.save, savedir + 'SlopeVersusDepth.png'
p6.close

; Plot slope/depth vs CME velocity
FOR i = 0, 6 DO BEGIN
  existentVelocityAndSingleWavelengthIndices = where(coronalDimmingFlareArray[existentCDAWVelocityIndices].wavelength EQ wavelengths(i))
  existentCDAWVelocity = matchedCDAWVelocities[existentCDAWVelocityIndices]
  slopes = coronalDimmingFlareArray[existentCDAWVelocityIndices].Slope
  slopesExistentAndSingleWavelength = slopes[existentVelocityAndSingleWavelengthIndices]
  depths = coronalDimmingFlareArray[existentCDAWVelocityIndices].Depth
  depthsExistentAndSingleWavelength = depths[existentVelocityAndSingleWavelengthIndices]
  CDAWVelocityExistentAndSingleWavelength = existentCDAWVelocity[existentVelocityAndSingleWavelengthIndices]
  
  ; Compute correlation coefficients
  finiteSlopeAndDepth = where(finite(slopesExistentAndSingleWavelength) EQ 1 AND finite(depthsExistentAndSingleWavelength) EQ 1, count)
  spearmanCoefficient = r_correlate(depthsExistentAndSingleWavelength(finiteSlopeAndDepth), slopesExistentAndSingleWavelength(finiteSlopeAndDepth))
  pearsonCoefficient = correlate(depthsExistentAndSingleWavelength(finiteSlopeAndDepth), slopesExistentAndSingleWavelength(finiteSlopeAndDepth)) 
  spearmanSignificance = PCORRE(spearmanCoefficient[0], count)
  pearsonSignificance = PCORRE(pearsonCoefficient[0], count)
    
  p8 = plot(CDAWVelocityExistentAndSingleWavelength, slopesExistentAndSingleWavelength / depthsExistentAndSingleWavelength, /CURRENT, LAYOUT = [3, 3, i+1], $
            linestyle='none', SYMBOL = 'Square', SYM_COLOR = 'green', /SYM_FILLED, $
            FONT_SIZE = 16, $
            title = 'Dimming Slope / Depth of ' + strtrim(wavelengths(i),2) + 'Å, S=' + number_formatter(spearmanCoefficient[0], decimals=2) + '/' + number_formatter(spearmanSignificance, decimals=2) + ' P=' + number_formatter(pearsonCoefficient, decimals=2) + '/' + number_formatter(pearsonSignificance, decimals=2), $
            xtitle = 'CDAW CME Velocity [km/s]', $
            ytitle = 'Dimming Slope / Depth [Hz]', $
            DIMENSIONS = [2560,1440], /BUFFER)
ENDFOR ; i loop
p8.save, savedir + 'SlopeOverDepthVsVelocity.png'
p8.close

; Plot slope/preflare versus CME velocity
FOR i = 0, 6 DO BEGIN
  existentVelocityAndSingleWavelengthIndices = where(coronalDimmingFlareArray[existentCDAWVelocityIndices].wavelength EQ wavelengths(i))
  existentCDAWVelocity = matchedCDAWVelocities[existentCDAWVelocityIndices]
  slopes = coronalDimmingFlareArray[existentCDAWVelocityIndices].Slope
  slopesExistentAndSingleWavelength = slopes[existentVelocityAndSingleWavelengthIndices]
  preflares = coronalDimmingFlareArray2[existentCDAWVelocityIndices].PreflareIrradiance
  preflareExistentAndSingleWavelength = coronalDimmingFlareArray2[existentCDAWVelocityIndices].PreflareIrradiance
  CDAWVelocityExistentAndSingleWavelength = existentCDAWVelocity[existentVelocityAndSingleWavelengthIndices]
  
  ; Compute correlation coefficients
  finiteSlope = where(finite(slopesExistentAndSingleWavelength) EQ 1, count)
  spearmanCoefficient = r_correlate(CDAWVelocityExistentAndSingleWavelength(finiteSlope), slopesExistentAndSingleWavelength(finiteSlope))
  pearsonCoefficient =  correlate(  CDAWVelocityExistentAndSingleWavelength(finiteSlope), slopesExistentAndSingleWavelength(finiteSlope)) 
  spearmanSignificance = PCORRE(spearmanCoefficient[0], count)
  pearsonSignificance = PCORRE(pearsonCoefficient[0], count)
  
  p9 = plot(CDAWVelocityExistentAndSingleWavelength, slopesExistentAndSingleWavelength / preflareExistentAndSingleWavelength, /CURRENT, LAYOUT = [3, 3, i+1], $
            linestyle='none', SYMBOL = 'Square', SYM_COLOR = 'green', /SYM_FILLED, $
            FONT_SIZE = 16, $
            title = 'Dimming Slope / Preflare of ' + strtrim(wavelengths(i),2) + 'Å, S=' + number_formatter(spearmanCoefficient[0], decimals=2) + '/' + number_formatter(spearmanSignificance, decimals=2) + ' P=' + number_formatter(pearsonCoefficient, decimals=2) + '/' + number_formatter(pearsonSignificance, decimals=2), $
            xtitle = 'CDAW CME Velocity [km/s]', $
            ytitle = 'Dimming Slope / Preflare [Hz]', $
            DIMENSIONS = [2560,1440], /BUFFER)
ENDFOR ; i loop
p9.save, savedir + 'SlopeOverPreflareVsVelocity.png'
p9.close

; Plot slope/preflare versus depth
FOR i = 0, 6 DO BEGIN
  singleWavelengthIndices = where(coronalDimmingFlareArray.wavelength EQ wavelengths(i))
  depths = coronalDimmingFlareArray[singleWavelengthIndices].depth
  slopes = coronalDimmingFlareArray[singleWavelengthIndices].slope
  preflares = coronalDimmingFlareArray2[singleWavelengthIndices].PreflareIrradiance
  
  ; Compute correlation coefficients
  finiteSlopeAndDepth = where(finite(slopes / preflares) EQ 1 AND finite(depths) EQ 1, count)
  spearmanCoefficient = r_correlate(depths(finiteSlopeAndDepth), slopes(finiteSlopeAndDepth) / preflares)
  pearsonCoefficient = correlate(depths(finiteSlopeAndDepth), slopes(finiteSlopeAndDepth) / preflares) 
  spearmanSignificance = PCORRE(spearmanCoefficient[0], count)
  pearsonSignificance = PCORRE(pearsonCoefficient[0], count)
   
  p10 = plot(depths*1E6, slopes / preflares, /CURRENT, LAYOUT = [3, 3, i+1], $
             linestyle='none', SYMBOL = 'Square', SYM_COLOR = 'green', /SYM_FILLED, $
             FONT_SIZE = 16, $
             title = 'Dimming Slope / Preflare of ' + strtrim(wavelengths(i),2) + 'Å, S=' + number_formatter(spearmanCoefficient[0], decimals=2) + '/' + number_formatter(spearmanSignificance, decimals=2) + ' P=' + number_formatter(pearsonCoefficient, decimals=2) + '/' + number_formatter(pearsonSignificance, decimals=2), $
             xtitle = 'Depth [µW/m!U2!N]', $
             ytitle = 'Dimming Slope / Preflare [Hz]', $
            DIMENSIONS = [2560,1440], /BUFFER)
ENDFOR ; i loop
p10.save, savedir + 'SlopeOverPreflare VsDepth.png'
p10.close

; Plot slope/preflare versus depth/preflare
FOR i = 0, 6 DO BEGIN
  singleWavelengthIndices = where(coronalDimmingFlareArray.wavelength EQ wavelengths(i))
  depths = coronalDimmingFlareArray[singleWavelengthIndices].depth
  slopes = coronalDimmingFlareArray[singleWavelengthIndices].slope
  preflares = coronalDimmingFlareArray2[singleWavelengthIndices].PreflareIrradiance
  
  ; Compute correlation coefficients
  finiteSlopeAndDepth = where(finite(slopes / preflares) EQ 1 AND finite(depths / preflares) EQ 1, count)
  spearmanCoefficient = r_correlate(depths(finiteSlopeAndDepth) / preflares, slopes(finiteSlopeAndDepth) / preflares)
  pearsonCoefficient = correlate(   depths(finiteSlopeAndDepth) / preflares, slopes(finiteSlopeAndDepth) / preflares)
  spearmanSignificance = PCORRE(spearmanCoefficient[0], count)
  pearsonSignificance = PCORRE(pearsonCoefficient[0], count)
   
  p11 = plot(depths / preflares, slopes / preflares, /CURRENT, LAYOUT = [3, 3, i+1], $
             linestyle='none', SYMBOL = 'Square', SYM_COLOR = 'green', /SYM_FILLED, $
             FONT_SIZE = 16, $
             title = 'Dimming Slope / Preflare of ' + strtrim(wavelengths(i),2) + 'Å, S=' + number_formatter(spearmanCoefficient[0], decimals=2) + '/' + number_formatter(spearmanSignificance, decimals=2) + ' P=' + number_formatter(pearsonCoefficient, decimals=2) + '/' + number_formatter(pearsonSignificance, decimals=2), $
             xtitle = 'Dimming Depth / Preflare ', $
             ytitle = 'Dimming Slope / Preflare [Hz]', $
            DIMENSIONS = [2560,1440], /BUFFER)     
ENDFOR ; i loop
p11.save, savedir + 'SlopeOverPreflareVsDepthOverPreflare.png'
p11.close

END