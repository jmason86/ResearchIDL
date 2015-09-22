;+
; NAME: 
;   ProduceAIALightCurves2
;   
; PURPOSE:
;   Explore AIA light curves. 
;   
; INPUTS: 
;   event: (hardcoded) Set to either 1 or 2 to select 2011046_15FEB_0156_X2.2 or 2011221_09AUG_0805_X6.9, respectively. 
;   useIDLSave: (hardcoded) If the saveset already exists from a prior run, skip computing the spatially integrated intensities
;   ngrid: (hardcoded) Set to the number of boxes desired in each direction e.g. ngrid = 10 results in a 10x10 grid
;   
; OPTIONAL INPUTS:
;   None
;   
; KEYWORD PARAMETERS:
;   None
;   
; OUTPUTS:
;   Movie of 171Å AIA at full resolution with an ngrid x ngrid grid overlaid
;   Plot of full disk integrated intensity at various wavelengths
;   Plot of integrated intensity in each grid as a ngrid x ngrid multiplot, one version with auto-scaled y-axis and another with constant y-axis
;   
; OPTIONAL OUTPUTS: 
;   None
;   
; RESTRICTIONS:
;   Requires: 
;     range.pro
;     readcol.pro
;      
; EXAMPLE: 
;   ProduceAIALightCurves2
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2013/1/1
;-
PRO ProduceAIALightCurves2

timerStart = systime(1)

; Hardcode options for program execution
event = 1
useIDLSave = 1

; Setup
CASE event OF
  1: BEGIN
    flareID = '2011046_15FEB_0156_X2.2'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011046_15FEB_0156_X2.2/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011046_15FEB_0156_X2.2/'
    message, /INFO, 'Running on event 2011046_15FEB_0156_X2.2'
  END
  2: BEGIN
    flareID = '2011221_09AUG_0805_X6.9'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011221_09AUG_0805_X6.9/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011221_09AUG_0805_X6.9/'
    message, /INFO, 'Running on event: 2011221_09AUG_0805_X6.9' 
  END
  3: BEGIN
    flareID = '2011216_04AUG_0357_M9.3'
    dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011216_04AUG_0357_M9.3/prepped/'
    saveloc = '/Users/jama6159/Dropbox/Research/Woods_LASP/Analysis/Coronal Dimming Analysis/2011216_04AUG_0357_M9.3/'
    message, /INFO, 'Running on event: 2011216_04AUG_0357_M9.3' 
  END  
ENDCASE
spawn, 'rm '+dataloc+'filesToProcess.txt'
CD, dataloc
spawn, 'ls *.fits > filesToProcess.txt'
readcol, dataloc + 'filesToProcess.txt', allAIAfits, format='a', /SILENT
numberOfFilesProcessed = n_elements(allAIAfits)
wavelengths = strarr(numberOfFilesProcessed)
FOR i = 0, numberOfFilesProcessed - 1 DO wavelengths(i) = strmid(allAIAfits(i), 7, 3, /REVERSE_OFFSET)
wavelengths171Indices = where(wavelengths EQ '171')
wavelengths211Indices = where(wavelengths EQ '211')
wavelengths094Indices = where(wavelengths EQ '094')
wavelengths193Indices = where(wavelengths EQ '193')
wavelengths131Indices = where(wavelengths EQ '131')
wavelengths335Indices = where(wavelengths EQ '335')
alphabet = string(bindgen(1,26)+(byte('A'))[0])

; Define grid
ngrid = 10
grid = range(0., 4095., npts=ngrid+1, /INTEGER)

IF useIDLSave EQ 0 THEN BEGIN
  
  ; Open movie file
  movieObject = IDLffVideoWrite(saveloc + 'AIA171GridMovie_FullResolution1.mp4')
  vidStream = movieObject.AddVideoStream(4096, 4096, 15) ; boxWidth+1, boxHeight+1, fps)
  
  ; Loop through images
  AIAIntegratedIntensity171 = fltarr(n_elements(wavelengths171Indices)) ; AIA intensities are in DN
  AIAIntegratedIntensity211 = fltarr(n_elements(wavelengths171Indices)) ; AIA intensities are in DN
  AIAIntegratedIntensity094 = fltarr(n_elements(wavelengths171Indices)) ; AIA intensities are in DN
  AIAIntegratedIntensity193 = fltarr(n_elements(wavelengths171Indices)) ; AIA intensities are in DN
  AIAIntegratedIntensity131 = fltarr(n_elements(wavelengths171Indices)) ; AIA intensities are in DN
  AIAIntegratedIntensity335 = fltarr(n_elements(wavelengths171Indices)) ; AIA intensities are in DN
  AIAIntegratedIntensity171Box = fltarr(n_elements(wavelengths171Indices), ngrid, ngrid)
  AIAIntegratedIntensity211Box = fltarr(n_elements(wavelengths211Indices), ngrid, ngrid)
  AIAIntegratedIntensity094Box = fltarr(n_elements(wavelengths094Indices), ngrid, ngrid)
  AIAIntegratedIntensity193Box = fltarr(n_elements(wavelengths193Indices), ngrid, ngrid)
  AIAIntegratedIntensity131Box = fltarr(n_elements(wavelengths131Indices), ngrid, ngrid)
  AIAIntegratedIntensity335Box = fltarr(n_elements(wavelengths335Indices), ngrid, ngrid)
  tOBSJD = dblarr(n_elements(wavelengths171Indices))
  ;FOR i = 0, n_elements(wavelengths171Indices) - 1 DO BEGIN
  ;FOR i = (n_elements(wavelengths171Indices) - 1) * 0.5, (n_elements(wavelengths171Indices) - 1) DO BEGIN
  FOR i = 0, 213 DO BEGIN
  
    ; Read AIA image for all wavelengths
    AIAfits171 = readfits(dataloc + allAIAfits(wavelengths171Indices(i)), header, /SILENT)
    AIAfits211 = readfits(dataloc + allAIAfits(wavelengths211Indices(i)), header, /SILENT)
    AIAfits094 = readfits(dataloc + allAIAfits(wavelengths094Indices(i)), header, /SILENT)
    AIAfits193 = readfits(dataloc + allAIAfits(wavelengths193Indices(i)), header, /SILENT)
    AIAfits131 = readfits(dataloc + allAIAfits(wavelengths131Indices(i)), header, /SILENT)
    AIAfits335 = readfits(dataloc + allAIAfits(wavelengths335Indices(i)), header, /SILENT)
    
    ; Overlay grid on full disk and create movie
    tmp171 = AIAfits171
    tmp171(where(AIAfits171 LE 0)) = 1 ; To prevent arithmetic error by taking log of small numbers
    i1 = image(alog10(tmp171), DIMENSIONS = [4096, 4096], RGB_TABLE = 3, /BUFFER)
    FOR j = 0, ngrid - 1 DO BEGIN  
      l1 = polyline([0, 4095], [grid(j), grid(j)], THICK = 10, COLOR = 'green', /DATA)
      l2 = polyline([grid(j), grid(j)], [0, 4095], THICK = 10, COLOR = 'green', /DATA)
      t1 = text(6, grid(j), alphabet(j), FONT_SIZE = 50, COLOR = 'green', /DATA)
      t2 = text(grid(-1) - 65, grid(j), alphabet(j), FONT_SIZE = 50, COLOR = 'green', /DATA)
      t3 = text(grid(j), 0, string(j + 1), FONT_SIZE = 50, COLOR = 'green', /DATA)
      t4 = text(grid(j), grid(-1) - 65, string(j + 1), FONT_SIZE = 50, COLOR = 'green', /DATA)
    ENDFOR ; j loop through grid spacing
    tOBS = strmid(header(8), 11, 16)
    t5 = text(0, 100, tOBS, FONT_SIZE = 50, COLOR = 'white', /DATA)
    timeInMovie = movieObject.Put(vidStream, i1.CopyWindow()) ; time returned in seconds
    
    ; Loop through grid boxes - X direction
    FOR j = 0, ngrid - 1 DO BEGIN 
      
      ; Loop through grid boxes - Y direction
      FOR k = 0, ngrid - 1 DO BEGIN
      
        ; Extract box
        AIAfits171Box = AIAfits171[grid[j]:grid[j+1], grid[k]:grid[k+1]]
        AIAfits211Box = AIAfits211[grid[j]:grid[j+1], grid[k]:grid[k+1]]
        AIAfits094Box = AIAfits094[grid[j]:grid[j+1], grid[k]:grid[k+1]]
        AIAfits193Box = AIAfits193[grid[j]:grid[j+1], grid[k]:grid[k+1]]
        AIAfits131Box = AIAfits131[grid[j]:grid[j+1], grid[k]:grid[k+1]]
        AIAfits335Box = AIAfits335[grid[j]:grid[j+1], grid[k]:grid[k+1]]
        
        ; Sum up all intensities in image
        AIAIntegratedIntensity171(i) = total(AIAfits171(where(AIAfits171 GE 0)))
        AIAIntegratedIntensity211(i) = total(AIAfits211(where(AIAfits211 GE 0)))
        AIAIntegratedIntensity094(i) = total(AIAfits094(where(AIAfits094 GE 0)))
        AIAIntegratedIntensity193(i) = total(AIAfits193(where(AIAfits193 GE 0)))
        AIAIntegratedIntensity131(i) = total(AIAfits131(where(AIAfits131 GE 0)))
        AIAIntegratedIntensity335(i) = total(AIAfits335(where(AIAfits335 GE 0)))
        AIAIntegratedIntensity171Box(i, j, k) = total(AIAfits171Box(where(AIAfits171Box GE 0)))
        AIAIntegratedIntensity211Box(i, j, k) = total(AIAfits211Box(where(AIAfits211Box GE 0)))
        AIAIntegratedIntensity094Box(i, j, k) = total(AIAfits094Box(where(AIAfits094Box GE 0)))
        AIAIntegratedIntensity193Box(i, j, k) = total(AIAfits193Box(where(AIAfits193Box GE 0)))
        AIAIntegratedIntensity131Box(i, j, k) = total(AIAfits131Box(where(AIAfits131Box GE 0)))
        AIAIntegratedIntensity335Box(i, j, k) = total(AIAfits131Box(where(AIAfits335Box GE 0)))
      ENDFOR ; k loop across grids - Y direction
    ENDFOR ; j loop across grids - X direction
    
    ; Grab date/time 
    tOBS = strmid(header(8), 11, 16)
    tOBSJDtmp = anytim2jd(tOBS)
    tOBSJD(i) = tOBSJDtmp.int + tOBSJDtmp.frac
    
    print, 'Computing spatially integrated intensities:' + string(float(i)/(n_elements(wavelengths171Indices) - 1) * 100) + '%'
  ENDFOR ; i loop across time
  
  ; Clean up movie
  movieObject.Cleanup
  
  ; Create IDL saveset to reduce future run times
  SAVE, tOBSJD, AIAIntegratedIntensity171, AIAIntegratedIntensity211, AIAIntegratedIntensity094, AIAIntegratedIntensity193, AIAIntegratedIntensity131, AIAIntegratedIntensity335, AIAIntegratedIntensity171Box, AIAIntegratedIntensity211Box, AIAIntegratedIntensity094Box, AIAIntegratedIntensity193Box, AIAIntegratedIntensity131Box, AIAIntegratedIntensity335Box, FILENAME = saveloc + 'AIAIntegratedIntensities1.sav'
ENDIF ELSE RESTORE, saveloc + 'AIAIntegratedIntensities.sav' ; useIDLSave 

;GOTO, SKIP ; Avoid the plotting. I know, I know.. it's a goto. 

; TMP: remove stupid 0s
bla = where(tOBSJD NE 0)

; Prepare time formatting
tOBSFormatted = label_date(DATE_FORMAT = ['%H:%I'])

; Plot the light curve for full disk (time series of intensities)
p1 = plot(tOBSJD[bla], AIAIntegratedIntensity171[bla], SYMBOL = 'square', THICK = 2, COLOR = 'orange', /BUFFER, $
          title = 'AIA Light Curve - Full Disk', $
          xtitle = 'Time', XSTYLE = 1, XTICKFORMAT = 'LABEL_DATE', $
          ytitle = 'Intensity [DN]', $
          NAME = '171Å')
p2 = plot(tOBSJD[bla], AIAIntegratedIntensity211[bla], SYMBOL = 'square', THICK = 2, COLOR = 'red', /OVERPLOT, $
          NAME = '211Å')
p3 = plot(tOBSJD[bla], AIAIntegratedIntensity094[bla], SYMBOL = 'square', THICK = 2, COLOR = 'blue', /OVERPLOT, $
          NAME = '094Å')
p4 = plot(tOBSJD[bla], AIAIntegratedIntensity193[bla], SYMBOL = 'square', THICK = 2, COLOR = 'black', /OVERPLOT, $
          NAME = '193Å')
p5 = plot(tOBSJD[bla], AIAIntegratedIntensity131[bla], SYMBOL = 'square', THICK = 2, COLOR = 'green', /OVERPLOT, $
          NAME = '131Å')
;p6 = plot(tOBSJD[bla], AIAIntegratedIntensity335, SYMBOL = 'square', THICK = 2, COLOR = 'violet', /OVERPLOT, $
;          NAME = '335Å')
;leg1 = legend(TARGET = [p4, p1, p2, p5, p3, p6])
leg1 = legend(TARGET = [p4, p1, p2, p5, p3])
p1.Save, saveloc + 'AIALightCurveFullDisk.png'
p1.Close 

; Define constant yrange
allBoxes = [AIAIntegratedIntensity171Box, AIAIntegratedIntensity211Box, AIAIntegratedIntensity094Box, AIAIntegratedIntensity193Box, AIAIntegratedIntensity131Box, AIAIntegratedIntensity335Box]
yrange = [min(allBoxes), max(allBoxes)]

dualPlot = 0
WHILE dualPlot LE 1 DO BEGIN 

  ; Multiplot light curve for all grids
  w = window(DIMENSIONS = [4096, 4096], /BUFFER)
  l = 1
  FOR k = 0, ngrid - 1 DO BEGIN ; Y direction
    FOR j = 0, ngrid - 1 DO BEGIN ; X direction
      
      IF dualPlot EQ 0 THEN $ 
        p6 = plot(tOBSJD[bla], AIAIntegratedIntensity171Box(bla, j, k), SYMBOL = 'square', THICK = 2, COLOR = 'orange', POSITION = [grid(j), grid(k), grid(j+1), grid(k+1)], /DEVICE, /CURRENT, $
                  XSTYLE = 1, XTICKFORMAT = 'LABEL_DATE', $
                  yrange = yrange, $
                  NAME = '171Å')
      IF dualPlot EQ 1 THEN $
        p6 = plot(tOBSJD[bla], AIAIntegratedIntensity171Box(bla, j, k), SYMBOL = 'square', THICK = 2, COLOR = 'orange', POSITION = [grid(j), grid(k), grid(j+1), grid(k+1)], /DEVICE, /CURRENT, $
                  XSTYLE = 1, XTICKFORMAT = 'LABEL_DATE', $
                  NAME = '171Å')
      p7 = plot(tOBSJD[bla], AIAIntegratedIntensity211Box(bla, j, k), SYMBOL = 'square', THICK = 2, COLOR = 'red', /OVERPLOT, $
                NAME = '211Å')
      p8 = plot(tOBSJD[bla], AIAIntegratedIntensity094Box(bla, j, k), SYMBOL = 'square', THICK = 2, COLOR = 'blue', /OVERPLOT, $
                NAME = '094')
      p9 = plot(tOBSJD[bla], AIAIntegratedIntensity193Box(bla, j, k), SYMBOL = 'square', THICK = 2, COLOR = 'black', /OVERPLOT, $
                NAME = '193Å')
      p10 = plot(tOBSJD[bla], AIAIntegratedIntensity131Box(bla, j, k), SYMBOL = 'square', THICK = 2, COLOR = 'green', /OVERPLOT, $
                NAME = '131Å')
      p11 = plot(tOBSJD[bla], AIAIntegratedIntensity335Box(bla, j, k), SYMBOL = 'square', THICK = 2, COLOR = 'violet', /OVERPLOT, $
                NAME = '335Å')
                          
      print, 'Plotting N x N light curves:' + string(float(l)/ngrid^2 * 100) + '%'
      l++ 
    ENDFOR ; j loop across grids - X axis
  ENDFOR ; k loop across grids - Y axis
  leg2 = legend(TARGET = [p9, p6, p7, p10, p8, p11], POSITION = [0.85, 0.98], /RELATIVE)
  FOR j = 0, ngrid - 1 DO BEGIN  
    t1 = text(6, grid(j), alphabet(j), FONT_SIZE = 50, COLOR = 'green', /DEVICE)
    t2 = text(grid(-1) - 65, grid(j), alphabet(j), FONT_SIZE = 50, COLOR = 'green', /DEVICE)
    t3 = text(grid(j), 0, string(j + 1), FONT_SIZE = 50, COLOR = 'green', /DEVICE)
    t4 = text(grid(j), grid(-1) - 65, string(j + 1), FONT_SIZE = 50, COLOR = 'green', /DEVICE)
  ENDFOR ; j loop through grid spacing
  max171Index = where(AIAIntegratedIntensity171 EQ max(AIAIntegratedIntensity171))
  AIAfits171 = readfits(dataloc + allAIAfits(wavelengths171Indices(max171Index)), /SILENT)
  tmp171 = AIAfits171
  tmp171(where(AIAfits171 LE 0)) = 1 ; To prevent arithmetic error by taking log of small numbers
  i1 = image(alog(tmp171), POSITION = [0, 0, 819, 819], /DEVICE, RGB_TABLE = 3, /CURRENT)
  IF dualPlot EQ 0 THEN $
    p6.Save, saveloc + 'AIALightCurveGriddedConstantScale.png'
  IF dualPlot EQ 1 THEN $
    p6.Save, saveloc + 'AIALightCurveGriddedAutoScale.png'
  p6.Close
  dualPlot++
ENDWHILE
SKIP:
; Cleanup
spawn, 'rm ' + dataloc + 'filesToProcess.txt'

print, '-=Program normal completion in ' + strcompress(string((systime(1)-timerStart) / 60), /REMOVE_ALL), ' minutes=-'

END