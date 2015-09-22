;+ 
; Program to produce light curves from spatial regions in AIA images. 
; 
; REQUIREMENTS: 
;   Solarsoft
; 
; INPUTS: 
;   Must set dataloc directory, containing AIA data in FITS format
;   User interaction -- selecting two points for a box bounding coronal loop(s)
; 
; OUTPUTS: 
;   Plots: light curves in a single AIA spectral band
;   Images: JPG format AIA cutouts based on user input
; 
; James Paul Mason
; 2012/4/5
;-

; Precompile necessary functions and procedures
@'/Users/jama6159/IDLWorkspace81/Research/zoomonselectedsquare.pro'
@'/Users/jama6159/Dropbox/IDLWorkspace81/Woods IDL Library/misc/write_jpeg_tv.pro'
@'/Users/jama6159/IDLWorkspace81/Default/parsepathandfilename.pro'
@'/Users/jama6159/Dropbox/IDLWorkspace81/Default/range.pro'

PRO ProduceAIALightCurves

timerStart = systime(1)

; Setup
dataloc = '/Users/jama6159/Documents/Research/Data/SDO/AIA/FullDisk/2011221_09AUG_0805_X6.9/prepped/'
spawn, 'rm '+dataloc+'filesToProcess.txt'
CD, dataloc
spawn, 'ls *.fits > filesToProcess.txt'
readcol, dataloc + 'filesToProcess.txt', allAIAfits, format='a', /SILENT
numberOfFilesProcessed = n_elements(allAIAfits)
totalIntensityTimeSeries = fltarr(numberOfFilesProcessed)
associatedTime = fltarr(numberOfFilesProcessed)


; Loop through all the AIA images in a single band but at multiple times
FOR i = 0, n_elements(allAIAfits) - 1 DO BEGIN 
  
  ;Notify user that the code is not hung up
  print, 'Processing '+strcompress(string(i),/remove_all)+' of '+strcompress(string(n_elements(allAIAfits)),/remove_all)
  
  ; Set up for each wavelength
  IF i GT 0 THEN previousWavelength = currentWavelength ELSE previousWavelength = ''
  currentWavelength = strmid(allAIAfits(i), 7, 3, /REVERSE_OFFSET)
  IF currentWavelength NE previousWavelength THEN BEGIN
    spawn, 'mkdir ' + dataloc + currentWavelength + '/LoopCutoutResults'
    saveloc = dataloc + currentWavelength + '/LoopCutoutResults/'
    mpegObject = OBJ_NEW('IDLgrMPEG', FRAME_RATE = 2, QUALITY = 100)
    j = 0
  ENDIF ; New wavelength
  
  ; Read in data
  AIAfits = readfits(dataloc+allAIAfits(i), header, /SILENT)
  ; If readfits fails, just continue the loop. 
  IF AIAfits EQ [-1] THEN BEGIN  
    print, 'Failed to read FITS file. Skipping.'
    numberOfFilesProcessed = numberOfFilesProcessed - 1
    CONTINUE
  ENDIF
  
  ; Retrieve T_OBS from the fits header and convert to minutes since first observation
  fitsT_OBSString = header(where(strpos(header,'T_OBS') EQ 0))
  T_OBS = strmid(fitsT_OBSString,strpos(header(where(strpos(header,'T_OBS') EQ 0)),'2'),19) ; complicated but necessary
  timeJD = anytim2jd(T_OBS)
  timeJD = double(timeJD.int)+timeJD.frac
  IF i EQ 0 THEN BEGIN 
    associatedTime(0) = timeJD 
    startingTime = T_OBS
  ENDIF $ ; i = 0
  ELSE BEGIN 
    startingTimeJD = anytim2jd(startingTime) & startingTimeJD = double(startingTimeJD.int)+startingTimeJD.frac
    associatedTime(i) = (timeJD - startingTimeJD)*24*60 ; *24*60 to convert to minutes
  ENDELSE
  
  ; Convert to a map array, retaining header information
  index2map,header,AIAfits,mapArray
  
  ; BEGIN USER INTERACTION
  IF i EQ 0 THEN BEGIN
    ; Plot the map to an x window in data units and log scale (default the rest of the settings)
    plot_map,mapArray,/log_scale
    
    ; Zoom feature 
    print, 'Right click to enable zooming. Left click to bypass.'
    cursor,x,y,/down
    didZoomOccur = 0
    IF !MOUSE.BUTTON EQ 4 THEN BEGIN 
        ZoomOnSelectedSquare, output=XYZoom
        print, 'Zoom Points = ', XYZoom
        ; Hardcode input
        XYZoom.xCen = 977.143
        XYZoom.yCen = -301.714
        XYZoom.ZoomFOV = 157.174
        plot_map,mapArray,/log_scale,FOV=XYZoom.zoomFOV/60,CENTER=[XYZoom.xCen,XYZoom.yCen]
        IF !MOUSE.BUTTON NE 4 THEN continueZoom = 0
        didZoomOccur = 1
      print, 'Zoom complete. Proceed with point selection.'
    ENDIF

    ; Have user select box for light curve, but only for the first image in the time series
    print, 'Select two corners of a bounding box either 1.) through multiple loops or 2.) along a single loop'
    xSelectedPoints = fltarr(2) & ySelectedPoints = fltarr(2)
    FOR j=0,1 DO BEGIN
      cursor,x,y,/down
      xSelectedPoints(j) = x
      ySelectedPoints(j) = y
    ENDFOR ;j loop
    print, 'X Points = ', xSelectedPoints
    print, 'Y Points = ', ySelectedPoints
    ; Hardcode input
    xSelectedPoints = [1029.40, 1044.55]
    ySelectedPoints = [-277.776, -304.714]
    
    ; Use the selected points to compute the angle, theta, of the hypotenuse of the bounding box
    theta = atan((ySelectedPoints(1) - ySelectedPoints(0)) / (xSelectedPoints(1) - xSelectedPoints(0)))
    
    ; Ask user if 1.) box crosses multiple loops -- then create light curve for each pixel or 2.) box along "single" loop -- then sum intensities and create light curve.
    boxOption = 0 
    WHILE boxOption NE 1 AND boxOption NE 2 DO BEGIN
      read, boxOption, prompt='Please select: 1.) Selected box crosses multiple coronal loops. 2.) Selected box runs along a "single" loop.   '
      IF boxOption NE 1 AND boxOption NE 2 THEN print, 'Please select one of the presented options.'
    ENDWHILE
  ENDIF ; i = 0 
  ; END USER INTERACTION
  
  ; Rotate the map
  mapArrayRotated = rot_map(mapArray, -theta*!RADEG)
  
  ; Use rotation matrix (using theta) on x,ySelectedPoints so they can be used in mapArrayRotated
  xSelectedPointsRotated = fltarr(2) & ySelectedPointsRotated = fltarr(2)
  FOR k = 0,1 DO xSelectedPointsRotated(k) = xSelectedPoints(k)*cos(theta) - ySelectedPoints(k)*sin(theta)
  FOR k = 0,1 DO ySelectedPointsRotated(k) = xSelectedPoints(k)*sin(theta) + ySelectedPoints(k)*cos(theta)
  XZoomRotated = XYZoom.xCen*cos(theta) - XYZoom.yCen*sin(theta)
  YZoomRotated = XYZoom.xCen*sin(theta) + XYZoom.yCen*cos(theta)
  
  ; Overlay the selected box for the loop(s) over the first zoomed image and save image
  IF currentWavelength NE previousWavelength THEN BEGIN
    plot_map,mapArrayRotated,/log_scale,FOV=XYZoom.zoomFOV/60,CENTER=[XZoomRotated,YZoomRotated]
    xBox = [xselectedpointsrotated(0),xselectedpointsrotated(0),xselectedpointsrotated(1),xselectedpointsrotated(1),xselectedpointsrotated(0)]
    yBox = [yselectedpointsrotated(0),yselectedpointsrotated(1),yselectedpointsrotated(1),yselectedpointsrotated(0),yselectedpointsrotated(0)]
    TEK_COLOR
    plots, xBox, yBox, color=2
    write_jpeg_tv, saveloc+'SelectedBox.jpg'
    LOADCT, 0, /SILENT
  ENDIF ; i = 0

  ; Create and show sub-map based on user selected bounding box
  sub_map, mapArrayRotated, subMapArrayRotated, xrange=[xSelectedPointsRotated(0),xSelectedPointsRotated(1)], yrange=[ySelectedPointsRotated(0),ySelectedPointsRotated(1)]
  plot_map, subMapArrayRotated, /log_scale
  
  ; Save map image and add frame to movie
  write_jpeg_tv, saveloc+'frame_'+string(i,format='(I5.5)')+'.jpg'
  mpegObject->put, TVRD(/ORDER)
  
  ; If box crosses multiple loops then create light curve for each row of pixels(arcsecs?). Sum across the short axis of the box, assuming still along the same loop
  IF boxOption EQ 1 THEN BEGIN 
    IF theta GE 1 THEN biggerBoxDimension = 'y' ELSE biggerBoxDimension = 'x'
    ; TODO: sum rows
    
  ENDIF ; boxOption = 1
  
  ; If box is along a single loop then sum intensities and create single light curve 
  IF boxOption EQ 2 THEN BEGIN
    totalIntensityTimeSeries(i) = total(subMapArrayRotated.data)
  ENDIF ; boxOption = 2
  
  ; Check if this is the last image of an individual wavelength
  IF i EQ n_elements(allAIAfits)-1 THEN nextWavelength = ''
  IF i LT n_elements(allAIAfits)-1 THEN BEGIN 
    parser = ParsePathAndFilename(allAIAfits(i+1))
    nextWavelength = parser.path
  ENDIF ; Next wavelength exists (i.e. not at end of file list)
  
  ; Output results for a wavelength once it is completed running
  IF nextWavelength NE currentWavelength THEN BEGIN
    relevantIndices = range(i-j,i,/integer)
    
    ; Set first time index to 0 now that single wavelength loop is finished
    associatedTime(i-j) = 0
    
    ; Produce and save movie
    mpegObject->save,filename='LoopMovie.mpg'
    spawn, 'mv LoopMovie.mpg '+saveloc ; mpegObject save doesn't like including saveloc in the call
    obj_destroy, mpegObject
    
    ; Compute peak time
    peakTime = strcompress(string(associatedTime(where(totalIntensityTimeSeries EQ max(totalIntensityTimeSeries[relevantIndices])))),/remove_all)
    
    ; Produce and save plots
    p1 = plot(associatedTime[relevantIndices], totalIntensityTimeSeries[relevantIndices], 'r2', $
              title='AIA Coronal Loop Light Curve: Peak at t = '+peakTime+' Minutes', $
              xtitle='Minutes Since '+startingTime, $
              ytitle='Spatially Integrated Intensity [DN]')
    p1.save, saveloc+'LightCurve.jpg'
    p1.close
    STOP
  ENDIF ; Next wavelength is different from the current one
  
  j++ ; Increment the index tracking number of images in each individual wavelength
ENDFOR ; i loop

; Cleanup
spawn, 'rm '+dataloc+'filesToProcess.txt'

print, '-=Program normal completion in '+strcompress(string(fix(systime(1)-timerStart)),/remove_all), ' seconds=-'

END