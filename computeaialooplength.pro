; Script to load an AIA images, create a map structure, plot it to a x window with data units, and 
; allow the user to select points in the image to compute a length along. 
;
; REQUIREMENTS: 
;   Solarsoft
; 
; INPUT:
;   filename = a fits file from the SDO/AIA cutout service
;   multi = how many lines the user would like to specify along the loop
;   pi = set to anything other than 0 to estimate total loop length as user selected loop height * π
; 
; OUTPUT:
;   totalLength = the total length in arcsecs of the selected curve (uses linear interpolation)
;   
; EXAMPLE:
;   totalLength = ComputeAIALoopLength('ssw_cutout_20110909_060011_AIA_131_.fts', 3)
;   When image displays, click multi times (3 in the above example)
; 
; James Paul Mason
; 2012/2/28

FUNCTION ComputeAIALoopLength, filename, multi, pi

; Including paths doesn't work, so CD instead
CD,'/Users/jama6159/Dropbox/IDLWorkspace81/Research'
;CD,'/Users/jmason86/IDLWorkspace81/Research'

; Default the input values if not provided
checkvar,multi,2
checkvar,pi,0

; Read in the data
IF exist(filename) THEN BEGIN
  fits = readfits(filename,header)
ENDIF ELSE BEGIN
  print, 'Please specify a filepath and filename.'
  return,-1
ENDELSE
IF fits EQ [-1] THEN BEGIN
  print, 'Failed reading FITS file. Ensure path/filename are correct and valid.'
  return, -1
ENDIF

; Convert to a map array, retaining header information
index2map,header,fits,mapArray

; Plot the map to an x window in data units and log scale (default the rest of the settings)
plot_map,mapArray,/log_scale
STOP
; Zoom feature 
print, 'Right click to enable zooming. Left click to bypass.'
cursor,x,y,/down
didZoomOccur = 0
IF !MOUSE.BUTTON EQ 4 THEN BEGIN 
  ;continueZoom = 1
  ;WHILE continueZoom EQ 1 DO BEGIN
    ZoomOnSelectedSquare, output=XYZoom
    plot_map,mapArray,/log_scale,FOV=XYZoom.zoomFOV/60,CENTER=[XYZoom.xCen,XYZoom.yCen]
    print, 'When finished zooming, left click.'
    IF !MOUSE.BUTTON NE 4 THEN continueZoom = 0
    didZoomOccur = 1
  ;ENDWHILE
  print, 'Zoom complete. Proceed with point selection.'
ENDIF

; Call code for computing the length
ClickToComputeLength, multi=multi, outlen=totalLength, outpoints=outpoints

; If /pi option is enabled, then use the length as the height of the loop and multiply by π to get half-circle for length
IF pi NE 0 THEN totalLength = totalLength * !PI

; Convert length in arcsec to km (arsecs/km = 725)
totalLength = totalLength * 725

; Save image with overplotted user-selected points
;saveloc = '/Users/jmason86/Dropbox/Research/Data/Cooling Rate Flare Loop Length Analysis/Late Phase/AIA Selected Loops/'
saveloc = '/Users/jama6159/Dropbox/Research/Data/Cooling Rate Flare Loop Length Analysis/Late Phase/AIA Selected Loops/'
parser = ParsePathAndFilename(filename)
savename = parser.filename + '_LoopLocOverlay.jpeg'
write_jpeg_tv,saveloc+savename
print, 'Image saved to ',saveloc+savename

; Save loop length output. NOTE: Must manually delete any time you need to start over, due to the append below. 
close,1 & openw,1,saveloc+'LoopLengths.txt', width=200, /append
printf,1, savename, ' ', totalLength
close,1
print, 'Total length = '+num2str(totalLength)+' km'
print, 'Loop length saved to ',saveloc+'LoopLengths.txt'

return, totalLength

END