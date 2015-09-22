;+
; NAME: 
;   SelectAIACutout
;   
; PURPOSE:
;   Function to show full-disk AIA data and allow the user to select a cutout region of interest.
;   Can display all the data already loaded to memory to find a good image. 
;   
; INPUTS: 
;   fullDiskAIADataCube: [float array] 3D array that contains the full-disk AIA data in the format
;                                      [n, m, imageIndex]
;   
; OPTIONAL INPUTS:
;   None
;   
; KEYWORD PARAMETERS:
;   None
;   
; OUTPUTS:
;   boxCorners: [float array] 4 element array containing the upper left and lower right corners of
;                             the selected box in the format [upperLeftX, upperLeftY, lowerRightX, lowerRightY]
;   
; OPTIONAL OUTPUTS: 
;   None
;   
; RESTRICTIONS:
;   None
;   
; EXAMPLE: 
;   boxCorners = SelectAIACutout(fullDiskAIADataCube)
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2012/9/7
;-
; Precompile routines
@rectangle.pro
FUNCTION SelectAIACutout, fullDiskAIADataCube

; Set up
fullDiskDimensions = size(fullDiskAIADataCube, /DIMENSIONS)
window, xsize = fullDiskDimensions(0), ysize = fullDiskDimensions(1), /FREE
keyboardEntry = 0

; Display first image
loadct, 0, /SILENT
tvscl, fullDiskAIADataCube(*, *, 0)
imageIndex = 0

; Begin WHILE loop to select frame
message, /INFO, 'Press , or . to display the previous or next image. WARNING: Keyboard will be inoperable until enter key is pressed.'
WHILE byte(keyboardEntry) NE 13 DO BEGIN
  ; Give user option to skip through images
  keyboardEntry = get_kbrd()
  IF keyboardEntry EQ ',' THEN BEGIN 
    imageIndex = imageIndex - 1 > 0
    tvscl, fullDiskAIADataCube(*, *, imageIndex)
  ENDIF
  IF keyboardEntry EQ '.' THEN BEGIN
    imageIndex = imageIndex + 1 < (fullDiskDimensions(2) - 1)
    tvscl, fullDiskAIADataCube(*, *, imageIndex)
  ENDIF
ENDWHILE ; Enter button pressed

; Begin WHILE looop to select corners
message, /INFO, 'Click to begin sizing the cutout region. Click again to exit.' 
cursor, xdown, ydown, /UP, /DEVICE
!MOUSE.button = 0
WHILE !MOUSE.BUTTON EQ 0 DO BEGIN
  cursor, xchange, ychange, /CHANGE, /DEVICE
  loadct, 0, /SILENT
  tvscl, fullDiskAIADataCube(*, *, imageIndex)
  TEK_COLOR
  rectangle, xdown, ydown, (xchange - xdown), (ychange - ydown), /DEVICE, COLOR = 3, /NOCLIP
ENDWHILE

; Ensure that the correct corners are always returned
topLeftX = min([xdown, xchange])
topLeftY = max([ydown, ychange])
bottomRightX = max([xdown, xchange])
bottomRightY = min([ydown, ychange])

; Create single array for return
boxCorners = [topLeftX, topLeftY, bottomRightX, bottomRightY]

wait, 2
wdelete

return, boxCorners

END