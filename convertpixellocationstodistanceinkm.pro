; Function to convert a list of pixel locations of arbitrary length into a distance measured in kilometers.
; Initially developed for computation of coronal loop lenghts in AIA level 1.5 1K x 1K images.
;
; INPUT: 
;   xArray = an array containing the x locations of interest (Pixels). Can be int, float, or double.
;   yArray = an array containing the y locations of interest (Pixels). Can be int, float, or double.
;   
; RETURN:
;   If no error:
;     Linearly interpolated total distance between points (Km). Float return.
;   If error: 
;     String describing the error.  
;   
; James Paul Mason
; 2012/1/31

FUNCTION ConvertPixelLocationsToDistanceInKm, xArray, yArray

; Perform basic checks on input
IF n_elements(xArray) NE n_elements(yArray) THEN RETURN, "Mismatch in X,Y arrays. Input should be an array of X-points and the corresponding array of Y-points" $
ELSE BEGIN 
  arrayLength = n_elements(xArray)
  xArray = double(xArray) ; Convert xArray to dblarr if it's not already
  yArray = double(yArray) ; Convert yArray to dblarr if it's not already
ENDELSE

; Allocate an array for distances. Should be 1 less element than the inputs because we'll be computing distances between those points. 
distanceArray = fltarr(arrayLength-1)

; Loop through the input arrays and compute distance between each point. Store distance in distance array.
FOR i = 1, arrayLength-1 DO BEGIN
  distanceArray(i-1) = sqrt( (xArray(i) - xArray(i-1))^2 + (yArray(i) - yArray(i-1))^2 )
ENDFOR ;i loop

; Compute the sum the distances in the distance array. This is still in pixels.
totalDistanceInPixels = total(distanceArray)

; Convert the total distance in pixels into arcseconds. 
;CDELT1 = 2.4 ; arcsec/pixel from AIA level 1.5 header file. 
CDELT1 = 0.599076 ; arcsec/pixel from AIA full resolution header file (2011/03/07 08:06:25 171Å)
totalDistanceInArcsecs = CDELT1 * totalDistanceInPixels

; Take projection effect into account
alpha = 77.23 ; Externally computed alpha for 2011/03/07 08:06:25 171Å. 
totalDistanceInArcsecsWithAlphaCorrection = totalDistanceInArcsecs/sin(alpha/!RADEG)

; Convert the total distance in arcseconds into Km and return.
KmPerArcsec = 725.
RETURN, KmPerArcsec * totalDistanceInArcsecsWithAlphaCorrection

END