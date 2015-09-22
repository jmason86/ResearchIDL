;+
; NAME: 
;   ProduceAIACutoutDifferenceMovie
;   
; PURPOSE:
;   Produce a movie of cutouts of differenced SDO/AIA images. Difference can be either running
;   (each image subtracts the previous) or absolute (each image subtracts the first frame). Can
;   also optionally save the original and/or differenced frames to disk. 
;   
; INPUTS: 
;   pathToAIAFullDiskData: [string] Full file system path to the directory containing the full disk
;                                   AIA data. 
;   wavelengths: [string array]     Contains the list of desired wavelengths to process in Å. Can be 
;                                   131, 171, 193, 211, 304, and/or 335. 
;   
;   
; OPTIONAL INPUTS:
;   upperLeftCornerOfCutoutBox: [integer array]  Optionally specify the upper left corner of 
;                                                the cutout box in image/index units in the form
;                                                [x,y]. Must then also specify 
;                                                lowerRightCornerOfCutoutBox.
;   lowerRightCornerOfCutoutBox: [integer array] Optionally specify the upper left corner of 
;                                                the cutout box in image/index units in the form
;                                                [x,y]. Must then also specify 
;                                                upperLeftCornerOfCutoutBox.
;   
; KEYWORD PARAMETERS:
;   RUNNING_DIFFERENCE:  This is the default. Each frame is produced by subtracting the current
;                        image from the temporally previous image. 
;   ABSOLUTE_DIFFERENCE: Each frame is produced by subtracting the current image from the first
;                        image in the time series. 
;   SAVE_FRAMES:         Save each individual differenced frame to disk. 
;   
; OUTPUTS:
;   MPEG movie in the pathToAIAFullDiskData directory of differenced AIA images in the specified 
;   wavelengths. 
;   
; OPTIONAL OUTPUTS: 
;   JPEG images of each frame in the movie. 
;   
; RESTRICTIONS:
;   Solarsoft must be installed
;   StringParse.pro
;   ParsePathAndFilename.pro
;   SelectAIACutout.pro
;   
; EXAMPLE: 
;   ProduceAIACutoutDifferenceMovie, '/Users/jama6159/Documents/Research/Data/SDO/AIA/Full Disk/20100709 CME/', $ 
;   [171], upperleftcornerofcutoutbox = [100, 100], lowerrightcornerofcutoutbox = [400, 400], /save_frames
;   
; MODIFICATION HISTORY: 
;   Written by: 
;     James Paul Mason 
;     2012/8/24
;-
PRO ProduceAIACutoutDifferenceMovie, pathToAIAFullDiskData, wavelengthsInput, $
                                     upperLeftCornerOfCutoutBox = upperLeftCornerOfCutoutBox, $
                                     lowerRightCornerOfCutoutBox = lowerRightCornerOfCutoutBox, $
                                     RUNNING_DIFFERENCE = running_difference, $
                                     ABSOLUTE_DIFFERENCE = absolute_difference, $
                                     SAVE_FRAMES = save_frames, $
                                     FPS = fps

; Time the code
timerStart = systim(1)

; Defaults
wavelengths = string(wavelengthsInput,format = '(I04)') ; Force wavelengths input to be string array
IF NOT keyword_set(absolute_difference) THEN running_difference = 1
IF keyword_set(absolute_difference) AND keyword_set(running_difference) THEN BEGIN
  message, /INFO, 'Running and absolute difference specified in keyword call. Please choose one or the other.'
  return
ENDIF
IF NOT keyword_set(fps) THEN fps = 10
IF fps GE 2 AND fps LT 5 THEN BEGIN
  message, /INFO, 'As of IDL 8.1, for unknown reasons the mpeg coder does not accept frame rates between from 2 to 4.'
  return
ENDIF

; Setup 
; If user wants to save the frames, create a 'frames' directory in pathToAIAFullDiskData
CD, pathToAIAFullDiskData
IF keyword_set(save_frames) THEN spawn, 'mkdir frames'

; TODO: arcsec conversion to pixel scale

; TODO: Support for IDL save cubes: save and read
; IF n_params EQ 1 then read the IDL save file, if n_params EQ 2 then go to default behavior of read_sdo for fits

; Create box
IF keyword_set(upperLeftCornerOfCutoutBox) AND keyword_set(lowerRightCornerOfCutoutBox) THEN BEGIN
  boxRangeX = [upperLeftCornerOfCutoutBox(0), lowerRightCornerOfCutoutBox(0)]
  boxRangeY = [upperLeftCornerOfCutoutBox(1), lowerRightCornerOfCutoutBox(1)]
  boxWidth = (boxRangeX(1) - boxRangeX(0)) > 494 ; Window function won't allow sizes < 494
  boxHeight = (boxRangeY(0) - boxRangeY(1)) > 342 ; Window function won't allow size < 342
ENDIF ; User specified box corners, if not handled inside loop by SelectAIACutout

; Loop through wavelengths
FOR wavelengthIndex = 0, n_elements(wavelengths)-1 DO BEGIN
  ; set wavelength variable to index of wavelengths
  wavelength = wavelengths(wavelengthIndex)
  
  ; Create string that contains the standard AIA filename and the wavelength the user specified
   fileList = file_search(pathToAIAFullDiskData + '/*' + wavelength + '.fits')
   IF n_elements(fileList) EQ 1 && fileList EQ -1 THEN BEGIN
    message, /INFO, 'No files found matching wavelength: ' + wavelength, /CONTINUE
   ENDIF
  
  ; Grab number of AIA files of specified wavelength
  numberOfImagesOfWavelength = n_elements(fileList) 
  
  ; Determine filename for the movie: wavelength + running/absolute
  IF keyword_set(absolute_difference) THEN $
    movieFilename = wavelength + '_AbsoluteDifference.mp4' ELSE $
    movieFilename = wavelength + '_RunningDifference.mp4'
  
  ; If the cutout box wasn't specified in the call, then run the function to show the user whatever
  ; frames they need to see to select the box and return the upperLeftCorner and lowerRightCorner
  IF NOT keyword_set(upperLeftCornerOfCutoutBox) OR NOT keyword_set(lowerRightCornerOfCutoutBox) THEN BEGIN
    ; Read the first full-disk image to determine the size of the image
    read_sdo, fileList(0), header, fullDiskAIAFirstImage, /SILENT
    fullDiskDimension = size(fullDiskAIAFirstImage, /DIMENSIONS)
    
    ; Declare array to store all images in time series and store that first image
    fullDiskAIADataCube = fltarr(fullDiskDimension(0), fullDiskDimension(1), numberOfImagesOfWavelength)
    fullDiskAIADataCube(*, *, 0) = fullDiskAIAFirstImage
    
    ; Read in remaining full-disk images in time series
    FOR timeIndex = 1, numberOfImagesOfWavelength - 1 DO BEGIN
      read_sdo, fileList(timeIndex), header, temporaryFullDiskAIA, /SILENT
      fullDiskAIADataCube(*, *, timeIndex) = temporaryFullDiskAIA
    ENDFOR ; timeIndex
    
    ; Call function to run user interaction and store corner values
    boxCorners = SelectAIACutout(fullDiskAIADataCube)
    upperLeftCornerOfCutoutBox = [boxCorners(0), boxCorners(1)]
    lowerRightCornerOfCutoutBox = [boxCorners(2), boxCorners(3)]
    
    ; Define the box width values which were skipped earlier
    boxRangeX = [upperLeftCornerOfCutoutBox(0), lowerRightCornerOfCutoutBox(0)]
    boxRangeY = [upperLeftCornerOfCutoutBox(1), lowerRightCornerOfCutoutBox(1)]
    boxWidth = (boxRangeX(1) - boxRangeX(0)) > 494 ; Window function won't allow sizes < 494
    boxHeight = (boxRangeY(0) - boxRangeY(1)) > 342 ; Window function won't allow size < 342
  ENDIF ; User didn't specify corners
  
  ; Initialize video creation object
  movieObject = IDLffVideoWrite(movieFilename)
  vidStream = movieObject.AddVideoStream(boxWidth, boxHeight, fps); boxWidth+1, boxHeight+1, fps)
  
  ; Allocate a 3D array to store AIA cutout timeseries, array errors ensue if using boxWidth/Height when >494/<342
  cutoutDifferenceImages = fltarr(boxRangeX(1) - boxRangeX(0) + 1, boxRangeY(0) - boxRangeY(1) + 1, numberOfImagesOfWavelength - 1)
  
  ; Read in the first image
  read_sdo, fileList(0), header, fullDiskImage, /SILENT
  cutoutImageFirst = fullDiskImage[boxRangeX(0):boxRangeX(1), boxRangeY(1):boxRangeY(0)] / header.int_time ; scaled by integration time
  
  ; Run loop through AIA timeseries to create a cube of all cutouts
  FOR timeIndex = 1, numberOfImagesOfWavelength - 1 DO BEGIN 
    read_sdo, fileList(timeIndex), header, fullDiskImage, /SILENT
    cutoutImageSecond = fullDiskImage[boxRangeX(0):boxRangeX(1), boxRangeY(1):boxRangeY(0)] / header.int_time ; scaled by integration time
    
    ; Difference the images
    cutoutDifferenceImages[*, *, timeIndex - 1] = cutoutImageSecond - cutoutImageFirst
    IF keyword_set(running_difference) THEN cutoutImageFirst = cutoutImageSecond
    
  ENDFOR ; timeIndex
  
  print, strcompress(string(fix(systime(1)-timerStart)),/remove_all) + ' seconds to finish reading/cutting out AIA images.'
  
  ; Determine and use maximum value in cutoutImages for optimal contrast scaling
  maxDifference = max(abs(cutoutDifferenceImages))
  cutoutDifferenceImages[0, 0, *] = -maxDifference
  cutoutDifferenceImages[0, 1, *] = maxDifference
  
  ; Load the first image
  imageObject = image(cutoutDifferenceImages[*, *, 0], DIMENSIONS = [boxWidth, boxHeight], /BUFFER)
  
  ; Loop through AIA cutout timeseries to compute difference and output
  FOR timeIndex = 1, numberOfImagesOfWavelength - 1 DO BEGIN
    ; Add the frame to the movie object
    imageObject.SetData, cutoutDifferenceImages[*, *, timeIndex - 1]
    timeInMovie = movieObject.Put(vidStream, imageObject.CopyWindow()) ; time returned in seconds
    
    ; If save_frames is set then save the difference image to disk as a JPEG
    IF keyword_set(save_frames) THEN BEGIN
      parser = ParsePathAndFilename(fileList(timeIndex))
      fitsFilename = parser.filename
      framesPath = parser.path + '/frames'
      parser2 = StringParse(fitsFilename, DELIMITER = '.')
      filename = parser2(0)
      jpegFilename = framesPath + '/' + filename + '.jpg'
      imageObject.save, jpegFilename, BORDER = 0
    ENDIF
    
  ENDFOR ; timeIndex
  
  ; Deallocate/close the image and movie objects
  imageObject.Close
  movieObject.Cleanup
ENDFOR ; wavelengthIndex

print, '-=Program normal completion in '+strcompress(string(fix(systime(1)-timerStart)),/remove_all), ' seconds=-'

END