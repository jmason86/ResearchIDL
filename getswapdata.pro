;+
; NAME:
;   GetSwapData
;
; PURPOSE:
;   Download data from PROBA2/SWAP (174 Å)
;
; INPUTS:
;   None required, but strongly encouraged to specify startDateTime and endDateTime, else will just get the single latest image
;
; OPTIONAL INPUTS:
;   startDateTime [string]: Date and time in the human format: yyyy-mm-dd hh-mm-ss, e.g., '2010-08-07 18:00:00'. 
;                           If startDateTime specified but not endDateTime, then the image closest to that time will be retrieved. 
;                           If no time is provided (e.g., '2010-08-07') then the whole day's data will be downloaded. 
;   endDateTime [string]:   Date and time in the human format: yyyy-mm-dd hh-mm-ss, e.g., '2010-08-07 23:00:00'
;   level [string]:         Either 'lvl0' or 'lvl1'. Level 1 is the default as it has already run the images through p2sw_prep
;
; KEYWORD PARAMETERS:
;   MAKE_MOVIE:              Set this to turn the downloaded images to a movie, saved to disk in the current directory. Does alog image scaling. 
;   MAKE_RUNNING_DIFF_MOVIE: Same idea as MAKE_MOVIE, but does running difference movie. 
;   MAKE_BASE_DIFF_MOVIE:    Same idea as MAKE_MOVIE, but does base difference movie. 
;   ALREADY_DOWNLOADED:      Set this if the images have already been downloaded (e.g., if you only want to make a movie)
;   DISPLAY_FIRST_IMAGE:     Set this to display the first image on screen. If MAKE_MOVIE also set, then will need to hit CONTINUE to make the movie. 
;                            WARNING: You should close the image before making the movie else it will keep stealing focus even though the movie images
;                            are buffered. 
;   VERBOSE:                 Set this to generate processing messages
;   
; OUTPUTS:
;   Fits files saved to disk in your home directory. 
;
; OPTIONAL OUTPUTS:
;   If MAKE_MOVIE keyword set, then will get a movie file on disk in your home directory. The filename is made relatively unique by using the 
;   startDateTime and endDateTime. 
;
; RESTRICTIONS:
;   Requires SSW, particularly the SWAP instrument package
;
; EXAMPLE:
;   GetSwapData, startDateTime = '2010-08-05 00:00:00', endDateTime = '2010-08-20 00:00:00', /MAKE_MOVIE, /MAKE_RUNNING_DIFF_MOVIE, /MAKE_BASE_DIFF_MOVIE
;
; MODIFICATION HISTORY:
;   2016/07/05: James Paul Mason: Wrote script.
;-
PRO GetSwapData, startDateTime = startDateTime, endDateTime = endDateTime, level = level, $
                 MAKE_MOVIE = MAKE_MOVIE, MAKE_RUNNING_DIFF_MOVIE = MAKE_RUNNING_DIFF_MOVIE, MAKE_BASE_DIFF_MOVIE = MAKE_BASE_DIFF_MOVIE, $ 
                 ALREADY_DOWNLOADED = ALREADY_DOWNLOADED, DISPLAY_FIRST_IMAGE = DISPLAY_FIRST_IMAGE, $
                 VERBOSE = VERBOSE

; Defaults
IF level EQ !NULL THEN level = 'lvl1'
CD, '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Data/PROBA2/SWAP'

;IF ~keyword_set(ALREADY_DOWNLOADED) THEN BEGIN
  ; Create SWAP object and set the data product level to be retrieved
  swap = obj_new('swap')
  swap -> set, filter = 'lv1', local = ALREADY_DOWNLOADED, VERBOSE = VERBOSE
  
  ; Define the time range to download
  IF startDateTime EQ !NULL THEN fileNames = swap -> list() ; Just the latest image
  IF startDateTime EQ !NULL AND endDateTime NE !NULL THEN fileNames = swap -> list(timerange = [startDateTime]) ; Just an image or a day
  IF startDateTime NE !NULL AND endDateTime NE !NULL THEN fileNames = swap -> list(timerange = [startDateTime, endDateTime])
  
  ; Download header data
  tic
  headers = swap -> list_index()
  IF keyword_set(VERBOSE) THEN message, /INFO, 'Time to get / load SWAP headers: ' + JPMPrintNumber(toc()) + ' seconds'
  IF ~keyword_set(ALREADY_DOWNLOADED) THEN BEGIN
    message, /INFO, 'You can now inspect the headers variable'
    STOP
  ENDIF
  
  ; Download the SWAP data
  tic
  swapData = swap -> getdata()
  IF keyword_set(VERBOSE) THEN message, /INFO, 'Time to get / load / make maps of SWAP data: ' + JPMPrintNumber(toc()) + ' seconds'

;ENDIF ELSE BEGIN ; If data were already downloaded then just read them in
;  ; TODO: Make this generic based on startDateTime and endDateTime
;  fileNames = file_search('swap_lv1_*.fits')
;  subFileNames = fileNames[3000:3999]
;
;  p2sw_prep, subFileNames, headers, swapData, /VERBOSE, /DESPIKE, /NORMALIZE
;  
;ENDELSE

; See first image
aia_lct, wave = 171, r, g, b
colortable = [[r], [g], [b]]
IF keyword_set(DISPLAY_FIRST_IMAGE) THEN BEGIN
  i1 = image(BYTSCL(swapData[*, *, 0]^0.25, 0.5, 4), RGB_TABLE = colortable, BACKGROUND_COLOR = 'black')
  t1 = text(0.15, 0.05, headers[0].DATE_D$OBS, FONT_COLOR = 'white')
  STOP
ENDIF

;;
; Make movies
;;

; Movie setup
xsize = 1024
ysize = 1024
fps = 10
bitrate = 1e7
firstImageScaled = bytscl(alog(swapData[*, *, 0]), min = 2.0, max = 8.0)
IF keyword_set(MAKE_MOVIE) THEN BEGIN
  movieObject = IDLffVideoWrite('SwapMovie ' + startDateTime + ' - ' + endDateTime + '.mp4')
  vidStream = movieObject.AddVideoStream(xsize, ysize, fps, BIT_RATE = bitrate)
ENDIF 
IF keyword_set(MAKE_RUNNING_DIFF_MOVIE) THEN BEGIN
  movieObjectRunning = IDLffVideoWrite('SwapMovieRunningDiff ' + startDateTime + ' - ' + endDateTime + '.mp4')
  vidStreamRunning = movieObjectRunning.AddVideoStream(xsize, ysize, fps, BIT_RATE = bitrate)
ENDIF
IF keyword_set(MAKE_BASE_DIFF_MOVIE) THEN BEGIN
  movieObjectBase = IDLffVideoWrite('SwapMovieBaseDiff ' + startDateTime + ' - ' + endDateTime + '.mp4')
  vidStreamBase = movieObjectBase.AddVideoStream(xsize, ysize, fps, BIT_RATE = bitrate)
ENDIF

IF keyword_set(MAKE_MOVIE) OR keyword_set(MAKE_RUNNING_DIFF_MOVIE) OR keyword_set(MAKE_BASE_DIFF_MOVIE) THEN BEGIN
  FOR timeStep = 0, n_elements(swapData[0, 0, *]) - 1 DO BEGIN
    tickObsect = tic() 
    
    imageToDisplay = bytscl(alog(swapData[*, *, timeStep]), min = 2.0, max = 8.0)
      
    IF keyword_set(MAKE_MOVIE) THEN BEGIN
      w = window(DIMENSIONS = [xsize, ysize], /DEVICE, /BUFFER, BACKGROUND_COLOR = 'black')
      i1 = image(imageToDisplay, AXIS_STYLE = 2, /CURRENT, RGB_TABLE = colortable, FONT_COLOR = 'white', $
                 TITLE = 'SWAP 174 Å', $
                 XTITLE = 'pixels', XCOLOR = 'white', $
                 YTITLE = 'pixels', YCOLOR = 'white')
      t1 = text(0, 0, headers[timeStep].DATE_D$OBS, FONT_COLOR = 'white')
        
      ;  Insert frame into movie
      timeInMovie = movieObject.Put(vidStream, w.CopyWindow()) ; time returned in seconds
      
      ; Save memory
      w.Close
    ENDIF ; Regular movie
    
    IF keyword_set(MAKE_RUNNING_DIFF_MOVIE) THEN BEGIN
      previousImage = swapData[*, *, timeStep - 1 > 0]
      w = window(DIMENSIONS = [xsize, ysize], /DEVICE, /BUFFER, BACKGROUND_COLOR = 'black')
      i2 = image(swapData[*, *, timeStep] - previousImage, AXIS_STYLE = 2, /CURRENT, FONT_COLOR = 'white', $
                 TITLE = 'SWAP 174 Å Running Diff', $
                 XTITLE = 'pixels', XCOLOR = 'white', $
                 YTITLE = 'pixels', YCOLOR = 'white')
      t2 = text(0, 0, headers[timeStep].DATE_D$OBS, FONT_COLOR = 'white')
      
      ;  Insert frame into movie
      timeInMovie = movieObjectRunning.Put(vidStreamRunning, w.CopyWindow()) ; time returned in seconds

      ; Save memory
      w.Close
    ENDIF
    
    IF keyword_set(MAKE_BASE_DIFF_MOVIE) THEN BEGIN
      w = window(DIMENSIONS = [xsize, ysize], /DEVICE, /BUFFER, BACKGROUND_COLOR = 'black')
      i3 = image(swapData[*, *, timeStep] - swapData[*, *, 0], AXIS_STYLE = 2, /CURRENT, FONT_COLOR = 'white', $
                 TITLE = 'SWAP 174 Å Base Diff', $
                 XTITLE = 'pixels', XCOLOR = 'white', $
                 YTITLE = 'pixels', YCOLOR = 'white')
      t3 = text(0, 0, headers[timeStep].DATE_D$OBS, FONT_COLOR = 'white')
      
      ;  Insert frame into movie
      timeInMovie = movieObjectBase.Put(vidStreamBase, w.CopyWindow()) ; time returned in seconds

      ; Save memory
      w.Close
    ENDIF
  
    ;progressBar = JPMProgressBar(100. * (timeStep + 1) / n_elements(swapData[0, 0, *]), progressBar = progressBar, NAME = 'Swap Movie Progress', $
    ;                             ticObject = tickObsect, runTimeText = runTimeText, etaText = etaText)
    message, /INFO, 'Movie(s) are ' + JPMPrintNumber(100. * (timeStep + 1) / n_elements(swapData[0, 0, *])) + '% complete'
  ENDFOR
  
  movieObject.Cleanup
  movieObjectRunning.Cleanup
  movieObjectBase.Cleanup
  
ENDIF ; Making any/all movies

END