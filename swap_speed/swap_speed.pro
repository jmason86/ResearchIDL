pro SWAP_speed, filename, n_frames, $
    IMG_NAME, TIMES, DIST_RD, ERR_DIST_RD, DIST_BD, ERR_DIST_BD, $
                SPEED, ERR_SPEED , START_DATE = START_DATE , END_DATE = END_DATE , Preprepped = Preprepped, eventNumber = eventNumber 




;PRO swap_speed, filename, n_frames, IMG_NAME, TIMES, DIST_RD, $
;	ERR_DIST_RD, DIST_BD, ERR_DIST_BD, SPEED, ERR_SPEED
;
; Point and click tool to compute distance beetween points and speed
; of propagatating features in EIT data.
;
; Input images are 512x512 pixels or rebined to 512x512 at the moment (full
; resolution to be implemented).
;
;INPUTS:
;	FILENAME  - file name of the first image needed (typically the last
;		    pre-eruption image)
;	Number_Of_Frames  - number of frames needed
;	
;	OPTIONAL INPUTS: 
;	  eventNumber [integer]: The event number corresponding to James Paul Mason's analysis of limb CME events in SWAP
;	
;OUTPUTS:
;	IMG_NAME  - string array with the list of names of selected files
; 	TIMES	  - times of the images
;	DIST_RD   - distances (km) of last points in running
;		    difference images
;	ERR_DIST_RD - error of distances in running difference images (km)
;	DIST_BD   - distances (km) of first and last points in base difference
;		    images
;	ERR_DIST_BD - error of distances in base difference images (km)
;	SPEED     - speed (km/s) of last points
;	ERR_SPEED - error of the speed (km/s)
;
;EXAMPLE:
;	IDL> .comp eit_speed
;	IDL> eit_speed, 'efz19970512.043450', 5
;	IDL> eit_speed, 'efz19970512.043450', 5, $
;		IMG_NAME, TIMES, DIST_RD, ERR_DIST_RD, DIST_BD, ERR_DIST_BD, $
;		SPEED, ERR_SPEED
;
;OTHERS ARE NOT YET IDENTIFIED
;
; Created by J. de Patoul Sept-2006
; Following modifications by A. Zhukov Jan-2008
; Following modifications by M. J. West Aug-2013
;
;============ REMARKS ========================
; Routines needed:
; 	b_movie2
; 	bdi
; 	circle_limb
; 	colors8
;	dist_v		(new)
; 	gcd_v		(new)
;	get_radius
; 	gettimearr2
;	great_circle	(new)
; 	image_scale
; 	mv_img2
;	my_time_stamp
; 	rdi
;	rdpix4cme_trace	(new)
; 	self_bdi
;=============================================

; 2016/07/11: James Paul Mason: Hard-coded directories
CD, '/Users/jmason86/Dropbox/Research/Postdoc_LASP/Data/PROBA2/SWAP'
saveloc = '/Users/' + getenv('username') + '/Dropbox/Research/Postdoc_LASP/Analysis/SWAP Limb CME Speeds/'

;Selecting filenames list (FILELIST) with wavelength information (WAVELENGTH),
;Array of files (B0) Array[ 512 , 512 , Number_Of_Frames + 1 ]
;Times of files (TIME_ARRAY) :

;===Select input parameters

;---Enter Instrument
    Instrument = 'AIA'
    Instrument = 'SWAP'

;---Enter Resolution
;    ImageSize = 512
    ImageSize = 1024

; 2016/07/13: James Paul Mason: Limb CME events in SWAP

IF eventNumber NE !NULL THEN BEGIN
  CASE eventNumber OF 
    1: BEGIN
      START_DATE = '2010-03-01 21:30:00'
      END_DATE = '2010-03-01 23:20:00' 
    END
    2: BEGIN
      START_DATE = '2010-08-18 04:30:00'
      END_DATE = '2010-08-18 06:00:00'
    END
    3: BEGIN
      START_DATE = '2011-02-24 07:00:00'
      END_DATE = '2011-02-24 09:00:00'
    END
    4: BEGIN
      START_DATE = '2011-03-19 10:30:00'
      END_DATE = '2011-03-19 12:20:00'
    END
    5: BEGIN
      START_DATE = '2011-03-29 19:30:00'
      END_DATE = '2011-03-29 21:00:00'
    END
    6: BEGIN
      START_DATE = '2011-05-09 20:00:00'
      END_DATE = '2011-05-09 22:00:00'
    END
    7: BEGIN
      START_DATE = '2011-05-18 17:00:00'
      END_DATE = '2011-05-18 19:00:00'
    END
    8: BEGIN
      START_DATE = '2011-07-09 15:00:00'
      END_DATE = '2011-07-09 17:00:00'
    END
  ENDCASE
ENDIF

;---Enter keywords
    IF NOT KEYWORD_SET( START_DATE ) THEN START_DATE = '2016-02-24 07:00:00'
    IF NOT KEYWORD_SET( END_DATE ) THEN END_DATE = '2016-02-24 09:00:00'
    IF NOT KEYWORD_SET( Preprepped ) THEN Preprepped =  1


;---Enter Wavelength
    wavelength = 171

    IF INSTRUMENT EQ 'SWAP' THEN BEGIN
        swap = obj_new( 'swap' )

;---When running data for the first time, run preprepped = 0 as this
;   downloads level 0 data, after, change this to level 1 data.

        IF Preprepped EQ 1 THEN BEGIN

            swap -> set, filter = 'lv0' , prep = 0 , local = 0 ;===Set to local=0 if this is the first time!
;            files = swap -> list( timerange = ['2014-03-28 18:40:00', '2014-03-28 18:47:00'] );, '2014-03-28 20:25:00'] )
            files = swap -> list( timerange = [ START_DATE , END_DATE ] )
            swap -> copy , filelist = files
            swap -> read , filelist = files
            filelist = swap -> list( timerange = [START_DATE , END_DATE] ) ;, '2014-03-28 20:25:00'] )
            p2sw_prep, filelist, header, data, /float, /normalize, /despike,$
            /outfits, /remove_horizontal ;, IMG
        ENDIF

        swap -> set, filter = 'lv1' , prep = 0 , local = 1 ;===Set to local=0 if this is the first time!
        filelist = swap -> list( timerange = [START_DATE , END_DATE] ) ;, '2014-03-28 20:25:00'] )
    ENDIF

;---Read in the Fits data
    MREADFITS , filelist , headerdata , Images

;--- MANIPULATE IMAGE HERE FOR SCALING
;images = bytscl(images^(0.25), 0., 4.0)


;---Now we create a time array of files and timearray
    b_movie2swap , filelist , Images , TIME_ARRAY , Image_Size = ImageSize

;---Define the device working on, and the number of frames used to
;---make movie, and create timearray of images.
    mydevice = !D.NAME
    frames = n_elements( filelist ) ;To be put in accordance with b_movie2
    timearr = gettimearr2SWAP( filelist , '' )
    Number_Of_Frames = N_elements( timearr )

;===Preparing byte-scaled and time-stamped movies
;   (rdi for Running Difference Images)
;   (bdi for Base Difference Images)

;---Set constrasts
    contrast = 1.0
    contrast_plain = 4.0

;---Create Running difference images and scale them
    rdiSWAP , Images , time_array , IM_DIFF , Run_Diff_time_array
    image_scale , im_diff , Run_Diff_time_array , frames - 1 , contrast , contrast_plain , /rdi , Run_Diff_Images

;---Create Base diference images
    bdiSWAP , Images , time_array , IM_DB , Base_Diff_time_array
    image_scale, im_db, Base_Diff_time_array , frames-1, contrast, contrast_plain, /bdi, Base_Diff_Images

;---Scale Basic Images
    image_scale, Images , time_array , frames , contrast , contrast_plain , /plain , Plain_Images

;---Prep the images
    ;hr = headfits(filelist[ 0 ] )
    ;print, headerdata.SWYCEN
    ;print, hr.SWYCEN


;---Draw a circle with center and radius from SWAP header  :
    FOR iStep = 0 , n_elements( Run_Diff_Images( 0 , 0 , * ) ) - 1 DO BEGIN
        Run_Diff_Images( * , * , iStep ) = circle_limb_swap( Run_Diff_Images( * , * , iStep ) , $
          headerdata( iStep + 1 ) , twolimb = 1 , Line_Thickness = 0.01 , Image_Size = ImageSize )
    ENDFOR
    FOR iStep = 0 , n_elements( Base_Diff_Images( 0 , 0 , * ) ) - 1 DO BEGIN
        Base_Diff_Images( * , * , iStep ) = circle_limb_swap( Base_Diff_Images( * , * , iStep ) , $
          headerdata( iStep + 1 ) , twolimb = 1 , Line_Thickness = 0.01 ,  Image_Size = ImageSize )
    ENDFOR


;---Create movies to compare plain images with difference images:
    SWAPComparisonMovie , Plain_Images( * , * , 1 : * ) , Run_Diff_Images  , Image_Size = 512
    SWAPComparisonMovie , Plain_Images( * , * , 1 : * ) , Base_Diff_Images , Image_Size = 512

;===Select points and then Compute speed and distance beetween points

;---We select points from several images and repeat until told to stop
    REPEAT BEGIN

;---Define variables used for calculating position and velocity:
        Image_Number = 0
        Repeat_Observations = 1			; for 'repeat begin' ...

img_select = BYTARR( ImageSize , ImageSize , 10 * Number_Of_Frames )

long_var =  DBLARR( 10 * Number_Of_Frames ) ; type and size of the variable
img_num 	= long_var ; vector with the numer of images selected
time_julday	= long_var
time_graphe 	= long_var
        X_Cursor_Position 		= long_var ; x_position pointed by the curser
        Y_Cursor_Position 		= long_var ; y_position pointed by the curser
        Time_Difference 		= long_var ; Time difference beetwen (P_ii and P_ii+1)
Time_Difference_graphe 	= long_var
V12_vect 	= long_var ; Speed beetwen two last points (P_ii and P_ii+1)
D12_vect 	= long_var ; Distance beetwen two last points (P_ii and P_ii+1)
D0I_vect 	= long_var ; Distance beetwen first and last points (P_0 and P_ii+1)
err_V12 	= long_var ; Speed error with accuracy beetwen (P_ii and P_ii+1)
err_D12 	= long_var ; Dist error with accuracy beetwen (P_ii and P_ii+1)
err_D0I 	= long_var ; Dist error with accuracy beetwen (P_0 and P_ii+1)
accuracy 	= 1 	   ; accuraty for compute the error in pixel
        img_font = '-adobe-helvetica-medium-r-normal--14-100-100-100-p-76-iso8859-1'

; 2016/07/12: James Paul Mason: Just hard-code setting to cartesian 
Spheric_Or_Cart_Coordinates = 1
;---Select if you want Spherical coordinates or Cartesian coordinates:
;        PRINT , 'Enter 1 for Cartesian coordinates, 2 for Spherical coordinates:'
;        READ , Spheric_Or_Cart_Coordinates , Prompt = ' '

;---Select type of the images:
        PRINT , 'Enter 1 for regular image, 2 for running difference,'
        PRINT , '3 for base difference:  '
        READ , Select_Image_Type , PROMPT = ' '
        IF ( Select_Image_Type EQ 1 ) THEN BEGIN & image_all = Plain_Images & ENDIF
        IF ( Select_Image_Type EQ 2 ) THEN BEGIN & image_all = Run_Diff_Images & ENDIF
        IF ( Select_Image_Type EQ 3 ) THEN BEGIN & image_all = Base_Diff_Images & ENDIF


;BDIFF = Base_Diff_Images
;DIFFR = Run_Diff_Images
;B0 = Images
;C0 = Plain_Images
        time_array_db = Base_Diff_time_array
        time_array_diff = Run_Diff_time_array
        img_type = Select_Image_Type
;ii = Image_Number
;xx = X_Cursor_Position
;yy = Y_Cursor_Position
;coord = Spheric_Or_Cart_Coordinates
;dt = Time_Difference

;---Select the first image ( Image_Number = 0 ):
        PRINT , 'Enter number of the first image :'
        READ , image_n , Prompt = ' '

        img_num[ Image_Number ] = image_n
        img_select[ * , * , Image_Number ] =  image_all[ * , * , img_num[ Image_Number ] ]

;---Calculate Julian time
        Image_Year = strmid( time_array[ img_num[ Image_Number ] ] , 0 , 4 )
        Image_Month = strmid( time_array[ img_num[ Image_Number ] ] , 5 , 2 )
        Image_Day = strmid( time_array[ img_num[ Image_Number ] ] , 8 , 2 ) 
        Image_Hour = strmid( time_array[ img_num[ Image_Number ] ] , 11 , 2 )
        Image_Min = strmid( time_array[ img_num[ Image_Number ] ] , 14 , 2 )
        Image_Sec = strmid( time_array[ img_num[ Image_Number ] ] , 17 , 2 )
        time_julday[ Image_Number ] = JULDAY( Image_Month , Image_Day , Image_Year , Image_Hour , Image_Min , Image_Sec )

;---Show the first image:
        FONT = !P.FONT & !P.FONT = 0 & DEVICE , FONT = times

;---Create window with scroll bars
        scrollwindow , xsize = ImageSize , ysize = ImageSize , $
        TITLE = 'Image' + strtrim( ROUND( img_num[ Image_Number ] ) ) +' :' + strtrim( filelist[ img_num[ Image_Number ] ] )
        TV , img_select[ * , * , Image_Number ]

;    WINDOW , 0 , ret = 2 , XPOS = 0 , YPOS = 1000 , xsize = ImageSize , ysize = ImageSize , $
;    TITLE = 'Image' + strtrim( ROUND( img_num[ Image_Number ] ) ) +' :' + strtrim( filelist[ img_num[ Image_Number ] ] )
;    WINDOW, XSIZE = N_ELEMENTS( img_select[ * , 0 , 0] ) , YSIZE = N_ELEMENTS( img_select[ 0 , * , 0] )
;    TV , img_select[ * , * , Image_Number ]


;---read pixels from the image

            rdpix4cme_SWAP , img_select[ * , * , Image_Number ] , 0 , 0 , XVERTS , YVERTS , /normal , Instrument = Instrument , header = headerdata( Image_Number ), ScaleSize = Scalesize
            X_Cursor_Position[ Image_Number ] = Xverts
            Y_Cursor_Position[ Image_Number ] = Yverts

; 2016/07/12: James Paul Mason: Just hard-code setting image_n to increment
image_n++
;---Select next image Image_Number = Image_Number + 1:
;         PRINT , '  '
;         PRINT , 'Enter the number of the next image :'
;        READ , image_n , Prompt = ' '

        img_num[ Image_Number + 1 ] = image_n

;---Repeat until happy with the selected images
        REPEAT BEGIN

;---Repeat individual observations
            IF Repeat_Observations EQ 1 THEN BEGIN
                img_select[ * , * , Image_Number + 1 ] = image_all[ * , * , img_num[ Image_Number + 1 ] ]
                Image_Year = STRMID( time_array[ img_num[ Image_Number + 1 ] ] , 0 , 4 )
                Image_Month = STRMID( time_array[ img_num[ Image_Number + 1 ] ] , 5 , 2 )
                Image_Day = STRMID( time_array[ img_num[ Image_Number + 1 ] ] , 8 , 2 )
                Image_Hour = STRMID( time_array[ img_num[ Image_Number + 1 ] ] , 11 , 2 )
                Image_Min = STRMID( time_array[ img_num[ Image_Number + 1 ] ] , 14 , 2 )
                Image_Sec = STRMID( time_array[ img_num[ Image_Number + 1 ] ] , 17 , 2 )
                time_julday[ Image_Number + 1 ] = JULDAY( Image_Month , Image_Day , Image_Year , Image_Hour , Image_Min , Image_Sec )
;---Show next image:
                FONT = !P.FONT & !P.FONT = 0 & DEVICE , font = times


;---Create window with scroll bars
                scrollwindow , xsize = ImageSize , ysize = ImageSize , $
                TITLE = 'Image' + strtrim( ROUND( img_num[ Image_Number + 1 ] ) ) + ' :' + strtrim( filelist[ img_num[ Image_Number + 1 ] ] )
                TV , img_select[ * , * , Image_Number + 1 ]

;---If screen is not big enough use this to align the bottom of the image.
;                IF N_ELEMENTS( img_select[ * , 0 , 0 ] ) LT 1024 THEN Image_Size = N_ELEMENTS( img_select[ * , 0 , 0 ] ) ELSE Image_Size = 1024
;                IF ( !d.y_size + !d.x_size NE Image_Size ) THEN BEGIN
;                    PRINT, 'Position the scroll will to align with bottom of image if screen not big enough'
;                    prompt_save = !prompt
;                    rp = ''
;                    read , rp , prompt = '1. Press ENTER to continue...'
;                    rp = strupcase(rp)
;                    !PROMPT = prompt_save
;                    IF rp EQ 'Q' THEN BEGIN
;                        CLOSE , /all
;                        message,'USER-BREAK'
;                    ENDIF
;                ENDIF

;            WINDOW , 0 , ret = 2 , XPOS = 0 , YPOS = 1000 , xsize = ImageSize , ysize = ImageSize, $
;            TITLE = 'Image' + STRTRIM( ROUND( img_num[ Image_Number + 1 ] ) ) + ' :' + STRTRIM( filelist[ img_num[ Image_Number + 1 ] ] )
;            TV , img_select[ * , * , Image_Number + 1 ]

;---Put hold point selected before P_ii+1 and Image_Number=0,..,Image_Number+1
                FOR jj = 0 , Image_Number DO BEGIN
                    XYOUTS , X_Cursor_Position[ jj ] - 8 , Y_Cursor_Position[ jj ] - 3 , '[' + STRTRIM( jj , 1 ) + ']' , /DEVICE
                ENDFOR
;---Select position P_ii+1 on the image (and i=0,..,ii+1)
;   and the great circle GC=(xx_cercle,yy_cercle) :
            IF Image_Number EQ 0 THEN BEGIN
			
;---First_Image_HeaderData : header of the first image. Only the header of the first image is used.
                First_Image_HeaderData = headerdata
                csi_img = itool_set_csi( headfits( filelist( Image_Number + 1 ) ) )


;			First_Image_HeaderData = headfits(eitlz+strtrim(strmid(filelist[img_num[ii+1]], 3, 4),2)$
;			+'\'+strtrim(strmid(filelist[img_num[ii+1]], 7, 2), 2)+'\'+strlowcase(filelist[img_num[ii+1]]))
; , stereofile , header_euvi , data_euvi
;    stereofile = './stereo/20090101_000600_n4euA.fts'
;    mreadfits , stereofile , header_euvi_out , data_euvi_out
;    secchi_prep , stereofile , header_euvi , data_euvi
;    STEREO_csi_img = itool_set_csi( header_euvi_out )
;    header_euvi_out , data_euvi_out
;    STEREO_csi_img = itool_set_csi( headfits( stereofile ) )
;STEREO_csi_img = itool_set_csi( headfits( stereofile ) )

;PRINT , 'PArameter  SWAP Parameter   STEREO Parameter'
;PRINT , 'NAXIS1 ' , csi_img.NAXIS1 , STEREO_csi_img.NAXIS1
;PRINT , 'NAXIS2 ' , csi_img.NAXIS2 , STEREO_csi_img.NAXIS2
;PRINT , 'CRPIX1 ' , csi_img.CRPIX1 , STEREO_csi_img.CRPIX1
;PRINT , 'CRPIX2 ' , csi_img.CRPIX2 , STEREO_csi_img.CRPIX2
;PRINT , 'CRVAL1 ' , csi_img.CRVAL1 , STEREO_csi_img.CRVAL1
;PRINT , 'CRVAL2 ' , csi_img.CRVAL2 , STEREO_csi_img.CRVAL2
;PRINT , 'CDELT1 ' , csi_img.CDELT1 , STEREO_csi_img.CDELT1
;PRINT , 'CDELT2 ' , csi_img.CDELT2 , STEREO_csi_img.CDELT2
;PRINT , 'CTYPE1 ' , csi_img.CTYPE1 , STEREO_csi_img.CTYPE1
;PRINT , 'CTYPE2 ' , csi_img.CTYPE2 , STEREO_csi_img.CTYPE2
;PRINT , 'CROTA ' , csi_img.CROTA , STEREO_csi_img.CROTA
;PRINT , 'REFLECT ' , csi_img.REFLECT , STEREO_csi_img.REFLECT
;PRINT , 'DATE_OBS ' , csi_img.DATE_OBS , STEREO_csi_img.DATE_OBS
;PRINT , 'FLAG ' , csi_img.FLAG , STEREO_csi_img.FLAG
;PRINT , 'RADIUS ' , csi_img.RADIUS , STEREO_csi_img.RADIUS


;---Now we make sure that we use the appropriate scale for displaying
;   our images
                ActualImageSize = Size( img_select )
                ScaleSize = ImageSize / ActualImageSize( 1 ) 
                csi_img.crpix1 = ScaleSize * csi_img.crpix1
                csi_img.crpix2 = ScaleSize * csi_img.crpix2
                csi_img.naxis1 = ScaleSize * csi_img.naxis1
                csi_img.naxis2 = ScaleSize * csi_img.naxis2
                csi_img.cdelt1 = ( 1.0 / ScaleSize ) * csi_img.cdelt1
                csi_img.cdelt2 = ( 1.0 / ScaleSize ) * csi_img.cdelt2
                csi_img.radius = ScaleSize * csi_img.radius

                rdpix4cme_SWAP , img_select[ * , * , 0 ] , X_Cursor_Position[ 0 ] , Y_Cursor_Position[ 0 ] , $
                  XVERTS , YVERTS , csi_img , Spheric_Or_Cart_Coordinates , XX_CERCLE , YY_CERCLE , /circle , Instrument = Instrument , header = headerdata( 0 ) , ScaleSize = Scalesize
            ENDIF
		IF Image_Number GT 0 THEN BEGIN
		    XYOUTS , xx_cercle , yy_cercle , '.' , /DEVICE

                    rdpix4cme_SWAP , img_select[ * , * , Image_Number + 1 ] , xx_cercle , yy_cercle , XVERTS , YVERTS , /addcircle , Instrument = Instrument , header = headerdata( Image_Number + 1 ) , ScaleSize = Scalesize

		ENDIF
		X_Cursor_Position[ Image_Number + 1 ] = xverts
		Y_Cursor_Position[ Image_Number + 1 ] = yverts
;---Time_Difference = time difference between images:
		IF time_julday[ Image_Number + 1 ] GE time_julday[ Image_Number ] THEN BEGIN
		    caldat , time_julday[ Image_Number + 1 ] - time_julday[ Image_Number ] + JULDAY( 1 , 1 , 1 , 0 , 0 , 0 ) , DM , DD , DY , DH , DMM , DS
		    Time_Difference[ Image_Number ] = ROUND( ( dd - 1 ) * 24 * 60 * 60 + dh * 60 * 60 + dmm * 60 + ds )
		ENDIF ELSE BEGIN
		    caldat , time_julday[ Image_Number ] - time_julday[ Image_Number + 1 ] + julday( 1 , 1 , 1 , 0 , 0 , 0 ) , DM , DD , DY , DH , DMM , DS
		    Time_Difference[ Image_Number ] = - ( ROUND( ( dd - 1 ) * 24 * 60 * 60 + dh * 60 * 60 + dmm * 60 + ds ) )
		ENDELSE
		; distance and speed between (P_ii, P_ii+1) and ii=0,..,ii+1 :




;---HERE65

;===IF Cartesian coordinate begin here
		IF (  Spheric_Or_Cart_Coordinates EQ 1 ) THEN BEGIN
		    dist_v , Time_Difference[ Image_Number ] , X_Cursor_Position[ Image_Number ] , $
                     Y_Cursor_Position[ Image_Number ] , X_Cursor_Position[ 1 + Image_Number ] , $
                     Y_Cursor_Position[ 1 + Image_Number ] , accuracy , OUT12 , Instrument = Instrument , Header = Headerdata( Image_Number ) , ScaleSize = Scalesize
		    
                    dist_v , Time_Difference[ Image_Number ] , X_Cursor_Position[ 0 ] , Y_Cursor_Position[ 0 ] , $
                     X_Cursor_Position[ 1 + Image_Number ] , Y_Cursor_Position[ 1 + Image_Number ] , $
                     accuracy , OUT0I , Instrument = Instrument , Header = Headerdata( 0 ) , ScaleSize = Scalesize

	            title1 = 'CARTESIAN COORDINATES: Distance and average speed between two points calculated in '
                    PRINT , ''
	            PRINT , 'Image number' + STRTRIM( ROUND( img_num[ Image_Number ] ) , 1 ) +  ' : ', filelist[ img_num[ Image_Number ] ]
	            PRINT , '  x_P' + STRTRIM( Image_Number , 1 ) + ' =' , ROUND( X_Cursor_Position[ 0 + Image_Number ] ) , ' pixels '
	            PRINT , '  y_P' + STRTRIM( Image_Number , 1 ) + ' =' , ROUND( Y_Cursor_Position[ 0 + Image_Number ] ) , ' pixels'
	            PRINT , 'Image number ' + STRTRIM( ROUND( img_num[ Image_Number + 1 ] ) , 1 ) + ' :  '  , filelist[ img_num[ Image_Number + 1 ] ]
	            PRINT , '  x_P' + STRTRIM( Image_Number + 1 , 1 ) + ' =' , ROUND( X_Cursor_Position[ 1 + Image_Number ] ) , ' pixels '
	            PRINT , '  y_P' + STRTRIM( Image_Number + 1 , 1 ) + ' =' , ROUND( Y_Cursor_Position[ 1 + Image_Number ] ) , ' pixels'
                    PRINT , title1
                    PRINT , '  dist(P' + STRTRIM( Image_Number , 1 ) + ',P' + STRTRIM( Image_Number + 1 , 1 ) + ')   = ' , out12[ 0 ] , ' Km     +/-' , out12[ 1 ] , ' Km'
	            PRINT , '  dist(P0,P' + STRTRIM( Image_Number + 1 , 1 ) + ')   = ' , out0I[ 0 ] , ' Km     +/-' , out0I[ 1 ],' Km'
	            PRINT , '  time(P' + STRTRIM( Image_Number , 1 ) + ',P' + STRTRIM( Image_Number + 1 , 1 ) + ')   = ', Time_Difference[ Image_Number ] , ' s'
                    PRINT , '  speed(P' + STRTRIM( Image_Number , 1 ) + ',P' + STRTRIM( Image_Number + 1 , 1 ) + ')  = ', out12[ 2 ] , ' Km/s   +/-' , out12[ 3 ] ,' Km/s'
                 ENDIF

;===IF Spherical coordiantes begin here 
	    IF (  Spheric_Or_Cart_Coordinates EQ 2 ) THEN BEGIN
		gcd_v, Time_Difference[ Image_Number ], X_Cursor_Position[ Image_Number ], Y_Cursor_Position[ Image_Number ], $
                  X_Cursor_Position[ 1 + Image_Number ] , Y_Cursor_Position[ 1 + Image_Number ] , accuracy, $
                  csi_img, OUT12, COORD_long_lat
		gcd_v, Time_Difference[ Image_Number ], X_Cursor_Position[ 0 ] , Y_Cursor_Position[ 0 ] , X_Cursor_Position[ 1 + $
                  Image_Number ] , Y_Cursor_Position[ 1 + Image_Number ] , accuracy , csi_img , OUT0I
		title1 = 'SPHERICAL COORDINATES: Distance and average speed between two points'
		PRINT , ''
		PRINT , 'Image number ' + STRTRIM( ROUND( img_num[ Image_Number ] ) , 1 ) + ' :  ' , filelist[ img_num[ Image_Number ] ]
		IF COORD_long_lat[ 0 ] GT 0 THEN latind = 'N' ELSE latind = 'S'
		PRINT , '  latitude_P' + STRTRIM( Image_Number , 1 ) + '   =' , ROUND( ABS( coord_long_lat[ 0 ] ) ) , ' ' , latind
		IF COORD_long_lat[ 1 ] GT 0 THEN longind = 'W' ELSE longind = 'E'
		PRINT , '  longitude_P' + STRTRIM( Image_Number , 1 ) + '  =' , ROUND( ABS( coord_long_lat[ 1 ] ) ) , ' ' , longind
		PRINT , 'Image number ' + STRTRIM( ROUND( img_num[ Image_Number + 1 ] ) , 1 ) + ' :  ' , filelist[ img_num[ Image_Number + 1 ] ]
		IF COORD_long_lat[ 2 ] GT 0 THEN latind = 'N' ELSE latind = 'S'
		PRINT , '  latitude_P' + STRTRIM( Image_Number + 1 , 1 ) + '   =' , ROUND( ABS( coord_long_lat[ 2 ] ) ) , ' ' , latind
		IF COORD_long_lat[ 3 ] GT 0 THEN longind = 'W' ELSE longind = 'E'
		PRINT , '  longitude_P' + STRTRIM( Image_Number + 1 , 1 ) + '  =' , ROUND( ABS( coord_long_lat[ 3 ] ) ) , ' ' , longind
		PRINT , title1
		PRINT , '  dist(P' + STRTRIM( Image_Number , 1 ) + ',P' + STRTRIM( Image_Number + 1 , 1 ) + ')   = ' , out12[ 0 ] , ' Km     +/-' , out12[ 1 ] ,' Km'
		PRINT , '  dist(P0,P' + STRTRIM( Image_Number + 1 , 1 ) + ')   = ' , out0I[ 0 ] ,' Km     +/-' , out0I[ 1 ] ,' Km'
		PRINT , '  time(P' + STRTRIM( Image_Number , 1 ) + ',P' + STRTRIM( Image_Number + 1 , 1 ) + ')   = ' , Time_Difference[ Image_Number ] , ' s'
	        PRINT , '  speed(P' + STRTRIM( Image_Number , 1 ) + ',P' + STRTRIM( Image_Number + 1 , 1 ) + ')  = ' , out12[ 2 ] , ' Km/s   +/-' , out12[ 3 ] ,' Km/s'
	    ENDIF

;===Output results
	    D12_vect[ Image_Number ] = out12[ 0 ]
	    D0I_vect[ Image_Number ] = out0I[ 0 ]
	    V12_vect[ Image_Number ] = out12[ 2 ]
	    err_D12[ Image_Number ]  = out12[ 1 ]
	    err_D0I[ Image_Number ]  = out0I[ 1 ]
	    err_V12[ Image_Number ]  = out12[ 3 ]

;===Create plot
	    Time_Difference_graphe[ Image_Number + 1 ] = Time_Difference[ Image_Number ] + Time_Difference_graphe[ Image_Number ]     ;second
	    Time_Difference_G = Time_Difference_graphe[ 1 : Image_Number + 1 ] / 60D ;minute
	    dummy = LABEL_DATE( DATE_FORMAT = [ '%H:%I' ] )
	    time_G = time_julday[ 1 : Image_Number + 1 ]
	    ImageTime = STRTRIM( STRMID( STRTRIM( Time_Difference_G , 1 ) , 0 , 2 ) ) + ':' + STRTRIM( STRMID( STRTRIM( Time_Difference_G , 1 ) , 3 , 2 ) )
	    FOR kk = 0 , N_ELEMENTS( Time_Difference_G ) - 1 DO BEGIN
                IF Time_Difference_G[ kk ] EQ 0 THEN ImageTime[ kk ] = '00:00'
            ENDFOR
	    IF img_num[ 0 ] EQ img_num[ 1 ] THEN im0to1 = 1 ELSE im0to1 = 0

            IF Image_Number GE 1 THEN BEGIN
		colors8 ; [0-black, 1-white, 2-red, 3-green, 4-blue, 5-yellow, 6-cyan, 7-magenta]
		WINDOW , 1 , RET = 2 , XPOS = 1000 , YPOS = 1000 , XSIZE = 750 , YSIZE = 750
		!P.MULTI = [ 0 , 1 , 2 , 0 , 0 ]
		!P.FONT = -1

		PLOT , time_G , D0I_vect[ 0 : Image_Number ] , background = 1 , COLOR = 0 , /NODATA , $
		  XSTYLE = 1 , XTICKFORMAT = 'label_date' , XTICKUNITS = 'Time' , $ ;xtickinterval=15,
		  XTITLE = 'Time of the second image (hh:mm)' , ytitle = 'Km' , $
		  YRANGE = [ D0I_vect[ 0 ] - 9272 , D0I_vect[ Image_Number ] + 9272 ] , $
		  YMARGIN = [ 5 , 5 ]
		AXIS , XAXIS = 1 , XSTYLE = 1 , XTICKS = Image_Number + 1 , XTICKV = time_G , XTICKN = ImageTime , $
		  XTITLE = TEXTOIDL( '\Deltat between the tow points (mm:ss)' ) , COLOR = 0
		OPLOT , time_G , D0I_vect[ 0 : Image_Number ] , PSYM = -2 , COLOR = 4
		ERRPLOT , time_G , D0I_vect[ 0 : Image_Number ] - err_D0I[ 0 : Image_Number ] , $
                  D0I_vect[ 0 : Image_Number ] + err_D0I[ 0 : Image_Number ] , COLOR = 4
		OPLOT , [time_G[ 0 ] + 0.001 ] , [ MAX( D0I_vect ) ] , PSYM = -2 , COLOR = 4
		XYOUTS , [ time_G[ 0 ] + 0.002 ] , [ MAX( D0I_vect ) ] , 'Distance' , COLOR = 4

		PLOT , time_G, V12_vect[ 0 : Image_Number ] , background = 1 , COLOR = 0 , /nodata , $
		  XSTYLE = 1 , XTICKFORMAT = 'label_date' , XTICKUNITS = 'Time' , $
		  XTITLE = 'Time of the second image (hh:mm)' , YTITLE = 'Km/s' , SUBTITLE = title1 , $
		  YRANGE = [ min( V12_vect[ im0to1 : Image_Number ] ) - 10 , MAX( V12_vect[ im0to1 : Image_Number ] ) + 10 ] , $
		  YMARGIN = [ 5 , 5 ]
		AXIS , XAXIS = 1 , XSTYLE = 1 , XTICKS = Image_Number + 1 , XTICKV = time_G , XTICKN = ImageTime , $
		  XTITLE = TEXTOIDL( '\Deltat between the tow points (mm:ss)' ) , COLOR = 0
		OPLOT, time_G, V12_vect[0:Image_Number], PSYM = -4 , COLOR = 2
		ERRPLOT , time_G , V12_vect[ 0 : Image_Number ] - err_V12[ 0 : Image_Number ] , $
                  V12_vect[ 0 : Image_Number ] + err_V12[ 0 : Image_Number ] , COLOR = 2
		IF im0to1 THEN BEGIN
		    OPLOT , time_G , V12_vect[0:1], PSYM = -4 , COLOR = 1
		    ERRPLOT , time_G[ 0 ] , V12_vect[ 0 ] - err_V12[ 0 ] , V12_vect[ 0 ] + err_V12[ 0 ] , COLOR = 1
		    OPLOT , [ time_G[ 1 ] ] , [ V12_vect[ 1 ] ] , PSYM = -4 , COLOR = 2
		ENDIF
		OPLOT , [ time_G[ 0 ] + 0.001 ] , [ max( V12_vect ) ] , PSYM = 4 , COLOR = 2
	    XYOUTS , [time_G[ 0 ] + 0.002 ] , [max( V12_vect ) ] , 'Average speed' , COLOR = 2
	    !P.MULTI = [ 0 , 1 , 1 , 0 , 0 ]
	    LOADCT , 0
	ENDIF
	img_font = '-adobe-helvetica-medium-r-normal--14-100-100-100-p-76-iso8859-1'
;=============== END P L O T =============

	PRINT , '  '
	PRINT , 'Enter 2 to continue, 1 to restart, 0 to stop'
	READ , cont_rest_stop , PROMPT = ' '


	IF ( ( img_type EQ 1 ) AND ( img_num[ Image_Number + 1 ] EQ Number_Of_Frames - 1) ) OR $
	  ( (img_type GE ( 2 AND 3 ) ) AND ( img_num[ Image_Number + 1 ] GE Number_Of_Frames - 2 ) ) THEN BEGIN
	    IF cont_rest_stop EQ 2 THEN BEGIN
	        PRINT , 'No more images : Enter 1 to restart, 0 to stop'
	        READ , cont_rest_stop, Prompt = ' '
	    ENDIF
	ENDIF
	IF ( cont_rest_stop EQ 2 ) THEN BEGIN
	    Repeat_Observations = 1
	    Image_Number = Image_Number + 1
	    img_num[ Image_Number + 1 ] = img_num[ Image_Number ] + 1
	ENDIF
	save_now = 0
	IF cont_rest_stop EQ 0 THEN BEGIN
	    Repeat_Observations = 0
	    start = 0
	    IF Image_Number GE 1 THEN BEGIN
	        PRINT , 'Do you want save ? Enter 1 to save, 0 to not save'
	        READ , save_now , Prompt = ' '
	    ENDIF
	ENDIF
        IF cont_rest_stop EQ 1 THEN BEGIN
	    Repeat_Observations = 0
	    start = 1
	    IF Image_Number GE 1 THEN BEGIN
	        PRINT , 'Do you want save ? Enter 1 to save, 0 to not save'
	        READ , save_now , Prompt = ' '
	    ENDIF
        ENDIF
    ENDIF

    ENDREP UNTIL ( Repeat_Observations EQ 0 )

    IF Image_Number GT 0 THEN WDELETE

;===Write the velocities to a file
    IF save_now THEN BEGIN
        IF file_test(saveloc + header[0].DATE_OBS) EQ 0 THEN file_mkdir, saveloc + header[0].DATE_OBS ; 2016/07/12: James Paul Mason: Make a ~unique directory to store analysis data in
        OPENW , 1 , saveloc + header[0].DATE_OBS + '/data_d_v.txt'
	PRINTF , 1 , ';' , title1
	PRINTF , 1 , ';'
	PRINTF , 1 , ';' , 'Contains:'
	PRINTF , 1 , ';' , 'n   lines for images name,'
	PRINTF , 1 , ';' , 'n-1 lines for times differences,'
	PRINTF , 1 , ';' , 'n-1 lines for distances, error, disatances from origine, error,'
	PRINTF , 1 , ';' , 'n-2 lines for speed, error.'
	PRINTF , 1 , ';' , ''
	PRINTF , 1 , ';' , ' n ='
	PRINTF , 1 , Image_Number + 1
	PRINTF , 1 , ';'
	PRINTF , 1 , '; EIT Images Name  '
	PRINTF , 1 , TRANSPOSE( STRTRIM( filelist[ img_num[ 0 : Image_Number + 1 ] ] , 1 ) )
	PRINTF , 1 , '; Time_Difference between the two images (mm:ss):'
	PRINTF , 1 , TRANSPOSE( STRTRIM( [ ImageTime[ 0 : Image_Number ] ] ) )
	PRINTF , 1 , '; Distance between the two points selected (Km):'
	PRINTF , 1 , TRANSPOSE( STRTRIM( D12_vect[ 0 : Image_Number ] , 1 ) )
	PRINTF , 1 , '; +/- correstonding Error:'
	PRINTF , 1 , TRANSPOSE( STRTRIM( err_D12[ 0 : Image_Number ] , 1 ) )
	PRINTF , 1 , '; Distance from the point 0 selected (Km):'
	PRINTF , 1 , TRANSPOSE( STRTRIM( D0I_vect[ 0 : Image_Number ] , 1 ) )
	PRINTF , 1 , '; +/- correstonding Error'
	PRINTF , 1 , TRANSPOSE( STRTRIM( err_D0I[ 0 : Image_Number ] , 1 ) )
	PRINTF , 1 , '; Average_Speed between the two points selected (Km/s):'
	PRINTF , 1 , TRANSPOSE( STRTRIM( V12_vect[ 1 : Image_Number ] , 1 ) )
	PRINTF , 1 , '; +/- correstonding Error'
	PRINTF , 1 , TRANSPOSE( STRTRIM( err_V12[ 1 : Image_Number ] , 1 ) )
	CLOSE , 1
	PRINT , '   Data has been saved in ', saveloc + header[0].DATE_OBS + '/data_d_v.txt'
	
	; 2016/07/13: James Paul Mason: average of all speeds and error printed to new file
	openw, lun, saveloc + header[0].DATE_OBS + '/Average CME Speed.txt', /GET_LUN
	printf, lun, 'Average speed [km/s] = ' + JPMPrintNumber(mean(V12_vect[0:image_number])) 
	printf, lun, 'Average error [km/s] = ' + JPMPrintNumber(sqrt(total(err_V12[0:image_number]^2))) ; sqrt of the sum of squares for uncertainty propagation
	close, lun
	
;===Create plots of velocity and position
	SET_PLOT , 'ps'
	DEVICE , /INCHES , XSIZE = 8.0 , YSIZE = 8.0 , FILENAME = saveloc + header[0].DATE_OBS + '/graph_d_v.ps' , BITS_PER_PIXEL = 8 , /COLOR
	COLORS8 ; [0-black, 1-white, 2-red, 3-green, 4-blue, 5-yellow, 6-cyan, 7-magenta]
        !P.MULTI = [ 0 , 1 , 2 , 0 , 0 ]
	!P.FONT = -1
	PLOT , time_G , D0I_vect[ 0 : Image_Number ] , BACKGROUND = 1 , COLOR = 0 , /NODATA , $
          XSTYLE = 1 , XTICKFORMAT = 'label_date' , XTICKUNITS = 'Time' , $      ;xtickinterval=15,
          XTITLE = 'Time of the second image (hh:mm)', YTITLE = 'Km', $
          YRANGE = [ D0I_vect[ 0 ] , D0I_vect[ Image_Number ] ] , $
          YMARGIN = [ 5 , 5 ]
	AXIS , XAXIS = 1 , XSTYLE = 1 , XTICKS = Image_Number + 1 , XTICKV = time_G , XTICKN = ImageTime, $
	  XTITLE = TEXTOIDL( '\Deltat between the tow points(mm:ss)' ) , color = 0
	OPLOT , time_G , D0I_vect[ 0 : Image_Number ] , PSYM = -2 , COLOR = 4
	ERRPLOT , time_G , D0I_vect[ 0 : Image_Number ] - err_D0I[ 0 : Image_Number ] , D0I_vect[ 0 : Image_Number ] + err_D0I[ 0 : Image_Number ] , COLOR = 4
	OPLOT , [ time_G[ 0 ] + 0.001 ] , [ MAX( D0I_vect ) ] , PSYM = -2 , COLOR = 4
        XYOUTS,[time_G[0]+0.002], [max(D0I_vect)], 'Distance',  COLOR = 4

	PLOT , time_G , V12_vect[ 0 : Image_Number ] , BACKGROUND = 1 , COLOR = 0, /NODATA , $
	  XSTYLE = 1 , XTICKFORMAT = 'label_date' , XTICKUNITS = 'Time' , $
	  XTITLE = 'Time of the second image (hh:mm)' , YTITLE = 'Km/s' , SUBTITLE = title1 , $
          YRANGE = [ min( V12_vect[ 1 : Image_Number ] ) - 10 , max( V12_vect[ 1 : Image_Number ] ) + 10 ] , $
          YMARGIN = [ 5 , 5 ]
	AXIS , XAXIS = 1 , XSTYLE = 1 , XTICKS = Image_Number + 1 , XTICKV = time_G , XTICKN = ImageTime , $
          XTITLE = textoidl( '\Deltat between the two points (mm:ss)') , COLOR = 0
	OPLOT , time_G , V12_vect[ 0 : Image_Number ] , PSYM = -4 , COLOR = 2
	ERRPLOT , time_G , V12_vect[ 0 : Image_Number ] - err_V12[ 0 : Image_Number ] , V12_vect[ 0 : Image_Number ] + err_V12[ 0 : Image_Number ] , COLOR = 2
	OPLOT , time_G , V12_vect[ 0 : 1 ] , PSYM = -4 , COLOR = 1
	ERRPLOT , time_G[ 0 ] , V12_vect[ 0 ] - err_V12[ 0 ] , V12_vect[ 0 ] + err_V12[ 0 ] , COLOR = 1
	OPLOT , [ time_G[ 1 ] ] , [ V12_vect[ 1 ] ] , PSYM = -4 , COLOR = 2
        OPLOT , [ time_G[ 0 ] + 0.001 ] , [ max( V12_vect ) ] , PSYM = 4 , COLOR = 2
	XYOUTS , [ time_G[ 0 ] + 0.002 ] , [ max( V12_vect ) ] , 'Average speed' , COLOR = 2
	!P.MULTI = [ 0 , 1 , 1 , 0 , 0 ]
	LOADCT , 0
	DEVICE , /CLOSE
	SET_PLOT , 'x'
	PRINT , ' Graph has been saved in ', saveloc + header[0].DATE_OBS + '/graph_d_v.ps'

;=====================
	FONT = !p.font & !p.font = 0 & device, font= img_font
	SET_PLOT , 'ps'
	FOR kk = 0 , Image_Number + 1 DO BEGIN
	    DEVICE , /inches , xsize = 6.0 , ysize = 6.0 , filename = saveloc + header[0].DATE_OBS + '/img_' + STRTRIM( kk , 1 ) + '.ps' , bits_per_pixel = 8 , /color
	    TV , img_select[ * , * , kk ]
	    XYOUTS , xx_cercle * 3E1 , yy_cercle * 3E1 , '.' , /DEVICE , COLOR = 255
	    FOR jj = 0 , kk DO BEGIN
                XYOUTS , ( X_Cursor_Position[ jj ] - 8 ) * 3E1 , ( ( Y_Cursor_Position[ jj ] ) - 3 ) * 3E1 , '[' + STRTRIM( jj , 1 ) + ']' , /DEVICE , color = 255
	    ENDFOR
	    DEVICE , /CLOSE
	ENDFOR
	SET_PLOT , 'x'
	!P.FONT = -1
	PRINT , '   Images has been saved in ' , saveloc + header[0].DATE_OBS + '/img_*.ps'
        PRINT , ''
;=====================
    ENDIF
    ENDREP UNTIL ( start EQ 0 )

    !P.MULTI = [ 0 , 1 , 1 , 0 , 0 ]

    img_name    = filelist[ img_num[ 0 : Image_Number + 1 ] ]
    times       = ImageTime[ 0 : Image_Number ]
    dist_rd     = D12_vect[ 0 : Image_Number ]
    err_dist_rd = err_D12[ 0 : Image_Number ]
    dist_bd     = D0I_vect[ 0 : Image_Number ]
    err_dist_bd = err_D0I[ 0 : Image_Number ]
    speed       = V12_vect[ 1 : Image_Number ]
    err_speed   = err_V12[ 1 : Image_Number ]

END
