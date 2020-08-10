Pro rdpix4cme_swap, Image, X0, Y0, X, Y, csi_img, coord, XX_PIXEL_CERCLE , YY_PIXEL_CERCLE, $
normal = normal , circle = circle , addcircle = addcircle , Instrument=Instrument , header=header , ScaleSize = Scalesize


;Example :
; AA=dindgen(512,512)
; for ii=0,511 do begin & AA[*,ii]=ii-(512/2) & endfor
; xx=AA
; yy=transpose(AA)
; disc = ((183D-10)^2 gt xx^2+yy^2)+( xx^2+yy^2 gt 183D^2)
; window, ret = 2, xsize = 512, ysize = 512
; tvscl, disc
;
; image = disc
; hr=headfits('$EIT_LZ/1997/05/efz19970512.043450')
; csi_img = itool_set_csi(hr)
; coord = 2 ;(sph�rical coordinate)
; rdpix4cme_trace, Image, 270, 270, X, Y, csi_img, coord, /circle

    COMPILE_OPT strictarr
    on_error , 2              ;Return to caller if an error occurs
;    PRINT , 'Press left or center mouse button for new output line.'
;    PRINT , '... right mouse button to exit.'
    Image_Parameters = size( image )

;---See if there are enough images to calculate a velocity
    IF Image_Parameters[ 0 ] NE 2 THEN MESSAGE , 'Image parameter not 2d.'

;---Find size of images
    Image_Parameters[ 1 ] = Image_Parameters[ 1 ] - 1           ;To n-1
    Image_Parameters[ 2 ] = Image_Parameters[ 2 ] - 1
    !MOUSE.BUTTON = 0
    IF N_ELEMENTS( x0 ) LE 0 THEN x0 = 0
    IF N_ELEMENTS( y0 ) LE 0 THEN y0 = 0

;---See if there is more than four images
    IF Image_Parameters[ Image_Parameters[ 0 ] + 1 ] GE 4 THEN form = 'F' ELSE form = 'I'

;---Define the type of computer used as this defines the ket stroke
    CASE !version.os_family OF
        'Windows': cr = STRING( "15b ) + STRING( "12b )       ; carriage and new line
        'MacOS': cr = STRING( "15b )                           ; carriage return"
        'unix': cr = STRING( "15b )                            ; carriage (for BC on UNIX use CR rather than CR/LF)
    ELSE: cr = STRING( "15b )                                  ; carriage return
    ENDCASE

;---The FORM is used to print to the screen the values from each left click
    FORM = "($,'x=',i4,', y=',i4,', value=',"+form+",a)"


;---If screen is not big enough use this to align the bottom of the image.
    IF N_ELEMENTS( Image[ * , 0 ] ) LT 1024 THEN Image_Size = N_ELEMENTS( Image[ * , 0 ] ) ELSE Image_Size = 1024

;    IF ( !d.y_size + !d.x_size NE Image_Size ) THEN BEGIN
;;        PRINT, 'Position the scroll will to align with bottom of image if screen not big enough'
;        prompt_save = !prompt
;        PRINT , ''
;        rp = ''
;        read , rp , prompt = 'If you need to reposition sun for measurments, move scroll bar and press ENTER key to continue...'
;        rp = strupcase(rp)
;        !prompt=prompt_save
;        IF rp EQ 'Q' THEN BEGIN
;             close,/all
;             message,'USER-BREAK'
;        ENDIF
;    ENDIF

;---Set commands for reading points
    PRINT
    PRINT , 'left click on Sun to select point (pixel number will be printed to the terminal)'
    PRINT , 'When happy with position, RIGHT click to ouput pixel numbers'
    IF KEYWORD_SET( normal ) THEN BEGIN

;---While NO right click then keep reading values
        WHILE !mouse.button NE 4 DO BEGIN

;---Read out cursor position
	    CURSOR , X , Y , 2 , /dev

;---Print out values following each left click, wait 1 sec and then
;   print '+' on image 
    
            IF ( !mouse.button AND 3 ) NE 0 THEN BEGIN        ;New line?
		PRINT , FORM = "($,a)" , STRING( "12b )         ;"
		WHILE ( !mouse.button NE 0 ) DO BEGIN 
		    WAIT , .1 
		    CURSOR , X , Y , 0 , /dev 
		    XYOUTS , X - 8 , Y -3 , '[+]' , /device	
		ENDWHILE
	    ENDIF
	    
;---Define the position of X and Y subtracting the minimum offset,
;   also see that point is less than the size of the image 
;   Outputs a 'X' at final location
            X = X - X0
            Y = Y - Y0
            IF ( x LE Image_Parameters[ 1 ] ) AND ( y LE Image_Parameters[ 2 ] ) AND ( x GE 0 ) AND ( y GE 0 ) THEN BEGIN
                IF ( !order EQ 1 ) THEN yy = Image_Parameters[ 2 ] - y ELSE yy = y
                PRINT , FORM = FORM , x , y , Image[ x , yy ] , cr
            ENDIF
	ENDWHILE
	PRINT , FORM = "(/)"
	XYOUTS , X - 8 , Y - 3 , '[X]' , /device
    ENDIF

;---Draws a circle on the screen?
    IF KEYWORD_SET( circle ) THEN BEGIN
	WHILE !mouse.button NE 4 DO BEGIN
	    CURSOR , X , Y , 2 , /dev
	    IF ( !MOUSE.BUTTON AND 3 ) NE 0 THEN BEGIN        ;New line?
	        PRINT , FORM = "($,a)" , STRING( "12b )           ;"
	        WHILE ( !MOUSE.BUTTON NE 0 ) DO BEGIN 
	            WAIT , .1 
                    CURSOR , X , Y , 0 , /dev 
                    XYOUTS , X - 8 , Y - 3 , '[+]' , /DEVICE
                    GREAT_CIRCLE_SWAP , X0 , Y0 , X , Y , csi_img , coord , XX_PIXEL_CERCLE , YY_PIXEL_CERCLE , Instrument = Instrument , Header = Header , ScaleSize = Scalesize
		    XYOUTS , xx_pixel_cercle , yy_pixel_cercle , '.' , /DEVICE	
	        ENDWHILE
            ENDIF
	    IF ( x LE Image_Parameters[ 1 ] ) AND ( y LE Image_Parameters[ 2 ] ) AND ( x GE 0 ) AND ( y GE 0 ) THEN BEGIN
	        IF ( !order EQ 1 ) THEN yy = Image_Parameters[ 2 ] - y ELSE yy = y
		PRINT , form = form , x , y , Image[ x , yy ] , cr
	    ENDIF
	ENDWHILE
	PRINT , FORM = "(/)"
	XYOUTS , X - 8 , Y - 3 , '[x]' , /DEVICE
	great_circle_SWAP , X0 , Y0 , X , Y , csi_img , coord , XX_PIXEL_CERCLE , YY_PIXEL_CERCLE , Instrument = Instrument , Header = Header , ScaleSize = Scalesize
	;XTOUTS , XX_PIXEL_CERCLE , YY_PIXEL_CERCLE , '.' , /DEVICE	
    ENDIF



if keyword_set( addcircle ) then begin
	xx_c = X0
	yy_c = Y0
	while !mouse.button ne 4 do begin
		CURSOR, X, Y, 2, /dev
		if (!mouse.button and 3) ne 0 then begin        ;New line?
		print,form="($,a)",string("12b)                 ;"
			while (!mouse.button ne 0) do begin 
				wait,.1 
				CURSOR, X, Y, 0, /dev 
				;=======================
				dist = dindgen(n_elements(xx_c))
				for ii = 0, n_elements(xx_c)-1 do begin 
					dist_v , 3, xx_c[ii], yy_c[ ii ], X, Y, 1, out , Instrument = Instrument , Header = Header , ScaleSize = Scalesize
					dist[ii] = out[0]
				endfor
				dist_index = where(dist eq min(dist)) 
				X = xx_c[dist_index [0]]
				Y = yy_c[dist_index [0]]
				;=======================
				XYOUTS, X-8, Y-3, '[+]', /DEVICE
			end
		endif
		if (x le Image_Parameters[1]) and (y le Image_Parameters[2]) and (x ge 0) and (y ge 0) then begin
			if (!order eq 1) then yy = Image_Parameters[2] - y else yy = y
			print,form = form , x , y , Image[ x , yy ] , cr
		endif
	endwhile
	print,form="(/)"
	dist = dindgen(n_elements(xx_c))
	for ii = 0, n_elements(xx_c)-1 do begin 
		dist_v , 3 , xx_c[ ii ] , yy_c[ ii ] , X , Y , 1 , out , Instrument = Instrument , Header = Header , ScaleSize = Scalesize
		dist[ ii ] = out[ 0 ]
	endfor
	dist_index = where( dist EQ min( dist ) ) 
	X = xx_c[ dist_index [ 0 ] ]
	Y = yy_c[ dist_index [ 0 ] ]
	XYOUTS , X - 8 , Y - 3 , '[X]' , /DEVICE
endif

WDELETE

END
