; This routine
;
; Created by B. Thompson 18-Jun-2004
; Following modifications by A. Zhukov Aug-2004
;

pro b_movie2swap, infile, ImagesIn, TIME_ARRAY, both=both, half=half, timerange=timerange,$
object=object, lz=lz, wave=wave, nofake=nofake, stamp=stamp,timearr=timearr,$
float = float , magic = magic , qtr = qtr , phot_norm = phot_norm , $
response_norm = response_norm , Image_Size = Image_Size

;===Read in optional parameters
    IF NOT KEYWORD_SET( magic ) THEN magic = 0 
    IF NOT KEYWORD_SET( qtr )  THEN qtr = 0 
    IF NOT KEYWORD_SET( infile ) THEN $
      infile = eit_catrd( timerange = timerange , lz = lz , object = object , wave = wave)
    IF NOT KEYWORD_SET(nofake) THEN nofake=0 
    IF NOT KEYWORD_SET( Image_Size ) THEN Image_Size = 512
    IF NOT KEYWORD_SET( phot_norm ) THEN phot_norm = 0 ELSE phot_norm = 1
    IF NOT KEYWORD_SET( float ) THEN float = 0 
    IF NOT KEYWORD_SET( response_norm ) THEN response_norm = 0 

;---To create a list of files
    needed_listing = infile( where( infile ne '' ) ) 
    frames = n_elements( needed_listing )

;---If not enough frames, create some fake ones
    IF ( frames LT 2 ) THEN nofake = 1 

;---Create Image and time arrays
    ImagesOut = FLTARR( Image_Size , Image_Size , frames )
    time_array = STRARR( frames )

;---Read in header files
    FOR i = 0 , frames - 1 do begin
        hr = headfits(needed_listing[ i ] )
        zero = WHERE( ImagesIn LT 0, count1 )
        IF count1 NE 0 THEN ImagesIn( zero ) = 0
        sz = eit_fxpar(hr, 'naxis1')

        IF sz( 0 ) NE Image_Size THEN BEGIN
            Images = REBIN( ImagesIn[ * , * , i ] , Image_Size, Image_Size ) * 4.0
            ImagesOut[ * , * , i ] = Images
            ImagesIn = ImagesOut
        ENDIF
        time_array[ i ] = strmid( eit_fxpar( hr , 'date-obs' ) , 0 , 10 ) + ' ' + strmid( eit_fxpar( hr , 'date-obs' ) , 11 , 8 )

    ENDFOR
    RETURN
END 
