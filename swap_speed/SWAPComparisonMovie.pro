PRO SWAPComparisonMovie , img , img2 , scl = scl , ind_scl = ind_scl , rate = rate , Image_Size = Image_Size

;---Resize the video so it fits on the screen, create a new array of values
    sz = size( img )
    frames = sz( 3 )
    Scale  = Image_Size * 1.0 / sz( 2 )
    Images = FLTARR( sz( 1 ) * Scale , sz( 2 ) * Scale , frames )
    Images2 = FLTARR( sz( 1 ) * Scale , sz( 2 ) * Scale , frames )
    
    IF sz( 1 ) NE Image_Size THEN BEGIN
        FOR iStep = 0 , frames - 1 do begin
            ImagesTemp = REBIN( Img[ * , * , iStep ] , Image_Size, Image_Size ) * 4.0
            Images[ *, * , iStep ] = ImagesTemp
            ImagesTemp = REBIN( Img2[ * , * , iStep ] , Image_Size, Image_Size ) * 4.0
            Images2[ *, * , iStep ] = ImagesTemp
        ENDFOR
    ENDIF

;---Now we combine the movies side by side
    img = [ Images , Images2 ]

;---Program to display difference and original movie
    IF NOT keyword_set( rate ) THEN rate = 50 
    sz = size( img )
    IF NOT keyword_set( ind_scl ) THEN ind_scl = 0
    IF ( sz( sz( 0 ) + 1 ) NE 1 ) THEN IF NOT ( ind_scl ) THEN scl = 1 

    IF NOT keyword_set( scl ) THEN scl = 0
    IF NOT keyword_set( ind_scl ) THEN ind_scl = 0

    IF ( scl ) THEN img1 = bytscl( img > .5 )
    size_b0 = SIZE( img )

;---Animate the movie
    XINTERANIMATE , SET = size_b0( 1 : 3 ) , /SHOW , /track 
    IF ( ind_scl ) THEN $ 
    FOR ii = 0 , size_b0( 3 ) - 1 DO XINTERANIMATE , FRAME = ii , IMAGE = bytscl( img( * , * , ii ) > .5 ) $
       ELSE IF ( scl ) THEN $
    FOR ii = 0 , size_b0( 3 ) - 1 DO XINTERANIMATE , FRAME = ii , IMAGE = img1( * , * , ii )  ELSE $
    FOR ii = 0 , size_b0( 3 ) - 1 DO XINTERANIMATE , FRAME = ii , IMAGE = img( * , * , ii )  
    XINTERANIMATE, rate ,/keep, block = 1
END   
