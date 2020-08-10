; Program to draw artificial limb on Sun

FUNCTION circle_limb_SWAP, Image , headerdata , Line_Thickness = Line_Thickness , twolimb = twolimb , Image_Size = Image_Size
;   pointing gives the date date or xcen ycen, radius
;   twolimb is used to create a black and white limb, this helps
;   visibility

    IF NOT keyword_set( Line_Thickness ) THEN Line_Thickness = .005
    IF NOT keyword_set( twolimb ) THEN twolimb = 0

    Image_Dimensions = SIZE( Image )

;    IF Image_Dimensions( 1 ) EQ 512 THEN half = 1 ELSE half = 0


;---Set as a default the scaling factor as 1.
    Scaling_Factor = 1.0
    IF Image_Dimensions( 1 ) NE Image_Size THEN BEGIN
        Scaling_Factor = Image_Size / Image_Dimensions( 1 )
    ENDIF

;---Define center point
    xcen = headerdata.crpix1 * Scaling_Factor
    ycen = headerdata.crpix2 * Scaling_Factor
;        xcen = 511.5
;        ycen = 511.5

;---euvi
    IF headerdata.INSTRUME EQ 'euvi' then begin
        radius = headerdata.rsun * Scaling_Factor / headerdata.cdelt1
    ENDIF

;---aia
    IF headerdata.INSTRUME EQ 'aia' then begin
        radius = headerdata.r_sun * Scaling_Factor ;/headerdata.cdelt1
    ENDIF

;---SWAP
    IF headerdata.INSTRUME EQ 'SWAP' then begin
;        radius = headerdata.RSUN_ARC * Scaling_Factor
        radius = headerdata.RSUN_ARC * Scaling_Factor / headerdata.cdelt1

;        radius = radius / 3.2
    ENDIF
    

;    IF Image_Dimensions( 1 ) EQ 512 THEN radius = headerdata.RSUN_ARC / 2.0 ELSE radius = headerdata.RSUN_ARC 

;---3.2 arcsec pixels in SWAP
;    radius = radius / 3.2

;    IF keyword_set( half ) THEN xcen = xcen * 0.5
;    IF keyword_set( half ) THEN ycen = ycen * 0.5
;    IF keyword_set( half ) THEN radius = radius

;    IF n_elements( radius ) GT 1 THEN BEGIN 
;        FOR iStep = 0 , n_elements( radius ) - 1 DO BEGIN
;            IF radius( iStep ) GT 300. THEN radius( iStep ) = radius( iStep ) * 0.15
;        ENDFOR
;    ENDIF ELSE BEGIN
;        IF radius gt 300. then radius = radius * 0.5
;    ENDELSE


;---This program constructs a square array called circle where each value is its distance to a given center
    dist_circle , circle , [ Image_Dimensions( 1 ) , Image_Dimensions( 2 ) ] , xcen( 0 ) , ycen( 0 )
    IF Image_Dimensions( 1 ) LT Image_Dimensions( 2 ) THEN Min_Dimension_Size = Image_Dimensions( 1 ) 
    IF Image_Dimensions( 2 ) LE Image_Dimensions( 1 ) THEN Min_Dimension_Size = Image_Dimensions( 2 ) 


;---We then create an array of points, containing the values of the radius
    Radius_Array = WHERE( ( circle GT radius * ( 1. - Line_Thickness ) ) $ 
                      AND ( circle LE radius * ( 1. + Line_Thickness ) ) $
                      );AND ( circle LT Min_Dimension_Size * 1.0 ) )


    IF ( twolimb ) THEN BEGIN
        Radius_Array = WHERE( ( circle GT radius * ( 1. + Line_Thickness ) ) $
                         AND ( circle LE radius * ( 1. + 2. * Line_Thickness ) ) )
        Radius_Array1 = WHERE( ( circle GT radius * ( 1. + 2. * Line_Thickness ) ) $
                         AND ( circle LE radius * ( 1. + 3. * Line_Thickness ) ) )
    ENDIF


;---Draw array on the original image
    Image( Radius_Array ) = 0
    IF ( twolimb ) THEN BEGIN
        Image( Radius_Array1 ) = 0
        Image( Radius_Array ) = 255
    ENDIF

    RETURN , Image

END

