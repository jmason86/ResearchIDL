pro great_circle_SWAP , x_P1 , y_P1 , x_P2 , y_P2 , csi_img , coord , XX_RESULT , YY_RESULT , Instrument = Instrument , Header = Header , ScaleSize = Scalesize
;---Program to draw circle or line on disk for the next point in the
;   image. coord 1 is cartesian and coord 2 is spherical


;===For Spherical Coordinates
    IF coord EQ 2 THEN BEGIN

;---From pixel to heliosphéric coordined (in rad for IDL)
        helio_P1 = cnvt_coord( x_P1 , y_P1 , csi = csi_img , from = 2 , to = 4 )
        lat1 = ( helio_P1[ 0 ] ) * ( !dPI / 180D )
        lon1 = ( helio_P1[ 1 ] ) * ( !dPI / 180D )

        helio_P2 = cnvt_coord( x_P2 , y_P2 , csi = csi_img , from = 2 , to = 4 )
        lat2 = ( helio_P2[ 0 ] ) * ( !dPI / 180D )
        lon2 = ( helio_P2[ 1 ] ) * ( !dPI / 180D )

;---Intersection long=+/-90°
        lon_p90 = 90 * (!dPI / 180D )
        lon_n90 = -90 * (!dPI / 180D ) 
    
        C_2 = ( cos( lon2 ) * cos( lat2 ) * sin( lat1 ) - cos( lon1 ) * cos( lat1 ) * sin( lat2 ) )
        C_3 = cos( lat1 ) * cos( lat2 ) * sin( lon2 - lon1 )

        lat_p90 = !dPI/2 + atan( ( +1 ) * C_3 / C_2 )
        lat_n90 = !dPI/2 + atan( ( -1 ) * C_3 / C_2 ) + !dPI

;---Calculate latitude
        FOR ii = 0 , 1 DO BEGIN
            lat_p = ( ( DINDGEN( 1000 ) / 1000 * 180 ) + ( lat_p90 ) * ( 180D / !dPI ) + ( 180 ) * ii ) * ( !dPI / 180D )
            C1 = ( SIN( lat2 ) * SIN( lon1 ) * COS( lat1 ) - SIN( lat1 ) * SIN( lon2 ) * COS( lat2 ) ) * COS( lat_p )
            C2 = ( COS( lon2 ) * COS( lat2 ) * SIN( lat1 ) - COS( lon1 ) * COS( lat1 ) * SIN( lat2 ) ) * COS( lat_p )
            C3 =  COS( lat1 ) * COS( lat2 ) * SIN( lon2 - lon1 ) * SIN( lat_p )
            C5 = C2 / C1
            C4 = -C3 / C1 * COS( ATAN( C5 ) )
            B = WHERE( C4 GT -1 AND C4 LT 1 )
            IF ii EQ 0 THEN BEGIN
                lon_gc_0 = ACOS( C4[ B ] ) + ATAN( C5[ B ] )
                lat_gc_0 = lat_p[ B ]
            ENDIF
            IF ii EQ 1 THEN BEGIN
                lon_gc_1 = ACOS( C4[ B ] ) + ATAN( C5[ B ] )
                lat_gc_1 = lat_p[ B ]
            ENDIF
        ENDFOR

;----Convert to degrees
        Pixel_Cercle_0 = cnvt_coord( lat_gc_0 * ( 180D / !dPI ) , lon_gc_0 * ( 180D / !dPI ) , csi = csi_img , from = 4 , to = 2 )
        xx_result_0 = INTERPOL( Pixel_Cercle_0[ * , 0 ] , 5000 )
        yy_result_0 = INTERPOL( Pixel_Cercle_0[ * , 1 ] , 5000 )

        Pixel_Cercle_1 = cnvt_coord( lat_gc_1 * ( 180D / !dPI ) , lon_gc_1 * ( 180D / !dPI ) , csi = csi_img , from = 4 , to = 2 )
        xx_result_1 = INTERPOL( Pixel_Cercle_1[ * , 0 ] , 5000 )
        yy_result_1 = INTERPOL( Pixel_Cercle_1[ * , 1 ] , 5000 )

;===Choice
        dist_0 = DINDGEN( 5000 )
        dist_1 = DINDGEN( 5000 )
        FOR kk = 0 , 5000 - 1 DO BEGIN
            dist_v , 1 , xx_result_0[ kk ] , yy_result_0[ kk ] , x_P1 , y_P1 , 1 , out0 , Instrument = Instrument , Header = Header , ScaleSize = Scalesize
            dist_0[ kk ] = out0[ 0 ]
            dist_v , 1 , xx_result_1[ kk ] , yy_result_1[ kk ] , x_P1 , y_P1 , 1 , out1 , Instrument = Instrument , Header = Header , ScaleSize = Scalesize
            dist_1[ kk ] = out1[ 0 ]
        ENDFOR
        xx_result = xx_result_0
        yy_result = yy_result_0
	IF min( dist_1 ) LT min( dist_0 ) THEN BEGIN
            xx_result = xx_result_1
            yy_result = yy_result_1
        ENDIF
    ENDIF

    IF coord EQ 1 THEN BEGIN
        vect = dindgen( csi_img.naxis1 * sqrt( 2 ) ) / ( max( dindgen( csi_img.naxis1 * sqrt( 2 ) ) ) ) * ( csi_img.naxis1 - 1 )
        IF ( x_P2 - x_P1 ) EQ 0 THEN BEGIN
            x = x_P1 + vect * 0
            y = vect
        ENDIF
        IF ( x_P2 - x_P1 ) NE 0 THEN BEGIN
            x = vect
            a = ( y_P2 - y_P1 ) / ( x_P2 - x_P1 )
            b = ( y_P1 * x_P2 - y_P2 * x_P1 ) / ( x_P2 - x_P1 )
            y = a * x + b
        ENDIF
        xx_result = ROUND( x )
        yy_result = ROUND( y )
    ENDIF

END
