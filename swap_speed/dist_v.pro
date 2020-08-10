; distance and velocity
;
; INPUT : 
;	TIME1 : time of point 1 (seconde)
;	X1    : Xaxis of point 1 (pixels)
;	Y1  : latitude of point 1 (pixels)
;	TIME2 : time of point 2 (in seconde)
;	X1 : longitude of point 2 (degre)
;	Y2  : latitude of point 2 (degre)
;
; OUTPUT :
;	V12_cc   : velocity from point1 to point2 in cartesian coordonet (Km/s)
; 	D12_cc   : distance between point1 and point2 in cartesian coordonet (Km)
;
; EXEMPLE :
;	out = gcd_v(time1,X1,Y1,time2,X2,Y2,R2)
;	print, 'in catesian coordoned :'
;	print, 'dist(P1,P2) = ', out[0], 'Km'
;	print, 'V12=dist(P1,P2)/dt = ', out[1], 'Km/s'

PRO dist_v , dt12, xx1, yy1, xx2, yy2, accuracy, out , Instrument = Instrument , header = header, ScaleSize = Scalesize

;===Input prameters
    IF Instrument EQ 'EIT' THEN BEGIN & pix_km = 5.24 * 723D & ENDIF; 1 pixel = 5.24*723D Km ;1" = 723 km ; EIT 2.6 arcsec per pixel
    IF Instrument EQ 'AIA' THEN BEGIN & pix_km = 0.6 * 723D & ENDIF; AIA 0.6 arcsec per pixel (shoudl multiply by 4 for 1024)
    IF Instrument EQ 'SWAP' THEN BEGIN & pix_km = header.cdelt1 * 723D & ENDIF

;===Calculate the distance

    XX1 = FLOAT( XX1 )
    XX2 = FLOAT( XX2 )
    YY1 = FLOAT( YY1 )
    YY2 = FLOAT( YY2 )
    D12 = SQRT( ( XX1 - XX2 )^2 + ( YY1 - YY2 )^2 ) * pix_km

;===Calculate the error :
    err = 2.0 * ( pix_km * accuracy )

;===Calculate the velocity
    IF dt12 EQ 0 THEN BEGIN
        print, 'same image => no velocity'
        v12 = 0
        v12_err = 0
    ENDIF ELSE BEGIN
        v12 = D12 / dt12
        v12_err = err / dt12
    ENDELSE

    out = [ D12 , err , V12 , V12_err ]

END
