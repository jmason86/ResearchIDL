; Great circle distance and velosity
;
; INPUT : 
;	TIME1 : time of point 1 (seconde)
;	LONG1 : longitude of point 1 (degre)
;	LAT1  : latitude of point 1 (degre)
;	R1    : rayon of the sun in the EIT_image1 (Km)
;	TIME2 : time of point 2 (in seconde)
;	LONG2 : longitude of point 2 (degre)
;	LAT2  : latitude of point 2 (degre)
;	R2    : rayon of the sun in the EIT_image2 (Km)

;
; OUTPUT :
;	V12_sc   : velocity from point1 to point2 in spherical coordonet (Km/s)
; 	D12_sc   : distance between point1 and point2 in spherical coordonet (Km)
;
; EXEMPLE :
;	gcd_v, time1,long1,lat1,R1,time2,long2,lat2,R2, out
; 	print, 'in spherical coordoned :'
;	print, 'dist(P1,P2) = ', out[0]
;	print, 'V12=dist(P1,P2)/dt = ', out[1]


pro gcd_v, dt12, xx1, yy1, xx2, yy2, accuracy, csi_img, OUT, COORD


; prametres :
;--------------

R = 7E5 ; Radius in Km
pix_km = csi_img.cdelt1*723D ; 1 pixel = 5.24*723D Km for 512



; convert pixels coordined to heliospherique coordined : 
;-------------------------------------------------------

helio_c1 = cnvt_coord(XX1, YY1, csi = csi_img, from = 2, to = 4)
helio_c2 = cnvt_coord(XX2, YY2, csi = csi_img, from = 2, to = 4)
coord = [helio_c1[0,0], helio_c1[0,1], helio_c2[0,0], helio_c2[0,1]]
       ;[   lat1°     ,   long1°     ,   lat2°      ,    long2°    ]
long1 = (helio_c1[0,1]/180D)*(!dPI) ; radian
long2 = (helio_c2[0,1]/180D)*(!dPI) ; radian
lat1  = (helio_c1[0,0]/180D)*(!dPI) ; radian
lat2  = (helio_c2[0,0]/180D)*(!dPI) ; radian

; comput distance :
;-------------------

D12 = R*acos( cos(lat1)*cos(lat2)*cos(long1-long2)+sin(lat1)*sin(lat2) )

; comput error :
;----------------

; translating of axis :
X1= float(abs(csi_img.CRPIX1-XX1)) 
X2= float(abs(csi_img.CRPIX1-XX2))
Y1= float(abs(csi_img.CRPIX2-YY1))
Y2= float(abs(csi_img.CRPIX2-YY2))

; distance from the centre :
dc_1 = sqrt( (X1)^2 + (Y1)^2 )
dc_2 = sqrt( (X2)^2 + (Y2)^2 )

err_1 =   R * abs ( acos( ((dc_1+accuracy)*pix_km)/R ) - acos( (dc_1*pix_km)/R) )
err_2 =   R * abs ( acos( ((dc_2+accuracy)*pix_km)/R ) - acos( (dc_2*pix_km)/R) )
err = err_1 + err_2


; comput velocity :
;------------------

if dt12 eq 0 then begin
	print, 'same image => no velocity'
	v12 = 0
	v12_err = 0
endif else begin
	v12 = (D12)/(dt12)
	v12_err = err/(dt12)
endelse


; output
;--------

out = [D12, err, v12, v12_err] 


END