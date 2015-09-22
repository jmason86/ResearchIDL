; Program to scale Meudon 2001 "transparency image" to "intensity image" via gamma.
; Program also can convert a ~2007 intensity levels table into a ~2001 transparancy levels table. 
; gamma was determined by looking at median value per mu ring in 2007 and 2001 images
; then finding a gamma that made them look the same: I2007 = alpha*exp(I2001/gamma)
; 
; James Paul Mason
; 4/20/11 

PRO gammascale, TABLE=table

IF ~keyword_set( TABLE ) then TABLE = 0

fits1 = readfits('/Users/jmason86/Dropbox/Research/Data/Meudon/mK010601.073100.fits',head)

alpha = 0.43 ;Value for scaling -- see above 

;basic disk info from header
radius_pix = 413.13
cenx = 443.5 & ceny = 457.0

fits2=fits1 ;copy the image to use for editing
gammaray = fltarr(n_elements(fits1[*,0]))

med = median(fits1) & fits1=fits1/med

;Loop through image
FOR i=0,n_elements(fits1[*,0])-1 DO BEGIN ;x direction
  FOR j=0,n_elements(fits1[0,*])-1 DO BEGIN ;y direction
    
    ;calculate mu
    dx = (i-cenx)/radius_pix & dy = (j-ceny)/radius_pix
    dist_pix = dx^2 + dy^2
    IF dist_pix LE 1.0 THEN BEGIN
      IF j EQ 400 THEN gammaray(i) = gamma1 ;trace through image
      
      mu = sqrt(1.0-dist_pix)
      gamma1 = -1.42E-3*mu^2 + 9.84E-3*mu + 0.895
      fits2(i,j) = alpha * exp(fits1(i,j)/gamma1) *med
    ENDIF ;dist_pix LE 1.0
    
  ENDFOR ;j loop
ENDFOR ;i loop


;View images
;p1 = window(dimensions=[891,914])
;i1 = image(fits1,/overplot)
;p2 = window(dimensions=[891,914])
;i2 = image(fits2,/overplot)

;writefits,'/Users/jmason86/Dropbox/Research/Data/Meudon/mK010601.073100_GammaScaled.fits',fits2,head

;Convert intensity table to transparency table
IF table NE 0 THEN BEGIN
  
  ;read in table
  readcol,'/Users/jmason86/Dropbox/Research/Data/LevelsDetermination/MDONintlevels.tab',$
  SKIPLINE=5, model,mu0,mu1,mu2,mu3,mu4,mu5,mu6,mu7,mu8,mu9,$
  format='a,f,f,f,f,f,f,f,f,f,f',/silent
  
  ;calculate gamma at the 10 mu's in the table
  gamma0 = -1.42E-3*mu0(0)^2 + 9.84E-3*mu0(0) + 0.895
  gamma1 = -1.42E-3*mu1(0)^2 + 9.84E-3*mu1(0) + 0.895
  gamma2 = -1.42E-3*mu2(0)^2 + 9.84E-3*mu2(0) + 0.895
  gamma3 = -1.42E-3*mu3(0)^2 + 9.84E-3*mu3(0) + 0.895
  gamma4 = -1.42E-3*mu4(0)^2 + 9.84E-3*mu4(0) + 0.895
  gamma5 = -1.42E-3*mu5(0)^2 + 9.84E-3*mu5(0) + 0.895
  gamma6 = -1.42E-3*mu6(0)^2 + 9.84E-3*mu6(0) + 0.895
  gamma7 = -1.42E-3*mu7(0)^2 + 9.84E-3*mu7(0) + 0.895
  gamma8 = -1.42E-3*mu8(0)^2 + 9.84E-3*mu8(0) + 0.895
  gamma9 = -1.42E-3*mu9(0)^2 + 9.84E-3*mu9(0) + 0.895
  
  ;calculate the transparancy values for 10 mus, 6 models
  mu0_transp = gamma0 * alog(mu0[1:6]/alpha) 
  mu1_transp = gamma1 * alog(mu1[1:6]/alpha) 
  mu2_transp = gamma2 * alog(mu2[1:6]/alpha) 
  mu3_transp = gamma3 * alog(mu3[1:6]/alpha) 
  mu4_transp = gamma4 * alog(mu4[1:6]/alpha) 
  mu5_transp = gamma5 * alog(mu5[1:6]/alpha) 
  mu6_transp = gamma6 * alog(mu6[1:6]/alpha) 
  mu7_transp = gamma7 * alog(mu7[1:6]/alpha) 
  mu8_transp = gamma8 * alog(mu8[1:6]/alpha) 
  mu9_transp = gamma9 * alog(mu9[1:6]/alpha) 
  
  ;write output to file
  openw,1,'/Users/jmason86/Dropbox/Research/Data/LevelsDetermination/MDONdenslevels.tab',width=200
  printf,1,'Density Table:Meudon_K3'
  printf,1,'Filter:?'
  printf,1,'Filter offset:0.0'
  printf,1,'Filter Units:?, Intensity Units:?'
  printf,1,'Models:6 Mus:10'
  printf,1,'Mu:  1.0000  0.9000  0.8000  0.7000  0.6000  0.5000  0.4000  0.3000  0.2000  0.1000'
  printf,1,model(1),mu0_transp(0),mu1_transp(0),mu2_transp(0),mu3_transp(0),mu4_transp(0),$
                    mu5_transp(0),mu6_transp(0),mu7_transp(0),mu8_transp(0),mu9_transp(0)
  printf,1,model(2),mu0_transp(1),mu1_transp(1),mu2_transp(1),mu3_transp(1),mu4_transp(1),$
                    mu5_transp(1),mu6_transp(1),mu7_transp(1),mu8_transp(1),mu9_transp(1)
  printf,1,model(3),mu0_transp(2),mu1_transp(2),mu2_transp(2),mu3_transp(2),mu4_transp(2),$
                    mu5_transp(2),mu6_transp(2),mu7_transp(2),mu8_transp(2),mu9_transp(2)
  printf,1,model(4),mu0_transp(3),mu1_transp(3),mu2_transp(3),mu3_transp(3),mu4_transp(3),$
                    mu5_transp(3),mu6_transp(3),mu7_transp(3),mu8_transp(3),mu9_transp(3)
  printf,1,model(5),mu0_transp(4),mu1_transp(4),mu2_transp(4),mu3_transp(4),mu4_transp(4),$
                    mu5_transp(4),mu6_transp(4),mu7_transp(4),mu8_transp(4),mu9_transp(4)
  printf,1,model(6),mu0_transp(5),mu1_transp(5),mu2_transp(5),mu3_transp(5),mu4_transp(5),$
                    mu5_transp(5),mu6_transp(5),mu7_transp(5),mu8_transp(5),mu9_transp(5)
                                                                                                                                         
  close,1
STOP

ENDIF ;table NE 0





END