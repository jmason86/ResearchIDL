;+
; NAME:
;   CreateSectorMaps
;
; PURPOSE:
;   Make sector maps for AnalyzeAIADimmingRegions
;
; INPUTS:
;   all171Headers [array]: First header in array will be used as reference
;   lat0 [int]: latitude of origin point for sectors
;   lon0 [int]: longitude of origin point for sectors e.g. N19W36 would be lat0 = 19, lon0 = 36
;
; OPTIONAL INPUTS:
;   saveloc [string]: path for save directory
;
; KEYWORD PARAMETERS:
; 
;
; OUTPUTS:
;   Save file with sectorMap, sectorRadius, sectorAngle, limbAngle, limbMap, maskLimb, maskOverlimb
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Requires solarsoft
;
; EXAMPLE:
; 
;
; MODIFICATION HISTORY:
;   Written by:
;     Rachel Hock
;     2013/1/20
;   2013/7/9, James Paul Mason: turned into a callable program for access from AnalyzeAIADimmingRegions
;-
PRO CreateSectorMaps, SAVELOC = saveloc, all171Headers, lat0 = lat0, lon0 = lon0

IF ~keyword_set(saveloc) THEN saveloc = './'
IF ~keyword_set(lat0) THEN lat0 = 0
IF ~keyword_set(lon0) THEN lon0 = 0

;; Define reference header
hdr = all171Headers[0]

;; Make variables for sectoring
xx = ((findgen(4096)#replicate(1, 4096))-hdr.crpix1)*hdr.cdelt1
yy = ((replicate(1, 4096)#findgen(4096))-hdr.crpix2)*hdr.cdelt2

latlon = arcmin2hel(reform(xx/60., 4096*4096.), reform(yy/60., 4096*4096.), date=hdr.t_obs)
lat = latlon[0,*]
lon = latlon[1,*]
lat = reform(lat, 4096, 4096)
lon = reform(lon, 4096, 4096)

pb0r = pb0r(hdr.t_obs)
x = cos(lat*!dtor)*sin(lon*!dtor)
y = sin(lat*!dtor)*cos(pb0r[1]*!dtor)-cos(lat*!dtor)*cos(lon*!dtor)*sin(pb0r[1]*!dtor)
z = sin(lat*!dtor)*sin(pb0r[1]*!dtor)+cos(lat*!dtor)*cos(lon*!dtor)*cos(pb0r[1]*!dtor)

x0 = cos(lat0*!dtor)*sin(lon0*!dtor)
y0 = sin(lat0*!dtor)*cos(pb0r[1]*!dtor)-cos(lat0*!dtor)*cos(lon0*!dtor)*sin(pb0r[1]*!dtor)
z0 = sin(lat0*!dtor)*sin(pb0r[1]*!dtor)+cos(lat0*!dtor)*cos(lon0*!dtor)*cos(pb0r[1]*!dtor)

xp = cos(90*!dtor)*sin(0*!dtor)
yp = sin(90*!dtor)*cos(pb0r[1]*!dtor)-cos(90*!dtor)*cos(0*!dtor)*sin(pb0r[1]*!dtor)
zp = sin(90*!dtor)*sin(pb0r[1]*!dtor)+cos(90*!dtor)*cos(0*!dtor)*cos(pb0r[1]*!dtor)

a = acos(x*x0 +y*y0 +z*z0 )
b = acos(xp*x0+yp*y0+zp*z0)
c = acos(xp*x +yp*y +zp*z ) 
radius = a
angle = acos( (cos(c)-cos(a)*cos(b))/(sin(a)*sin(b)) )*!radeg
ii = where(lon gt lon0)
angle[ii] = 360-angle[ii]

maskLimb = xx*0
maskLimb[where( (xx^2+yy^2) le (0.975*pb0r[2]*60.)^2 )] = 1

;; Make sector mask
dr = 0.2
da = 30.

sectorMap = xx*0
sectorMap[where(radius le dr/2. AND maskLimb eq 1)] = 1

sectorRadius = 0
sectorAngle = 0

ii = where(angle gt 360.-da/2., nn)
if nn gt 0 then angle[ii] = 360.-angle[ii]

i = 2
for iradius=dr/2., 2.5, dr do begin
   for iangle=0, 360, da do begin
      ii = where(radius gt iradius AND radius le iradius+dr AND angle gt iangle-da/2. AND angle le iangle+da/2. AND maskLimb eq 1, nn)
      if nn gt 0 then begin
         sectorMap[ii] = i
         sectorRadius = [sectorRadius, iradius+dr/2.]
         sectorAngle =[sectorAngle, iangle]
         i = i+1
      endif
   endfor
endfor

;; Over-limb mask
maskOverlimb = xx*0
maskOverlimb[where( (xx^2+yy^2) ge (1.025*pb0r[2]*60.)^2 )] = 1

limbAngle = atan(x, -y)*!radeg
limbMap = limbAngle*0
i=1
for iangle=-180, 180, da do begin
   ii = where(limbAngle gt iangle AND limbAngle le iangle+da AND maskOverlimb eq 1, nn)
   if nn gt 0 then begin
      limbMap[ii] = i
      i = i+1
   endif
endfor

;; Adjust at theta=0

; Resample maps to 1024 x 1024
sectorMap = congrid(sectorMap, 1024, 1024)
limbMap = congrid(limbMap, 1024, 1024)
maskLimb = congrid(maskLimb, 1024, 1024)
maskOverlimb = congrid(maskOverlimb, 1024, 1024)

;; Save
save, sectorMap, sectorRadius, sectorAngle, limbAngle, limbMap, maskLimb, maskOverlimb, lat0, lon0, FILENAME = saveloc + 'SectorMaps.sav'

END
