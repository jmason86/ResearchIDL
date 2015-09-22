;docformat = 'rst'

;+
;Given a time, heliocentric latitude and longitude of point on Sun,
;calculate the corresponding alpha angle, and beta angle from the center of the solar disk. Both take 
;into account B0 "nodding" of Sun
;
;:Params: 
;  jd: in, required
;    Julian Date of observation, UTC, TAI, TT, doesn't matter. This function 
;    is not accurate enough to differentiate.
;  lat_: in, required
;    Heliocentric latitude in degrees
;  lon_: in, required
;    Heliocentric longitude in degrees, 0deg through sub-earth point
;:Keywords:
;  Alpha: out, required
;    Horizontal angle from center of solar disk, arcmin, positive east "left"
;    on published sam images
;  Beta: out, required
;    Vertical angle from center of solar disk, arcmin, positive north "up" on
;    published sam images
;:Categories:
;  utility
;-
pro sam_ll_to_ab,jd,lat_,lon_,alpha=alpha,beta=beta
  ;Solar almanac
  rs=697500d; km, surface of transition region 2000km above photosphere radius 695500km
  junk=eve_sun_almanac(jd,R=R,B0=B0)
  r_1au=r*1.49597870691d8; km, center of sun to center of Earth

  ;Heliocentric vector
  lat=lat_*!dpi/180d
  lon=lon_*!dpi/180d
  rrs=rs*[cos(lat)*cos(-lon),cos(lat)*sin(-lon),sin(lat)]

  ;Rotate into SNR (around SNR Y axis)
  c=cos(B0);
  s=sin(B0);
  rrs2=[c*rrs[0]-s*rrs[2],rrs[1],s*rrs[0]+c*rrs[2]]
  rrs=rrs2

  t_alp=rrs[1]/rrs[0];
  t_bet=rrs[2]/rrs[0];
  
  alpha=60d*180d*atan(t_alp)/!dpi
  beta=60d*180d*atan(t_bet)/!dpi

end
