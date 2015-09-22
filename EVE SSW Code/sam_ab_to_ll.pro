;docformat = 'rst'

;+
;Given a time, alpha angle, and beta angle from the center of the solar disk, 
;calculate the corresponding heliocentric latitude and longitude. Both take 
;into account B0 "nodding" of Sun
;
;:Params: 
;  jd: in, required
;    Julian Date of observation, UTC, TAI, TT, doesn't matter. This function 
;    is not accurate enough to differentiate.
;  Alpha: in, required
;    Horizontal angle from center of solar disk, arcmin, positive east "left"
;    on published sam images
;  Beta: in, required
;    Vertical angle from center of solar disk, arcmin, positive north "up" on
;    published sam images
;:Keywords:
;  lat: out, required
;    Heliocentric latitude in degrees
;  lon: out, required
;    Heliocentric longitude in degrees, 0deg through sub-earth point
;  r_1au: out, optional
;    Distance from Earth to Sun in km.
;  B0: out, optional
;    Heliocentric latitude of sub-earth point. "Nodding" angle of Sun as viewed
;    from Earth, positive when north pole of Sun is tilted towards Earth.
;:Categories:
;  utility
;-
pro sam_ab_to_ll,jd,alpha,beta,lat=lat,lon=lon,r_1au=r_1au,b0=b0
  if n_elements(alpha) gt 1 then begin
    lat=dblarr(size(alpha,/dim))
    lon=lat
    for i=0,n_elements(lon)-1 do begin
      ab_to_ll,jd,alpha[i],beta[i],lat=this_lat,lon=this_lon,r_1au=r_1au,b0=b0
      lat[i]=this_lat
      lon[i]=this_lon
    end
    return
  end

  junk=eve_sun_almanac(jd,R=R,B0=B0)
  rs=697500d; km, surface of transition region 2000km above photosphere radius 695500km
  r_1au=r*1.49597870691d8; km, center of sun to center of Earth

  t_alp=tan(alpha/60d*!dpi/180d)
  t_bet=tan(beta/60d*!dpi/180d)

  ;Solve ray-sphere equation 
  r0=[-r_1au,0,0]
  v=[1,t_alp,t_bet]

  A=total(v*v)         ;always positive, magnitude about 1
  B=2*total(r0*v)      ;Always negative
  C=total(r0*r0)-rs^2  ;always positive in this problem

  ;We want the smallest root greater than zero, the opposite of The Price Is Right
  eve_quadratic_formula,a,b,c,root1=t1,root2=t2
  ;If t1<0, then take t2. If it's <0 also, so be it. If t2<t1 and t2>0, then take t2.
  w=where(t1 lt 0 or (t2 lt t1 and t2 gt 0),count,comp=nw,ncomp=ncount)
  t=t1
  if count gt 0 then t[w]=t2[w]

  ;Finish ray equation to get intersect point
  rrs=r0+v*t
;  print,"rrs ",rrs

  ;Rotate into heliocentric equatorial (around SNR Y axis)
  c=cos(-B0*!dtor);
  s=sin(-B0*!dtor);
  rrs2=[c*rrs[0]-s*rrs[2],rrs[1],s*rrs[0]+c*rrs[2]]
  rrs=rrs2

  ;lat and lon in degrees
  lat=180d/!dpi*asin(rrs[2]/Rs)
  lon=180d/!dpi*atan(-rrs[1],-rrs[0])
end
