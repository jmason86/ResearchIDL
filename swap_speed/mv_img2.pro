PRO mv_img2, img, scl=scl, ind_scl = ind_scl, rate = rate


if not keyword_set(rate) then rate = 50 
sz = size(img)
if not keyword_set(ind_scl) then ind_scl = 0
if (sz(sz(0)+1) ne 1) then if not (ind_scl) then scl = 1 
if not keyword_set(scl) then scl = 0
if not keyword_set(ind_scl) then ind_scl = 0
;LOADCT, 8, BOTTOM=LONG(.3*!d.n_colors)
if (scl) then img1 = bscl(img>.5)
size_b0 = SIZE(img)
XINTERANIMATE, SET=size_b0(1:3), /SHOW ,/track 
if (ind_scl) then $ 
FOR ii=0,size_b0(3)-1 DO XINTERANIMATE, FRAME=ii, IMAGE=bscl(img(*,*,ii)>.5) $
   else if (scl) then $
FOR ii=0,size_b0(3)-1 DO XINTERANIMATE, FRAME=ii, IMAGE=img1(*,*,ii)  else $
FOR ii=0,size_b0(3)-1 DO XINTERANIMATE, FRAME=ii, IMAGE=img(*,*,ii)  

XINTERANIMATE, rate ,/keep, block = 1
END   
