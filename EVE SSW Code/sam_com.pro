;+
;Given a MEGS-A image, find the center of mass of the SAM portion of it
;
;:Params:
;  img: in, required
;    A MEGS-A image, in its normal orientation, of size [2048,1024]
;:Returns:
;:private-file:
;-
function sam_com,img   
  chop=double(img[730*2:890*2-1,512:*])

  dark=mean(chop[*,388:*])
  chop-=dark
  
  s=size(chop,/dim)
  x_com=total(total(chop,2)*dindgen(s[0]))/total(chop)
  y_com=total(total(chop,1)*dindgen(s[1]))/total(chop)
  
  return,[x_com+730*2,y_com+512]

end
  