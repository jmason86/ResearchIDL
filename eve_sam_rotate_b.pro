;
;	Rotate SAM image to verify roll angle for rocket flight (SAM grating version)
;
;	Chris Jeppesen  June 2012
;	Andrew Jones	June 2012
;
;
;	USAGE:
;		IDL>  eve_sam_rotate
;

function eve_affine_matrix,img,AA,interp,dimx,dimy,_extra=extra
  a=AA[0,0]
  b=AA[1,0]
  c=AA[2,0]
  d=AA[0,1]
  e=AA[1,1]
  f=AA[2,1]
  P=[[c,b],[a,0]]
  Q=[[f,e],[d,0]]
  return,poly_2d(img,P,Q,_extra=extra)
end

pro eve_sam_rotate
  xc=160d ;center of SAM solar image X coordinate in pixels
  yc=120d ;center of SAM solar image y coordinate in pixels
  rx=105d ;Radius of SAM solar image in horizontal direction in pixels
  ry=100d ;Radius of SAM solar image in vertical   direction in pixels
  xfc=580
  yfc=271
  A1=[[1d,0,-xc],[0,1,-yc],[0,0,1]] ;T(-xc,-yc)
  A2=[[ry/rx,0,0],[0,1,0],[0,0,1]] ;S(rx/ry,1)
  A4=[[rx/ry,0,0],[0,1,0],[0,0,1]] ;S(ry/rx,1)
  A5=[[1d,0,xc],[0,1,yc],[0,0,1]]   ;T(xc,yc)


  xfp=[286d,979,!values.d_nan,275,969,!values.d_nan,188,1021,!values.d_nan,209,934,!values.d_nan,305,881,!values.d_nan,580,580]-xfc
  yfp=[241d,312,!values.d_nan,256,290,!values.d_nan,271, 271,!values.d_nan,292,251,!values.d_nan,302,238,!values.d_nan,496, 47]-yfc
  ifn = 'latest_sam.png'
  junk=query_image(ifn,info)
  img_rgb=read_png(ifn)   ;load SAM image
  img_r=double(reform(img_rgb[0,*,*]))
  print,"Left click to record an active region, middel click (Option key +left button) when done"
  window,1,XSIZE=info.dimensions[0],YSIZE=info.dimensions[1]
  erase
  tv,img_r
  cursor,this_x,this_y,/dev,/up
  ar_count = 0L
  while (!mouse.button ne 2) and (ar_count lt 10) do begin
    ; print, 'Button = ', !mouse.button
    ar_count += 1L
    if n_elements(x_active) eq 0 then begin
      x_active=this_x
      y_active=this_y
    end else begin
      x_active=[x_active,this_x]
      y_active=[y_active,this_y]
    end
    if n_elements(x_active) gt 0 then begin
      erase
      tv,img_r
      plots,x_active,y_active,psym=2,color='0000ff'x,/dev
    end
    cursor,this_x,this_y,/dev,/up
  end  

  theta=0 ;Angle to rotate SAM image in radians
  
  z=[[x_active],[y_active],[x_active*0+1]]
  
  print,"Left click to rotate left, right click to rotate right, middle (Option key +left button) to quit"
  cursor,this_x,this_y,/dev,/up
  co=['0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x,'c0c0c0'x,'c0c0c0'x,'c0c0c0'x,'0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x,'0000ff'x]
  while !mouse.button ne 2 do begin
    s=sin(double(theta)*!dtor)
    k=cos(double(theta)*!dtor)
    A3=[[k,-s,0],[s,k,0],[0,0,1]]    ;R(theta)
    A=A5##A4##A3##A2##A1             ;Complete affine matrix
    zp=invert(A) ## z
    erase
      
    img_rot=eve_affine_matrix(img_r,a,missing=0)
    tv,img_rot
    for i=0,n_elements(x_active)-1 do plots,xfp+zp[i,0],yfp+zp[i,1],/dev,color=co
    xyouts,320,10,string(theta),/dev
    cursor,this_x,this_y,/dev,/up
    if !mouse.button eq 1 then theta-=5
    if !mouse.button eq 4 then theta+=5
    print, FORMAT='("Rotation = " ,I4, " degrees")', theta  
  end  
  
  ; save rotated image
  jfile2 = 'latest_sam_rotated_'+strtrim(long(theta),2)+'.jpg'
  print, 'Rotated SAM image saved as ', jfile2
  ;write_jpeg_tv, jfile2
end
