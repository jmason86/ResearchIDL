pro eve_sam_rotate
  erase
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
  img_rgb=read_png('latest_sam.png')   ;load SAM image
  img_r=double(reform(img_rgb[0,*,*]))
  print,"Left click to record an active region, middle click when done"
  tv,img_r
  cursor,this_x,this_y,/dev,/up
  while !mouse.button ne 2 do begin
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
  
  print,"Left click to rotate left, right click to rotate right, middle to quit"
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
  end  
    
end