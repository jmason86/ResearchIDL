pro sam_pix_to_ab,r0,theta,alpha=alpha,beta=beta
  alpha=dblarr(320,240)
  beta=dblarr(320,240)
  if n_elements(r0) eq 0 then r0=[164.5,120.6,-21200]
  if n_elements(theta) eq 0 then theta=!dtor*15.5d
  
  ;Figure the image matrix
  n=[sin(theta),0,cos(theta)]
  nx=n[0]
  ny=n[1]
  nz=n[2]
  mx=0
  my=1
  mz=-my*ny/nz
  m=[mx,my,mz]
  m/=norm(m)
  p=crossp(m,n)
  A_r2i=[[P],[M],[N]]
  A_i2r=transpose(A_r2i)
  
  for i=0,319 do begin
    for j=0,239 do begin
      r_i=[i,j,0]
      r_r=A_i2r ## r_i
      rt=r_r-r0
      alpha[i,j]=-atan(rt[0]/rt[2])
      beta[i,j]=atan(rt[1]/rt[2])
    end
  end
  ;Convert radians to arcmin
  alpha=alpha*60d*180d/!dpi
  beta=beta*60d*180d/!dpi
end