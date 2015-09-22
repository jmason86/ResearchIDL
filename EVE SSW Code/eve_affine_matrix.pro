;docformat = 'rst'
;+
;Do an affine transformation on an image, specified by an affine matrix
;
;:Params:
;  img: in, required
;       input image
;  AA: in, required, type="float(3,3)"
;      Affine transformation matrix -- 3x3 element matrix of the form [[a,b,c],[d,e,f],[0,0,1]]. 
;      Last row is never referenced, so it may be omitted
;  interp: in, optional
;         If set, passed along to POLY_2D          
;  dimx: in, optional
;         If set, passed along to POLY_2D          
;  dimy: in, optional
;         If set, passed along to POLY_2D
;:Keywords:          
;  _extra: optional
;   All other named parameters are passed to POLY_2D
;:Returns:
;  Image of the same shape as input image, with transformed image data in it and clipped by it.
;
;:Description:
;Homogeneous coordinates: For our purposes, it just extends a 2-component
;vector into 3 components using 1 as the third coordinate.  
;
;For each point on the input image z=[[x],[y],[1]] in homogeneous coordinates, 
;transform by matrix A to get the point on the output image z'=[[x'],[y'],[1]] 
;as follows::
;
;  z'=[[x'],[y'],[1]] = A ## z = [[a,b,c],[d,e,f],[0,0,1]] ## [[x],[y],[1]]
;
;Written out, we get::
;
;  x'=a*x+b*y+c
;  y'=d*x+e*y+c
;
;The whole reason we use matrices here is becasue several affine transformations
;can be concatenated into one. An example use case is as follows: In Rocket EVE,
;the rocket can roll to any position and put solar north at any angle relative
;to the SAM CCD. We need to take an image taken with north at one angle and 
;rotate it to that taken at some other angle. This is complicated by the fact that
;the SAM image is stretched in one axis. To reproject an image, perform the following
;transformations in order::
;
; Translate so that the center of the SAM image is at the origin: A1=T(-xc,-yc)
; Scale the image horizontally such that it is circular: A2=S(yr/xr,1)
; Rotate the image: A3=R(theta)
; Scale the image horizontally back to elliptical: A4=S(xr/yr,1)
; Translate the image back home: A5=T(xc,yc)
;
;The concatenation of those transformations is represented by a single affine matrix
;calculated by matrix multiplication::
;
; A = A5 ## A4 ## A3 ## A2 ## A1
;
;This function works by calculating from A the two polynomial coefficient matrices P and Q
;needed by the IDL library function POLY_2D, then calling that function. POLY2D requires
;two square matrices of coefficients and calculates the output location z'=[[x'],[y']] as follows:
;
;x'=sum_i=0^N(sum_j=0^N(P[i,j]*x^i*y^j))
;y'=sum_i=0^N(sum_j=0^N(Q[i,j]*x^i*y^j))
;
;The P and Q matrices are generated from A as follows::
;
; P=[[c,b],[a,0]]
; Q=[[f,e],[d,0]]
;
;
;:Examples: 
;  To solve the SAM problem above, do this::
;    img=...;load SAM image
;    xc=... ;center of SAM solar image X coordinate in pixels
;    yc=... ;center of SAM solar image y coordinate in pixels
;    rx=... ;Radius of SAM solar image in horizontal direction in pixels
;    ry=... ;Radius of SAM solar image in vertical   direction in pixels
;    theta=.. ;Angle to rotate SAM image in radians
;    s=sin(theta)
;    k=cos(theta)
;    A1=[[0,0,-xc],[0,0,-yc],[0,0,1]] ;T(-xc,-yc)
;    A2=[[rx/ry,0,0],[0,1,0],[0,0,1]] ;S(rx/ry,1)
;    A3=[[k,s,0],[-s,k,0],[0,0,1]]    ;R(theta)
;    A4=[[ry/rx,0,0],[0,1,0],[0,0,1]] ;S(ry/rx,1)
;    A5=[[0,0,xc],[0,0,yc],[0,0,1]]   ;T(xc,yc)
;    AA=A5##A4##A3##A2##A1             ;Complete affine matrix
;    img_rot=eve_affine_matrix(img,AA)     ;Rotation performed
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
