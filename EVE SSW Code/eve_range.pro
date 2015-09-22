;docformat = 'rst'

;+
;Generate Linear array covering a range of values
;:Params:
;  a: in, required
;    minimum value of the range
;  b: in, required
;    maximum value of the range. Required
;  n: in, out, optional
;    number of elements in the range. Required if delta= is not set,
;    optional output if delta= is set (see below)
;:keywords:
;  invdelta: in, optional
;    if set to a number, n is ignored and the reciprocal of this value is used as an 
;    element spacing. n is then set to the appropriate value
;  delta: in, optional
;    if set to a number, n is ignored and this value is used as an 
;    element spacing. n is then set to the appropriate value
;  inclusive: in, optional
;    When not set, the resulting array runs
;    from a to 'almost' b. There are n elements in the array, 
;    but continuing the pattern, the n+1 element would have 
;    the value b
;:returns:
;  a 1D array of doubles, length n, with the appropriate values
;
;examples::
; IDL> print,eve_range(0,1,10)
;    0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9
; IDL> print,eve_range(0,1,delta=0.1)
;    0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9
; IDL> print,eve_range(0,1,invdelta=10)
;    0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9
;
;Note that the values go from 0 to almost 1, but go up in steps of 0.1
;
;::
; IDL> print,eve_range(0,1,10,/inc)
;    0.0    0.1111    0.2222    0.3333    0.4444    0.5556    0.6667    0.7778    0.8889    1.0
;
;The values go from 0 to 1, but go up in steps of 0.111, 1/9, not 1/10
;
;::
; IDL> print,eve_range(0,1,11,/inc)
;    0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9   1.0
; IDL> print,eve_range(0,1,delta=0.1,/inc)
;    0.0    0.1    0.2    0.3    0.4    0.5    0.6    0.7    0.8    0.9   1.0
;
;These go from 0 to 1, in steps of 1/10, but have 11 elements
;:Categories:
;  utility
;-
function eve_range,a,b,n,invdelta=invdelta,delta=delta,inclusive=inclusive
  if keyword_set(invdelta) then begin
    n=(double(b)-double(a))*double(invdelta)
    ;print,n
    n=fix(n+0.5)
    if keyword_set(inclusive) then n++
    return,dindgen(n)/double(invdelta)+double(a)
  end else begin
    if keyword_set(delta) then begin
      n=(double(b)-double(a))/double(delta)
      n=fix(n+0.999d)
      if keyword_set(inclusive) then n++
    end else begin
      delta=(double(b)-double(a))/double(n-keyword_set(inclusive))
    end
    return,dindgen(n)*double(delta)+double(a)
  end
end

