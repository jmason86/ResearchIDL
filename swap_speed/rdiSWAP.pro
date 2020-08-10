; This routine calculates running difference images
; LAST MODIFIED: 20041103 (AZ)
;

pro rdiSWAP, im, time_array, im_diff, time_array_diff, short = short, step = step

if not keyword_set(step) then step = 1

sizes = size(im)
xsz = sizes[1]
ysz = sizes[2]
frames = n_elements(time_array)
im_diff = fltarr(xsz, ysz, frames - 1)
time_array_diff = strarr(frames - 1)


  if step eq 1 then begin

      for i = 0, frames - 2 do begin
 
        im_diff[*, *, i] = im[*, *, i+1] - im[*, *, i]
        if not keyword_set(short) then time_array_diff[i] = time_array[i+1]+' - '+strmid(time_array[i], 11, 8) else time_array_diff[i] = strmid(time_array[i+1], 11, 8) +' - '+strmid(time_array[i], 11, 8) 


 
      endfor

  endif else begin

      for i  = 0, frames - 1 - step do begin
      
        im_diff[*, *, i] = im[*, *, i+step] - im[*, *, i]
        if not keyword_set(short) then time_array_diff[i] = time_array[i+step]+' - '+strmid(time_array[i], 11, 8) else time_array_diff[i] = strmid(time_array[i+step], 11, 8) +' - '+strmid(time_array[i], 11, 8) 
      
      endfor

  endelse





return
end
