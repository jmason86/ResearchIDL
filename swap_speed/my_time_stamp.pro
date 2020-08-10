; MY_TIME_STAMP
; MY_TIME_STAMP is a modification of a standard SSW routine TIME_STAMP which
; doesn't work on my PC... It XYOUTS a time stamp on each frame of an EIT movie
; (or other 3-dimensional byte array).
; D.M. fecit, 1996 May 10.
; Added narrow keyword for windows too narrow to fit entire time stamp,
; 1996 September 6. D.M. fecit.
; Added no_preserve keyword to accommodate large arrays.
; 1996 December 30. D.M. fecit.
; Woke up and started ORing the movie frames with a small time stamp
; window instead of displaying and reading back the entire movie.
; D.M. fecit, 1997 March 11.
; J.S. Newmark - 2000 January 31 - bug fixes, new features.
; A. N. Zhukov - 2002 June 3 - font problem fixed
;
function my_time_stamp, movie, wave, time_array, x0, y0, narrow = narrow, $
   no_preserve = no_preserve, big_font = big_font
;
if not keyword_set(narrow) then narrow = 0
;
if keyword_set(no_preserve) then preserve = 0 else preserve = 1
if keyword_set(big_font) then begin
   avant_garde = '-adobe-helvetica-medium-r-normal--25-180-100-100-p-130-iso8859-1'
endif else begin
   avant_garde = '-adobe-helvetica-medium-r-normal--14-100-100-100-p-76-iso8859-1'
endelse
;
;wavelength = [171, 195, 284, 304]
;if wave le 40 then begin
;   color = wave
;endif else begin
;   j_wave = -1
;   for i_wave = 0, 3 do begin
;      if wave eq wavelength(i_wave) then j_wave = i_wave
;   end
;   color = 42 + j_wave
;end
;
if preserve then begin
;
   b0 = movie & sz_b0 = size(b0) & nx = sz_b0(1) & ny = sz_b0(2)
   if sz_b0(0) eq 2 then n_frame = 1 else n_frame = sz_b0(3)
;
   if nx lt 128 or (keyword_set(big_font) and nx lt 256)  then narrow = 1
;
   if !d.window ge 0 then original_window = !d.window else original_window = -1
   if not narrow then begin
      window, /free, /pixmap, xsize = 256, ysize = 32
   endif else begin
      window, /free, /pixmap, xsize = 128, ysize = 48
   end
;
;   loadct, color, file = getenv('coloreit')
;
   font = !p.font & !p.font = 0 & device, font=times
   for i_frame = 0, n_frame - 1 do begin
      if not narrow then begin
         xyouts, 0, 0, time_array(i_frame), /device
         nx0 = 256
      endif else begin
         date_line = strmid(time_array(i_frame), 0, 11)
         time_line = strmid(time_array(i_frame), 11, 8)
         xyouts, 0, 24, date_line, /device
         xyouts, 0, 0, time_line, /device
         nx0 = nx - x0 - 1
      end
      time_frame = tvrd()
      b0(x0, y0, i_frame) = $
         b0(x0:x0 + nx0 - 1, y0:y0 + 32 + 16*narrow, i_frame) $
         or time_frame
      erase
   end
;
endif else begin
;
   sz_movie = size(movie) & nx = sz_movie(1) & ny = sz_movie(2)
   if sz_movie(0) eq 2 then n_frame = 1 else n_frame = sz_movie(3)
;
   if nx lt 128 or (keyword_set(big_font) and nx lt 256)  then narrow = 1
;
   if !d.window ge 0 then original_window = !d.window else original_window = -1
   if not narrow then begin
      window, /free, /pixmap, xsize = 256, ysize = 32
   endif else begin
      window, /free, /pixmap, xsize = 128, ysize = 48
   end
;
;   loadct, color, file = getenv('coloreit')
;
   font = !p.font & !p.font = 0 & device, font= avant_garde
   for i_frame = 0, n_frame - 1 do begin
      if not narrow then begin
         xyouts, 0, 0, time_array(i_frame), /device
         nx0 = 256
      endif else begin
         date_line = strmid(time_array(i_frame), 0, 11)
         time_line = strmid(time_array(i_frame), 11, 8)
         xyouts, 0, 24, date_line, /device
         xyouts, 0, 0, time_line, /device
         nx0 = nx - x0 - 1
      end
      time_frame = tvrd()
      if narrow then time_frame = time_frame(0:nx0 - 1 - x0, *)
      movie(x0, y0, i_frame) = $
         movie(x0:x0 + nx0 - 1, y0:y0 + 32 + 16*narrow, i_frame) $
         or time_frame
      erase
   end
;
end
;
!p.font = -1
wdelete & if original_window ge 0 then wset, original_window
;
if preserve then return, b0 else return, movie & end
