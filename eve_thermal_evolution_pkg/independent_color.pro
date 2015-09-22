;+
; NAME:
;	independent_color
;
; PURPOSE:
;	to be able to display the same color no matter if the device
;	is set for 8 bit or 24 bit color
;
; CATEGORY:
;	what level(s) of SEE processing is this procedure used for
;	(valid levels are L0A, L0B, L1, L2, L3)
;
; CALLING SEQUENCE:  
;	x = independent_color()
;	x = independent_color(/true)
;
; INPUTS:
;	true is keyword when machine uses true color
;
; OUTPUTS:  
;	values for these colors:
;	red,orange,yellow,green,blue,purple,white,black
;
; COMMON BLOCKS:
;	none
;
; PROCEDURE:
;	1. determine which type of color display is being used
;       2. set color values for that display
;
; MODIFICATION HISTORY:
;	Karen Turk 6/18/01
;   Don Woodraska 3/13/03 Modified to use !d.table_size instead of
;   device for compatibility with z-buffer and pixmap.
;       Karen Turk 6/13/03 Added keyword true for machines that use
;       true color
;
; CVS Modification Log
;
;   $Log: independent_color.pro,v $
;   Revision 8.0  2005/06/15 18:51:21  see_sw
;   commit of version 8.0
;
;   Revision 8.0  2004/07/20 20:18:26  turkk
;   commit of version 8.0
;
;   Revision 7.0  2004/07/08 23:02:54  turkk
;   commit of version 7.0
;
;   Revision 6.2  2003/06/13 22:11:47  turkk
;   added true keyword for machines that only use true color
;
;   Revision 6.1  2003/03/18 18:56:02  dlwoodra
;   compatibility with z-buffer device
;
;   Revision 6.0  2003/03/05 19:32:48  dlwoodra
;   version 6 commit
;
;   Revision 5.20  2002/09/06 23:21:34  see_sw
;   commit of version 5.0
;
;   Revision 4.0  2002/05/29 18:10:01  see_sw
;   Release of version 4.0
;
;   Revision 3.0  2002/02/01 18:55:27  see_sw
;   version_3.0_commit
;
;   Revision 1.2  2001/12/21 21:24:00  turkk
;   added 3 more colors to the structure
;
;
;idver='$Id: independent_color.pro,v 8.0 2005/06/15 18:51:21 see_sw Exp $'
;
;-

function independent_color,true=true

loadct,39,/silent
;   1. determine which type of color display is being used
if !d.name ne 'PS' then begin
    if !d.table_size gt 256 then this_depth=24 else this_depth=0
;    device,get_visual_depth= this_depth
    
    if (this_depth gt 8 or size(true,/type) ne 0) then begin
        red = '0000ff'xl
        orange ='00bbff'xl
        yellow = '00ffff'xl
        green='00ff00'xl
        blue ='ff0000'xl
        purple='ffaaaa'xl
        aqua ='ffff00'xl
        rust = '001177'xl
        light_blue='ffaa00'xl
        white ='ffffff'xl
        black='000000'xl
    endif else begin
        red = long(144.0/158.0*!d.table_size)
        yellow = long(118.0/158.0*!d.table_size) 
        orange = long(127.0/158.0*!d.table_size)
        green= long(90.0/158.0*!d.table_size)
        blue = long(40.0/158.0*!d.table_size)
        purple = long(20.0/158.0*!d.table_size)
        aqua = long(65.0/158.0*!d.table_size)
        rust = long(132.0/158.0*!d.table_size)
        light_blue = long(55.0/158.0*!d.table_size) 
        white = long(158.0/158.0*!d.table_size)
        black = long(0.0/158.0*!d.table_size)
    endelse
endif

if !d.name eq 'PS' then begin 
 
    
    red = long(144.0/158.0*!d.table_size)
    orange = long(127.0/158.0*!d.table_size)
    yellow = long(118.0/158.0*!d.table_size)
    green= long(90.0/158.0*!d.table_size)
    blue = long(40.0/158.0*!d.table_size)
    purple = long(20.0/158.0*!d.table_size)
    aqua = long(65.0/158.0*!d.table_size)
    rust = long(132.0/158.0*!d.table_size)
    light_blue = long(55.0/158.0*!d.table_size)
    white = long(158.0/158.0*!d.table_size)
    black = long(0.0/158.0*!d.table_size)
endif


;2. set color values for that display
shade = {red:red ,orange:orange ,yellow:yellow ,green:green ,blue:blue ,purple:purple, aqua:aqua, rust:rust, light_blue:light_blue, white:white ,black:black}


return, shade

end
