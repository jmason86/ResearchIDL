;This program is a wrapper for eltheory.pro
;eltheory uses Thomson scattering to calculate the mass from intensity
;This program use eltheory to find the scaling factor for a mass
;calculate in the plane-of-sky to some angle out of the sky plane

;Input : the Heliographic longitude of the CME

function scale_mass, Heliographic_longitude

;This values are arbitray since we are taking the ratio from eltheory              
r = 10.0     ;radial distance from the sun 

;Calculate POS angle  
;Assumes that CME is on the Earth-side of the Sun 
pos = 90-abs(heliographic_longitude)

;-Calculate the mass in the plane of the sky 
eltheory,r,0.0,rout,B0
eltheory,r,pos,rout,B_theta
   
   m = B0/b_theta

return,m
end
