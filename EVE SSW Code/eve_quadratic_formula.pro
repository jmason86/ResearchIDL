;docformat = 'rst'
;+
;Finds the real roots of the quadratic equation a*x^2+b*x+c=0
;
;This formula is one we all learned in kindergarten, and yet it is surprisingly
;easy to make subtle mistakes which will cause your code to silently give the 
;wrong answer. We use this function to encapsulate the quadratic formula instead
;of hard-coding it throughout the higher-level code.
;
;This uses the suggestion (but not code) from Numerical Recipes section 5.6
;to improve numerical stability. There are actually two forms of the quadratic formula:
;
;x=(-b+-sqrt(b^2-4*a*c))/(2*a) 
;
;and
;
;x=(2*c)/(-b+-sqrt(b^2-4*a*c))
;
;When calculating the discriminant, if either or both of a or c is small, then the discriminant 
;will be near b^2, and using the minus sign in either form will result in subtraction of two
;nearly equal numbers, b-sqrt(d) ~= b-sqrt(b^2). As it happens, using the + sign in the /2a
;form produces the same root as the - sign in the 2c/ form, so we can use the + sign from
;both forms to get each root without ever subtracting the two nearly equal numbers. The
;following code uses this idea, but works when b is negative as well.
;
;:Params:
;   a: in, required
;    Quadratic coefficient. May be an array of any shape. All values must be nonzero,
;       zero values will return roots of NaN, even if there is a solution to the resulting
;       linear equation.
;   b: in, required
;     Linear coefficient. May be array, but must be same shape as a
;   c: in, required
;     Constant coefficient. May be array, but must be same shape as a
;:Keywords:
;   d: out, optional
;     Discriminant. Same shape as a. Wherever there is a negative value in this
;       array, there are no real roots, and root1 and root2 will have NaN in
;       the corresponding elements
;   root1: out, optional
;     Root found by using +/2a form of quadratic formula
;   root2: out, optional
;     Root found by using 2c/+ form of quadratic formula
;:Categories:
;  utility
;-
pro eve_quadratic_formula,a,b,c,d=d,root1=root1,root2=root2
  s=b*0d +1d
  w=where(b lt 0,count)
  if count gt 0 then s[w]=-1d

  d=b^2-4*a*c
  q=(b+s*sqrt(d))/2d
  root1=q/a;
  root2=c/q;
end
