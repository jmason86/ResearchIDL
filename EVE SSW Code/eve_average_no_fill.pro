;docformat = 'rst'
;+
;Take the average (or averages) of a series of nonzero finite numbers.
;If negative or non-finite numbers are in the series, treat those 
;samples as fill data and ignore them. Return the average of only the 
;non-negative finite values
;
;If the data is multi-dimensional, you may specify a dimension to average
;along. For instance, if you have a 4D array of dimension [5,20,10,2] and
;want to average along the third dimension, the program will compute the total
;of each column along that dimension and divide it by the number of valid
;values in each column, resulting in an array that is [5,20,2] in size.
;
;:Params:
;  data_: in, required 
;    Data to be averaged. Positive and zero values are counted towards the average,
;    while negative, infinite, and NaN values (collectively "bad" values) are not.
;    It is as if the bad values were not even present. 
;  column: in, optional
;    If set, this will do an average along a single dimension of the array, like
;    total(data_,column) would do a total along that dimension. If set, result
;    is an array of one less dimension than the input data. Must be a scalar 
;    between 1 and the number of dimensions in the input array
;
;:Returns:
;  If column is set, an array of one less dimension than the input data. If 
;  column is not set, a scalar. In either case, the result is the average with
;  fill data ignored.
;  
;:Categories:
;  utility
;-
function eve_average_no_fill,data_,column
  data=data_
  f=data ge 0 and finite(data);Find the slots where there is no fill, do it this way
              ;to catch both negative numbers and NaNs. Put in the finite() check to 
              ;catch infinities
  n=total(f,column)
  w=where(f,count,complement=nw,ncomp=n_count)
  if n_count gt 0 then data[nw]=0 ;replace the fill with something that 
                                  ;won't throw off the average
  result=total(data,column)/n
  
  ;If all of the values in a column are fill, n for that 
  ;column will be 0 and result will be NaN, so fix that.
  w=where(~finite(result),count)
  if count gt 0 then result[w]=-1 

  return,result
end 

