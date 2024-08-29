module mod_cross

 contains

 function cross_product(a,b)
 implicit none
 real(kind(0.d0)),intent(in)	:: a(3),b(3)
 real(kind(0.d0))		        :: cross_product(3)

  cross_product(1) = a(2)*b(3)-a(3)*b(2) 
  cross_product(2) = a(3)*b(1)-a(1)*b(3)
  cross_product(3) = a(1)*b(2)-a(2)*b(1)

 return
 end function cross_product

end module mod_cross
