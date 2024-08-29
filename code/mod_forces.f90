! From equation 16 of Gordovskyy et al. 2010.

module mod_forces

contains
 
subroutine force_cg(mu,v_par,vel_deriva,b_unit,E,u_E,gradB,vec1,vec2,f_cg)
use mod_parameters
 
implicit none
real(kind(0.d0)), intent(in)                :: mu, v_par
real(kind(0.d0)), dimension(3), intent(in)  :: vel_deriva,b_unit,E,u_E,gradB,vec1,vec2
real(kind(0.d0)), dimension(4), intent(out) :: f_cg
 
f_cg(1) = vel_deriva(1) + v_par*b_unit(1)
f_cg(2) = vel_deriva(2) + v_par*b_unit(2)
f_cg(3) = vel_deriva(3) + v_par*b_unit(3)
f_cg(4) = (q/m)*dot_product(E,b_unit) - mu*dot_product(b_unit,gradB) + v_par*dot_product(u_E,vec1) + dot_product(u_E,vec2)

end subroutine force_cg

end module mod_forces
