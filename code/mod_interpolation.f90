module mod_interpolation

contains

subroutine interpolation(ax,ay,az,i,j,k,var_ijk,var_ijk1,var_ij1k,var_ij1k1,var_i1jk,var_i1jk1,var_i1j1k,var_i1j1k1,var)
 
implicit none
real(kind(0.d0)), intent(in)  :: ax, ay, az
integer, intent(in)           :: i, j, k
real(kind(0.d0)), intent(in)  :: var_ijk,var_ijk1,var_ij1k,var_ij1k1,var_i1jk,var_i1jk1,var_i1j1k,var_i1j1k1
real(kind(0.d0)), intent(out) :: var
 
var = (1.d0-ax)*(1.d0-ay)*(1.d0-az)*var_ijk + (1.d0-ax)*(1.d0-ay)*az*var_ijk1 + &
      (1.d0-ax)*ay*(1.d0-az)*var_ij1k + (1.d0-ax)*ay*az*var_ij1k1 + ax*(1.d0-ay)*(1.d0-az)*var_i1jk &
      + ax*(1.d0-ay)*az*var_i1jk1 + ax*ay*(1.d0-az)*var_i1j1k + ax*ay*az*var_i1j1k1

end subroutine interpolation

end module mod_interpolation
