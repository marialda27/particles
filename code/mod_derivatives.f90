module mod_derivatives
 
 contains
 
 subroutine gradf(fxp,fxm,fyp,fym,fzp,fzm,dxf,dyf,dzf)
 use mod_parameters
 
 implicit none
 real(kind(0.d0)), intent(in)   :: fxp,fxm,fyp,fym,fzp,fzm
 real(kind(0.d0)), intent(out)  :: dxf, dyf, dzf 

 dxf =  (fxp-fxm)/(2.d0*dx)
 dyf =  (fyp-fym)/(2.d0*dy)
 dzf =  (fzp-fzm)/(2.d0*dz)

  end subroutine gradf
 
end module mod_derivatives
