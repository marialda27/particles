module mod_integrator

contains

subroutine integrator(x,v_par,mu)
use mod_parameters
use mod_fields
use mod_cross
use mod_derivatives
use mod_forces

implicit none
real(kind(0.d0)), dimension(n_part,3), intent(inout)   :: x
real(kind(0.d0)), dimension(n_part), intent(inout)     :: mu, v_par
real(kind(0.d0))                                       :: Bmod, Bmod_xp, Bmod_xm, Bmod_yp, Bmod_ym, Bmod_zp, Bmod_zm
real(kind(0.d0)), dimension(3)                         :: B, E, b_unit, u_E 
real(kind(0.d0)), dimension(3)                         :: B_xp, B_xm, B_yp, B_ym, B_zp, B_zm
real(kind(0.d0)), dimension(3)                         :: E_xp, E_xm, E_yp, E_ym, E_zp, E_zm
real(kind(0.d0)), dimension(3)                         :: gradB, gradbx, gradby, gradbz, graduEx, graduEy, graduEz
real(kind(0.d0)), dimension(3)                         :: b_unit_xp, b_unit_xm, b_unit_yp, b_unit_ym, b_unit_zp, b_unit_zm
real(kind(0.d0)), dimension(3)                         :: u_E_xp, u_E_xm, u_E_yp, u_E_ym, u_E_zp, u_E_zm
real(kind(0.d0)), dimension(3)                         :: vec1, vec2, vec3, vec4, vel_deriva
real(kind(0.d0)), dimension(4)                         :: k1, k2, k3, k4
real(kind(0.d0))                                       :: v_par1, v_par2, v_par3 

integer                                                :: i_part


print*, 'Integrating...'

do  i_part = 1, n_part
    call Bfield(x(i_part,1),x(i_part,2),x(i_part,3),B(1),B(2),B(3),Bmod)
    ! unit vector B
    b_unit = B/Bmod                       
    
    call Efield(x(i_part,1),x(i_part,2),x(i_part,3),E(1),E(2),E(3))
    ! vel_deriva ExB/B2
    u_E = cross_product(E,b_unit)/Bmod
    !print*, 'u_E',u_E
    
    call Bfield(x(i_part,1)+dx,x(i_part,2),x(i_part,3),B_xp(1),B_xp(2),B_xp(3),Bmod_xp)
    call Bfield(x(i_part,1)-dx,x(i_part,2),x(i_part,3),B_xm(1),B_xm(2),B_xm(3),Bmod_xm)
    call Bfield(x(i_part,1),x(i_part,2)+dy,x(i_part,3),B_yp(1),B_yp(2),B_yp(3),Bmod_yp)
    call Bfield(x(i_part,1),x(i_part,2)-dy,x(i_part,3),B_ym(1),B_ym(2),B_ym(3),Bmod_ym)
    call Bfield(x(i_part,1),x(i_part,2),x(i_part,3)+dz,B_zp(1),B_zp(2),B_zp(3),Bmod_zp)
    call Bfield(x(i_part,1),x(i_part,2),x(i_part,3)-dz,B_zm(1),B_zm(2),B_zm(3),Bmod_zm)
    
 
    call gradf(Bmod_xp,Bmod_xm,Bmod_yp,Bmod_ym,Bmod_zp,Bmod_zm,gradB(1),gradB(2),gradB(3))

    b_unit_xp = (/B_xp(1), B_xp(2), B_xp(3)/)/Bmod_xp
    b_unit_xm = (/B_xm(1), B_xm(2), B_xm(3)/)/Bmod_xm
    b_unit_yp = (/B_yp(1), B_yp(2), B_yp(3)/)/Bmod_yp
    b_unit_ym = (/B_ym(1), B_ym(2), B_ym(3)/)/Bmod_ym
    b_unit_zp = (/B_zp(1), B_zp(2), B_zp(3)/)/Bmod_zp
    b_unit_zm = (/B_zm(1), B_zm(2), B_zm(3)/)/Bmod_zm
    
    ! grad b_unit
    call gradf(b_unit_xp(1),b_unit_xm(1),b_unit_yp(1),b_unit_ym(1),b_unit_zp(1),b_unit_zm(1),gradbx(1),gradbx(2),gradbx(3))
    call gradf(b_unit_xp(2),b_unit_xm(2),b_unit_yp(2),b_unit_ym(2),b_unit_zp(2),b_unit_zm(2),gradby(1),gradby(2),gradby(3))
    call gradf(b_unit_xp(3),b_unit_xm(3),b_unit_yp(3),b_unit_ym(3),b_unit_zp(3),b_unit_zm(3),gradbz(1),gradbz(2),gradbz(3))
    
    call Efield(x(i_part,1)+dx,x(i_part,2),x(i_part,3),E_xp(1),E_xp(2),E_xp(3))
    call Efield(x(i_part,1)-dx,x(i_part,2),x(i_part,3),E_xm(1),E_xm(2),E_xm(3))
    call Efield(x(i_part,1),x(i_part,2)+dy,x(i_part,3),E_yp(1),E_yp(2),E_yp(3))
    call Efield(x(i_part,1),x(i_part,2)-dy,x(i_part,3),E_ym(1),E_ym(2),E_ym(3))
    call Efield(x(i_part,1),x(i_part,2),x(i_part,3)+dz,E_zp(1),E_zp(2),E_zp(3))
    call Efield(x(i_part,1),x(i_part,2),x(i_part,3)-dz,E_zm(1),E_zm(2),E_zm(3))

    u_E_xp = cross_product(E_xp,b_unit_xp)/Bmod_xp 
    u_E_xm = cross_product(E_xm,b_unit_xm)/Bmod_xm
    u_E_yp = cross_product(E_yp,b_unit_yp)/Bmod_yp 
    u_E_ym = cross_product(E_ym,b_unit_ym)/Bmod_ym
    u_E_zp = cross_product(E_zp,b_unit_zp)/Bmod_zp 
    u_E_zm = cross_product(E_zm,b_unit_zm)/Bmod_zm
    
    ! grad uE
    call gradf(u_E_xp(1),u_E_xm(1),u_E_yp(1),u_E_ym(1),u_E_zp(1),u_E_zm(1),graduEx(1),graduEx(2),graduEx(3))
    call gradf(u_E_xp(2),u_E_xm(2),u_E_yp(2),u_E_ym(2),u_E_zp(2),u_E_zm(2),graduEy(1),graduEy(2),graduEy(3))
    call gradf(u_E_xp(3),u_E_xm(3),u_E_yp(3),u_E_ym(3),u_E_zp(3),u_E_zm(3),graduEz(1),graduEz(2),graduEz(3))
    

    
    ! vec1 => (b_unit*nabla)b_unit
    vec1(1) = dot_product(b_unit,gradbx)
    vec1(2) = dot_product(b_unit,gradby)
    vec1(3) = dot_product(b_unit,gradbz)
    
    ! vec2 => (u_E*nabla)b_unit
    vec2(1) = dot_product(u_E,gradbx)
    vec2(2) = dot_product(u_E,gradby)
    vec2(3) = dot_product(u_E,gradbz)
    
    ! vec3 => (b_unit*nabla)u_E
    vec3(1) = dot_product(b_unit,graduEx)
    vec3(2) = dot_product(b_unit,graduEy)
    vec3(3) = dot_product(b_unit,graduEz)
    
    ! vec4 => (u_E*nabla)u_E
    vec4(1) = dot_product(u_E,graduEx)
    vec4(2) = dot_product(u_E,graduEy)
    vec4(3) = dot_product(u_E,graduEz)
    
    ! From equation 15 of Gordovskyy et al. 2010.
    
    vel_deriva = u_E + m*v_par(i_part)*v_par(i_part)/(q*Bmod)*cross_product(b_unit,vec1) + &
    m*mu(i_part)/(q*Bmod)*cross_product(b_unit,gradB) + &
    m*v_par(i_part)/(q*Bmod)*cross_product(b_unit,vec3) + &
    m*v_par(i_part)/(q*Bmod)*cross_product(b_unit,vec2) + &
    m/(q*Bmod)*cross_product(b_unit,vec4)
    
    call force_cg(mu(i_part),v_par(i_part),vel_deriva,b_unit,E,u_E,gradB,vec1,vec2,k1)
    v_par1 = v_par(i_part) + 0.5d0*dt*k1(4)

    call force_cg(mu(i_part),v_par1,vel_deriva,b_unit,E,u_E,gradB,vec1,vec2,k2)
    v_par2 = v_par(i_part) + 0.5d0*dt*k2(4)

    call force_cg(mu(i_part),v_par2,vel_deriva,b_unit,E,u_E,gradB,vec1,vec2,k3)
    v_par3 = v_par(i_part) + dt*k3(4)

    call force_cg(mu(i_part),v_par3,vel_deriva,b_unit,E,u_E,gradB,vec1,vec2,k4)

    x(i_part,1) = x(i_part,1)   + dt/6.d0*(k1(1)+2.d0*k2(1)+2.d0*k3(1)+k4(1))
    x(i_part,2) = x(i_part,2)   + dt/6.d0*(k1(2)+2.d0*k2(2)+2.d0*k3(2)+k4(2))
    x(i_part,3) = x(i_part,3)   + dt/6.d0*(k1(3)+2.d0*k2(3)+2.d0*k3(3)+k4(3))
    v_par(i_part) = v_par(i_part) + dt/6.d0*(k1(4)+2.d0*k2(4)+2.d0*k3(4)+k4(4))
    
    if(abs(v_par(i_part)) .ge. c_light) then
        print*, 'v_par', v_par(i_part) 
        stop
    endif
    
    
 
end do



end subroutine integrator


end module mod_integrator
