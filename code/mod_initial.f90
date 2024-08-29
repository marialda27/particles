module mod_initial

contains

subroutine initial_distribution(x,v_par,mu)

use mod_parameters
use mod_fields
use nrtype, only : sp 
use nr, only : ran1, gasdev 
use mod_data

implicit none
real(kind(0.d0)), dimension(n_part,3), intent(out)   :: x
real(kind(0.d0)), dimension(n_part), intent(out)     :: v_par,mu
real(sp), save                                       :: idum = -123213, idum1 = -238571
real(kind(0.d0))                                     :: v_mod, v_mod2, B_mod, v_perp2, r1, alpha, v_th
real(kind(0.d0)), dimension(3)                       :: v, B
integer                                              :: i_part

! write initial data
!open(11,file = '/home/cecere/Documentos/particle_code/plots/initial_pos.dat',status = 'unknown')
open(11,file = '/home/cecere/programas/particles/plots/initial_pos.dat',status = 'unknown')


call readdata(1)

do i_part = 1, n_part
    !print*, 'i_part',i_part
    ! random loop angles
    call ran1(idum)
    alpha = 2.d0*pi*idum
    ! random radius
    call ran1(idum)
    r1 = sqrt(idum)
    
    ! initial distribution of particles inside of loop in physical coordinates
    x(i_part,1) = r_loop*r1*dcos(alpha) ! x, y inside a circle of 0.7*r_loop        		           
    x(i_part,2) = r_loop*r1*dsin(alpha) 

    ! putting the particles in a disk of height dh_loop, centred in z0_loop
    call ran1(idum)
    x(i_part,3) = idum*dh_loop - 0.5d0*dh_loop + z0_loop
    !print*, 'z',x(i_part,3)
    
    ! thermal velocity for the particles
    v_th = dsqrt(2.d0*kb*Temp/m)
    !print*, 'thermal vel km/s',v_th/1000.
    
    ! maxwellian velocity distribution 
    call gasdev(idum1)
    v(1) = idum1*v_th  	
    call gasdev(idum1)
    v(2) = idum1*v_th      	
    call gasdev(idum1)
    v(3) = idum1*v_th   
    
    ! calculate de v_mod 
    v_mod2 = v(1)*v(1) + v(2)*v(2) + v(3)*v(3) 
    v_mod = dsqrt(v_mod2)
    
    ! interpolate B to the physical coordinates
    call Bfield(x(i_part,1),x(i_part,2),x(i_part,3),B(1),B(2),B(3),B_mod)
    !print*, 'Bx,By,Bz,B_mod',B(1),B(2),B(3),B_mod

    ! output
    v_par(i_part) = dot_product(v,B)/B_mod

    v_perp2 = v_mod2 - v_par(i_part)*v_par(i_part)
    !output
    mu(i_part) = 0.5d0*v_perp2/B_mod 
    
    write(11,*) 0.0, x(i_part,1), x(i_part,2), x(i_part,3), v_par(i_part), mu(i_part) , i_part

end do

close(11)

end subroutine initial_distribution

end module mod_initial
