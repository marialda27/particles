!=====================================================
! This program solves the evolution of test particles 
! Update  : 27/02/2024
! configuration : gfortran -fno-strict-overflow mod_parameters.f90  nrtype.f90 nrutil.f90 nr.f90 ran_state.f90 ran1.f90 gasdev.f90 mod_data.f90 mod_boundary.f90 mod_interpolation.f90 mod_fields.f90 mod_initial.f90 mod_cross.f90 mod_derivatives.f90 mod_forces.f90 mod_integrator.f90 main.f90 -o part
! Author: M. Cécere
!=====================================================

program particles

use mod_initial
use mod_integrator
use mod_parameters
use mod_boundary
use mod_data

implicit none
real(kind(0.d0)), dimension(n_part,3)    :: x
real(kind(0.d0)), dimension(n_part)      :: mu, v_par
integer                                  :: time, i_part, edge, iter

allocate(f_Bx(nx,ny,nz))
allocate(f_Ex(nx,ny,nz))
allocate(f_vx(nx,ny,nz))
allocate(f_By(nx,ny,nz))
allocate(f_Ey(nx,ny,nz))
allocate(f_vy(nx,ny,nz))
allocate(f_Bz(nx,ny,nz))
allocate(f_Ez(nx,ny,nz))
allocate(f_vz(nx,ny,nz))

! initialise the particle distribution
print*, 'Initialising distribution of non-thermal particles'
call initial_distribution(x,v_par,mu)


! write data
!open(12,file = '/home/cecere/Documentos/particle_code/plots/time_pos.dat',status = 'unknown')
open(12,file = '/home/cecere/programas/particles/plots/time_pos.dat',status = 'unknown')
write(12,*) 'time ', 'x ', 'y ', 'z ', 'vpar ', 'mu ', 'id'

edge = 0
do time = 1, n_time
    call readdata(time)
    ! returns the integrated variables 
    do iter = 1, 50
        call integrator(x,v_par,mu)
    end do
    do i_part = 1, n_part
        !print*,'i_part', i_part
        call boundary_position(x(i_part,1),x(i_part,2),x(i_part,3),edge)
        if (edge == 0) then
            print*,'writing'
            write(12,*) time*dt, x(i_part,1), x(i_part,2), x(i_part,3), v_par(i_part), mu(i_part), i_part
        end if
    end do
end do

close(12)

deallocate(f_Bx)
deallocate(f_Ex)
deallocate(f_vx)
deallocate(f_By)
deallocate(f_Ey)
deallocate(f_vy)
deallocate(f_Bz)
deallocate(f_Ez)
deallocate(f_vz)


end program
