module mod_data

real, dimension(:,:,:), allocatable :: f_Bx
real, dimension(:,:,:), allocatable :: f_Ex
real, dimension(:,:,:), allocatable :: f_vx
real, dimension(:,:,:), allocatable :: f_By
real, dimension(:,:,:), allocatable :: f_Ey
real, dimension(:,:,:), allocatable :: f_vy
real, dimension(:,:,:), allocatable :: f_Bz
real, dimension(:,:,:), allocatable :: f_Ez
real, dimension(:,:,:), allocatable :: f_vz

contains

subroutine readdata(time)
use mod_parameters



implicit none
integer               :: i,j,k !,irow,nrows
integer, intent(in)   :: time
character(255)        :: timechar
!character(255)        :: path = '/run/media/cecere/7d4a4576-37a1-4a7a-bcab-27905bc96ce8/data/'
character(255)        :: path = '/home/cecere/programas/particles/data/'
real                  :: percentage

print*, 'Reading data', time

! Convertir el entero a una cadena
write(timechar, '(I0)') time

!nrows = nx*ny*nz

open(20,file=trim(path) // trim('field_mag_') // trim(timechar) // trim('.dat'),access='stream',status='old',action='read')
open(21,file=trim(path) // trim('field_ele_') // trim(timechar) // trim('.dat'),access='stream',status='old',action='read')
open(22,file=trim(path) // trim('field_vel_') // trim(timechar) // trim('.dat'),access='stream',status='old',action='read')

!do irow=1,nrows

!  if (mod(irow, nint(real(nrows) * 0.1)) == 0) then
!      write(*, '(A, I0, A,1x)', advance='no') 'Progreso: ', 100 * irow/nrows, '%'
!      if (irow == nrows) then
!        write(*, *) ! Nueva línea al finalizar
!      end if
!  end if
!  read(20) i,j,k,f_Bx(i,j,k),f_By(i,j,k),f_Bz(i,j,k)
!  read(21) i,j,k,f_Ex(i,j,k),f_Ey(i,j,k),f_Ez(i,j,k)
!  read(22) i,j,k,f_vx(i,j,k),f_vy(i,j,k),f_vz(i,j,k)
  
  
  !print*, 'i,j,k,Bx,By,Bz',i,j,k,f_Bx(i,j,k),f_By(i,j,k),f_Bz(i,j,k)
!end do

! Set the progress interval

do k = 1, nz
     percentage = real(k) / real(nz) * 100.0
     if (mod(k, nint(nz*0.1)) == 0) then
      write(*, '(A, I3, A, I3, A, F5.1, A)') 'Progress: ', k, ' of ', nz, ' (', percentage, '%)'
     end if
     do j = 1, ny
          do i = 1, nx 
                read(20) f_Bx(i,j,k),f_By(i,j,k),f_Bz(i,j,k)
                read(21) f_Ex(i,j,k),f_Ey(i,j,k),f_Ez(i,j,k)
                read(22) f_vx(i,j,k),f_vy(i,j,k),f_vz(i,j,k)
                !print*, 'i,j,k,Bx,By,Bz',i,j,k,f_Bx(i,j,k),f_By(i,j,k),f_Bz(i,j,k)
                !print*, 'i,j,k,Ex,Ey,Ez',i,j,k,f_Ex(i,j,k),f_Ey(i,j,k),f_Ez(i,j,k)
                !print*, 'i,j,k,vx,vy,vz',i,j,k,f_vx(i,j,k),f_vy(i,j,k),f_vz(i,j,k)
                !f_Ex(i,j,k) = f_Ex(i,j,k)/1000000.
                !f_Ey(i,j,k) = f_Ey(i,j,k)/1000000.
                !f_Ez(i,j,k) = f_Ez(i,j,k)/1000000.
          end do
     end do
end do
!print*,'termine'

close(20)
close(21)
close(22)
end subroutine readdata

end module mod_data



  
