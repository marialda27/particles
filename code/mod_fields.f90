module mod_fields

contains

subroutine Bfield(x,y,z,Bx,By,Bz,B_mod)

use mod_parameters
use mod_interpolation
use mod_data

implicit none
real(kind(0.d0)), intent(in)   :: x, y, z
real(kind(0.d0)), intent(out)  :: Bx, By, Bz, B_mod
integer                        :: i, j ,k
real(kind(0.d0))               :: xn, yn, zn, xi, yj, zk, ax, ay, az   
real(kind(0.d0))               :: Bx_ijk   ,By_ijk   ,Bz_ijk
real(kind(0.d0))               :: Bx_ijk1  ,By_ijk1  ,Bz_ijk1
real(kind(0.d0))               :: Bx_ij1k  ,By_ij1k  ,Bz_ij1k
real(kind(0.d0))               :: Bx_ij1k1 ,By_ij1k1 ,Bz_ij1k1
real(kind(0.d0))               :: Bx_i1jk  ,By_i1jk  ,Bz_i1jk
real(kind(0.d0))               :: Bx_i1jk1 ,By_i1jk1 ,Bz_i1jk1
real(kind(0.d0))               :: Bx_i1j1k ,By_i1j1k ,Bz_i1j1k
real(kind(0.d0))               :: Bx_i1j1k1,By_i1j1k1,Bz_i1j1k1

! given x in physical coordinates, calculate position in the grid
xn = (x-xmin) * dble(nx)/Lx   
yn = (y-ymin) * dble(ny)/Ly  
zn = (z-zmin) * dble(nz)/Lz  

! indexes in the grid  i = [1,nx], j = [1,ny], k = [1,nz]
i = int(xn-0.5d0) + 1
j = int(yn-0.5d0) + 1
k = int(zn-0.5d0) + 1
!print*,'i,j,k',i,j,k

! physical positions of the centres of the cells
xi = xmin + dfloat(2*i-1)*dx*0.5d0
yj = ymin + dfloat(2*j-1)*dy*0.5d0
zk = zmin + dfloat(2*k-1)*dz*0.5d0

! weights
ax = (x - xi)/dx
ay = (y - yj)/dy
az = (z - zk)/dz

Bx_ijk    = f_Bx(i,j,k)
By_ijk    = f_By(i,j,k)
Bz_ijk    = f_Bz(i,j,k)
Bx_ijk1   = f_Bx(i,j,k+1)
By_ijk1   = f_By(i,j,k+1)
Bz_ijk1   = f_Bz(i,j,k+1)
Bx_ij1k   = f_Bx(i,j+1,k)
By_ij1k   = f_By(i,j+1,k)
Bz_ij1k   = f_Bz(i,j+1,k)
Bx_ij1k1  = f_Bx(i,j+1,k+1)
By_ij1k1  = f_By(i,j+1,k+1)
Bz_ij1k1  = f_Bz(i,j+1,k+1)
Bx_i1jk   = f_Bx(i+1,j,k) 
By_i1jk   = f_By(i+1,j,k)
Bz_i1jk   = f_Bz(i+1,j,k)
Bx_i1jk1  = f_Bx(i+1,j,k+1)
By_i1jk1  = f_By(i+1,j,k+1) 
Bz_i1jk1  = f_Bz(i+1,j,k+1)
Bx_i1j1k  = f_Bx(i+1,j+1,k)
By_i1j1k  = f_By(i+1,j+1,k)
Bz_i1j1k  = f_Bz(i+1,j+1,k)
Bx_i1j1k1 = f_Bx(i+1,j+1,k+1)
By_i1j1k1 = f_By(i+1,j+1,k+1)
Bz_i1j1k1 = f_Bz(i+1,j+1,k+1)

call interpolation(ax,ay,az,i,j,k,Bx_ijk,Bx_ijk1,Bx_ij1k,Bx_ij1k1,&
                   Bx_i1jk,Bx_i1jk1,Bx_i1j1k,Bx_i1j1k1,Bx)
  
call interpolation(ax,ay,az,i,j,k,By_ijk,By_ijk1,By_ij1k,By_ij1k1,&
                   By_i1jk,By_i1jk1,By_i1j1k,By_i1j1k1,By)
  
call interpolation(ax,ay,az,i,j,k,Bz_ijk,Bz_ijk1,Bz_ij1k,Bz_ij1k1,&
                   Bz_i1jk,Bz_i1jk1,Bz_i1j1k,Bz_i1j1k1,Bz)

B_mod = dsqrt(Bx*Bx + By*By + Bz*Bz)

end subroutine Bfield
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
subroutine Efield(x,y,z,Ex,Ey,Ez)

use mod_parameters
use mod_interpolation
use mod_data

implicit none
real(kind(0.d0)), intent(in)   :: x, y, z
real(kind(0.d0)), intent(out)  :: Ex, Ey, Ez
integer                        :: i, j ,k
real(kind(0.d0))               :: xn, yn, zn, xi, yj, zk, ax, ay, az 
real(kind(0.d0))               :: Ex_ijk   ,Ey_ijk   ,Ez_ijk
real(kind(0.d0))               :: Ex_ijk1  ,Ey_ijk1  ,Ez_ijk1
real(kind(0.d0))               :: Ex_ij1k  ,Ey_ij1k  ,Ez_ij1k
real(kind(0.d0))               :: Ex_ij1k1 ,Ey_ij1k1 ,Ez_ij1k1
real(kind(0.d0))               :: Ex_i1jk  ,Ey_i1jk  ,Ez_i1jk
real(kind(0.d0))               :: Ex_i1jk1 ,Ey_i1jk1 ,Ez_i1jk1
real(kind(0.d0))               :: Ex_i1j1k ,Ey_i1j1k ,Ez_i1j1k
real(kind(0.d0))               :: Ex_i1j1k1,Ey_i1j1k1,Ez_i1j1k1

! given x in physical coordinates, calculate position in the grid
xn = (x-xmin) * dble(nx)/Lx   
yn = (y-ymin) * dble(ny)/Ly  
zn = (z-zmin) * dble(nz)/Lz  

! indexes in the grid  i = [1,nx], j = [1,ny], k = [1,nz]
i = int(xn-0.5d0) + 1
j = int(yn-0.5d0) + 1
k = int(zn-0.5d0) + 1

! physical positions of the centres of the cells
xi = xmin + dfloat(2*i-1)*dx*0.5d0
yj = ymin + dfloat(2*j-1)*dy*0.5d0
zk = zmin + dfloat(2*k-1)*dz*0.5d0

! weights
ax = (x - xi)/dx
ay = (y - yj)/dy
az = (z - zk)/dz

Ex_ijk    = f_Ex(i,j,k)
Ey_ijk    = f_Ey(i,j,k)
Ez_ijk    = f_Ez(i,j,k)
Ex_ijk1   = f_Ex(i,j,k+1)
Ey_ijk1   = f_Ey(i,j,k+1)
Ez_ijk1   = f_Ez(i,j,k+1)
Ex_ij1k   = f_Ex(i,j+1,k)
Ey_ij1k   = f_Ey(i,j+1,k)
Ez_ij1k   = f_Ez(i,j+1,k)
Ex_ij1k1  = f_Ex(i,j+1,k+1)
Ey_ij1k1  = f_Ey(i,j+1,k+1)
Ez_ij1k1  = f_Ez(i,j+1,k+1)
Ex_i1jk   = f_Ex(i+1,j,k) 
Ey_i1jk   = f_Ey(i+1,j,k)
Ez_i1jk   = f_Ez(i+1,j,k)
Ex_i1jk1  = f_Ex(i+1,j,k+1)
Ey_i1jk1  = f_Ey(i+1,j,k+1) 
Ez_i1jk1  = f_Ez(i+1,j,k+1)
Ex_i1j1k  = f_Ex(i+1,j+1,k)
Ey_i1j1k  = f_Ey(i+1,j+1,k)
Ez_i1j1k  = f_Ez(i+1,j+1,k)
Ex_i1j1k1 = f_Ex(i+1,j+1,k+1)
Ey_i1j1k1 = f_Ey(i+1,j+1,k+1)
Ez_i1j1k1 = f_Ez(i+1,j+1,k+1)

call interpolation(ax,ay,az,i,j,k,Ex_ijk,Ex_ijk1,Ex_ij1k,Ex_ij1k1,&
                   Ex_i1jk,Ex_i1jk1,Ex_i1j1k,Ex_i1j1k1,Ex)
  
call interpolation(ax,ay,az,i,j,k,Ey_ijk,Ey_ijk1,Ey_ij1k,Ey_ij1k1,&
                   Ey_i1jk,Ey_i1jk1,Ey_i1j1k,Ey_i1j1k1,Ey)
  
call interpolation(ax,ay,az,i,j,k,Ez_ijk,Ez_ijk1,Ez_ij1k,Ez_ij1k1,&
                   Ez_i1jk,Ez_i1jk1,Ez_i1j1k,Ez_i1j1k1,Ez)

end subroutine Efield

end module mod_fields
