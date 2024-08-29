!=====================================================
!This program reads a dataset that is one dimensional array of
! structures  {
!                 character*80
!                 integer
!                                   }
! Update  : 04/03/2024
! configuration : gfortran compound_read.f90 -I/usr/include -L/usr/lib -lhdf5_fortran -lhdf5
! Author: M. Cécere
!=====================================================

PROGRAM COMPOUND

USE HDF5 ! This module contains all necessary modules

IMPLICIT NONE

CHARACTER(LEN=110), PARAMETER              :: filename = "welbalanced20t_hdf5_plt_cnt_0004" ! File name
CHARACTER(LEN=80), PARAMETER               :: dsetname = "integer scalars"     ! Dataset name
INTEGER, PARAMETER                         :: dimsize = 15        ! Size of the dataset
!character(len=600) :: path = '/home/cecere/Documentos/particle_code/read/'
!character(len=600) :: path = '/home/cecere/programas/particles/read/'
character(len=600) :: path = '/home/cecere/programas/particles/data/'
character(len=600) :: path2 = '/home/cecere/programas/itt/idl711linux.x86/idl71/examples/data/base20MKt/'

INTEGER(HID_T)                             :: file_id             ! File identifier
INTEGER(HID_T)                             :: dset_id             ! Dataset identifier
INTEGER(HID_T)                             :: dspace_id           ! Dataspace identifier
INTEGER(HID_T)                             :: dtype_id            ! Compound datatype identifier
INTEGER(HID_T)                             :: dt1_id              ! Memory datatype identifier (for character field)
INTEGER(HID_T)                             :: dt2_id              ! Memory datatype identifier (for integer field)
INTEGER(HID_T)                             :: dt5_id, dt6_id      ! Memory datatype identifier
INTEGER(HID_T)                             :: plist_id            ! Dataset transfer property
INTEGER(SIZE_T)                            :: typesize


INTEGER(HSIZE_T), DIMENSION(1)             :: dims = (/dimsize/)  ! Dataset dimensions
INTEGER                                    :: rank = 1          ! Dataset rank

INTEGER                                    :: error             ! Error flag
INTEGER(SIZE_T)                            :: type_size         ! Size of the datatype
INTEGER(SIZE_T)                            :: type_sizec        ! Size of the character datatype
INTEGER(SIZE_T)                            :: type_sizei        ! Size of the integer datatype
INTEGER(SIZE_T)                            :: offset            ! Member's offset
CHARACTER(LEN=80), DIMENSION(dimsize)      :: char_member
CHARACTER(LEN=80), DIMENSION(dimsize)      :: char_member_out     ! Buffer to read data out
INTEGER, DIMENSION(dimsize)                :: int_member
INTEGER                                    :: i,j,k
INTEGER(HSIZE_T), DIMENSION(1)             :: data_dims

!integer(8)                                 :: dataset_id_magx
integer(8)                                 :: dataset_id_magx, dataset_id_magy, dataset_id_magz
integer(8)                                 :: dataset_id_velx, dataset_id_vely, dataset_id_velz
integer(8)                                 :: dims_data(4), maxdims_data(4)
real, allocatable                          :: magx(:,:,:,:), dmagx(:,:)
real, allocatable                          :: magy(:,:,:,:), dmagy(:,:)
real, allocatable                          :: magz(:,:,:,:), dmagz(:,:)
real, allocatable                          :: velx(:,:,:,:), dvelx(:,:)
real, allocatable                          :: vely(:,:,:,:), dvely(:,:)
real, allocatable                          :: velz(:,:,:,:), dvelz(:,:)
real, allocatable                          :: delex(:,:)
real, allocatable                          :: deley(:,:)
real, allocatable                          :: delez(:,:)
INTEGER                                    :: nxb, nyb, nzb, iprocs, jprocs, kprocs, ndims
integer                                    :: iby, ibx, iy, ix
real                                       :: c_light = 299792458.d0    

data_dims(1) = dimsize
!print*,'dimsize',dimsize
!
! Initialise FORTRAN interface.
!
!print*,'antes error ********************************************************'
CALL h5open_f(error)
!print*,'despues error ******************************************************'
!
! Open the file.
!
!print*,'antes open *********************************************************'
CALL h5fopen_f (trim(path2)//trim(filename), H5F_ACC_RDONLY_F, file_id, error)
!CALL h5fopen_f ("welbalanced20t_hdf5_plt_cnt_0004", H5F_ACC_RDWR_F, file_id, error)

!print*,'despues open *******************************************************'
!
! Open the dataset.
!
CALL h5dopen_f(file_id, dsetname, dset_id, error)
!
! Create memory datatype to read character member of the compound datatype.
!
CALL h5tcopy_f(H5T_NATIVE_CHARACTER, dt2_id, error)
typesize = 80
CALL h5tset_size_f(dt2_id, typesize, error)
CALL h5tget_size_f(dt2_id, type_size, error)
CALL h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
offset = 0
CALL h5tinsert_f(dt1_id, "name", offset, dt2_id, error)
!
! Read part of the dataset and display it.
!
CALL h5dread_f(dset_id, dt1_id, char_member_out, data_dims, error)
!print*,'*******************************************************************'
!write(*,'(a,1x)') (trim(char_member_out(i)), i=1, dimsize)


!
CALL h5tcopy_f(H5T_NATIVE_INTEGER, dt2_id, error)
typesize = 4
CALL h5tset_size_f(dt2_id, typesize, error)
CALL h5tget_size_f(dt2_id, type_size, error)
CALL h5tcreate_f(H5T_COMPOUND_F, type_size, dt1_id, error)
offset = 0
CALL h5tinsert_f(dt1_id, "value", offset, dt2_id, error)
!
! Read part of the dataset and display it.
!
CALL h5dread_f(dset_id, dt1_id, int_member, data_dims, error)
!print*,'*******************************************************************'
!write(*,*) (int_member(i), i=1, dimsize)

!
nxb =    int_member(1)
nyb =    int_member(2)
nzb =    int_member(3)
iprocs = int_member(8)
jprocs = int_member(9)
kprocs = int_member(10)
!print*,'*******************************************************************'
!print*, 'nxb,nyb,nzb,iprocs,jprocs,kprocs',  nxb,nyb,nzb,iprocs,jprocs,kprocs
!
!
! Open the data
call h5dopen_f(file_id, 'magx', dataset_id_magx, error)
call h5dopen_f(file_id, 'magy', dataset_id_magy, error)
call h5dopen_f(file_id, 'magz', dataset_id_magz, error)
call h5dopen_f(file_id, 'velx', dataset_id_velx, error)
call h5dopen_f(file_id, 'vely', dataset_id_vely, error)
call h5dopen_f(file_id, 'velz', dataset_id_velz, error)

! Obtener la información del espacio del conjunto de datos
call h5dget_space_f(dataset_id_magx, dspace_id, error)

! Obtener el número de dimensiones del conjunto de datos
call h5sget_simple_extent_ndims_f(dspace_id, ndims, error)
!print*,'*******************************************************************'
!print*, 'ndims', ndims

! Obtener las dimensiones del conjunto de datos
call h5sget_simple_extent_dims_f(dspace_id, dims_data, maxdims_data, error)
!print*,'*******************************************************************'
!print*, 'dims_data', dims_data

! Asignar memoria para almacenar los datos
allocate(magx(dims_data(1),dims_data(2),dims_data(3),dims_data(4)))
allocate(magy(dims_data(1),dims_data(2),dims_data(3),dims_data(4)))
allocate(magz(dims_data(1),dims_data(2),dims_data(3),dims_data(4)))
allocate(velx(dims_data(1),dims_data(2),dims_data(3),dims_data(4)))
allocate(vely(dims_data(1),dims_data(2),dims_data(3),dims_data(4)))
allocate(velz(dims_data(1),dims_data(2),dims_data(3),dims_data(4)))
allocate(dmagx(nxb*iprocs,nyb*jprocs))
allocate(dmagy(nxb*iprocs,nyb*jprocs))
allocate(dmagz(nxb*iprocs,nyb*jprocs))
allocate(dvelx(nxb*iprocs,nyb*jprocs))
allocate(dvely(nxb*iprocs,nyb*jprocs))
allocate(dvelz(nxb*iprocs,nyb*jprocs))
allocate(delex(nxb*iprocs,nyb*jprocs))
allocate(deley(nxb*iprocs,nyb*jprocs))
allocate(delez(nxb*iprocs,nyb*jprocs))

! Leer los datos desde el conjunto de datos
call h5dread_f(dataset_id_magx, H5T_NATIVE_REAL, magx, dims_data, error)
call h5dread_f(dataset_id_magy, H5T_NATIVE_REAL, magy, dims_data, error)
call h5dread_f(dataset_id_magz, H5T_NATIVE_REAL, magz, dims_data, error)
call h5dread_f(dataset_id_velx, H5T_NATIVE_REAL, velx, dims_data, error)
call h5dread_f(dataset_id_vely, H5T_NATIVE_REAL, vely, dims_data, error)
call h5dread_f(dataset_id_velz, H5T_NATIVE_REAL, velz, dims_data, error)
!print*,'*******************************************************************'
!print*, 'magx',magx(1,1,1,1)

! Build the data 
do iby = 0, jprocs-1
     do ibx = 0, iprocs-1
          do iy = 0, nyb-1
               do ix = 0, nxb-1
                    dmagx(ibx*nxb+ix+1,iby*nyb+iy+1)=magx(ix+1,iy+1,1,ibx+iby*iprocs+1)
                    dmagy(ibx*nxb+ix+1,iby*nyb+iy+1)=magy(ix+1,iy+1,1,ibx+iby*iprocs+1)
                    dmagz(ibx*nxb+ix+1,iby*nyb+iy+1)=magz(ix+1,iy+1,1,ibx+iby*iprocs+1)
                    dvelx(ibx*nxb+ix+1,iby*nyb+iy+1)=velx(ix+1,iy+1,1,ibx+iby*iprocs+1)
                    dvely(ibx*nxb+ix+1,iby*nyb+iy+1)=vely(ix+1,iy+1,1,ibx+iby*iprocs+1)
                    dvelz(ibx*nxb+ix+1,iby*nyb+iy+1)=velz(ix+1,iy+1,1,ibx+iby*iprocs+1)
               enddo
          enddo
     enddo
enddo
!print*,'*******************************************************************'
!print*,'dmagx',dmagx(1,1)

!open(10,file = 'test.dat',status = 'unknown')
!do j = 1, nyb*jprocs
!     do i = 1, nxb*iprocs
!          write(10,*) dmagx(i,j)
!     end do
!end do
!close(10)

! convert from cgs to mks
dmagx = dmagx*1e-4
dmagy = dmagy*1e-4
dmagz = dmagz*1e-4

dvelx = dvelx*1e-2
dvely = dvely*1e-2
dvelz = dvelz*1e-2

delex = dvely*dmagz - dvelz*dmagy
deley = dvelx*dmagy - dvely*dmagx
delez = dvelz*dmagx - dvelx*dmagz

!delex =  delex/100.
!deley =  deley/100.
!delez =  delez/100.

open(10,file = trim(path) // trim('field_mag_1.dat'),access='stream',status='unknown',action='write')    ! binary plane
open(11,file = trim(path) // trim('field_vel_1.dat'),access='stream',status='unknown',action='write')    ! binary plane
open(12,file = trim(path) // trim('field_ele_1.dat'),access='stream',status='unknown',action='write')    ! binary plane
do k = 1, nyb*jprocs
     do j = 1, nyb*jprocs
          do i = 1,nxb*iprocs
               write(10) sqrt(dmagx(i,j)*dmagx(i,j)+dmagz(i,j)*dmagz(i,j)), &
               & sqrt(dmagx(i,j)*dmagx(i,j)+dmagz(i,j)*dmagz(i,j)), dmagy(i,j)
               write(11) sqrt(dvelx(i,j)*dvelx(i,j)+dvelz(i,j)*dvelz(i,j)), &
               & sqrt(dvelx(i,j)*dvelx(i,j)+dvelz(i,j)*dvelz(i,j)), dvely(i,j)
               write(12) sqrt(delex(i,j)*delex(i,j)+delez(i,j)*delez(i,j)), &
               & sqrt(delex(i,j)*delex(i,j)+delez(i,j)*delez(i,j)), deley(i,j)
          end do
     end do
end do
close(10)
close(11)
close(12)
!
! Close all open objects.
!
call h5dclose_f(dataset_id_magx, error)
call h5dclose_f(dataset_id_magy, error)
call h5dclose_f(dataset_id_magz, error)
call h5dclose_f(dataset_id_velx, error)
call h5dclose_f(dataset_id_vely, error)
call h5dclose_f(dataset_id_velz, error)
CALL h5dclose_f(dset_id, error)
CALL h5tclose_f(dt1_id, error)
CALL h5tclose_f(dt2_id, error)
CALL h5fclose_f(file_id, error)
!
! Close FORTRAN interface.
!
CALL h5close_f(error)

! Deallocate de memory
deallocate(magx)
deallocate(magy)
deallocate(magz)
deallocate(dmagx)
deallocate(dmagy)
deallocate(dmagz)
deallocate(velx)
deallocate(vely)
deallocate(velz)
deallocate(dvelx)
deallocate(dvely)
deallocate(dvelz)
deallocate(delex)
deallocate(deley)
deallocate(delez)

END PROGRAM COMPOUND


