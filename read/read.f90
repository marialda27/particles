program read_hdf5
  use hdf5
  implicit none

  integer(8) :: file_id
  integer    :: ierr

  ! Abrir el archivo
  call h5open_f(ierr)
  call h5fopen_f("welbalanced20t_hdf5_plt_cnt_0004", H5F_ACC_RDONLY_F, file_id, ierr)

  if (ierr /= 0) then
    print *, "Error al abrir el archivo HDF5"
    stop
  endif

  ! Cerrar el archivo
  call h5fclose_f(file_id, ierr)
  call h5close_f(ierr)

end program read_hdf5
