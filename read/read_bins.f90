program read

implicit none

real, dimension(100) :: data, data2
integer              :: i

open(10,file='example_unfo.data',form='unformatted',status='old',action='read') ! fortran unformatted binary
open(11,file='example_ascii.data',status='old',action='read')                   ! ascii
open(12,file='example_stream.data',access='stream',status='old',action='read')  ! binary plane
open(13,file='example_unfo2.data',form='unformatted',status='old',action='read') ! fortran unformatted binary
open(14,file='example_stream2.data',access='stream',status='old',action='read')  ! binary plane

read(10) data

do i = 1, 4
    print*, data(i), data(96+i)
end do

do i = 1, 100
    read(12) data2(i)
end do

do i = 1, 4
    print*, data2(i), data2(96+i)
end do

end program read
