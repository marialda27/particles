program testbin

implicit none
real, dimension(100) :: data
integer              :: i

open(10,file='example_unfo.data',form='unformatted',status='unknown',action='write') ! fortran unformatted binary
open(11,file='example_ascii.data',status='unknown',action='write')                   ! ascii
open(12,file='example_stream.data',access='stream',status='unknown',action='write')  ! binary plane
open(13,file='example_unfo2.data',form='unformatted',status='unknown',action='write') ! fortran unformatted binary
open(14,file='example_stream2.data',access='stream',status='unknown',action='write')  ! binary plane


do i = 1, 100
    data(i) = real(i)
end do

do i = 1, 100
    write(12) data(i)
    write(13) data(i)
end do

write(10) data
write(11,*) data
write(14) data

close(10)
close(11)
close(12)
close(13)
close(14)



end program testbin
