module mod_boundary

contains

subroutine boundary_position(x,y,z,edge)
use mod_parameters
 
implicit none
real(kind(0.d0)), intent(in) :: x, y, z
integer,intent(out)          :: edge

if (x.lt.xmin+dx*0.5d0)then
    edge = 1
end if

if (y.lt.ymin+dy*0.5d0)then
    edge = 1
end if

if (z.lt.zmin+dz*0.5d0)then
    edge = 1
end if

if (x.gt.xmax-dx*0.5d0)then
    edge = 1
end if

if (y.gt.ymax-dy*0.5d0)then
    edge = 1
end if

if (z.gt.zmax-dz*0.5d0)then
     edge = 1
end if
 
end subroutine boundary_position

 
end module mod_boundary
