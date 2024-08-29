module mod_parameters

! parameters of the run
integer, parameter           :: n_part = 1.d1        ! particles number
integer, parameter           :: n_time = 2           ! number of iteration in time
real(kind(0.d0)), parameter  :: dt = 0.01d0         ! time step dt

! parameters of the initial conditions
real(kind(0.d0)), parameter  :: r_loop = 0.25d9      ! loop radius
real(kind(0.d0)), parameter  :: dh_loop = 1.d5       ! espesor en z donde se ponen las particulas
real(kind(0.d0)), parameter  :: z0_loop = 0.         ! initial z coordinate where particles are
real(kind(0.d0)), parameter  :: Temp = 3.d7         ! temperature of the background

! electrons properties 
   
real(kind(0.d0)), parameter  :: q = -1.602d-19       ! electron charge
real(kind(0.d0)), parameter  :: m = 9.109d-31        ! electron mass

! universal constants 
real(kind(0.d0)), parameter  :: pi = dacos(-1.d0)
real(kind(0.d0)), parameter  :: c_light = 299792458.d0     ! velocidad de la luz en m/s
real(kind(0.d0)), parameter  :: kb = 1.381d-23             ! Boltzmann constant

! grid properties
integer, parameter           :: nx = 490, ny = 490, nz = 490    
real(kind(0.d0)), parameter  :: xmin = -1.75e9 , xmax = 1.75d9               
real(kind(0.d0)), parameter  :: ymin = -1.75e9 , ymax = 1.75d9                
real(kind(0.d0)), parameter  :: zmin = -1.75e9 , zmax = 1.75d9                 
real(kind(0.d0)), parameter  :: Lx = abs(xmax - xmin)
real(kind(0.d0)), parameter  :: Ly = abs(ymax - ymin) 
real(kind(0.d0)), parameter  :: Lz = abs(zmax - zmin)
real(kind(0.d0))             :: dx = Lx/dble(nx-1)
real(kind(0.d0))             :: dy = Ly/dble(ny-1)
real(kind(0.d0))             :: dz = Lz/dble(nz-1)

end module mod_parameters
