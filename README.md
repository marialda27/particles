# MAC particle code

We present a new particle code for the emulation of gyrosynchrotron emission. This code is structured in 3 main programs:
* **Read and write**: with this code we transform `hdf5` data to binary plain data.
* **Evolution program**: in this code we seed a non-thermal particles in a region of interest, and the code evolves the trajectories of the particles to obtain the final energy distribution of the particles due the effect of the electric and magnetic fields.
* **GS emission**: with this code we can read the spatial distribution of the non-thermal particles, to calculate de GS emission with `FoMo` code [Van Doorsselaere et al. 2016](https://ui.adsabs.harvard.edu/abs/2016FrASS...3....4V).

For more details, we invite you to read the following paper: [Cécere et al. 2025](https://ui.adsabs.harvard.edu/abs/2023MNRAS.522.2553P/abstract). If you use this code (or part of it) for a scientific work, we kindly ask you to cite this paper.

<!-- --------------------------------------------------------------------- -->
## 1. Required software

A `gfortran` compiler and the following libraries:
* `HDF5`

Optional:
* `Something`

For **macOS** users, we recommend to install the `something` and `hdf5` libraries with **Homebrew** (https://brew.sh/, just `brew install gcc hdf5`).

<!-- --------------------------------------------------------------------- -->
## 2. Installation

Download the codes: read, code, and gsemission

<!-- --------------------------------------------------------------------- -->
## 3. Execution

### 3.1. read

* Go to the `read/` folder.
* Compile the code with:
```
gfortran compound_read.f90 -I/usr/include -L/usr/lib -lhdf5_fortran -lhdf5
```
Run the executable `a.out`. This code read `hdf5` data and write in `/pathcode/particles/data/` the velocity, electric and magnetic fields in binary plain data files.

### 3.2. code

* Go to the `code/` folder.
* Compile the code with:
```
gfortran -fno-strict-overflow mod_parameters.f90  nrtype.f90 nrutil.f90 nr.f90 ran_state.f90 ran1.f90 gasdev.f90 mod_data.f90 mod_boundary.f90 mod_interpolation.f90 mod_fields.f90 mod_initial.f90 mod_cross.f90 mod_derivatives.f90 mod_forces.f90 mod_integrator.f90 main.f90 -o part
```

Run the executable `part`. This code read the $v_x$, $v_y$, $v_z$, $E_x$, $E_y$, $E_z$, $B_x$, $B_y$, $B_z$ from the MHD simulation and these data is used to evolve the trajectory of the non-thermal particles.

### 3.3. gs-emission

* Go to the `gsemission/` folder.
* Compile the code with:
```
something
```

Run the executable `gs`. This code ...
<!-- --------------------------------------------------------------------- -->
## 4. Code structures

### 4.1. code

* `main`: this subroutine writes in `pathcode/particles/plots/time_pos.dat` the data with the information of `time`, $x$, $y$, $z$, $v_{\parallel}$, $\mu$ and `id` of each particle.
* `initial`: this subroutine writes in `pathcode/particles/plots/initial_pos.dat` the information of `time`, $x$, $y$, $z$, $v_{\parallel}$, $\mu$ and `id` of each particle for initial time. First, we seed a random distribution of particles inside of coronal loop with a Maxwellian velocity distribution. Given these positions, we calculate the interpolated magnetic field from the data of the MHD simulation. With this information we calculate $v_{\parallel}$, $\mu$ for each particle.
* `data`: this subroutine calls from `pathcode/particles/data/` the binary plane files of the velocity, electric and magnetic fields obtained from the `hdf5` files of the simulation data.
* `integrator`: for each particle, this subroutine calculates the Lorentz force to evolve the position, $v_{\parallel}$ and $\mu$.
* `boundary`: this subroutine check if the particle evolved fall outside of the region of interest.
* `parameters`: in this subroutine we set the parameters of the problem and the MHD simulation in CGS units.
* `cross`:
* `derivatives`:
* `fields`:
* `forces`:
* `interpolation`:

**References:**

* $n_{\rm part}$ (`integer`): number of particles of the tracer (e.g., a halo) (set to $0$ if not applicable),
* $(x,y,z)$ (`float`): cartesian coordinates of the tracer (in arbitrary units, usually in ${\rm Mpc}$ or $h^{-1}{\rm Mpc}$),
* $(v_x,v_y,v_z)$ (`float`): velocity components of the tracer (in ${\rm km}/{\rm s}$). **NOTE:** the velocities are not used for void identification, but they are needed for dynamical analyses that we are planning to incorporate (e.g., calculation of the velocity profile). You can set them to $0$ if not applicable.

<!-- --------------------------------------------------------------------- -->
## 5. Python utilities

To use these tools, we recommend install a 3.7 python version.

<!-- --------------------------------------------------------------------- -->
## 6. Running an example

The `Example/` folder contains an example of how to identify voids. We provide a DM-halo catalogue: `halos_ascii.dat`, and a `vars.conf` file suitable for this catalogue. You can download the halo catalogue here: https://iate.oac.uncor.edu/~cmcorrea/halos_ascii.dat.

**Characteristics of the halo catalogue:**

* Format: `ASCII`,
* Origin: A flat-$\Lambda$CDM simulation, $1024^3$ particles, $\Omega_m=0.25$, $h=0.7$, $\sigma_{8/h}=0.9$, snapshot $z=0.51$, in real space,
* Box size: $1000~h^{-1}{\rm Mpc}$,
* Muss cut: $2\times10^{12}~h^{-1}{\rm M}_{\odot}$,
* Number of haloes: $6784818$.

**Reader:**

We provide a python module: `voids_read.py`, with two functions to read the void catalogues:
* `read_sph` :: for the spherical void finder,
* `read_pop` :: for the popcorn void finder.

We also provide a script: `abundance.py`, to make a quick plot of the radii distribution to check the void identification.
The same scripts are also written in R: `read_voids.r` and `abundance.r`.
