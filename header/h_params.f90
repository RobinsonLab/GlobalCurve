module h_params

	type default_struct
! these param values are determined at runtime.
! parameters read from file specified by DEFAULT_FILENAME below
		
		logical :: simul
		real :: r_min,r_max ! limits on quadrature
		integer :: pts_per_dist
		logical :: log_plot
		real :: eps
		real :: flambda
		integer :: maxit
		real :: chi_sqr_tol ! discontinue l-s search when chi_sqr_change < chi_sqr_tol
		real(8) :: svd_tol
!		real :: flambda_start ! if glo_chi_sqr < flambda start then can begin dividing flambda by flambda_div
		real :: flambda_div ! see above
		logical :: verbose
		real :: rig_scale
		real :: conf_lim ! for rigorous error analysis
		logical :: mgs
		integer :: grid_size 
		integer :: tot_grid_size
		logical :: color !hard copy in color = .true. else it's b&w
		logical :: ps ! if postscript desired then set = .true. else GIF is produced
		integer :: minimizaton_type ! used by function minimize: 1 = marquadt
		integer :: num_boot_iter ! may need a kind=? here  I don't know the range of 
								! type integer

	end type default_struct
! all global params are in the variable default

! define global params

	type (default_struct) :: default
	real,parameter :: PI=3.1415927,E=2.7182818284590452
	real,parameter :: hw_conv=2.35482/2.0 ! = sqrt(8 Ln(2)) ! /2.0 gives hwhm
	integer, parameter :: ADDR_LEN = 4	! function address is currently
									! specificed with integer(4)
	character(12),parameter :: DEFAULT_FILENAME = 'default.in'
	integer, parameter :: DEFAULT_F_N = 62
	integer, parameter :: STRING_LEN = 72
	integer, parameter :: ANSWER_FILE = 6	
	integer, parameter :: DATA_FILE = 7
	integer, parameter :: EXPT_MAX = 100
	integer, parameter :: PARAM_MAX = 512
	integer, parameter :: CHAN_MAX = 10000
	real, parameter :: EPS = 0.00001 !jmr: changed from 0.001 8/21/00
	real, parameter :: MIN_DELCHANGE = 0.00000001
	integer, parameter :: ISEED = 123457
	integer,parameter :: PEAK_COUNTS = 10000
	integer,parameter :: PLOT_SYMBOL = 1 !symbol num for PGPLOT plotting
	character(3),parameter :: DEVICE_TYPE = '/w9'
	integer,parameter :: MAX_RIG_DIM = 3
	real,parameter :: MAX_FLAMBDA = 1.0E+12
	real,parameter :: MIN_FLAMBDA = 1.0E-20
	integer,parameter :: MAX_OUT_OF_BOUNDS=10
	integer,parameter :: NUM_BINS=35
	real,parameter :: X_TOL=0.001 ! tolerance for root finding used in bisect
end module h_params