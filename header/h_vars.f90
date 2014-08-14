module h_vars
	use h_params
	use h_struct
	implicit none

! most are set in readans or main
	integer :: error_analysis_type ! 0 = none; 1 = grid_search; 2 = bootstrap
	character(STRING_LEN) :: ans_file_name
	integer :: num_total_global_param
	integer :: num_free_global_param
	integer :: num_fixed_global_param
	integer :: tot_num_data_points
	integer :: num_expt
	integer :: global_DOF
	integer :: num_iter	
	real :: global_reduced_chi_sqr
	real :: save_global_reduced_chi_sqr
				
! constructor=constructor_logic
! destructor=destructor_logic
!/////////////

! 3 main logic arrays
! map_from goes from the least squares vector to the global param vector
! map_to goes from the global param vector to the least squares vector
	integer,allocatable,dimension(:) :: map_from !dim= num_free_global_param
	integer,allocatable,dimension(:) :: map_to !dim=num_total_global_param
	logical,allocatable,dimension(:) :: fixed !dim=num_total_global_param

! used in least-squares fitting
	real(8),allocatable,dimension(:,:) :: curvature,cov,ginva !dim = num_free_global_param**2 !ginva
	real(8),allocatable,dimension(:) :: gradient,delta,diag !dim = num_free_global_param
	real,allocatable,dimension(:) :: correct !dim = num_free_global_param

!\\\\\\\\\\\\\\

! constructor=constructor_general
! destructor=destructor_general
!/////////////
	integer,allocatable,dimension(:) :: local_DOF !dim = num_expt
	real,allocatable,dimension(:) :: local_reduced_chi_sqr !dim = num_expt

!\\\\\\\\\\\\\


!	real,allocatable,dimension(:) :: cdata,cdata2
!	real,allocatable,dimension(:) :: param_temp
!	integer :: deviceID
!	integer :: len,ios,start
!	real(8),allocatable,dimension(:,:) :: cov
!	real(8),allocatable,dimension(:) :: gradient,delta,diag !,diffn ,vec,delta2
!	real,allocatable,dimension(:) :: temp
!	real(8) :: cond ! 1/condition # of the curvature matrix
!	integer,dimension(2) :: arr=(/1,2/)
!	integer,dimension(3) :: lims
!	integer :: irank
!	real(8) :: tol
!	integer :: num_rig_free_param


! rigorous error analysis section

	logical :: skip_minimization ! set to true when you want to bootstrap on simulated data
	integer :: rig_arr_size !default%tot_grid_size**current_rig%num_rig_param
	real :: tgt ! = chi^2 significance level for support plane analysis
	real :: x_r ! used in bootstrapping; set by wacko_func
!	integer :: posn ! position in rig_arr -- calculated by pos

! constructor=
! destructor=
!///////////////
	real,allocatable,dimension(:) :: fixed_param_val ! dim=current_rig%num_rig_param
	integer,allocatable,dimension(:) :: rig_index ! dim=current_rig%num_rig_param
	type(rig_struct),allocatable,dimension(:) :: rig_arr ! dim=arr_size
	real,allocatable,dimension(:) :: free_param_temp !dim=num_free_global_param

!\\\\\\\\\\\\\\\


end module h_vars