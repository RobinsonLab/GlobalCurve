module h_struct
use h_params

!	type logical_struct
!		logical :: lrate,lenthalpy,lentropy,lactiv,ldist,lgaus, &
!			lloren,lunif,lsas,lbound,lgamma,lgap,lconc,llife,llength, &
!			lksv,lbeta,lphi,lptwo,ltherm,lslowrate,lslowbound,lback
!	end type logical_struct


! the following are declared pointer so that they can be dynamically allocated.
	type data_struct
!		character(len=8) :: dataType
!		character(len=30) :: fileName
		real,pointer :: lamp(:)
		real,pointer :: d_lamp(:)
		real,pointer :: datum(:)
		integer,pointer :: datum_ptr(:) ! used by virtual data sets
										! index array used to map global params
										! to datum.  The value is the location of the global
										! param in param_list.
		real,pointer :: temp_data(:)
		real,pointer :: cdata(:)
		real,pointer :: cdata_foreward(:)
		real,pointer :: cdata_backward(:)
		! workspace for convolutions
		real,pointer :: s_lamp(:) ! q_shifted lamp
		real,pointer :: z(:)
		real,pointer :: zhat(:)
		real,pointer :: xwk(:)
		real,pointer :: ywk(:)
		real,pointer :: wk(:)
		integer :: nz
		! finish workspace
		real,pointer :: x_pts(:)
		real,pointer :: resid(:)
		real,pointer :: resid_low(:)
		real,pointer :: weight(:)
		real :: tcal
		integer :: lims(3)
		integer :: len ! = lims2-lims1+1
		integer :: num_points ! N in (N-m-1) = lims2-lims3+1
!		integer :: loc_DOF ! (N-m-1)
	end type data_struct

	type param_basis_struct
		integer,pointer,dimension(:) :: param_basis 
					! could be allocatable instead of pointer
	end type param_basis_struct

	type param_logic_struct
		type(param_basis_struct),pointer,dimension(:) :: param
	end type param_logic_struct

!	external func ! the generic name for an answer file specified model 

	type model_struct
		type (param_logic_struct),pointer :: model
		integer :: model_param ! logic for this model's "bounds"
		integer(ADDR_LEN) :: model_addr	! Want to have a ptr to a subroutine
									! model.  But this will have to do.
!		integer :: num_decays ! the number of decay curves a model routine will
				! produce. e.g. subroutine diffuse produces 2: donor and acceptor
!		pointer(func_ptr,func)
		integer :: flag = 0
		type (model_struct), pointer :: next
	end type model_struct

	type expt_struct
		real :: scale ! a weighting factor for the experiment used in global analysis
		integer :: class ! identifies the class this expt is in - used in expt_fit
						! and in readans
						! all models for a given experiment must be of the same class
		character(3) :: expt_type
		integer :: data_type ! used by readans --- slated for elimination upon adoption of HDF
		character(STRING_LEN) :: x_lab,y_lab
!		character(STRING_LEN) :: x_units
		integer :: expt_param ! pointer to global param vector; such as qshift
		integer :: num_local_param 
		integer,pointer,dimension(:) :: local_param !dim=num_local_param
			!used for calculating derivatives quickly
			!identifies the global_param_num of a given "free" local_param
		integer :: num_local_free_param 
		integer,pointer,dimension(:) :: local_map_from !dim=num_local_free_param
		character(STRING_LEN) :: file_name
		type (model_struct),pointer :: expt
		type (expt_struct), pointer :: next
	end type expt_struct

	type param_struct
		real :: val ! wanted it to be target but it wouldn't work
		real :: save_val
		real :: min,max
		character(STRING_LEN) :: name
!		real :: temp_val
		logical :: fixed
!		integer :: map_to
	end type param_struct	

	type rig_logic_struct
		integer :: 	num_rig_param
		integer,pointer,dimension(:) :: param_loc !dim = num_rig_param
		real,pointer,dimension(:,:) :: lim !dim = [num_rig_param,2]
		type (rig_logic_struct),pointer :: next
	end type rig_logic_struct

	type rig_struct
		real,dimension(:),pointer :: point
		! point contains the { {a_i},chi_sqr } info where i is of
		! dimension num_rig_param
		real,dimension(:),pointer :: free_param
		integer :: num_iter ! useful for diagnontics.
	end type rig_struct

	type color_struct
		integer :: WHITE,BLACK,BLUE,GREEN,YELLOW,RED
	end type color_struct

! finished defining types

! define global variables

	type(expt_struct), pointer :: expt_list
	type(param_struct),allocatable,target,dimension(:) :: param_list
	type (data_struct),allocatable,target,dimension(:) :: data_list
	type(rig_logic_struct),pointer :: rig_list

end module h_struct