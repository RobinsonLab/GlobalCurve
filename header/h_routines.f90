module h_routines
interface

	subroutine add_noise_to_data(current_expt,current_data)
		use h_struct
		implicit none
		type(expt_struct), pointer :: current_expt
		type(data_struct), pointer :: current_data
	end subroutine

	subroutine expt_type_parser(current_expt)
		use h_struct
		type(expt_struct), pointer :: current_expt
	end subroutine

	function asymptotic_elipse(current_rig) result(res)
		use h_struct
		type(rig_logic_struct),pointer :: current_rig
		integer :: res
	end function

	subroutine output_results(elapsed_time,add_noise)
		implicit none
		real,intent(in) ::elapsed_time
		logical,intent(in),optional :: add_noise
	end subroutine

	subroutine calc_chi_sqr()
	end subroutine

	subroutine calc_covariance()
	end subroutine

	subroutine calc_curvature_gradient()
	end subroutine

	function convolute(p,fluor,q_shift) result (intensity)
		use h_struct
		implicit none		
		type (data_struct),pointer :: p
		real,dimension(:),intent(in) :: fluor
		real,intent(in) :: q_shift
		real,dimension(size(fluor)) :: intensity 
	end function

	subroutine expt_fit(pos,current_data,current_expt)
		use h_struct
		implicit none	
		integer,intent(in) :: pos 
		type (data_struct),pointer :: current_data
		type(expt_struct),pointer :: current_expt
	end subroutine

	subroutine constructor_general()
	end subroutine

	subroutine destructor_general()
	end subroutine

	subroutine destructor_particular()
	end subroutine

	subroutine generate_lamp(tcal,fwhm,lim,lamp)
		use h_params
		implicit none	
		real,intent(in) :: tcal
		real,intent(in) :: fwhm
		integer,intent(in) :: lim
		real,dimension(CHAN_MAX),intent(out) :: lamp
	end subroutine
	
	subroutine get_default ()	
	end subroutine

	subroutine calc_local_map_from(mask)
		use h_vars
		implicit none		
		logical,dimension(num_total_global_param),intent(in) :: mask
	end subroutine

	subroutine constructor_logic(mask)
		use h_vars
		implicit none		
		logical,dimension(num_total_global_param),intent(in) :: mask
	end subroutine

	subroutine destructor_logic()
	end subroutine

	function main() result(res)
		integer :: res
	end function

	function minimize() result(res)
		integer :: res
	end function

	function marquadt() result(res)
		integer :: res
	end function

	function cluster() result(res)
		integer :: res
	end function

	subroutine model_lexicon(model_name,model_id)
		use h_params
		implicit none
		character(STRING_LEN),intent(in) :: model_name
		integer(ADDR_LEN),intent(out) :: model_id
	end subroutine

	function plot_points(x_points,y_points,x_lims,y_lims,title, &
		axes_label,symbol,device,add_to_plot,color,llog) result(deviceID)
		implicit none 
		real,dimension(:),target,intent(in) :: x_points
		real,dimension(:),target,intent(in) :: y_points
		real,dimension(2),optional,target,intent(in) :: x_lims
		real,dimension(2),optional,target,intent(inout) :: y_lims
		character(*),optional,intent(in) :: title
		character(*),dimension(2),optional,intent(in) :: axes_label
		integer,optional,intent(in) :: symbol
		character(*),optional,intent(in) :: device
		logical,optional, intent(in) :: add_to_plot
		integer,optional,intent(in) :: color
		logical,optional, intent(in) :: llog
		integer :: deviceID
	end function

	function readans() result(res)
		integer :: res
	end function

	function rieman_integrate(fcn,N,dr) result (res)
		implicit none
		real,pointer,dimension(:) :: fcn
		real,intent(in) :: dr
		integer,intent(in) :: N
		real :: res
	end function

	function shift(lamp,d_lamp,q_shift)
		implicit none
		real,dimension(:),intent(in) :: lamp
		real,dimension(:) :: d_lamp
		real,intent(in) :: q_shift
		real,dimension(size(lamp)) :: shift
	end function

	function trapezoidal_integrate(fcn,N,dr) result (res)
		implicit none
		real,dimension(:) :: fcn
		real,intent(in) :: dr
		integer,intent(in) :: N
		real :: res
	end function

	subroutine tri_ge(alpha,beta,gamma,N,x,b)
		implicit none
		integer,intent(in) :: N
		real,dimension(N-1),intent(in) :: alpha,gamma
		real,dimension(N),intent(in) ::beta
		real,dimension(N),intent(in) :: b
		real,dimension(N),intent(out) :: x
	end subroutine

! this routine slated for elimination

	subroutine tri_mat_mult(alpha,beta,gamma,N,x,y)
		implicit none
		real,dimension(:) :: alpha,beta,gamma
		real,dimension(:) :: x
		real,dimension(:) :: y
		integer,intent(in) :: N
	end subroutine


end interface
end module h_routines
