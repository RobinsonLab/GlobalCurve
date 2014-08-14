module h_rigorous
interface
	
function area_under_the_curve(current_model,xl,xr,total_area) result(area)
	use h_struct
	implicit none
	type(model_struct), pointer :: current_model
	real,intent(in) :: xl,xr,total_area
	real :: area
end function

	function do_bootstrap() result(res)
		integer :: res
	end function

function wacko_func(current_model,x_l,extra_params)
	use h_struct
	type(model_struct), pointer :: current_model
	real,dimension(:),intent(in),optional :: extra_params
	real,intent(in) :: x_l
	real :: wacko_func
end function

function rigorous_driver(current_rig) result(res)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
	integer :: res
end function


function crunch(current_rig,dont_use_free_param_temp) result(res)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
	logical,optional :: dont_use_free_param_temp
	integer :: res
end function
subroutine arr_write(current_rig)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
end subroutine

function corners_2D(current_rig,count) result(res)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
	integer,intent(in) :: count
	integer :: res
end function

function corners_3D(current_rig,count) result(res)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
	integer,intent(in) :: count
	integer :: res
end function

function f_conf(chi_sqr_min,ndeg_1,ndeg_2,conf)
	implicit none
	real,intent(in) :: chi_sqr_min
	integer,intent(in) :: ndeg_1,ndeg_2
	real,optional :: conf
	real :: f_conf
end function

subroutine output_rig_res(current_rig)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
end subroutine

function pos(vec)
	implicit none
	integer,dimension(:),intent(in) :: vec
	integer :: pos
end function

function rigorous(current_rig)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
	integer :: rigorous
end function

function sides_3D(current_rig,count) result(res)
	use h_struct
	implicit none
	type(rig_logic_struct),pointer :: current_rig
	integer,intent(in) :: count
	integer :: res
end function

subroutine update_fixed_param_val(current_rig)
	use h_struct ! param_list
	implicit none
	type(rig_logic_struct),pointer :: current_rig
end subroutine

subroutine constructor_rigorous(current_rig)
	use h_vars
	implicit none
	type(rig_logic_struct),pointer :: current_rig
end subroutine

subroutine destructor_rigorous()
	implicit none
end subroutine

end interface
end module h_rigorous