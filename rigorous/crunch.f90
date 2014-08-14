function crunch(current_rig,dont_use_free_param_temp) result(res)
! Note: free_param_temp is optional.  If it is passed then the calling
! routine has used gradient information to provide a guess for
! the free param values.  Otherwise crunch will use the free param
! values at the chi_sqr minimum.
	use h_vars
	use h_params
	use h_routines, only: minimize
	use h_struct
	use h_rigorous, only: update_fixed_param_val,arr_write
	implicit none

	type(rig_logic_struct),pointer :: current_rig
	logical,optional :: dont_use_free_param_temp
	integer :: res

	! h_vars,intent(in)
	!	free_param_temp

! Local Var's
	!	real,dimension(current_rig%num) :: fixed_param_val

! Begin
	if (present(dont_use_free_param_temp)) then !if present then it's true
		param_list(map_from(:))%val = param_list(map_from(:))%save_val
	else
		param_list(map_from(:))%val = free_param_temp(:)
	endif
	call update_fixed_param_val(current_rig)
	res=minimize()
	! test res here ******
	call arr_write(current_rig)

	return
end function crunch