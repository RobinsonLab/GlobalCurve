subroutine update_fixed_param_val(current_rig)
	! calculate the rigorous param values at the grid point speccified by rig_index using
	! limits defined in current_rig%lim

! (index+GRID_SIZE)/(2*GRID_SIZE) = (x-lim1)/(lim2-lim1)
	use h_vars
	use h_params ! default
	use h_struct ! param_list
	implicit none
	type(rig_logic_struct),pointer :: current_rig

	! h_vars,intent(in)
	!	integer,dimension(current_rig%num_rig_param),intent(in) :: rig_index

	! h_struct,intent(in)
	!	rig_list

	! h_struct,intent(out)
	!	param_list


! Begin
	param_list(current_rig%param_loc(:))%val =(current_rig%lim(:,2)-current_rig%lim(:,1)) * &
		real(rig_index+default%grid_size)/real(2*default%grid_size)+current_rig%lim(:,1)

end subroutine update_fixed_param_val
