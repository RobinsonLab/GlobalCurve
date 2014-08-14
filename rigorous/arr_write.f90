subroutine arr_write(current_rig)
! here we are comming very close to object oriented programming.  This is the only function
! that writes to arr.  arr could be defined in h_rigorous as private instead of being 
! passed as a variable
	use h_vars
	use h_struct
	use h_rigorous, only: pos	
	implicit none

	type(rig_logic_struct),pointer :: current_rig

	! h_vars,intent(in)
	!	type(rig_struct),allocatable,dimension(:) :: rig_arr
	!	integer,dimension(:),intent(in) :: index	
	!	integer,intent(in) :: num_iter

	! h_struct,intent(in)
	!	param_list

! x = fixed_param_val = param_list(current_rig%param_loc(:))%val
! y = global_reduced_chi_sqr	

! Local Var's
	integer :: posn

! Begin
	posn=pos(rig_index)
	rig_arr(posn)%point = (/ param_list(current_rig%param_loc(:))%val,global_reduced_chi_sqr /)
	rig_arr(posn)%free_param(:)=param_list( map_from(:) )%val
	rig_arr(posn)%num_iter=num_iter

end subroutine arr_write
