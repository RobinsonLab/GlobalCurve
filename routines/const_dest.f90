subroutine constructor_general()
	use h_vars
	implicit none

	allocate(local_DOF(num_expt))
	allocate(local_reduced_chi_sqr(num_expt)) 


end subroutine constructor_general

subroutine destructor_general()
! call this destructor routine when the program is ready to shut down
	use h_vars
	use h_struct
	implicit none

! Local Var's
	type(rig_logic_struct),pointer :: current_rig,rig_ptr
	type(expt_struct),pointer :: current_expt,expt_ptr
	type(model_struct),pointer :: current_model,model_ptr
	integer :: i

! Begin
	deallocate(local_DOF)
	deallocate(local_reduced_chi_sqr)

! destructor for those core logic and data structures in readans

	!pertaining to expt_list 
	current_expt=>expt_list
	expt_loop: do 
		if(associated(current_expt%next)) then
			expt_ptr=>current_expt%next
		else
			nullify(expt_ptr)
		endif
		current_model=>current_expt%expt
		model_loop: do 
			if(associated(current_model%next)) then
				model_ptr=>current_model%next
			else
				nullify(model_ptr)
			endif
			
			do i=1,size(current_model%model%param)
				deallocate(current_model%model%param(i)%param_basis)
			enddo !i
			deallocate(current_model%model%param)
			deallocate(current_model)
			if (.not. associated(model_ptr)) exit model_loop
			current_model=>model_ptr
		enddo model_loop
		deallocate(current_expt)
		if (.not. associated(expt_ptr)) exit expt_loop
		current_expt=expt_ptr
	enddo expt_loop

	! pertaining to data_list
	do i=1,num_expt
		if (associated(data_list(i)%temp_data)) deallocate (data_list(i)%temp_data)
		if (associated(data_list(i)%lamp)) then
			deallocate (data_list(i)%lamp)
			deallocate (data_list(i)%s_lamp)
			deallocate (data_list(i)%d_lamp)
		endif
		deallocate (data_list(i)%datum)
		deallocate (data_list(i)%cdata)
		deallocate (data_list(i)%cdata_foreward)
		deallocate (data_list(i)%cdata_backward)
		deallocate (data_list(i)%x_pts)
		deallocate (data_list(i)%resid)
		deallocate (data_list(i)%resid_low)
		deallocate (data_list(i)%weight)
	enddo !n
	deallocate(data_list)

	!pertaining to param_list
	deallocate(param_list)
	deallocate(fixed)
	deallocate(map_to)

	!pertaining to rig_list
	if (error_analysis_type == 1) then !grid search then
		current_rig=>rig_list
		rig_loop: do 
			if(associated(current_rig%next)) then
				rig_ptr=>current_rig%next
			else
				nullify(rig_ptr)
			endif
			deallocate(current_rig%param_loc)
			deallocate(current_rig%lim)
			deallocate(current_rig)
			if (.not. associated(rig_ptr)) exit rig_loop
			current_rig=>rig_ptr
		enddo rig_loop
	endif
end subroutine destructor_general


subroutine destructor_particular()
! call this destructor routine when the program is ready to shut down
	use h_routine_specific_vars
	implicit none

! Begin
	if (associated(fsv)) deallocate(fsv)

end subroutine