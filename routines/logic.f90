subroutine constructor_logic(mask)
! This routine is called when num_free_global_param is changed i.e. we have a new mask
! if mask is true then the param is fixed
! set num_total_global_param and num_free_global_param before calling this routine!
	use h_vars
	implicit none
	logical,dimension(num_total_global_param) :: mask
! Global Vars
	! h_vars,intent(in)
	!	num_total_global_param
	!	num_free_global_param
		
	! h_vars,intent(out)
	!	map_to
	!	map_from


! Local Vars
	integer :: i,j

! Begin
	if (allocated(map_from)) call destructor_logic() ! new num_free_global_param
	allocate(map_from(num_free_global_param))

	allocate(curvature(num_free_global_param,num_free_global_param))
	allocate(ginva(num_free_global_param,num_free_global_param))
	allocate(cov(num_free_global_param,num_free_global_param))

	allocate(gradient(num_free_global_param))
	allocate(delta(num_free_global_param))
	allocate(diag(num_free_global_param))
	allocate(correct(num_free_global_param))

	j=1
	do i=1,num_total_global_param
		if (.not. mask(i)) then
			map_from(j)=i
			map_to(i)=j
			j=j+1
		else
			map_to(i)=0 !not realy used
		endif	
	enddo

!	num_free_global_param=j

	call calc_local_map_from(mask)

end subroutine constructor_logic

subroutine destructor_logic()

	use h_vars
	implicit none

! Begin
	deallocate(map_from)

	deallocate(curvature)
	deallocate(ginva)
	deallocate(cov)
	
	deallocate(gradient)
	deallocate(delta)
	deallocate(diag)
	deallocate(correct)

end subroutine destructor_logic

subroutine calc_local_map_from(mask)
! determine num_local_param and define num_local_free_param

	use h_vars
	use h_struct
	implicit none
	logical,dimension(num_total_global_param),intent(in) :: mask

	! h_vars,intent(in)
	!	logical,dimension(:),intent(in) :: mask
	!	type(expt_struct), pointer :: expt_list

 	! h_vars,intent(out)
	!	type(expt_struct), pointer :: expt_list


! Local Var's
	integer :: i,j,k
	type(expt_struct), pointer :: current_expt

! Begin
	current_expt=>expt_list
	i=1 ! counter over experiments
	expt_loop2: do
		if (associated(current_expt%local_map_from)) deallocate(current_expt%local_map_from)
		! figure out num_local_free_param
		k=0
		do j=1,current_expt%num_local_param
			if (.not. mask(current_expt%local_param(j))) then
				k=k+1
			endif
		enddo !j

		current_expt%num_local_free_param=k	
		allocate(current_expt%local_map_from(current_expt%num_local_free_param))
!***********
		k=0
		do j=1,current_expt%num_local_param
			if (.not. mask(current_expt%local_param(j))) then
				k=k+1
				current_expt%local_map_from(k)=current_expt%local_param(j) 
			endif
		enddo !j

		if (.not. associated(current_expt%next)) exit expt_loop2
		! no more experiments left
		! else keep going
		current_expt=>current_expt%next
		i=i+1
	enddo expt_loop2

	return

end subroutine calc_local_map_from


