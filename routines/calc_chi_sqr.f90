subroutine calc_chi_sqr()
	use h_vars
	use h_struct
	use h_routines, only : expt_fit
	implicit none

	! h_struct,intent(inout)
	!	type(data_struct),dimension(:),target,intent(inout) :: data_list
	!	type(param_struct),dimension(:),intent(inout) :: param_list

	! h_struct,intent(in)
	!	type(expt_struct),pointer :: expt_list

	! h_vars,intent(out)
	!	real :: global_reduced_chi_sqr
	!	real,dimension(num_expt) :: local_reduced_chi_sqr

	! h_vars,intent(in)
	!	integer,intent(in) :: global_DOF
	!	integer,pointer,dimension(:) :: local_DOF


! Local Vars
	real :: chi_sqr
	integer :: i
	type(expt_struct),pointer :: current_expt
	type (data_struct),pointer :: current_data
	integer,dimension(3) :: lims

! Begin
	chi_sqr =0.
	current_expt=>expt_list
	i=1 ! counter over experiments
	expt_loop: do 
		current_data=>data_list(i)
		lims(:)=current_data%lims(:)
		call expt_fit(0,current_data,current_expt)
		current_data%resid(1:) = current_data%datum(lims(3)-lims(1)+1:) - current_data%cdata(lims(3)-lims(1)+1:)
		local_reduced_chi_sqr(i) = sum(current_data%resid(:)**2 * current_data%weight(:))
		! lims(2)-lims(1)+1	as an endpoint for exp_data%resid	
!		! do not include virtual data sets in calculation of global chi^2
!		if (current_expt%data_type .ne. 4) &
			chi_sqr = chi_sqr + local_reduced_chi_sqr(i)*current_expt%scale
		local_reduced_chi_sqr(i)=local_reduced_chi_sqr(i)*current_expt%scale/local_DOF(i)
		if (.not. associated(current_expt%next)) exit expt_loop
		! no more experiments left
		! else keep going
		current_expt=>current_expt%next
		i=i+1
	enddo expt_loop
	global_reduced_chi_sqr=chi_sqr/global_DOF
	return
end subroutine calc_chi_sqr

