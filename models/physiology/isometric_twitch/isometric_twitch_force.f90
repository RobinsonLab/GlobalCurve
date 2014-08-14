function isometric_twitch_force(current_model,current_data) result(force)
	use h_struct
	use h_routine_specific_vars
	use h_models, only: calc_isometric_twitch
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: force

! constants
	integer,parameter :: n=3

! Local Var's
	real,dimension(current_data%len,n) :: y
	real :: offset,F_zero
! begin
! the raw F that is calculated is the duty cycle of myosin x-bridges
! so Fo needs to scale this value to the data.
	offset=param_list(current_model%model%param(3)%param_basis(1))%val
	F_zero=param_list(current_model%model%param(3)%param_basis(2))%val
	call init_tsv()
! ****
!! we're going to commit a no-no here just for testing
!	current_data%len=600
!	allocate(current_data%x_pts(600))
! *****

!	range_to=max-min
	y=calc_isometric_twitch(current_model,current_data,n)
	force(:)=y(:,3)
	force=force*F_zero + offset 
end function