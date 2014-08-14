function isotonic_twitch_atpase(current_model,current_data) result(ATPase)
	use h_struct
	use h_physiology_models, only: calc_isotonic_twitch
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: ATPase

! constants
	integer,parameter :: n=3

! Local Var's
	real,dimension(current_data%len,n) :: y
	real :: offset,A_zero
	real,dimension(current_data%len) :: g_save
! begin
! the raw F that is calculated is the duty cycle of myosin x-bridges
! so Fo needs to scale this value to the data.
	offset=param_list(current_model%model%param(3)%param_basis(1))%val
	A_zero=param_list(current_model%model%param(3)%param_basis(2))%val

! ****
!! we're going to commit a no-no here just for testing
!	current_data%len=600
!	allocate(current_data%x_pts(600))
! *****

!	range_to=max-min
	y=calc_isotonic_twitch(current_model,current_data,n,g_save)
	ATPase(:)=g_save(:)*y(:,3)
	ATPase=ATPase*A_zero + offset 

end function

