function three_state_ktr_force(current_model,current_data) result(force)
	use h_struct
	use h_models, only: calc_three_state_ktr
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
	offset=param_list(current_model%model%param(2)%param_basis(1))%val
	F_zero=param_list(current_model%model%param(2)%param_basis(2))%val
!	range_to=max-min
	y=calc_three_state_ktr(current_model,current_data,n)
	force(:)=y(:,3)
	force=force*F_zero + offset 
end function