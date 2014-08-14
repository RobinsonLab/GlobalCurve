function three_state_ktr_bound(current_model,current_data) result(bound)
	use h_struct
	use h_models
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bound

! constants
	integer,parameter :: n=3

! Local Var's
	real,dimension(current_data%len,n) :: y
	real :: offset,B_zero
! begin
! the raw F that is calculated is the duty cycle of myosin x-bridges
! so Fo needs to scale this value to the data.
	offset=param_list(current_model%model%param(2)%param_basis(1))%val
	B_zero=param_list(current_model%model%param(2)%param_basis(2))%val
!	range_to=max-min
	y=calc_three_state_ktr(current_model,current_data,n)
	bound(:)=y(:,2)+y(:,3)
	bound=bound*B_zero + offset 
end function