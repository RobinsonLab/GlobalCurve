function frac(current_model,current_data) result(out)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: out

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
real :: param1,param2

! This model can be used to specify the label ratio in an ET experiment

! Begin

	param1=param_list(current_model%model%param(1)%param_basis(1))%val
	param2=param_list(current_model%model%param(1)%param_basis(2))%val


	out(1)=param1/(param1+param2)

return

end function frac
