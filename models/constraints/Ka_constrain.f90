function Ka_constrain(current_model,current_data) result(out)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: out

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

real,dimension(size(current_model%model%param(1)%param_basis)) :: K1,K2

! This is a simple linear constraint model

! Begin
K1=param_list(current_model%model%param(1)%param_basis)%val
K2=param_list(current_model%model%param(2)%param_basis)%val

out=K1*(1.+K2)

return

end function
