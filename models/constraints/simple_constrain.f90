function simple_constrain(current_model,current_data) result(out)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: out

! Global params
	! h_vars,intent(in)
	!	type(param_struct),dimension(:),intent(in) :: param_list

! Local Parameters
real,dimension(size(current_model%model%param(1)%param_basis)) :: coeff,param

! This is a simple linear constraint model

! Begin
param=param_list(current_model%model%param(1)%param_basis)%val
coeff=param_list(current_model%model%param(2)%param_basis)%val

out=sum(coeff*param)

return

end function simple_constrain
