module h_constr_models
interface

function simple_constrain(current_model,current_data) result(out)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: out
end function

function Ka_constrain(current_model,current_data) result(out)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: out
end function

function frac(current_model,current_data) result(out)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: out
end function
end interface
end module h_constr_models