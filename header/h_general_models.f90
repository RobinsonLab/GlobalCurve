module h_general_models
interface


function gaussian_wrapper(current_model,current_data) result(y)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: y
end function

end interface
end module h_general_models