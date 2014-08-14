module h_binding_models
interface

function hill(current_model,current_data) result(bind)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind
end function

function three_state_simple_model(current_model,current_data) result(bind)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind
end function


function simulate_ca_gr_titr(current_model,current_data) result(bind)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind
end function

function frac_sat(current_model,current_data) result(bind)
	use h_struct
	implicit none	
	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len) :: bind
end function

end interface
end module h_binding_models