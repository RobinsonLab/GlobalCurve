module h_plots
interface

function color_scheme(bow,color) result(colors)
	use h_struct
	implicit none
	logical,intent(in) :: bow,color
	type(color_struct) :: colors
end function color_scheme

subroutine time_dom_fluor_plot(current_expt,expt_data_ptr,colors)
	use h_struct
	use h_params
	implicit none
	type(expt_struct), pointer :: current_expt
	type (data_struct),pointer :: expt_data_ptr
	type(color_struct),intent(in) :: colors
end subroutine

subroutine binding_plot(current_expt,expt_data_ptr,colors)
	use h_struct
	use h_params
	implicit none
	type(expt_struct), pointer :: current_expt
	type (data_struct),pointer :: expt_data_ptr
	type(color_struct),intent(in) :: colors
end subroutine

subroutine plot_driver(current_expt,expt_data_ptr)
	use h_params
	use h_struct
!	use h_plots
	implicit none
	type(expt_struct), pointer :: current_expt
	type (data_struct),pointer :: expt_data_ptr
end subroutine

subroutine time_dom_fluor_simul_plot(current_expt,expt_data_ptr,colors)
	use h_struct
	use h_params
	implicit none
	type(expt_struct), pointer :: current_expt
	type (data_struct),pointer :: expt_data_ptr
	type(color_struct),intent(in) :: colors
end subroutine


end interface
end module h_plots