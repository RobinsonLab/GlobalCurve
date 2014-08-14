subroutine kinetics_plot_drv(current_expt,current_data)
	use h_params
	use h_struct
	use h_plots
	use h_utils
	implicit none
	type(expt_struct), pointer :: current_expt
	type(data_struct),pointer :: current_data
!	integer,intent(in) :: i ! experiment #
	integer :: device_id,ios

! Local Var's
	character(STRING_LEN) :: out_file_name,temp
	type(color_struct) :: colors
	integer :: pgopen
! Begin
	colors = color_scheme(bow=.false.,color=.true.)
	! output to screen
	device_id = pgopen(DEVICE_TYPE)
	if (default%simul) then 
		call kinetics_simul_plot(current_expt,current_data,colors)
	else
		call kinetics_plot(current_expt,current_data,colors)
	endif
	call pgclos
	! output as GIF or post_script file
	colors = color_scheme(bow=.true.,color=default%color)
	if (default%ps) then
		ios=make_out_file_name(current_expt%file_name,'ps',temp)
		! add '/cps' to temp = out_file_name
		write(out_file_name, '(a,a)' ) trim(temp),'/cps'    
		! cps is for color post script
	else ! output as GIF
		ios=make_out_file_name(current_expt%file_name,'gif',temp)
		! add '/gif' to temp = out_file_name
		write(out_file_name, '(a,a)' ) trim(temp),'/gif'    
	end if
	device_id = pgopen(out_file_name)
	if (default%simul) then 
		call kinetics_simul_plot(current_expt,current_data,colors)
	else
		call kinetics_plot(current_expt,current_data,colors)
	endif
	call pgclos

	return
10 format( a,' (',i3,')' )
end subroutine kinetics_plot_drv