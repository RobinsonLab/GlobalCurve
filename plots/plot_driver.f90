subroutine plot_driver(current_expt,current_data)
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
	if (default%simul) then
		select case(current_expt%data_type)
		case(0)
			! just a scaling so don't plot out
		case(1)
			! a time domain fluorescence experiment
			device_id = pgopen(DEVICE_TYPE)
			call time_dom_fluor_simul_plot(current_expt,current_data,colors)
			call pgclos
		case(2)
			! a kinetics experiement
!			call kinetics_plot_drv(current_expt,current_data)
		case(3)
!			call binding_simul_plot(current_expt,current_data,colors)
		case(4)
		! do nothing
		end select 
	else
		select case(current_expt%data_type)
		case(0)
			! just a scaling so don't plot out
		case(1)
			! a time domain fluorescence experiment
			device_id = pgopen(DEVICE_TYPE)
			call time_dom_fluor_plot(current_expt,current_data,colors)
			call pgclos		
		case(2)
			! a kinetics experiement
!			call kinetics_plot_drv(current_expt,current_data)
		case(3)
			device_id = pgopen(DEVICE_TYPE)
			call binding_plot(current_expt,current_data,colors)
			call pgclos
		case(4)
		! do nothing
		end select 
	endif

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
	if (default%simul) then
		select case(current_expt%data_type)
		case(0)
			! just a scaling so don't plot out
		case(1)
			! a time domain fluorescence experiment
			device_id = pgopen(out_file_name)
			call time_dom_fluor_simul_plot(current_expt,current_data,colors)
			call pgclos
		case(2)
			! a kinetics experiement
!			call kinetics_plot_drv(current_expt,current_data)
		case(3)
!			call binding_simul_plot(current_expt,current_data,colors)
		case(4)
		! do nothing
		end select 
	else
		select case(current_expt%data_type)
		case(0)
			! just a scaling so don't plot out
		case(1)
			! a time domain fluorescence experiment
			device_id = pgopen(out_file_name)
			call time_dom_fluor_plot(current_expt,current_data,colors)
			call pgclos
		case(2)
			! a kinetics experiement
!			call kinetics_plot_drv(current_expt,current_data)
		case(3)
			device_id = pgopen(out_file_name)
			call binding_plot(current_expt,current_data,colors)
			call pgclos
		case(4)
		! do nothing
		end select 
	endif

	return
10 format( a,' (',i3,')' )
end subroutine plot_driver