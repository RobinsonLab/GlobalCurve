subroutine time_dom_fluor_simul_plot(current_expt,current_data,colors)
	use h_struct
	use h_params
	implicit none
	type(expt_struct), pointer :: current_expt
	type (data_struct),pointer :: current_data
	type(color_struct),intent(in) :: colors
!	integer,intent(in) :: i ! experiment #

! Local Vars
!	real,dimension(current_data%num_points) :: weighted_resid
!	real,dimension(current_data%num_points/2) :: acv,seac,auto_correl
	real :: scale
	real :: x_min,x_max,y_min,y_max
!	integer :: correl_len=current_data%num_points/2
	integer :: start
	character(STRING_LEN) :: title

! Begin

! for future reference:
! create postscript graphs.
! output: expt_file_name w/ ps as extension for main plot
!         expt_file_name w/ aut as extension for autocorrelation
!         expt_file_name w/ con as extension for confidence limits


! --- top panel


	scale=maxval(current_data%cdata)
	start=current_data%len-current_data%num_points + 1

	call pgpage		
	call pgsvp(0.1,0.95,0.1,0.9)
	x_min=current_data%x_pts(1)
	x_max=current_data%x_pts(current_data%len)
	y_min=0.
	y_max=scale
	call pgswin( x_min,x_max,y_min,y_max )
	call pgsci(colors%WHITE)
	call pgbox ('bcnst',0.0,0,'bcnst',0.0,0)
!	write(title,10)trim(current_expt%file_name),i
	call pgsci(colors%WHITE)
	CALL PGLAB(trim(current_expt%x_lab),trim(current_expt%y_lab), &
		trim(current_expt%file_name))
	!plot out the data
	call pgsci(colors%WHITE)
	call pgpt(current_data%len,current_data%x_pts(:), &
		current_data%datum(:),PLOT_SYMBOL)
	!plot out the lamp
	call pgsci(colors%GREEN)
	call pgline(current_data%len,current_data%x_pts(:), &
		current_data%lamp(:)*scale)

	return
! 10 format( a,' (',i3,')' )
end subroutine time_dom_fluor_simul_plot