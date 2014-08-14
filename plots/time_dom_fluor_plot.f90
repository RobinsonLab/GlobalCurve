subroutine time_dom_fluor_plot(current_expt,current_data,colors)
	use h_struct
	use h_params
	implicit none
	type(expt_struct), pointer :: current_expt
	type (data_struct),pointer :: current_data
	type(color_struct),intent(in) :: colors
!	integer,intent(in) :: i ! experiment #

! Local Vars
	real,dimension(current_data%num_points) :: weighted_resid
	real,dimension(current_data%num_points/2) :: acv,seac,auto_correl
	real :: scale
	real :: x_min,x_max,y_min,y_max
	integer :: correl_len
	integer :: start
	integer :: j
!	character(STRING_LEN) :: title

! Begin

! for future reference:
! create postscript graphs.
! output: expt_file_name w/ ps as extension for main plot
!         expt_file_name w/ aut as extension for autocorrelation
!         expt_file_name w/ con as extension for confidence limits


! --- top panel

	correl_len=current_data%num_points/2
	scale=maxval(current_data%datum)
	start=current_data%len-current_data%num_points + 1

!	call pgpage		
	call pgsvp(0.1,0.95,0.30,0.9)
	x_min=current_data%x_pts(1)
	x_max=current_data%x_pts(current_data%len)
	y_min=0.
	y_max=scale
	call pgswin( x_min,x_max,y_min,y_max )
	call pgsci(colors%WHITE)
	call pgbox ('bcst',0.0,0,'bcnst',0.0,0)
!	write(title,10)trim(current_expt%file_name),i
	call pgsci(colors%WHITE)
	CALL PGLAB('',trim(current_expt%y_lab),trim(current_expt%file_name))
	!plot out the data
	call pgsci(colors%WHITE)
	call pgpt(current_data%len,current_data%x_pts(:), &
		current_data%datum(:),PLOT_SYMBOL)
	!plot out the lamp
	call pgsci(colors%GREEN)
	call pgline(current_data%len,current_data%x_pts(:), &
		current_data%lamp(:)*scale)
	!plot out the calculated data
	call pgsci(colors%RED)
	call pgsls(1)
	call pgline(current_data%num_points, &
		current_data%x_pts(start:),current_data%cdata(start:))
! botom panel : the residuals
	weighted_resid=current_data%resid*sqrt(current_data%weight)
	call pgsvp(0.1,0.95,0.1,0.26)
	! want residual plot to be symmetric in the y axis
	y_max=maxval(weighted_resid)
	y_min=minval(weighted_resid)
	y_max=max(y_max,-y_min)
	!round y_max up to the first decimal place
	y_max=ceiling(y_max*10.)/10.
	y_min=-y_max
	! use same x_min and x_max as before.
	call pgswin( x_min,x_max,y_min,y_max )
	call pgsci(colors%WHITE)
	call pgbox ('bcnst',0.0,0,'bcnst',y_max,0)
	call pgsci(colors%WHITE)
!	write(x_lab,'(a,a,a)') 'Time (',trim(opts%x_scale),')'
	call pglab(trim(current_expt%x_lab),'Residuals','')
	!      CALL PGPT(5,XS,YS,9)
	! draw a line across the x_axis'
	call pgsls(4)
	call pgsci(colors%WHITE)
	call pgline(2,(/x_min,x_max/),(/0.0,0.0/))
	! plot out the residuals
	call pgsci(colors%RED)
	call pgsls(1)
	call pgline(current_data%num_points, &
	current_data%x_pts(start:),weighted_resid)
! the inset: plot out the autocorrelation function
	correl_len=current_data%num_points/2
	! calculate autocorrelation function
	! max_lag=correl_len-1
	call acf(current_data%num_points,weighted_resid,0, &
		0,0,0.,correl_len-1,acv,auto_correl, &
		seac)
	!acv and seac are not used	
	call pgsvp(0.58,0.92,0.75,0.85)
	! want residual plot to be symmetric in the y axis
	y_max=maxval(auto_correl(2:))
	y_min=minval(auto_correl(2:))
	y_max=max(y_max,-y_min)
	!round y_max up to the first decimal place
	y_max=ceiling(y_max*10.)/10.
	y_min=-y_max
	! use same x_min and x_max as before.
	call pgswin( 1.,real(correl_len-1),y_min,y_max )
	call PGSCH (0.7)
	call pgsci(colors%WHITE)
	call pgbox ('bcnst',0.0,0,'bcnst',y_max,0)
	call pgsci(colors%WHITE)
	call pglab('Lag (counts)','Autocorr.','')
	!      CALL PGPT(5,XS,YS,9)
	! draw a line across the x_axis'
	call pgsci(colors%white) 
	call pgsls(4)
    call pgline(2,(/1.,real(correl_len-1)/),(/0.0,0.0/))
	call pgsci(colors%BLUE) !color to blue
	call pgsls(1)
	call pgline(correl_len-1,(/ (real(j),j=1,correl_len-1) /), &
		auto_correl(2:correl_len) )
	call PGSCH (1.0)

	
	return

end subroutine time_dom_fluor_plot