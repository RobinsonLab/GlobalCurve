subroutine expt_fit(pos,current_data,current_expt)
! note for now we are calling the routies w/o an explicit x vector.  The functions are
! calculating data based on num of points and a tcal.
! Other kinds of data are given in {x,y} pairs for example -- in which case the functions
! would need to read the x_vals and calculate y's only for those val's.

	use h_vars
	use h_params
	use h_struct
	use h_models
	use h_routines, only : convolute
	implicit none

	! h_struct
	! intent(in)
	!	param_list(:)%val

	integer,intent(in) :: pos ! for position, used in determining if the calculated data
	! is at +,0,or -1 * delchange in calculating derivatives
	type (data_struct),pointer :: current_data
	type(expt_struct),pointer :: current_expt

! Local Var's
	type(model_struct), pointer :: current_model
	real,dimension(current_data%len) :: calc_data,fluor,res
	pointer (p_rout,rout) ! digital/compac fortran special function
					! that allows us to point to subroutines which
					! is a feature not suported by F95.

! Begin
	! for virtual data sets we need to move param values into data.
!	if (current_expt%data_type == 4) &
!		current_data%datum(:)=param_list(current_data%datum_ptr(:))%val
	calc_data(:)=0.
	current_model=>current_expt%expt
	model_loop: do
		p_rout=current_model%model_addr
		fluor=rout(current_model,current_data)
			! scale model output by the scale factor param. for that model			
		! if a constr model don't scale
		if (current_expt%data_type == 4) then
			calc_data(:)=calc_data(:)+fluor(:)
		else
			calc_data(:)=calc_data(:)+param_list(current_model%model_param)%val*fluor(:)
		endif
		if (.not. associated(current_model%next)) exit model_loop
		! no more models left for this expt.
		! else keep going
		current_model=>current_model%next
	enddo model_loop
	select case (current_expt%class)	! class determines how the calculated data is 
										! post processed
		case(0)
			res=calc_data
		case(1)
			res=convolute(current_data,calc_data, &
				param_list(current_expt%expt_param)%val ) ! last param is qshift
	end select
	select case (pos)	! for derivative calculations
		case (-1)
			current_data%cdata_backward=res
		case (0)
			current_data%cdata=res
		case (1)
			current_data%cdata_foreward=res
	end select

end subroutine expt_fit
