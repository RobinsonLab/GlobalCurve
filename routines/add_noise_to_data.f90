subroutine add_noise_to_data(current_expt,current_data)
! datum = cdata+noise
! currently only coded for time resloved fluorescence noise
	use h_struct
	use numerical_libraries, only:rnset,rnnoa
	implicit none

	! h_struct,intent(in)
	!	current_data%cdata
	! h_struct,out
	!	current_data%datum

	type(expt_struct), pointer :: current_expt
	type(data_struct), pointer :: current_data


! Local Params

	real,parameter :: percent = 0.025
! Local Var's

	integer :: i,j
	real,dimension(current_data%len) :: noise
	real :: sigma
! Begin

!	call rnset(seed)
	call rnnoa (current_data%len,noise) ! get a gaussian distributed RV
	do j=1,current_data%len
		select case(current_expt%data_type)
		case(1) ! time resolved fluorescence
			if ( current_data%cdata(j) > 0) then
				sigma=sqrt(current_data%cdata(j))
			else
				sigma=0.
			endif
			current_data%datum(j)=current_data%cdata(j)+sigma*noise(j)
		case(2,3)
			sigma=current_data%cdata(j)*percent
			current_data%datum(j)=current_data%cdata(j)+sigma*noise(j)
		case(4)
			current_data%datum(j)=current_data%cdata(j)
		end select
	enddo !j


end subroutine
