subroutine output_results(elapsed_time,add_noise)
!	ans_file_name,param_list,expt_list,data_list, &
!	num_iter,elapsed_time,global_chi_sqr,add_noise,map_to,fixed,cov)
	use h_vars
	use h_params
	use h_struct
	use h_routines
	use h_utils, only: calc_gaus_mean_hw,calc_gamma_mean_hw,make_out_file_name, &
		error_handler, append_string
	use h_plots
	use h_fluor_wrapper_models, only: gaus_dist_et,gamma_dist_et
	implicit none

	real,intent(in) ::elapsed_time
	logical,intent(in),optional :: add_noise

	! h_vars,intent(in)
	!	integer,intent(in) :: num_iter
	!	real,intent(in) :: global_reduced_chi_sqr
	!	type(param_struct),pointer,dimension(:) :: param_list
	!	type(expt_struct), pointer :: expt_list
	!	type (data_struct),pointer, dimension(:) :: data_list
	!	integer,dimension(:),pointer :: map_to
	!	logical,dimension(:),pointer :: fixed
	!	character(STRING_LEN) :: ans_file_name
	! h_vars,intent(inout)
	!	real(8),dimension(num_free_global_param,num_free_global_param) :: cov
! Local Vars
	type(expt_struct),pointer :: current_expt
	type(data_struct),pointer :: current_data
	type(model_struct), pointer :: current_model
	integer :: i,j,k,ios,deviceID
	real :: lamp_max,data_max
	character(STRING_LEN) :: out_file_name,title,temp
	integer :: start
	real :: scale
	integer :: device_id,pgopen
	real :: x_min,x_max,y_min,y_max
	real,dimension(:),allocatable :: weighted_resid,acv,auto_correl
	real,dimension(1) :: seac
	integer :: len
	real :: sigma
	integer :: correl_len
	real,dimension(2) :: mean,hw,TE
	real,dimension(num_free_global_param) :: var
if(default%simul) then
	!save the data and plot out the results.
	current_expt=>expt_list
	i=1
	expt_loop2: do !loop over experiments
	current_data=>data_list(i)
		open(unit = DATA_FILE,file=current_expt%file_name, &
			status='unknown',iostat=ios)
		if (ios /= 0) call error_handler('Error opening file: ',current_expt%file_name)
		select case(current_expt%data_type)
		case(1) ! time resolved fluorescence
			write(DATA_FILE,20)data_list(i)%tcal,(data_list(i)%lims(j),j=1,3)
		case(2,3)
			write(DATA_FILE,25)current_expt%x_lab,current_expt%y_lab
			write(DATA_FILE,*)' Lims'
		case(4)
			! do nothing
		case(5)
			write(DATA_FILE,25)current_expt%x_lab,current_expt%y_lab
			write(DATA_FILE,*)"1  100"
		end select

		do j=1,data_list(i)%lims(1)-1
			write(DATA_FILE,*)0.0,0.0
		enddo !j
		do j=1,current_data%len
			select case(current_expt%data_type)
			case(1) ! time resolved fluorescence
!				if (add_noise) then
!					write(DATA_FILE,50)current_data%lamp(j),current_data%datum(j),sigma
!				else
					write(DATA_FILE,50)current_data%lamp(j),current_data%datum(j) 
!				endif
			case(2,3) ! kinetics, binding
!				if (add_noise) then
!					write(DATA_FILE,50)current_data%x_pts(j),current_data%datum(j),sigma
!				else
					write(DATA_FILE,50)current_data%x_pts(j),current_data%datum(j) 
!				endif
			case(4) ! virtual data set
			! don't do anything
			case(5) ! input driven kinetics
				write(DATA_FILE,50)current_data%x_pts(j),current_data%lamp(j), &
					current_data%datum(j) 
			end select
		enddo !j
		close(DATA_FILE,iostat=ios)
		call plot_driver(current_expt,current_data)
		if (.not. associated(current_expt%next)) exit expt_loop2
		! no more experiments left
		! else keep going
		current_expt=>current_expt%next
		i=i+1
	enddo expt_loop2
!	call pgclos



else ! not a simulation
	! calculate prelim estimates of param std.
	forall( j=1:num_free_global_param )
		var(j)=sqrt( abs( cov(j,j) ) )
	end forall
	forall( j=1:num_free_global_param )
		forall( k=j+1:num_free_global_param)
			cov(j,k)=cov(j,k)/( var(j)*var(k) )
		end forall
		cov(j,j)=0.0
	end forall	
	! fill in the lower triangular part of the cov matrix
	forall(j=2:num_free_global_param)
		forall(k=1:j-1)
			cov(j,k)=cov(k,j)
		end forall !k
	end forall !j


	ios=make_out_file_name(ans_file_name,'out',out_file_name) 
	if (ios /= 0) call error_handler('error constructing output file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
	
	write(*,*)'Number of iterations: ',num_iter
	write(*,*)'Global Chi Sqr: ',global_reduced_chi_sqr
	write(*,*)'Analysis time: ',elapsed_time
	write(*,*)'  #  Parameter      Value      Standard Deviation Est.'
	write(DATA_FILE,*)out_file_name
	write(DATA_FILE,*)'Number of iterations: ',num_iter
	write(DATA_FILE,*)'Global Chi Sqr: ',global_reduced_chi_sqr
	write(DATA_FILE,*)'Analysis time: ',elapsed_time
	write(DATA_FILE,*)'  #  Parameter      Value      Standard Deviation Est.'
	do i=1,size(param_list)
		if (fixed(i)) then
			write(*,30)i,param_list(i)%name,param_list(i)%val
			write(DATA_FILE,30)i,param_list(i)%name,param_list(i)%val
		else
		! write out error estimate too
		j=map_to(i)
			write(*,35)i,param_list(i)%name,param_list(i)%save_val, &
				global_reduced_chi_sqr*var(j)
			write(DATA_FILE,35)i,param_list(i)%name,param_list(i)%save_val, &
				global_reduced_chi_sqr*var(j)
		endif
	enddo !i

	! if we have distributed ET then we need to do some post processing to
	! get mean distance and half_width.
	! Loop over all experiments and models
	current_expt=>expt_list
	i=1
	expt_loop: do !loop over experiments
		current_model=>current_expt%expt
		j=1
		model_loop: do
			if (current_model%model_addr==loc(gaus_dist_et)) then
				call calc_gaus_mean_hw(current_model,mean,hw,TE)
				write(*,36)'Experiment(',i,',',j,')'
				write(*,*)'For the observable ET range:'
				write(*,37)' Mean =',mean(1),',hw =',hw(1),', mean TE =',TE(1)
				write(*,*)'For the distances 0 to rmax:'
				write(*,37)' Mean =',mean(2),',hw =',hw(2),', mean TE =',TE(2)

				write(DATA_FILE,36)'Experiment(',i,',',j,')'
				write(DATA_FILE,*)'For the observable ET range:'
				write(DATA_FILE,37)' Mean =',mean(1),',hw =',hw(1),', mean TE =',TE(1)
				write(DATA_FILE,*)'For the distances 0 to rmax:'
				write(DATA_FILE,37)' Mean =',mean(2),',hw =',hw(2),', mean TE =',TE(2)

			elseif (current_model%model_addr==loc(gamma_dist_et)) then
				call calc_gamma_mean_hw(current_model,mean,hw,TE)
				write(*,36)'Experiment(',i,',',j,')'
				write(*,*)'For the observable ET range:'
				write(*,37)' Mean =',mean(1),',hw =',hw(1),', mean TE =',TE(1)
				write(*,*)'For the distances 0 to rmax:'
				write(*,37)' Mean =',mean(2),',hw =',hw(2),', mean TE =',TE(2)

				write(DATA_FILE,36)'Experiment(',i,',',j,')'
				write(DATA_FILE,*)'For the observable ET range:'
				write(DATA_FILE,37)' Mean =',mean(1),',hw =',hw(1),', mean TE =',TE(1)
				write(DATA_FILE,*)'For the distances 0 to rmax:'
				write(DATA_FILE,37)' Mean =',mean(2),',hw =',hw(2),', mean TE =',TE(2)
			endif
			if (.not. associated(current_model%next)) exit model_loop
			! no more models left for this expt.
			! else keep going
			current_model=>current_model%next
			j=j+1
		enddo model_loop
		if (.not. associated(current_model%next)) exit expt_loop
		! no more models left for this expt.
		! else keep going
		current_model=>current_model%next
		i=i+1
	enddo expt_loop


	close(DATA_FILE)

	ios=make_out_file_name(ans_file_name,'cov',out_file_name) 
	if (ios /= 0) call error_handler('error constructing output file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
	do i=1,size(param_list)
		if (.not. fixed(i)) then
			j=map_to(i)
!			write(*,35)i,param_list(i)%name,param_list(i)%save_val, &
!				sqrt( abs (  cov(  j,j ) ) )
			write(DATA_FILE,60) param_list(i)%name,cov(j,:)
		endif
	enddo !i
	close(DATA_FILE)


! Begin the graphs
! first print to the screen then output postscript.
! print to the screen first.

	current_expt=>expt_list
	i=1
	expt_loop4: do !loop over experiments
		current_data=>data_list(i)
		scale=maxval(current_data%datum)
		ios=append_string(current_expt%file_name,'.tmp',out_file_name) 
		if (ios /= 0) call error_handler('error constructing output file name')
		open(unit = DATA_FILE,file=out_file_name, &
			status='unknown',iostat=ios)
		if (ios /= 0) call error_handler('Error opening file: ',out_file_name)
! write the header
		select case(current_expt%data_type)
		case(1) ! time resolved fluorescence
			write(DATA_FILE,20)data_list(i)%tcal,(data_list(i)%lims(j),j=1,3)
		case(2,3)
			write(DATA_FILE,27)current_expt%x_lab,current_expt%y_lab,current_expt%y_lab, 'residuals'
		case(4)
			! do nothing
		end select
! padd with zero's if necessary
		do j=1,data_list(i)%lims(1)-1
			write(DATA_FILE,*)0.0,0.0,0.0
		enddo !j
		do j=1,current_data%len
			select case(current_expt%data_type)
			case(1) ! time resolved fluorescence
				write(DATA_FILE,45)current_data%lamp(j),current_data%datum(j), &
					current_data%cdata(j)
!
!				write(DATA_FILE,50)current_data%lamp(j),current_data%datum(j), &
!					current_data%cdata(j),current_data%resid(j)*sqrt(current_data%weight(j))
			case(2,3) ! kinetics, binding
				write(DATA_FILE,50)current_data%x_pts(j),current_data%datum(j), &
					current_data%cdata(j),current_data%resid(j)*sqrt(current_data%weight(j))
			case(4) ! virtual data set
			! don't do anything
			end select
		enddo !j
		close(DATA_FILE,iostat=ios)

! make the graphs
		call plot_driver(current_expt,current_data)
   		if (.not. associated(current_expt%next)) exit expt_loop4
		! no more experiments left
		! else keep going
		current_expt=>current_expt%next
		i=i+1
	enddo expt_loop4

endif ! if simul
!	CALL PGCLOS

!pause
return
10 format( a,' (',i3,')' ) 
20 format(F10.8,3I10)
25 format('         ',2A16)
27 format('         ',4A16)
30 format(I4,' ',A15,ES11.4)
35 format(I4,' ',A15,ES11.4,' +/-',ES12.3)
36 format(A,I2,A,I2,A)
37 format(A,F8.3,A,F8.3,A,F5.3)
40 format(3F10.8)
45 format(3ES15.5)
50 format(4ES15.5)
60 format(a10,<num_free_global_param>F9.5)
end subroutine output_results