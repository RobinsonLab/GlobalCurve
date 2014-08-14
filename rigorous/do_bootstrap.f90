function do_bootstrap() result(res)
	use h_params
	use	h_struct
	use h_routines
	use h_rigorous
	use h_vars
	use h_utils
	implicit none
	
	integer :: res

! Local Params
	integer,parameter :: num_gaussians = 3
	logical,parameter :: do_monte_carlo = .true. ! for debugging
! Local Var's
	type save_param_struct
		character(STRING_LEN) :: name
		real :: val
		real,dimension(2) :: interval
	end type
	character(STRING_LEN) :: out_file_name
	integer :: i,i2,j
	integer :: ios,res2
	real,dimension(default%num_boot_iter,num_free_global_param) :: boot_res_arr
!	real,dimension(num_free_global_param) :: save_param_val
	real :: r_num_bins
	real :: alpha,mu,sigma
	integer :: bin
	integer,dimension(num_free_global_param,NUM_BINS) :: bin_arr
	type(save_param_struct),dimension(num_free_global_param) :: save_param
	real,dimension(num_free_global_param,NUM_BINS) :: bin_grid
	real :: val_min,val_max,x_l,x_l1,x_l2,x_r1,x_r2
	real :: val_range,bin_width
	type(expt_struct), pointer :: current_expt
	type(model_struct), pointer :: model_list,current_model
	type(data_struct), pointer :: current_data
	type(param_logic_struct),pointer :: param_logic_ptr
	integer :: lims1,lims2,lims3,num_data_points_used,len
	real :: tot_area
! Begin
	r_num_bins = real(NUM_BINS)
	param_list(map_from(:))%save_val=param_list(map_from(:))%val
	!need a copy of param_list%var and a copy of the calculated data w/o noise
	current_expt=>expt_list
	!loop through the experiments
	i=1 ! counter over experiments
	expt_loop: do 
		current_data=>data_list(i)
		call expt_fit(0,current_data,current_expt)
		current_data%temp_data=current_data%cdata
		if (.not. associated(current_expt%next)) exit expt_loop
		! no more experiments left
		! else keep going
		current_expt=>current_expt%next
		i=i+1
	enddo expt_loop

	ios=make_out_file_name(ans_file_name,'raw',out_file_name) 
	if (ios /= 0) call error_handler('error constructing output file name')
	open(unit = DATA_FILE,file=out_file_name, &
		status='unknown',iostat=ios)
	if (ios /= 0) call error_handler('Error opening file: ',out_file_name)

	if (do_monte_carlo) then

		do j=1,default%num_boot_iter
				current_expt=>expt_list
			!loop through the experiments adding noise to the data
			i=1 ! counter over experiments
			expt_loop2: do 
				current_data=>data_list(i)
				current_data%cdata=current_data%temp_data
				call add_noise_to_data(current_expt,current_data)
				if (.not. associated(current_expt%next)) exit expt_loop2
				! no more experiments left
				! else keep going
				current_expt=>current_expt%next
				i=i+1
			enddo expt_loop2
			! restore the original param values
			param_list(map_from(:))%val=param_list(map_from(:))%save_val
			res2=minimize()
			if (res2 > 0) then ! minimize returned with a reduced num of global free param
				res=2
				return
			endif
			boot_res_arr(j,:)=param_list(map_from(:))%val
			write(DATA_FILE,*)boot_res_arr(j,:)
		enddo ! j
		close(DATA_FILE)
else
		do i=1,default%num_boot_iter
			read(DATA_FILE,*)boot_res_arr(i,:)
		enddo !i
		close(DATA_FILE)
	! now bin the data in boot_res_arr
	bin_arr(:,:)=0

	do j=1,num_free_global_param
		! determine bin limits and width
		! use EPS to give us a little breathing room
		val_min=minval(boot_res_arr(:,j))-EPS
		val_max=maxval(boot_res_arr(:,j))+EPS
		val_range=val_max-val_min
		bin_width=val_range/r_num_bins
		do i=1,NUM_BINS
			bin_grid(j,i)=bin_width*(real(i)-0.5)
		enddo
		do i=1,default%num_boot_iter
		! calculate bin# for this point
			bin=ceiling( r_num_bins/val_range*(boot_res_arr(i,j)-val_min) )
			bin_arr(j,bin)=bin_arr(j,bin)+1
		enddo !i
		! Ok, got our binns.  Now Fit the binned data to a sum of gaussians
		! To do the fit we need a whole new set of logic.
		! We need to save off the names and values of the free global params.
		save_param(:)%name=param_list(map_from(:))%name
		save_param(:)%val=param_list(map_from(:))%val
		! Everything else we can get rid of.
		call destructor_logic()
		call destructor_general()
! effectively implement a readans
	
! these are deallocated in destructor_general
		num_total_global_param=3*num_gaussians

		allocate(param_list(num_total_global_param))
		allocate(fixed(num_total_global_param)) 
		allocate(map_to(num_total_global_param))		

	! these names don't get used; they are just there for documentation.
		param_list(1)%name='alpha1'
		param_list(2)%name='alpha2'	
		param_list(3)%name='alpha3'	
		param_list(4)%name='mu1'
		param_list(5)%name='mu2'
		param_list(6)%name='mu3'		
		param_list(7)%name='sigma1'
		param_list(8)%name='sigma2'
		param_list(9)%name='sigma3'

		alpha=default%num_boot_iter/6.
		mu=save_param(j)%val
		sigma=sqrt( dot_product(bin_grid(j,:),(bin_arr(j,:)-save_param(j)%val)**2 )/ &
			default%num_boot_iter)

	! define our preliminary estimates
		param_list(1)%val=alpha*4. ! = 2/3 of the integrated intensity
		param_list(2)%val=alpha ! 1/6 of integrated intensity
		param_list(3)%val=alpha ! 1/6 of integrated intensity
		param_list(4)%val=mu
		param_list(5)%val=mu-sigma
		param_list(6)%val=mu+sigma
		param_list(7)%val=sigma
		param_list(8)%val=sigma
		param_list(9)%val=sigma

		fixed(:)=.false.
		param_list(:)%min=0.


	! construct expt_list.
		allocate(expt_list)
		current_expt=>expt_list
		nullify(current_expt%next)
! *** work on this ... need go give a weight etc.
!model lexicon -> gaussian_wrapper
	
	! specify models
	! first gaussian
		allocate(model_list)
		current_model=>model_list
		nullify(current_model%next)
		current_expt%expt=>current_model
		allocate(param_logic_ptr)
		allocate(param_logic_ptr%param(3))
		allocate(param_logic_ptr%param(1)%param_basis(3))
		param_logic_ptr%param(1)%param_basis(:)=(/1,2,3/) !not num_gaussian friendly
		allocate(param_logic_ptr%param(2)%param_basis(3))
		param_logic_ptr%param(2)%param_basis(:)=(/4,5,6/)
		allocate(param_logic_ptr%param(3)%param_basis(3))
		param_logic_ptr%param(3)%param_basis(:)=(/7,8,9/)
		current_model%model=>param_logic_ptr	

	! done setting up expt_list.  Now finish setting up the logic
		call constructor_logic(fixed) 
		call constructor_general()

	! now write the data

		lims1=1
		lims2=i
		lims3=1
		num_data_points_used = lims2-lims3+1
		len=lims2-lims1+1
		ALLOCATE (data_list(1)%datum(len))
		ALLOCATE (data_list(1)%cdata(len))
		ALLOCATE (data_list(1)%cdata_foreward(len))
		ALLOCATE (data_list(1)%cdata_backward(len))
		ALLOCATE (data_list(1)%x_pts(len))
		ALLOCATE (data_list(1)%resid(num_data_points_used))
		ALLOCATE (data_list(1)%resid_low(num_data_points_used))
		ALLOCATE (data_list(1)%weight(num_data_points_used))
		data_list(1)%weight(:)=1.
		data_list(1)%x_pts(:)=bin_grid(j,:)
		data_list(1)%datum(:)=bin_arr(j,:)	
		data_list(1)%lims(1)=lims1
		data_list(1)%lims(2)=lims2
		data_list(1)%lims(3)=lims3
		data_list(1)%len=len
		data_list(1)%num_points=num_data_points_used
	
		write(*,*)'Calculating confidence limits for parameter: ',save_param(j)%name
		! now fit
		res2=minimize()
		if (res2 > 0) then ! minimize returned with a reduced num of global free param
			res=2
			return
		endif
		! the total area is just the sum of the alpha's
		! tot_area should not be far from default%num_boot_iter
		tot_area=0.
		do i=1,num_gaussians
			tot_area=tot_area+param_list(1+3*(i-1))%val
		enddo !i

		x_l1=mu-2.5*sigma ! give the brackets for x_l to be used in bisect
		x_l2=mu-0.25*sigma
		x_r1=mu+0.25*sigma ! give the brackets for x_r to be used in bisect
		x_r2=mu+2.5*sigma
		! calculate the parameter confidence limits
		x_l=bisect(wacko_func,current_model,(/x_r1,x_r2,tot_area/),x_l1,x_l2)
		!x_r was calculated in wacko_func
		save_param(j)%interval=(/x_l,x_r/)

	enddo !j
endif !do_monte_carlo	
!	close(DATA_FILE)

end function