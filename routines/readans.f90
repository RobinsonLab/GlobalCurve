! problem here in couning number of fitting params.  What if we wanted it so that all
! the alpha's sum to one.  Here we introduce a constraint on alpha's using a virtual
! data set.  The constraint models don't actually know that they are special because they
! utilize data sets just like any other model.

! A word about virtual data sets.  Since the data is virtual, it shouldn't enter into the
! chi^2 calculations. Furthermore the model's class is simple (=0) which means no post-
! processing is necessary such as a convolution.

! Now we want the sum of alpha's to equal one.  We introduce another parameter (call it 
! sum_alpha) which is fixed to a value of one?  This way the number of free param's
! doesn't change.

function readans() result(res)

	use h_params
	use h_struct
	use h_utils
	use h_routines
	use h_vars
!	use h_w_vars
	implicit none
	integer :: res

! This routine sets the following global variables

! Global Variables

	! h_vars,intent(in)
	!	character(STRING_LEN),intent(in) :: ans_file_name

	! h_struct,intent(out)
	!	type(expt_struct), pointer :: expt_list
	!	type(param_struct),pointer,dimension(:) :: param_list
	!	type (data_struct),pointer, dimension(:) :: data_list
	!	type(rig_logic_struct),pointer :: rig_list

	! h_vars,intent(out)
	!	integer :: num_tot_global_param
	!	integer :: num_free_global_param
	!	integer :: tot_num_data_points
	!	integer :: num_expt
	!	integer,pointer,dimension(current_expt%num_local_free_param) :: local_DOF
	!	integer :: global_DOF
	!	integer,pointer,dimension(:) :: map_from
	!	integer,dimension(:),pointer :: map_to
	!	logical,dimension(:),pointer :: fixed_vec
	!	integer :: error_analysis_type
	!	logical :: skip_minimization



! Local Variables
	
	type(expt_struct), pointer :: current_expt
	type(model_struct), pointer :: model_list,current_model
	type(param_logic_struct),pointer :: param_logic_ptr
	type(rig_logic_struct),pointer :: current_rig
	type(param_struct),dimension(PARAM_MAX) :: temp_param_list

	character(STRING_LEN) :: str,name,lamp_file_name,file_name,line

	integer :: i,j,k,l,m,n,num,ios,num_local_param,int
	integer :: num_data_points_used,tot_num_data_points_used

	integer :: number ! data_length
	integer :: lims1,lims2,lims3,len
	integer :: nz
	integer :: num_rig_param
	integer :: ldf
	integer :: data_length

	integer,dimension(PARAM_MAX) :: temp_local_param,temp_map_from
	integer,dimension(PARAM_MAX,PARAM_MAX) :: param_temp
	integer,dimension(PARAM_MAX) :: param_len ! max 10 param vectors per model
	! changed to PARAM_MAX 8/2/00 to accomodate blast.ans
	integer,dimension(MAX_RIG_DIM) :: rig_param_loc

	logical :: temp_fixed !rig

	real :: tcal,dat1,dat2,dat3,dat4
	real :: val,min,max
	real :: fwhm

	real,dimension(CHAN_MAX) :: lamp,datum,sigma,x_pts
!	integer,dimension(CHAN_MAX) :: ptr
	integer,dimension(CHAN_MAX) :: temp_datum_ptr
	real,dimension(MAX_RIG_DIM,2) :: rig_param_lim

! these vars are defined in the experiment type struct
	real :: scale ! a weighting factor for the experiment used in global analysis
	integer :: class ! identifies the class this expt is in - used in expt_fit
				! all models for a given experiment must be of the same class
	character(3) :: expt_type
	character(STRING_LEN) :: x_lab,y_lab


! Here's how things work.  We don't know how many experiments there
! will be.  So we could allocate an array of experiments; instead
! we're going to organize experiments as a linked list because we
! don't need "random access" to the list.  
!	Now each experiment is
! comprised of one or more models.  Like photons might be comming
! and going from and to all sorts of compartments.  A single model 
! can't describe all this behavior so we need to specify a set of
! models to descrive commings and goings.  The set of models in an
! experiment is also implemented as a linked list.
!	Each model in our list must contain some information. (1)  The
! program needs to know which subroutine to call to invoke the model
! Ideally, this would be implemented as a pointer to a function or
! subroutine.  Compac f95 allows one to do this, but it won't let
! you specify a funciton pointer as part of a devived type.  So we
! will do the next best thing.  Define an integer(4) to be used as
! the memory address of the funciton we are pointing to.  Lastly, we
! need to pass the parameter values to the subroutine that is now
! "pointed to".  We need a general way of doing things.  Some models
! require complicated parameter inputs such as assumed shape arrays.
! The way we do this is to have an array of vectors.
! For example: model diffuse accepts an {alpha_i},{tau_i}, and
! {R0,mu,sigma}.  So if there were 2 lifetimes then the logic "matrix"
! would consist of {{a1,a2},{tau1,tau2},{R0,mu,sigma}} of dim ({2},{2},{3}}.
! Why use the term logic matrix?  Because we have all the parameters
! which characterize our biological system arranged in a given order.
! We want to say that our model uses tau1 which is at position x in our
! parameter list.  So the driver for the subroutine must grab the value
! or the param pointed to in the logic "matrix" and send it to the 
! subroutine.

! Each model has its own input needs and the 
! front end needs to be aware of this.  So for future consideration
! each model needs to have its own "header" file that can be read
! by the front end and implemented in the user interface.  In other
! words the front end is responsible for getting the logic right.

! Begin

!******************************************************************************
!					read in the global parameters
!******************************************************************************

open(unit = ANSWER_FILE,file=ans_file_name,status='old',iostat=ios)
	if (ios /= 0) call error_handler('Error opening answer file: ',ans_file_name)


i=0 ! global param counter
j=0 ! global free param counter
loop1: do
	loop2: do
		read(ANSWER_FILE,10)str
		if (index(str,'%') == 0) exit loop2 ! Not a comment
	enddo loop2
	if (index(str,'# end') > 0) exit loop1	
	! else
	i=i+1
	read(str,*) num,name,temp_fixed,val,min,max
	temp_param_list(i)%val=val
	temp_param_list(i)%name=name
	temp_param_list(i)%min=min
	temp_param_list(i)%max=max
	temp_param_list(i)%fixed=temp_fixed
	if (temp_fixed == .false.) j=j+1
!		temp_map_from(j)=i
enddo loop1

loop3: do
	loop4: do
		read(ANSWER_FILE,10)str
		if (index(str,'%') == 0) exit loop4 ! Not a comment
	enddo loop4
	if (index(str,'# end') > 0) exit loop3	
	! else
	i=i+1
	read(str,*) num,name,temp_fixed,val,min,max
	temp_param_list(i)%val=val ! if (i != val) then something's wrong
	temp_param_list(i)%name=name
	temp_param_list(i)%min=min
	temp_param_list(i)%max=max
	temp_param_list(i)%fixed=temp_fixed
	if (temp_fixed == .false.) j=j+1
!		temp_map_from(j)=i
enddo loop3

! done reading in the parameters. Store them
num_total_global_param=i
num_free_global_param=j
!num_fixed_global_param=i-j

! these are deallocated in destructor_general
allocate(param_list(num_total_global_param))
allocate(fixed(num_total_global_param)) 
allocate(map_to(num_total_global_param))

forall (i=1:num_total_global_param)
	param_list(i)%val=temp_param_list(i)%val
	param_list(i)%min=temp_param_list(i)%min
	param_list(i)%max=temp_param_list(i)%max
	param_list(i)%name=temp_param_list(i)%name
!	param_list(i)%fixed=temp_param_list(i)%fixed
	fixed(i)=temp_param_list(i)%fixed
end forall !do !i


!******************************************************************************
!							read in the logic
!******************************************************************************

i=0 !counter for num_expt
iloop: do ! loop over expts
	
	loop5: do
		read(ANSWER_FILE,10)str
		if (index(str,'%') == 0) exit loop5! Not a comment
	enddo loop5
	if (index(str,'# end') > 0) exit iloop ! b/c we're done reading
							! in experiments
	! else
!	file_name=str
!	num_local_param=0
	if (i == 0) then ! this is the first expt.
		allocate(expt_list)
		current_expt=>expt_list
		nullify(current_expt%next)
		nullify(current_expt%local_map_from)
	else
		allocate(current_expt%next)
		current_expt=>current_expt%next
		nullify(current_expt%next)
		nullify(current_expt%local_map_from)
	endif
! just save the file name for now.  We'll read in the data later
	current_expt%file_name=trim_string(str)
	i=i+1
	loop13: do
		read(ANSWER_FILE,10)str
		if (index(str,'%') == 0) exit loop13
	enddo loop13
 	read(str,*)scale,expt_type,x_lab,y_lab
	current_expt%scale=scale ! this is a scale factor which weights the local experiment
								! in the global chi^2 calculation (it is a Lagrange multiplier)
	current_expt%expt_type=expt_type
	call expt_type_parser(current_expt)
	current_expt%x_lab=trim_string(x_lab)
	current_expt%y_lab=trim_string(y_lab)





	j=0
	num_local_param=0
	jloop: do ! loop over models
		loop6: do
			read(ANSWER_FILE,10)str
			if (index(str,'%') == 0) exit loop6 ! Not a comment
		enddo loop6
		if (index(str,'# end') > 0) exit jloop ! b/c we're done reading
								! in models for this experiment
		! else
!		model_name=str
		if (j == 0) then ! no models for this expt yet
			allocate(model_list)
			current_model=>model_list
			nullify(current_model%next)
			current_expt%expt=>current_model
		else
			allocate(current_model%next)
			current_model=>current_model%next
			nullify(current_model%next)
		endif
		j=j+1
		! define pseudo-pointer to model subroutine
		
		call model_lexicon(str,current_model%model_addr)
		k=0
		kloop: do ! loop over param vectors
			loop7: do
				read(ANSWER_FILE,10) str
				if (index(str,'%') == 0) exit loop7 ! Not a comment
			enddo loop7
			if (index(str,'# end') > 0) exit kloop
			k=k+1
			l=0
			lloop: do ! loop over basis vectors
				if (index(str,'# end') > 0) exit lloop
				! else
				read(str,*) num
				num_local_param=num_local_param+1
				temp_local_param(num_local_param)=num
				l=l+1
				param_temp(k,l)=num
				loop8: do
					read(ANSWER_FILE,10)str
					if (index(str,'%') == 0) exit loop8 ! Not a comment
				enddo loop8
			enddo lloop
			param_len(k)=l
		enddo kloop ! done reading param vectors for model j
	
		allocate(param_logic_ptr)
		allocate(param_logic_ptr%param(k))
! don't know if the compiler is going to like this
		do m=1,k
			allocate(param_logic_ptr%param(m)%param_basis(param_len(m)))
			param_logic_ptr%param(m)%param_basis(:)=param_temp(m,1:param_len(m))
		enddo !m
		current_model%model=>param_logic_ptr
		! read in scale factors for that model i.e. bounds
		! it is conceivable that there are none but for now
		! just assume there is one.
		loop10: do
			read(ANSWER_FILE,10)str
			if (index(str,'%') == 0) exit loop10 ! Not a comment
		enddo loop10
		read(str,*) num ! such as bounds
		current_model%model_param=num
		num_local_param=num_local_param+1
		temp_local_param(num_local_param)=num
	enddo jloop !loop over models
	loop9: do
		read(ANSWER_FILE,10)str
		if (index(str,'%') == 0) exit loop9 ! Not a comment
	enddo loop9
	read(str,*) num !such as qshift
	current_expt%expt_param=num
	num_local_param=num_local_param+1
	temp_local_param(num_local_param)=num

	allocate(current_expt%local_param(num_local_param))
	current_expt%local_param(:)=temp_local_param(1:num_local_param)
	current_expt%num_local_param=num_local_param

enddo iloop !loop over experiments
num_expt=i
! set up the num_local_free_param and the local_map_from variables



call constructor_logic(fixed) 
call constructor_general()


!******************************************************************************
!					read in the logic for rigorous analysis
!******************************************************************************

	do
		read(ANSWER_FILE,10)str
		if (index(str,'%') == 0) exit ! Not a comment
	enddo
	if (index(str,'none') .ne. 0) then ! none
		error_analysis_type = 0
	elseif ((index(str,'grid') .ne. 0) .or. (index(str,'support') .ne. 0)) then ! grid_search
		error_analysis_type = 1
	elseif ((index(str,'boot') .ne. 0) .or. (index(str,'monte') .ne. 0)) then ! bootstrap
		error_analysis_type = 2
	else
		write(*,*)'Error in error analysis type specification'
		res=1
		return
	endif

	read(ANSWER_FILE,*)skip_minimization

if (error_analysis_type==1) then ! grid_search
	rig_loop_1: do
		loop12: do
			read(ANSWER_FILE,10)str
			if (index(str,'%') == 0) exit loop12 ! Not a comment
		enddo loop12
		if (index(str,'# end') > 0) exit rig_loop_1
		! else
		num_rig_param=0
		if (.not. associated(rig_list)) then ! this is the first rig
			allocate(rig_list)
			current_rig=>rig_list
			nullify(current_rig%next)
		else
			allocate(current_rig%next)
			current_rig=>current_rig%next
			nullify(current_rig%next)
		endif
		rig_loop_2: do
			num_rig_param=num_rig_param+1
			number = nitems(str)
			if (number == 1) then
				read(str,*) num
			else
				read(str,*) num,dat1,dat2
				rig_param_lim(num_rig_param,1)=dat1
				rig_param_lim(num_rig_param,2)=dat2
			endif
			rig_param_loc(num_rig_param)=num
			loop11: do
				read(ANSWER_FILE,10)str
				if (index(str,'%') == 0) exit loop11 ! Not a comment
			enddo loop11
			if (index(str,'# end') > 0) exit rig_loop_2
		enddo rig_loop_2
		current_rig%num_rig_param=num_rig_param
		allocate(current_rig%param_loc(num_rig_param))
		current_rig%param_loc(:)=rig_param_loc(1:num_rig_param)
		allocate(current_rig%lim(num_rig_param,2))
		if (number == 3) then
			current_rig%lim(:,:)=rig_param_lim(1:num_rig_param,:)
		endif
	enddo rig_loop_1
endif ! error_analysis_type=1

! if bootstrap then by default calculate the 1 D error limits on all free var's

close(unit = ANSWER_FILE,iostat=ios)

!******************************************************************************
!					read in the data files
!******************************************************************************

allocate(data_list(num_expt))
tot_num_data_points=0
current_expt=>expt_list
n=1

do 
! test the current_expt%data_format and call appropriate routine
select case(current_expt%data_type)
case (1) ! fluorescence lifetime
	if(default%simul)then
		write(*,*)'Enter simulation parameters for expt # ',n
!		do
!			write(*,*)'Enter lamp file name for simulation: '
!			read(*,'(a30)')lamp_file_name
!			open(unit = DATA_FILE,file=lamp_file_name, &
!				status='old',iostat=ios)
!			if (ios == 0) then
!				exit ! successful
!			else
!				write(*,*) 'Try again // '
!			end if
!		end do
!		i=0
!		do
!			read(DATA_FILE,*,iostat=ios) dat1
!			if (ios == -1) exit !end of file found
!			i=i+1
!			lamp(i)=dat1
!			datum(i)=0.
!		end do 
!		close(DATA_FILE,iostat=ios)
		write(*,*)'Enter tcal, lims1, lims2, lims3: '	
		read(*,*)tcal,lims1,lims2,lims3	
		write(*,*)'Enter fwhm of the lamp function(in units of tcal): '
		read(*,*)fwhm
!		len=lims2-lims1+1
		call generate_lamp(tcal,fwhm,lims1,lamp)
		datum(:)=0.

	else ! simul == .false.

		open(unit = DATA_FILE,file = current_expt%file_name, &
			status = 'old',iostat=ios)
		if (ios /= 0) call error_handler('Error opening data file: ', &
			current_expt%file_name)
   		read(DATA_FILE,*)tcal,lims1,lims2,lims3

! figure out weather a weighting file is input (three numbers across in file)
! or use standard weighting (two numbers across in a file)

        read(DATA_FILE,'(A72)')line
        number = nitems(line)
        backspace(DATA_FILE)

		if(number.eq.3) then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2,dat3
				if (ios == -1) exit !end of file found
				i=i+1
				lamp(i)=dat1
				datum(i)=dat2
				sigma(i)=dat3
			end do
		elseif(number.eq.2)then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2
				if (ios == -1) exit !end of file found
				i=i+1
				lamp(i)=dat1
				datum(i)=dat2
				if (dat2 > EPS) then
					sigma(i)=sqrt(dat2) ! photon counting noise
				else
					sigma(i)=EPS
				endif
			end do
!			data_length=i
		else
			call error_handler('Error reading data from file: ',current_expt%file_name)
		end if
		close(DATA_FILE)
	end if ! simul

!	where (sigma(:) < EPS) sigma(:) = EPS
	num_data_points_used = lims2-lims3+1
	len=lims2-lims1+1
	ALLOCATE (data_list(n)%lamp(len))
	ALLOCATE (data_list(n)%s_lamp(len))
	ALLOCATE (data_list(n)%d_lamp(len))
	ALLOCATE (data_list(n)%datum(len))
	if (error_analysis_type == 2) 	ALLOCATE (data_list(n)%temp_data(len))
	ALLOCATE (data_list(n)%cdata(len))
	ALLOCATE (data_list(n)%cdata_foreward(len))
	ALLOCATE (data_list(n)%cdata_backward(len))
	ALLOCATE (data_list(n)%x_pts(len))
	ALLOCATE (data_list(n)%resid(num_data_points_used))
	ALLOCATE (data_list(n)%resid_low(num_data_points_used))
	ALLOCATE (data_list(n)%weight(num_data_points_used))
	lamp(lims1:lims2)=lamp(lims1:lims2)/maxval(lamp(lims1:lims2))
	data_list(n)%d_lamp=derivative(lamp(lims1:lims2)) ! used to q_shift the lamp
	data_list(n)%lamp(:)=lamp(lims1:lims2)
	data_list(n)%datum(:)=datum(lims1:lims2) 
	data_list(n)%x_pts=(/(j*tcal,j=1,len)/)
	data_list(n)%weight(:)=1/sigma(lims3:lims2)**2
	data_list(n)%tcal=tcal
	data_list(n)%lims(1)=lims1
	data_list(n)%lims(2)=lims2
	data_list(n)%lims(3)=lims3
	data_list(n)%len=len
	data_list(n)%num_points=num_data_points_used
	! calculate nz to be used in convolution
	nz=2**ceiling(log(real(2*len-1))/log(2.))
	data_list(n)%nz=nz
	allocate(data_list(n)%z(nz))
	allocate(data_list(n)%zhat(nz))
	allocate(data_list(n)%xwk(nz))
	allocate(data_list(n)%ywk(nz))
	allocate(data_list(n)%wk(2*nz+15)) ! as reqired by imsl rconv

case (2) ! kinetics

	if(default%simul)then

		write(*,*)'Enter number of channels, time per channel: '
		read(*,*)lims2,tcal
		lamp=(/(j*tcal,j=1,lims2)/)

	else ! simul == .false.

		open(unit = DATA_FILE,file = current_expt%file_name, &
			status = 'old',iostat=ios)
		if (ios /= 0) call error_handler('Error opening data file: ', &
			current_expt%file_name)
!   		read(DATA_FILE,*)tcal,lims1,lims2,lims3

        read(DATA_FILE,'(A72)')line
        number = nitems(line)
        backspace(DATA_FILE)

		if(number.eq.3) then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2,dat3
				if (ios == -1) exit !end of file found
				i=i+1
				x_pts(i)=dat1
				datum(i)=dat2
!				sigma(i)=dat3
			end do
		elseif(number.eq.2)then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2
				if (ios == -1) exit !end of file found
				i=i+1
				x_pts(i)=dat1
				datum(i)=dat2
				sigma(i)=1.0 ! unweighted non-linear least squares
			end do
!			data_length=i
		else
			call error_handler('Error reading data from file: ',current_expt%file_name)
		end if
		close(DATA_FILE)
	end if ! simul
! for now, use all the data points

	sigma(1:i)=0.005*maxval(datum(1:i)) ! normalize to peak
	lims1=1
	lims2=i
	lims3=1

!	where (sigma(:) < EPS) sigma(:) = EPS
	num_data_points_used = lims2-lims3+1
	len=lims2-lims1+1
!	ALLOCATE (data_list(n)%lamp(len))
	ALLOCATE (data_list(n)%datum(len))
	if (error_analysis_type == 2) 	ALLOCATE (data_list(n)%temp_data(len))
	ALLOCATE (data_list(n)%cdata(len))
	ALLOCATE (data_list(n)%cdata_foreward(len))
	ALLOCATE (data_list(n)%cdata_backward(len))
	ALLOCATE (data_list(n)%x_pts(len))
	ALLOCATE (data_list(n)%resid(num_data_points_used))
	ALLOCATE (data_list(n)%resid_low(num_data_points_used))
	ALLOCATE (data_list(n)%weight(num_data_points_used))
	data_list(n)%datum(:)=datum(lims1:lims2) 
	data_list(n)%x_pts=x_pts(lims1:lims2) 
	data_list(n)%weight(:)=1/sigma(lims3:lims2)**2
!	data_list(n)%tcal=tcal
	data_list(n)%lims(1)=lims1
	data_list(n)%lims(2)=lims2
	data_list(n)%lims(3)=lims3
	data_list(n)%len=len
	data_list(n)%num_points=num_data_points_used


case (3) ! binding

	if(default%simul)then
!		write(*,*)'Enter a file name for the independent data points: '
!		read(*,*)lamp_file_name
		open(unit = DATA_FILE,file = current_expt%file_name, &
			status = 'old',iostat=ios)
		if (ios /= 0) call error_handler('Error opening data file: ', &
			current_expt%file_name)
		i=0
        read(DATA_FILE,'(A72)')line ! comment line
        read(DATA_FILE,'(A72)')line ! comment line
		do 
			read(DATA_FILE,*,iostat=ios)dat1
			if (ios == -1) exit !end of file found
			i=i+1
			x_pts(i)=dat1
			datum(i)=0.0
			sigma(i)=1.0
		end do
		close(DATA_FILE)
		lims1=1
		lims2=i
		lims3=1
	else ! simul == .false.
		open(unit = DATA_FILE,file = current_expt%file_name, &
			status = 'old',iostat=ios)
		if (ios /= 0) call error_handler('Error opening data file: ', &
			current_expt%file_name)
!   		read(DATA_FILE,*)lims1,lims2,lims3

!! we're cheating here, undo the lims.. stuff
!lims1=1
!lims2=51
!lims3=1

!		read(DATA_FILE,'(A72)')line
! the second line contains lims information

        read(DATA_FILE,*,iostat=ios)lims1,lims2
		lims3=lims1

		read(DATA_FILE,'(A72)')line
        number = nitems(line)
        backspace(DATA_FILE)

		if(number.eq.3) then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2,dat3
				if (ios == -1) exit !end of file found
				i=i+1
				x_pts(i)=dat1
				datum(i)=dat2
				sigma(i)=dat3
			end do
		elseif(number.eq.2)then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2
				if (ios == -1) exit !end of file found
				i=i+1
				x_pts(i)=dat1
				datum(i)=dat2

!				if (dat2 > EPS) then
!					sigma(i)=sqrt(dat2) ! photon counting noise
!				else
!					sigma(i)=EPS
!				endif
			end do
			sigma(1:i)=0.005*maxval(datum(1:i)) ! normalize to peak
		else
			call error_handler('Error reading data from file: ',current_expt%file_name)
		end if
!		data_length=i
		close(DATA_FILE)
		if (i < lims2) lims2=i
	endif ! simul




!	lims1=1
!	lims2=i
!	lims3=1

!	where (sigma(:) < EPS) sigma(:) = EPS
	num_data_points_used = lims2-lims3+1
	len=lims2-lims1+1
!	ALLOCATE (data_list(n)%lamp(len))
	ALLOCATE (data_list(n)%datum(len))
	if (error_analysis_type == 2) 	ALLOCATE (data_list(n)%temp_data(len))
	ALLOCATE (data_list(n)%cdata(len))
	ALLOCATE (data_list(n)%cdata_foreward(len))
	ALLOCATE (data_list(n)%cdata_backward(len))
	ALLOCATE (data_list(n)%x_pts(len))
	ALLOCATE (data_list(n)%resid(num_data_points_used))
	ALLOCATE (data_list(n)%resid_low(num_data_points_used))
	ALLOCATE (data_list(n)%weight(num_data_points_used))
	data_list(n)%datum(:)=datum(lims1:lims2) 
	data_list(n)%x_pts=x_pts(lims1:lims2)
	data_list(n)%weight(:)=1/sigma(lims3:lims2)**2
!	data_list(n)%tcal=tcal
	data_list(n)%lims(1)=lims1
	data_list(n)%lims(2)=lims2
	data_list(n)%lims(3)=lims3
	data_list(n)%len=len
	data_list(n)%num_points=num_data_points_used

case (4) ! constraint
	open(unit = DATA_FILE,file = current_expt%file_name, &
		status = 'old',iostat=ios)
	if (ios /= 0) call error_handler('Error opening data file: ', &
		current_expt%file_name)
!   		read(DATA_FILE,*)tcal,lims1,lims2,lims3

!       read(DATA_FILE,'(A72)')line
!       number = nitems(line)
!       backspace(DATA_FILE)

!		if(number.eq.3) then    <-- number always equals 2 
		! some simple constraint models like sum_of_alphas may not use x_pts
	i=0
	do 
		read(DATA_FILE,*,iostat=ios)dat1,dat2
		if (ios == -1) exit !end of file found
		i=i+1
!		lamp(i)=dat1
		datum(i)=dat1
!		temp_datum_ptr(i)=int
		sigma(i)=dat2
		! a word here about why sigma is set to one.  The purpose of a constraint is to
		! constrain parameter values, not fit using them.  This is a very important 
		! distinction.  In the latter case one is performing a least squares analysis on 
		! the parameters from the first round.  What the program does is use a weighting
		! in the calculation of curvature and gradient.  When the weight is set to one, 
		! a fit is implimented.  When it is infinity then any discrepency between datum and
		! cdata is blown up in the the gradient vector.  The fitting tunction will attempt
		! to minimize the discrepancy at all costs because it is encurring a major penalty
		! for the residual not being zero.  In practice, one must settle for something
		! between the extremes because if the constraint component swamps out the actual 
		! data in terms of contributing to the gradient then the chi^2 change tolerance
		! will be reached prematurely.  

		! Start out with: To enforce a constraint, weight the constraint as a 10.
	end do
	close(DATA_FILE)

! for now, use all the data points

	lims1=1
	lims2=i
	lims3=1

!	where (sigma(:) < EPS) sigma(:) = EPS
	num_data_points_used = lims2-lims3+1
	len=lims2-lims1+1
!	ALLOCATE (data_list(n)%lamp(len))
	ALLOCATE (data_list(n)%datum(len))
	ALLOCATE (data_list(n)%datum_ptr(len)) ! a holdover from virtual data sets
	if (error_analysis_type == 2) 	ALLOCATE (data_list(n)%temp_data(len))
	ALLOCATE (data_list(n)%cdata(len))
	ALLOCATE (data_list(n)%cdata_foreward(len))
	ALLOCATE (data_list(n)%cdata_backward(len))
	ALLOCATE (data_list(n)%x_pts(len))
	ALLOCATE (data_list(n)%resid(num_data_points_used))
	ALLOCATE (data_list(n)%resid_low(num_data_points_used))
	ALLOCATE (data_list(n)%weight(num_data_points_used))
	data_list(n)%datum_ptr(:)=temp_datum_ptr(1:len)
	! don't bother filling up datum
	data_list(n)%datum=datum
	data_list(n)%weight(:)=1/sigma(lims3:lims2)**2
!	data_list(n)%tcal=tcal
	data_list(n)%lims(1)=lims1
	data_list(n)%lims(2)=lims2
	data_list(n)%lims(3)=lims3
	data_list(n)%len=len
	data_list(n)%num_points=num_data_points_used

case (5) ! input driven kinetics

		open(unit = DATA_FILE,file = current_expt%file_name, &
			status = 'old',iostat=ios)
		if (ios /= 0) call error_handler('Error opening data file: ', &
			current_expt%file_name)
        read(DATA_FILE,'(A72)')line
! the second line contains lims information
        read(DATA_FILE,*,iostat=ios)lims1,lims2
		lims3=lims1
	if(default%simul)then
		i=0
		do 
			read(DATA_FILE,*,iostat=ios)dat1,dat2
			if (ios == -1) exit !end of file found
			i=i+1
			x_pts(i)=dat1
			lamp(i)=dat2
!			datum(i)=dat2
			if (dat3 < EPS) then
				sigma(i) = EPS
			else
				sigma(i)=dat3
			endif
		end do
		close(DATA_FILE)
	else ! simul == .false.
		read(DATA_FILE,'(A72)')line
        number = nitems(line)
        backspace(DATA_FILE)
		if(number.eq.4) then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2,dat3,dat4
				if (ios == -1) exit !end of file found
				i=i+1
				x_pts(i)=dat1
				lamp(i)=dat2
				datum(i)=dat3
				if (dat4 < EPS) then
					sigma(i) = EPS
				else
					sigma(i)=dat4
				endif
			end do
		elseif(number.eq.3)then
			i=0
			do 
				read(DATA_FILE,*,iostat=ios)dat1,dat2,dat3
				if (ios == -1) exit !end of file found
				i=i+1
				x_pts(i)=dat1
				lamp(i)=dat2
				datum(i)=dat3
				sigma(i)=1.0 ! unweighted non-linear least squares
			end do
!			data_length=i
		else
			call error_handler('Error reading data from file: ',current_expt%file_name)
		end if
		close(DATA_FILE)
	endif ! simul
! for now, use all the data points

!	lims1=1
!	lims2=i
!	lims3=1

!	where (sigma(:) < EPS) sigma(:) = EPS
	num_data_points_used = lims2-lims3+1
	len=lims2-lims1+1
	ALLOCATE (data_list(n)%x_pts(len))
	ALLOCATE (data_list(n)%lamp(len))
	ALLOCATE (data_list(n)%datum(len))
	if (error_analysis_type == 2) 	ALLOCATE (data_list(n)%temp_data(len))
	ALLOCATE (data_list(n)%cdata(len))
	ALLOCATE (data_list(n)%cdata_foreward(len))
	ALLOCATE (data_list(n)%cdata_backward(len))
	ALLOCATE (data_list(n)%resid(num_data_points_used))
	ALLOCATE (data_list(n)%resid_low(num_data_points_used))
	ALLOCATE (data_list(n)%weight(num_data_points_used))
	data_list(n)%datum(:)=datum(lims1:lims2) 
	data_list(n)%x_pts=x_pts(lims1:lims2) 
	data_list(n)%lamp=lamp(lims1:lims2) 
	data_list(n)%weight(:)=1/sigma(lims3:lims2)**2
!	data_list(n)%tcal=tcal
	data_list(n)%lims(1)=lims1
	data_list(n)%lims(2)=lims2
	data_list(n)%lims(3)=lims3
	data_list(n)%len=len
	data_list(n)%num_points=num_data_points_used
end select

	ldf=num_data_points_used-current_expt%num_local_free_param
	if (ldf < 1) then
		if (current_expt%data_type .ne. 4) & ! then issue a warning
 			write(*,*) "Warning:: Local Number of degrees of freedom is less than 1!"
		local_DOF(n)=1
	else	
		local_DOF(n)=ldf
	endif
	tot_num_data_points=tot_num_data_points + num_data_points_used 
	if (.not. associated(current_expt%next)) exit	
	current_expt=>current_expt%next
	n=n+1
end do 

global_DOF=tot_num_data_points-num_free_global_param

if (global_DOF < 1) then
	write(*,*) "Warning:: Global Number of degrees of freedom is less than 1!"
	global_DOF=1
endif
res=0
return

10 format(<STRING_LEN>A) 

end function readans

