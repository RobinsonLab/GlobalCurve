function steepest_descent() result(res)
! this function minimizes the global_reduced_chi_sqr scalar given the expermint
! description in expt_list.  If a param keep going out of bounds then the function
! returns with num_bad_var set to the free_global_param number otherwise it returns a zero.

	use h_params ! default
	use h_vars  
	use h_struct
	use h_routines
	use h_utils
	use numerical_libraries ! IMSL numerical routines
	implicit none

	integer :: res !the pathologic free global param # (0 if everything is fine) 

! eliminated reduced l-s param vector dynamic size reduction. jmr 4-2-00

! Global Var's
	! h_vars,intent(in)
	!	integer,intent(in) :: num_free_global_param 
	!	integer,intent(in) :: num_points
	!	integer,dimension(num_free_global_param),intent(in) :: map_from
	!	integer,dimension(num_total_global_param),intent(in) :: map_to
	!	type (data_struct),pointer, dimension(:) :: data_list
	!	type(expt_struct), pointer :: expt_list
	!	type(param_struct),pointer,dimension(:) :: param_list
	!	integer :: global_DOF
	!	integer :: num_expt


	! h_vars,intent(out)
	!	real,intent(out) :: global_reduced_chi_sqr
	!	real,dimension(num_expt),intent(out) :: local_reduced_chi_sqr 
	!	integer,intent(out) :: num_iter
	!	real(8),dimension(num_free_global_param,num_free_global_param) :: curvature,ginva
	!	real(8),dimension(num_free_global_param) :: gradient,delta,diag
	!	real,dimension(num_free_global_param) :: correct

! Local Params ?????
	real,parameter :: FD_MIN = 1.1

! Local vars
	type(expt_struct), pointer :: current_expt
	type (data_struct),pointer :: current_data
	real(8),allocatable,dimension(:,:):: deriv_data ! size changes according to data len
	real :: flambda
	real :: chi_sqr_change
!	real :: div_scale
	real :: chi_sqr_low
!	real :: flambda_div
!	logical :: special,old,old2
	integer :: irank
	integer :: i,j,k
	integer,dimension(num_free_global_param) :: out_of_bounds
	real,dimension(num_free_global_param) :: param_val_low

! Begin
	num_expt=size(data_list)
!	out_of_bounds(:)=0
!	reduce = .false.
!	temp_fix(:) = .false.
!	div_scale=2.0
!	flambda_div=default%flambda_div
!	old=.true.
!	old2=.false.
	! see below for a discussion of old and old2
!	special = .false.

	flambda=default%flambda
	num_iter=0

!	global_DOF=num_points-w_num_free_global_param-1
	! before we do anything we need to make sure that all parameters are within range
	! if not then give them a nudge just above the minimum value
	
	do i=1,num_free_global_param
		if (param_list(map_from(i))%val<=param_list(map_from(i))%min*(1.0+default%eps) ) then
			write(*,*)'marquadt called with param ',trim(param_list(map_from(i))%name), &
				' out of bounds'
			write(*,*)'setting param to its minimum allowable value'
			param_list(map_from(i))%val=param_list(map_from(i))%min*(1.0+10*default%eps)
		endif
	enddo !i

! call function to calculate residuals and chi sqr for referencing
	call calc_chi_sqr() !find the starting minimum chi_sqr value
	chi_sqr_low = global_reduced_chi_sqr
!	chi_sqr,loc_chi_sqr,global_DOF,data_list, &
!		expt_list,param_list)
	! the chi_sqr and the params are the reference (lowest)

	! save off starting values
	param_val_low(:)=param_list(map_from(:))%val
!	param_list(map_from(:))%save_val=param_list(map_from(:))%val 
!	global_reduced_chi_sqr = chi_sqr 



!	almost_there = .false.

	write(*,999) num_iter,global_reduced_chi_sqr


ls_loop: do ! least squares loop

! new iteration over all experiments to minimize chi sqr.

	call calc_curvature_gradient()
	!curvature,gradient,w_num_free_global_param, &
	!	data_list,expt_list,param_list,w_map_to)
	
	forall( j=1:num_free_global_param )
		diag(j)=curvature(j,j)
	end forall

! due to the way the curvature matrix is calculated, if a diagonal element
! is zero then the whole row will be zero; ergo the curvature matrix is singular
! This means that there is no functional dependence on a particular parameter.
! We need to knock the parameter out and recalculate.
! The most likely reason for a diagonal element being zero is that its corresponding
! parameter is also zero.

	do i=1,num_free_global_param
		if (diag(i) == 0.) then
			j=map_from(i)
			write(*,*)'Curvature matrix is singular for global variable: ',j
			res=j
			return
		endif
	enddo

! normalize the curvature matrix and add flambda across the diagonal

	forall( j=1:num_free_global_param )
		gradient(j)=gradient(j)/sqrt( diag(j) )
		forall( k=j+1:num_free_global_param)
			curvature(j,k)=curvature(j,k)/sqrt( diag(j)	* diag(k) )
		end forall
		curvature(j,j)=1.0+flambda
	end forall	
	! fill in the lower triangular part of the curvature matrix
	forall(j=2:num_free_global_param)
		forall(k=1:j-1)
			curvature(j,k)=curvature(k,j)
		end forall !k
	end forall !j
!	call dlfcds(w_num_free_global_param,curvature, &
!		w_num_free_global_param,cholesky,w_num_free_global_param,cond)
	! output: cond = 1/condidion number of curvature and cholesky is 
	! the upper triangular matrix R of the cholesky factorization of
	! curvature A=R^T*R

	! if the condition number gets too big (1/cond) then don't bother
	! computing it because it is only going to get bigger.  Instead
	! use iterative refinement in the back substitution solver

	! solve the system using iterative refinement
	! everything is in double precision using IMSL routines

!	call dlfids(w_num_free_global_param,curvature, &
!		w_num_free_global_param,cholesky,w_num_free_global_param, &
!		gradient,delta,res)
!	call dl2ads(w_num_free_global_param,curvature,w_num_free_global_param,&
!	gradient,delta,cholesky,vec)

	call dlsgrr(num_free_global_param,num_free_global_param,&
		curvature,num_free_global_param,default%svd_tol,irank,ginva,&
		num_free_global_param)
	delta=matmul(ginva,gradient)
!	diffn=delta-delta2
	correct(:)=real(delta(:))/sqrt(diag(:))
	param_list(map_from(:))%val=param_list(map_from(:))%val+correct(:)
! test to see if updated param val's are within the allowed range
!write(*,*)param_list(map_from(1))%name,param_list(map_from(1))%val
!write(*,*)param_list(map_from(2))%name,param_list(map_from(2))%val	
!if (num_iter == 63) then
!	pause
!endif

	do i=1,num_free_global_param
		if (param_list(map_from(i))%val<=param_list(map_from(i))%min+default%eps ) then
			! for now you get no warnings.  If you are less than the min then that's it.
			! you get set to min and no longer count in the chi sqr minimization.
			! A gentler approach would be to give one warning then push the parameter out;
			! then if the param goes out of bounds again then fix it to zero and carry on
			! without the parameter.
			
			write(*,*)'Minimum exceeded for param: ',trim(param_list(map_from(i))%name), &
				param_list(map_from(i))%val
			out_of_bounds(i)=out_of_bounds(i)+1
			if (out_of_bounds(i)==MAX_OUT_OF_BOUNDS) then
				!x strikes and you're out.

!				write(*,*)'Recalling chi sqr minimization with reduced free params'
!				write(*,*)'Fixing param value at: ',param_list(map_from(i))%min
!				param_list(map_from(i))%val=param_list(map_from(i))%min
!				param_list(map_from(i))%save_val=param_list(map_from(i))%min
!				reduce = .true.
!				temp_fix(i)=.true.
				res = i
				return
			else
				! restore the resid from resid_low
				! resids are used in the calc_curvature_gradient subroutine

				do j=1,num_expt !why ????
					data_list(j)%resid(:)=data_list(j)%resid_low(:)
				enddo !j
				flambda=flambda*1000. ! don't weight the curvature so much.
				param_list(map_from(:))%val=param_val_low(:)
				cycle ls_loop
			endif
		endif
	enddo

	call calc_chi_sqr()
	chi_sqr_change=global_reduced_chi_sqr - chi_sqr_low
	num_iter=num_iter+1
	if (default%verbose) then
		do j=1,num_expt
			write(*,998) j,local_reduced_chi_sqr(j)
		end do !j
	endif 
	write(*,999) num_iter,global_reduced_chi_sqr,flambda,chi_sqr_change !,flambda_div,special,div_scale

! one thing that can happen is that flambda can oscillate between two values: one giving 
! a minus chi_sqr and the other giving a positive.  What happens is that the program
! is too aggressive in attempting to reduce flambda.  What we need is some kind of memory
! consider this scenario
! iteration n gives chi_sqr_change < 0
! so flambda is decreased
! iteration n+1, chi_sqr_change > 0
! so flambda is increased
! iteration n+2 gives chi_sqr_change < 0
! decrease flambda
! ... waste a loop by increasing flambda which is going to give us a chi_sqr_change > 0
! instead be smart and don't decrease flambda because that's what we did -2 iteration ago.
! so if n-2 was a decrease and n-1 was an increase then dont do a decrease now.  Istead 
! leave flambda alone.
! let's let a decrease = .true. and an increase = .false.
! old is the most recent result
! old2 is the result twice removed



	if (chi_sqr_change < 0.) then
		!good, store off the results
		param_val_low(:)=param_list(map_from(:))%val
		chi_sqr_low=global_reduced_chi_sqr
		flambda=flambda/default%flambda_div
!		if (special) then ! then !
!			! if div_scale is 2 then we are interval halfing.
!			! the interval halfing worked
!			! reset special
!			special=.false.
!			div_scale=2.0
!			if (flambda > MIN_FLAMBDA) flambda = flambda / flambda_div
!		else
!!			if ( old2 .and. (.not. old) ) then ! got here if have neg,pos,neg (neg now)
!!				special = .true.
!!				flambda_div = FD_MIN + (flambda_div-FD_MIN)/div_scale
!!				flambda = flambda / flambda_div
!!			else
!				if (flambda > MIN_FLAMBDA) flambda = flambda / flambda_div
!			endif
!		endif
		do j=1,num_expt
			data_list(j)%resid_low(:)=data_list(j)%resid(:)
		enddo !j
!		old2=old
!		old=.true.
	else

!		! *** quick and dirty. Fix later
!!		call calc_chi_sqr(chi_sqr,loc_chi_sqr,global_DOF,data_list, &
!!			expt_list,param_list)
!		! restore flambda_div
!		if (special) then
!			! restore flambda
!			flambda=flambda*flambda_div 
!			! restore flambda_div
!			flambda_div=div_scale*(flambda_div-FD_MIN)+FD_MIN
!			! now this time don't step so large.
!			div_scale=div_scale*2
!			if (div_scale > 100000) then
!				div_scale=100000
!				write(*,*)'div_scale > 1000'
!			endif
!			flambda_div = FD_MIN + (flambda_div-FD_MIN)/div_scale
!		else
			flambda=flambda*default%flambda_div
!		endif
		! need to recalculate the curvature at the param_val_min point with a new flambda
		param_list(map_from(:))%val=param_val_low(:)
		! restore the residuals also
		do j=1,num_expt
			data_list(j)%resid(:)=data_list(j)%resid_low(:)
		enddo !j


!		! next time don't try to reduce flambda_div by so much

!		old2=old
!		old=.false.
	endif
		


! conditions for exiting ls_loop
	if (flambda>MAX_FLAMBDA) exit ls_loop			
		! chi_sqr_min is the minimum.  Increasing flambda is not helping
	if ( (abs(chi_sqr_change) < default%chi_sqr_tol) ) exit ls_loop
		! yippie we're done
	if (num_iter == default%maxit) then
		write (*,*) 'maximum number of iterations exceeded'
		exit ls_loop
	endif



enddo ls_loop

! save the chi_sqr_minimum and the param_values at that point
!param_list(map_from(:))%save_val=param_list(map_from(:))%val
!global_reduced_chi_sqr=chi_sqr_low
write(*,997) chi_sqr_low,num_iter
res=0
return

997 format ('Output: Global Chi Sqr = ',F11.5,' Iterations = ',I3)
998 format ('Local Chi Sqr (',I3,') = ',F11.4)
999 format (' Iter:',I3,' Global Chi Sqr = ',F13.5,' Lambda = ',ES10.1,' Change = ',F14.8,F12.8,L,ES7.1)
end function 