subroutine constructor_rigorous(current_rig)
	use h_vars
	use h_params
	implicit none

	type(rig_logic_struct),pointer :: current_rig

	! h_vars,intent(out)
	!	rig_arr_size
	!	type(rig_struct),dimension(:) :: rig_arr
	!	real,dimension(:) :: fixed_param_val !dim=current_rig%num_rig_param
	!	integer,allocatable,dimension(:) :: rig_index ! dim=current_rig%num_rig_param
	!	rig_arr
	!	free_param_temp

! Local Vars
	integer :: i
	integer :: dim

! Begin
	dim=current_rig%num_rig_param
	rig_arr_size=default%tot_grid_size**dim
	allocate(rig_arr(rig_arr_size))
	allocate(rig_index(dim))
	allocate(free_param_temp(num_free_global_param))
	allocate(fixed_param_val(current_rig%num_rig_param))
	do i=1,rig_arr_size
		allocate( rig_arr(i)%point(dim+1) ) ! {x_1 , .. x_n , y }
		allocate( rig_arr(i)%free_param(num_free_global_param) )
	enddo !i

end subroutine


subroutine destructor_rigorous()
	use h_vars
	implicit none

! Local Vars
	integer :: i

! Begin
	deallocate(rig_index)
	deallocate(free_param_temp)
	deallocate(fixed_param_val)
	do i=1,rig_arr_size
		deallocate(rig_arr(i)%point)
		deallocate(rig_arr(i)%free_param)
	enddo !i
	deallocate(rig_arr)
end subroutine destructor_rigorous

function rigorous(current_rig) result(res)
! this routine performs a 1,2,or 3 dimensional grid search in chi_sqr space
! the results are output in: $=base(ans_file_name) 
! $.tgt : tells where to draw the conficence line
! $.dmp : contents of arr
! $.rig : {x,chi_sqr} where {x} is the array of grid point locations
! $.scl : contents of arr scaled as follows x values are normalized to the param values
!			@ the chi_sqr global min. and chi_sqr is scaled from 0..inf and 1 is where chi_sqr
!			is equal to the conficence line.
	use h_params
	use h_vars
	use h_struct
!	use h_routines, only: minimize
	use h_rigorous
	implicit none

	type(rig_logic_struct),pointer :: current_rig
	integer :: res

	! h_vars,intent(in)
	!	type (data_struct),pointer, dimension(:) :: data_list
	!	type(expt_struct), pointer :: expt_list
	!	type(param_struct),pointer,dimension(:) :: param_list
	!	integer,dimension(max),intent(in) :: map_from
	!	integer,dimension(:),intent(in) :: map_to
	!	logical,dimension(:),intent(in) :: fixed
	!	real,intent(in) :: chi_sqr_min
	!	integer,intent(in) :: num_points

	! h_vars,intent(out)
	!	real,dimension(:),intent(out) :: loc_chi_sqr !dim over num of expts
	!	num_iter
	!	type(rig_struct),dimension(:) :: rig_arr
	!	real,dimension(:) :: fixed_param_val !dim=current_rig%num_rig_param
	!	integer,allocatable,dimension(:) :: rig_index ! dim=current_rig%num_rig_param
	!	arr
	!	real :: tgt
	!	posn
	!	free_param_temp

! Local Var's
	integer :: dim
	integer :: i,j,k,count
	integer :: pos0,pos1,pos2,pos3
!	real,dimension(default%tot_grid_size) :: points_1D
!	real,dimension(default%tot_grid_size,default%tot_grid_size) :: points_2D


! Begin
	dim=current_rig%num_rig_param
	tgt=f_conf(global_reduced_chi_sqr,num_free_global_param,global_DOF, &
		default%conf_lim) 
	! tgt is the target chi^2 value that is used as the level of significance.
	!	it is based on the F statistic.  f_conf is called with the param default%conf_lim
	!	which is the confidence level -- usually .67


check_dim: select case (dim)

case (1)
	! center
	i=0
	rig_index=(/i/)
	write(*,90)rig_index,pos((/i/))
	! if we are doing a manual grid search then the minimum may not lie on a grid point
	! so we need to calculate the chi^2 minimum for the grid point at i=0
	if (default%mgs) then
		res=crunch(current_rig,dont_use_free_param_temp=.true.)
	else 
		! no need to minimize b/c the middle of the conf lim is the chi_sqr_min
!		fixed_param_val=param_list( param_loc(:) )%save_val
		num_iter=0
		call arr_write(current_rig)
	endif 

	! make the +/- 1 box without derivatives
	! right
	i=1
	rig_index=(/i/)
	write(*,90)rig_index,pos((/i/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

	! left
	i=-1
	rig_index=(/i/)
	write(*,90)rig_index,pos((/i/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)


! use the derivatives of the grid points at the present surface
! to calculate the points at the next concentric surface.
	do count=2,default%grid_size
		! right
		i=count
		! use derivative data to predict the free_param values
		! (i)=2*(i-1) - (i-2)
		rig_index=(/i/)	
		write(*,90)rig_index,pos((/i/))
		pos1=pos( (/ (i-1) /) )
		pos2=pos( (/ (i-2) /) )
		free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
			rig_arr(pos2)%free_param(:) 
!		free_param_temp(:) = rig_arr(pos1)%free_param(:)
		res=crunch(current_rig)

		! left
		i=-count
		! (i)=(i+1) - (i+2)
		rig_index=(/i/)
		write(*,90)rig_index,pos((/i/))
		pos1=pos( (/ (i+1) /) )
		pos2=pos( (/ (i+2) /) )
		free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
			rig_arr(pos2)%free_param(:)
!		free_param_temp(:) = rig_arr(pos1)%free_param(:)
		res=crunch(current_rig)

	enddo !count

! for debugging
!	do i=1,default%tot_grid_size
!		points_1D(i)=rig_arr(i)%point(2)
!	enddo !i
	
	! output the results


call output_rig_res(current_rig)
	
!	pause

case (2)
	! center
	i=0
	j=0
	rig_index=(/i,j/)
	write(*,90)rig_index,pos((/i,j/))
	if (default%mgs) then
!		param_list(map_from(:))%val=param_list(map_from(:))%save_val
		! use the %val paramaters that have been provided by the user as estimates
		res=crunch(current_rig,dont_use_free_param_temp=.true.)
	else ! no need to minimize b/c the middle of the conf lim is the chi_sqr_min
		num_iter=0
		call arr_write(current_rig)
	endif 
!	param_list(map_from(:))%temp_val=param_list(map_from(:))%save_val	
! make the +/- 1 box without derivatives

	! right
	i=1
	j=0
	rig_index=(/i,j/)
	write(*,90)rig_index,pos((/i,j/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

	! down
	i=0
	j=-1
	rig_index=(/i,j/)
	write(*,90)rig_index,pos((/i,j/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

	! left
	i=-1
	j=0
	rig_index=(/i,j/)
	write(*,90)rig_index,pos((/i,j/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

	! up
	i=0
	j=1
	rig_index=(/i,j/)
	write(*,90)rig_index,pos((/i,j/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)
	! now calc the corners
	count=1	
	res=corners_2D(current_rig,count)

! use the derivatives of the grid points at the present surface
! to calculate the points at the next concentric surface.

	do count=2,default%grid_size
		! right
		write(*,*) 'Count = ',count
		i=count
		do j = -(i-1),(i-1) 
			! use derivative data to predict the free_param values
			! (i,j)=2*( (i-1),j ) - ( (i-2),j )
			rig_index=(/i,j/)
			write(*,90)rig_index,pos((/i,j/))
			pos1=pos( (/(i-1),j/) )
			pos2=pos( (/(i-2),j/) )
			free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
				rig_arr(pos2)%free_param(:)
			res=crunch(current_rig)
		enddo !j
		! down
		j=-count
		do i = (j+1),-(j+1)
			! (i,j)=2*( i,(j+1) ) - ( i,(j+2) )
			rig_index=(/i,j/)
			write(*,90)rig_index,pos((/i,j/))
			pos1=pos( (/ i,(j+1) /) )
			pos2=pos( (/ i,(j+2) /) )
			free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
				rig_arr(pos2)%free_param(:)
			res=crunch(current_rig)
		enddo !i
		! left
		i=-count
		do j = (i+1),-(i+1)
			! (i,j)=2*( (i+1),j ) - ( (i+2),j )
			rig_index=(/i,j/)
			write(*,90)rig_index,pos((/i,j/))
			pos1=pos( (/ (i+1),j /) )
			pos2=pos( (/ (i+2),j /) )
			free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
				rig_arr(pos2)%free_param(:)
			res=crunch(current_rig)
		enddo !j
		! up
		j=count
		do i=-(j-1),(j-1)
			! (i,j)=2*(i,(j-1) ) - (i,(j-2) )
			rig_index=(/i,j/)
			write(*,90)rig_index,pos((/i,j/))
			pos1=pos( (/ i,(j-1) /) )
			pos2=pos( (/ i,(j-2) /) )
			free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
				rig_arr(pos2)%free_param(:)
			res=crunch(current_rig)
		enddo !i
		write (*,*) 'Calculating corners'
		! now calc the corners
		res= corners_2D(current_rig,count)
	enddo !count

! for debugger visualization

!	do i=1,default%tot_grid_size
!		do j=1,default%tot_grid_size
!			points_2D(i,j)=rig_arr((i-1)*default%tot_grid_size+j)%point(3)
!		enddo !j
!	enddo !i


! output the results
	call output_rig_res(current_rig)

!	pause
! ********** 3D ************

case (3)

! center

	i=0
	j=0
	k=0
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	if (default%mgs) then
		! use the %val paramaters that have been provided by the user as estimates
		res=crunch(current_rig,dont_use_free_param_temp=.true.)
	else ! no need to minimize b/c the middle of the conf lim is the chi_sqr_min
		num_iter=0
		call arr_write(current_rig)
	endif 
!	param_list(map_from(:))%temp_val=param_list(map_from(:))%save_val	
	
! make the +/- 1 box without derivatives

! (1)
	i=1
	j=0
	k=0
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

! (2)
	i=0
	j=1
	k=0
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

! (3)
	i=-1
	j=0
	k=0
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

! (4)
	i=0
	j=-1
	k=0
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

! (5)
	i=0
	j=0
	k=1
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

! (6)
	i=0
	j=0
	k=-1
	rig_index=(/i,j,k/)
	write(*,90)rig_index,pos((/i,j,k/))
	res=crunch(current_rig,dont_use_free_param_temp=.true.)

! calculate the sides
	count = 1
	res= sides_3D(current_rig,count)

! calculate the corners

	res= corners_3D(current_rig,count)

! finished first generation set-up

! now use gradient info to "spiral" out
! a cube has 6 faces: 3 faces correspond to the positive sides.
! so have a count variable from 2 to GRID_SIZE and calculate the positive
! face then the negative face.

! we've just taken care of 0 and 1
	do count=2,default%grid_size
		write(*,*) 'Count = ',count
		! +i face
		i=count
		do j = -(i-1),(i-1) 
			do k= -(i-1),(i-1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( (i-1),j,k ) - ( (i-2),j,k )
				rig_index=(/i,j,k/)
				write(*,90)rig_index,pos((/i,j,k/))
				pos1=pos( (/(i-1),j,k/) )
				pos2=pos( (/(i-2),j,k/) )
				free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
					rig_arr(pos2)%free_param(:)
				res=crunch(current_rig)
			enddo !k
		enddo !j
		! -i face
		i=-count
		do j = (i+1),-(i+1) 
			do k = (i+1),-(i+1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( (i+1),j,k ) - ( (i+2),j,k )
				rig_index=(/i,j,k/)
				write(*,90)rig_index,pos((/i,j,k/))
				pos1=pos( (/(i+1),j,k/) )
				pos2=pos( (/(i+2),j,k/) )
				free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
					rig_arr(pos2)%free_param(:)
				res=crunch(current_rig)
			enddo !k
		enddo !j
		! +j face
		j=count
		do i = -(j-1),(j-1) 
			do k= -(j-1),(j-1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,(j-1),k ) - ( i,(j-2),k )
				rig_index=(/i,j,k/)
				write(*,90)rig_index,pos((/i,j,k/))
				pos1=pos( (/i,(j-1),k/) )
				pos2=pos( (/i,(j-2),k/) )
				free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
					rig_arr(pos2)%free_param(:)
				res=crunch(current_rig)
			enddo !k
		enddo !i
		! -j face
		j=-count
		do i = (j+1),-(j+1) 
			do k = (j+1),-(j+1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,(j+1),k ) - ( i,(j+2),k )
				rig_index=(/i,j,k/)
				write(*,90)rig_index,pos((/i,j,k/))
				pos1=pos( (/i,(j+1),k/) )
				pos2=pos( (/i,(j+2),k/) )
				free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
					rig_arr(pos2)%free_param(:)
				res=crunch(current_rig)
			enddo !k
		enddo !i
		! +k face
		k=count
		do i = -(k-1),(k-1) 
			do j= -(k-1),(k-1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,j,(k-1) ) - ( i,j,(k-2) )
				rig_index=(/i,j,k/)
				write(*,90)rig_index,pos((/i,j,k/))
				pos1=pos( (/i,j,(k-1)/) )
				pos2=pos( (/i,j,(k-2)/) )
				free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
					rig_arr(pos2)%free_param(:)
				res=crunch(current_rig)
			enddo !j
		enddo !i
		! -k face
		k=-count
		do i = (k+1),-(k+1) 
			do j = (k+1),-(k+1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,j,(k+1) ) - ( i,j,(k+2) )
				rig_index=(/i,j,k/)
				write(*,90)rig_index,pos((/i,j,k/))
				pos1=pos( (/i,j,(k+1)/) )
				pos2=pos( (/i,j,(k+2)/) )
				free_param_temp(:) = 2*rig_arr(pos1)%free_param(:)  - &
					rig_arr(pos2)%free_param(:)
				res=crunch(current_rig)
			enddo !j
		enddo !i

		! calculate the sides

		res= sides_3D(current_rig,count)

		! calculate the corners

		res= corners_3D(current_rig,count)

	enddo !count

	! output the results

	call output_rig_res(current_rig)


case default
	! only coded for up to 3 dimensions!
	res=1
	return

end select check_dim
	res=0
	return

90 format('grid point: {',<dim>I4,' }',I5)

end function rigorous
