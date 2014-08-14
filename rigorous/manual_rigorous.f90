subroutine manual_rigorous(max,dim,param_loc,lim,map_from,chi_sqr_min, &
	loc_chi_sqr,global_DOF,expt_data, &
	expt_list,param_list,ans_file_name,default)
use h_params
use h_routines, only: minimize
use h_struct
use h_rigorous

implicit none

integer,intent(in) :: max
integer,intent(in) :: dim
integer,dimension(dim),intent(in) :: param_loc 
real,dimension(dim,2),intent(in) :: lim 
integer,dimension(max),intent(in) :: map_from
real,intent(in) :: chi_sqr_min
real,dimension(:),intent(out) :: loc_chi_sqr !dim over num of expts
integer,intent(in) :: global_DOF
type (data_struct),pointer, dimension(:) :: expt_data
type(expt_struct), pointer :: expt_list
type(param_struct),pointer,dimension(:) :: param_list
character(STRING_LEN),intent(in) :: ans_file_name
type (default_struct),intent(in) :: default


! Local Var's
!	integer,dimension(dim) :: index	

	type(rig_struct),dimension(TOT_GRID_SIZE**dim) :: arr,arr2
!	integer :: num_iter
	real,dimension(max) :: free_param_temp
	real,dimension(dim) :: fixed_param_val
	integer :: arr_size
	integer :: i,j,k,count
	real,dimension(dim) :: store_param_val
	real,dimension(TOT_GRID_SIZE) :: points_1D
	real,dimension(TOT_GRID_SIZE,TOT_GRID_SIZE) :: points_2D
	real :: tgt
	integer :: num_iter
	real :: chi_sqr
	integer,dimension(dim) :: index
	integer :: pos0,pos1,pos2,pos3


! this routine performs a 1,2,or 3 dimensional grid search in chi_sqr space
! the results are output in: $=base(ans_file_name) 
! $.tgt : tells where to draw the conficence line
! $.dmp : contents of arr
! $.rig : {x,chi_sqr} where {x} is the array of grid point locations
! $.scl : contents of arr scaled as follows x values are normalized to the param values
!			@ the chi_sqr global min. and chi_sqr is scaled from 0..inf and 1 is where chi_sqr
!			is equal to the conficence line.


! Begin


	store_param_val=param_list( param_loc(:) )%save_val
	arr_size=TOT_GRID_SIZE**dim
!	allocate(arr(arr_size))
! it would be nice if we  could dynamically assign rig_struct to
! the dimensions we need,  We can't so.....
	do i=1,arr_size
		allocate( arr(i)%point(dim+1) ) ! {x_1 , .. x_n , y }
		allocate( arr(i)%free_param(max) )
		allocate( arr2(i)%point(dim+1) ) ! {x_1 , .. x_n , y }
		allocate( arr2(i)%free_param(max) )
	enddo !i


! End set-up



	tgt=f_conf(chi_sqr_min,dim,global_DOF-dim,default%conf_lim) 
	! calculate to 1.5*sigma for gaussian distr.
	
check_dim: select case (dim)



case (1)


	! center
	i=0
	index=(/i/)
	fixed_param_val=update_fixed_param_val(dim,lim,index)
	call arr_write(arr,index,chi_sqr_min,fixed_param_val, &
			param_list( map_from(:) )%save_val,num_iter=0 )
	! make the +/- 1 box without derivatives
	! right
	i=1
	index=(/i/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info

	! left
	i=-1
	index=(/i/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info


! use the derivatives of the grid points at the present surface
! to calculate the points at the next concentric surface.
	do count=2,GRID_SIZE
		! right
		i=count
		! use derivative data to predict the free_param values
		! (i)=2*(i-1) - (i-2)
		index=(/i/)	
		pos1=pos_1D( (/ (i-1) /) )
		pos2=pos_1D( (/ (i-2) /) )
!		free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
!			arr(pos2)%free_param(:) 
		free_param_temp(:) = arr(pos1)%free_param(:)


		call crunch(max,dim,param_loc,lim, &
			map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
			param_list,arr,default,index,free_param_temp=free_param_temp)

		! left
		i=-count
		! (i)=(i+1) - (i+2)
		index=(/i/)
		pos1=pos_1D( (/ (i+1) /) )
		pos2=pos_1D( (/ (i+2) /) )
!		free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
!			arr(pos2)%free_param(:)
		free_param_temp(:) = arr(pos1)%free_param(:)
		call crunch(max,dim,param_loc,lim, &
			map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
			param_list,arr,default,index,free_param_temp=free_param_temp)

	enddo !count
	do i=1,TOT_GRID_SIZE
		points_1D(i)=arr(i)%point(2)
	enddo !i
	
	! output the results


call output_rig_res(max,dim,arr_size,chi_sqr_min,ans_file_name,arr,arr2, &
	param_list,tgt,map_from,param_loc)
	
	pause

case (2)
	! center
	i=0
	j=0
	index=(/i,j/)
	fixed_param_val=update_fixed_param_val(dim,lim,index)
	call arr_write(arr,index,chi_sqr_min,fixed_param_val, &
		param_list( map_from(:) )%save_val,num_iter=0)	
	
! make the +/- 1 box without derivatives
	! right
	i=1
	j=0
	index=(/i,j/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info
	! down
	i=0
	j=-1
	index=(/i,j/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info
	! left
	i=-1
	j=0
	index=(/i,j/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info
	! up
	i=0
	j=1
	index=(/i,j/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info
	! now calc the corners
	count=1	
	call corners_2D(count,max,dim,store_param_val,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default)

! use the derivatives of the grid points at the present surface
! to calculate the points at the next concentric surface.

	do count=2,GRID_SIZE
		! right
		write(*,*) 'Count = ',count
		i=count
		do j = -(i-1),(i-1) 
			! use derivative data to predict the free_param values
			! (i,j)=2*( (i-1),j ) - ( (i-2),j )
			index=(/i,j/)
			pos1=pos_2D( (/(i-1),j/) )
			pos2=pos_2D( (/(i-2),j/) )
			free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
				arr(pos2)%free_param(:)
			call crunch(max,dim,param_loc,lim, &
				map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
				param_list,arr,default,index,free_param_temp=free_param_temp)

		enddo !j
		! down
		j=-count
		do i = (j+1),-(j+1)
			! (i,j)=2*( i,(j+1) ) - ( i,(j+2) )
			index=(/i,j/)
			pos1=pos_2D( (/ i,(j+1) /) )
			pos2=pos_2D( (/ i,(j+2) /) )
			free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
				arr(pos2)%free_param(:)
			call crunch(max,dim,param_loc,lim, &
				map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
				param_list,arr,default,index,free_param_temp=free_param_temp)
		enddo !i
		! left
		i=-count
		do j = (i+1),-(i+1)
			! (i,j)=2*( (i+1),j ) - ( (i+2),j )
			index=(/i,j/)
			pos1=pos_2D( (/ (i+1),j /) )
			pos2=pos_2D( (/ (i+2),j /) )
			free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
				arr(pos2)%free_param(:)
			call crunch(max,dim,param_loc,lim, &
				map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
				param_list,arr,default,index,free_param_temp=free_param_temp)
		enddo !j
		! up
		j=count
		do i=-(j-1),(j-1)
			! (i,j)=2*(i,(j-1) ) - (i,(j-2) )
			index=(/i,j/)
			pos1=pos_2D( (/ i,(j-1) /) )
			pos2=pos_2D( (/ i,(j-2) /) )
			free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
				arr(pos2)%free_param(:)
			call crunch(max,dim,param_loc,lim, &
				map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
				param_list,arr,default,index,free_param_temp=free_param_temp)
		enddo !i
		write (*,*) 'Calculating corners'
		! now calc the corners
		call corners_2D(count,max,dim,store_param_val,param_loc,lim, &
			map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
			param_list,arr,default)


	enddo !count

! for debugger visualization

	do i=1,TOT_GRID_SIZE
		do j=1,TOT_GRID_SIZE
			points_2D(i,j)=arr((i-1)*TOT_GRID_SIZE+j)%point(3)
		enddo !j
	enddo !i


! output the results


call output_rig_res(max,dim,arr_size,chi_sqr_min,ans_file_name,arr,arr2, &
	param_list,tgt,map_from,param_loc)




pause
!	case (3)
		
! center

	i=0
	j=0
	k=0
	index=(/i,j,k/)
	fixed_param_val=update_fixed_param_val(dim,lim,index)
	call arr_write(arr,index,chi_sqr_min,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter=0)	
	
! make the +/- 1 box without derivatives

! (1)

	i=1
	j=0
	k=0
	index=(/i,j,k/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info

! (2)

	i=0
	j=1
	k=0
	index=(/i,j,k/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info
	
! (3)

	i=-1
	j=0
	k=0
	index=(/i,j,k/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info
! (4)

	i=0
	j=-1
	k=0
	index=(/i,j,k/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info
! (5)

	i=0
	j=0
	k=1
	index=(/i,j,k/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info

! (6)

	i=0
	j=0
	k=-1
	index=(/i,j,k/)
	call crunch(max,dim,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default,index) ! no deriv. info

! calculate the sides
	count = 1
	call sides_3D(count,max,dim,store_param_val,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default)

! calculate the corners

	call corners_3D(count,max,dim,store_param_val,param_loc,lim, &
		map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
		param_list,arr,default)

! finished first generation set-up

! now use gradient info to "spiral" out
! now a cube has 6 faces: 3 faces correspond to the positive sides.
! so have a count variable from 2 to GRID_SIZE and calculate the positive
! face then the negative face.

	do count=2,GRID_SIZE
		write(*,*) 'Count = ',count
		! +i face
		i=count
		do j = -(i-1),(i-1) 
			do k= -(i-1),(i-1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( (i-1),j,k ) - ( (i-2),j,k )
				index=(/i,j,k/)
				pos1=pos_3D( (/(i-1),j,k/) )
				pos2=pos_3D( (/(i-2),j,k/) )
				free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
					arr(pos2)%free_param(:)
				call crunch(max,dim,param_loc,lim, &
					map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
					param_list,arr,default,index,free_param_temp=free_param_temp)
			enddo !k
		enddo !j
		! -i face
		i=-count
		do j = (i+1),-(i+1) 
			do k = (i+1),-(i+1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( (i+1),j,k ) - ( (i+2),j,k )
				index=(/i,j,k/)
				pos1=pos_3D( (/(i+1),j,k/) )
				pos2=pos_3D( (/(i+2),j,k/) )
				free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
					arr(pos2)%free_param(:)
				call crunch(max,dim,param_loc,lim, &
					map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
					param_list,arr,default,index,free_param_temp=free_param_temp)
			enddo !k
		enddo !j
		! +j face
		j=count
		do i = -(j-1),(j-1) 
			do k= -(j-1),(j-1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,(j-1),k ) - ( i,(j-2),k )
				index=(/i,j,k/)
				pos1=pos_3D( (/i,(j-1),k/) )
				pos2=pos_3D( (/i,(j-2),k/) )
				free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
					arr(pos2)%free_param(:)
				call crunch(max,dim,param_loc,lim, &
					map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
					param_list,arr,default,index,free_param_temp=free_param_temp)
			enddo !k
		enddo !i
		! -j face
		j=-count
		do i = (j+1),-(j+1) 
			do k = (j+1),-(j+1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,(j+1),k ) - ( i,(j+2),k )
				index=(/i,j,k/)
				pos1=pos_3D( (/i,(j+1),k/) )
				pos2=pos_3D( (/i,(j+2),k/) )
				free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
					arr(pos2)%free_param(:)
				call crunch(max,dim,param_loc,lim, &
					map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
					param_list,arr,default,index,free_param_temp=free_param_temp)
			enddo !k
		enddo !i
		! +k face
		k=count
		do i = -(k-1),(k-1) 
			do j= -(k-1),(k-1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,j,(k-1) ) - ( i,j,(k-2) )
				index=(/i,j,k/)
				pos1=pos_3D( (/i,j,(k-1)/) )
				pos2=pos_3D( (/i,j,(k-2)/) )
				free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
					arr(pos2)%free_param(:)
				call crunch(max,dim,param_loc,lim, &
					map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
					param_list,arr,default,index,free_param_temp=free_param_temp)
			enddo !j
		enddo !i
		! -k face
		k=-count
		do i = (k+1),-(k+1) 
			do j = (k+1),-(k+1)
				! use derivative data to predict the free_param values
				! (i,j,k)=2*( i,j,(k+1) ) - ( i,j,(k+2) )
				index=(/i,j,k/)
				pos1=pos_3D( (/i,j,(k+1)/) )
				pos2=pos_3D( (/i,j,(k+2)/) )
				free_param_temp(:) = 2*arr(pos1)%free_param(:)  - &
					arr(pos2)%free_param(:)
				call crunch(max,dim,param_loc,lim, &
					map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
					param_list,arr,default,index,free_param_temp=free_param_temp)
			enddo !j
		enddo !i

		! calculate the sides

		call sides_3D(count,max,dim,store_param_val,param_loc,lim, &
			map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
			param_list,arr,default)

		! calculate the corners

		call corners_3D(count,max,dim,store_param_val,param_loc,lim, &
			map_from,loc_chi_sqr,global_DOF,expt_data,expt_list, &
			param_list,arr,default)

	enddo !count

	! output the results

	call output_rig_res(max,dim,arr_size,chi_sqr_min,ans_file_name,arr,arr2, &
		param_list,tgt,map_from,param_loc)

	end select check_dim

	return


end subroutine manual_rigorous
