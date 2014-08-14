subroutine corners_3D(free_param_temp,arr,param_list,fixed_param_val, &
	param_loc,max,chi_sqr,loc_chi_sqr,global_DOF,dim,num_iter, &
	map_from,expt_data,expt_list,param_list,default)

use h_params
use h_routines, only: minimize
use h_struct
use h_utils, only: pos_3D,pos,update_fixed_param_val,arr_write

implicit none

integer,intent(in) :: max
integer,intent(in) :: dim
integer,dimension(dim),intent(in) :: param_loc 
real,dimension(dim),intent(in) :: lim 
integer,dimension(max),intent(in) :: map_from
real,intent(in) :: chi_sqr_min
real,dimension(:),intent(out) :: loc_chi_sqr !dim over num of expts
integer,intent(in) :: global_DOF
type (data_struct),pointer, dimension(:) :: expt_data
type(expt_struct), pointer :: expt_list
type(param_struct),pointer,dimension(:) :: param_list
type (default_struct),intent(in) :: default
type(rig_struct),dimension(TOT_GRID_SIZE**dim),intent(inout) :: arr
real,dimension(dim),intent(in) :: store_param_val

! Local Vars

integer :: i,j,k
integer,dimension(3) :: index
integer :: pos0,pos1,pos2,pos3
real :: chi_sqr
integer :: num_iter
real,dimension(max) :: free_param_temp
real,dimension(dim) :: fixed_param_val

! Begin

! to calculate the function (vector valued) i.e. map_from(:) of param_list
! start at the origin.  Jump to (1,0,0) by taking df/dx in the (1,0,0) 
! direction.  From (1,0,0) jump to (1,1,0) by heading df/dx in the 
! (0,1,0) direction.  Finally jump from (1,1,0) to (1,1,1) by taking a
! df/dx step in the (0,0,1) direction.  If f_1 is the vector valued function
! at position (1,0,0) then the first step size will be f_1 - f_0 where f_0 is
! the function evaluated at the origin.  The next step size will be f_2 - f_0, and
! the third will be f_3 - f_0.  Now f_n which is the function value at
! point (1,1,1) will be: f_n = f_0 + (f_1 - f_0) + (f_2 - f_0) + (f_3 - f_0)
! ergo: f_n = f_1 + f_2 + f_3 - 2*f_0.



! corner (1)

	i=count
	j=count
	k=count
	index=(/i,j,k/)
	pos0=pos_3D( (/i-1,j-1,k-1/) )
	pos1=pos_3D( (/i,j-1,k-1/) )
	pos2=pos_3D( (/i-1,j,k-1/) )
	pos3=pos_3D( (/i-1,j-1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )

! corner (2)

	i=-count
	j=count
	k=count
	index=(/i,j,k/)
	pos0=pos_3D( (/i+1,j-1,k-1/) )
	pos1=pos_3D( (/i,j-1,k-1/) )
	pos2=pos_3D( (/i+1,j,k-1/) )
	pos3=pos_3D( (/i+1,j-1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )

! corner (3)

	i=-count
	j=-count
	k=count
	index=(/i,j,k/)
	pos0=pos_3D( (/i+1,j+1,k-1/) )
	pos1=pos_3D( (/i,j+1,k-1/) )
	pos2=pos_3D( (/i+1,j,k-1/) )
	pos3=pos_3D( (/i+1,j+1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )
! corner (4)

	i=count
	j=-count
	k=count
	index=(/i,j,k/)
	pos0=pos_3D( (/i-1,j+1,k-1/) )
	pos1=pos_3D( (/i,j+1,k-1/) )
	pos2=pos_3D( (/i-1,j,k-1/) )
	pos3=pos_3D( (/i-1,j+1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )

! corner (5)

	i=count
	j=count
	k=-count
	index=(/i,j,k/)
	pos0=pos_3D( (/i-1,j-1,k+1/) )
	pos1=pos_3D( (/i,j-1,k+1/) )
	pos2=pos_3D( (/i-1,j,k+1/) )
	pos3=pos_3D( (/i-1,j-1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )

! corner (6)

	i=-count
	j=count
	k=-count
	index=(/i,j,k/)
	pos0=pos_3D( (/i+1,j-1,k+1/) )
	pos1=pos_3D( (/i,j-1,k+1/) )
	pos2=pos_3D( (/i+1,j,k+1/) )
	pos3=pos_3D( (/i+1,j-1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )

! corner (7)

	i=-count
	j=-count
	k=-count
	index=(/i,j,k/)
	pos0=pos_3D( (/i+1,j+1,k+1/) )
	pos1=pos_3D( (/i,j+1,k+1/) )
	pos2=pos_3D( (/i+1,j,k+1/) )
	pos3=pos_3D( (/i+1,j+1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )

! corner (8)

	i=count
	j=-count
	k=-count
	index=(/i,j,k/)
	pos0=pos_3D( (/i-1,j+1,k+1/) )
	pos1=pos_3D( (/i,j+1,k+1/) )
	pos2=pos_3D( (/i-1,j,k+1/) )
	pos3=pos_3D( (/i-1,j+1,k/) ) 
	free_param_temp(:) = arr(pos1)%free_param(:) + &
		arr(pos2)%free_param(:) + arr(pos3)%free_param(:) - &
		2*arr(pos0)%free_param(:)
	param_list(map_from(:))%val = free_param_temp(:)
	fixed_param_val=update_fixed_param_val(store_param_val,lim,index)
	param_list( param_loc(:) )%val = fixed_param_val(:)
	call minimize(max,chi_sqr,loc_chi_sqr, &
		global_DOF-dim,num_iter,map_from,expt_data,expt_list, &
		param_list,default)
	call arr_write(arr,index,chi_sqr,fixed_param_val, &
		param_list( map_from(:) )%val,num_iter )


