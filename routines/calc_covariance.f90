subroutine calc_covariance()
! output: cov
! only the upper triangular part of cov is significant
	use h_vars
	use h_params ! default
	use h_struct
	use h_routines
	use h_utils
	use numerical_libraries
	implicit none

	! h_vars,intent(in)
	!	integer,intent(in) :: num_free_global_param
	!	type (data_struct),pointer, dimension(:) :: data_list
	!	type(expt_struct), pointer :: expt_list
	!	type(param_struct),pointer,dimension(:) :: param_list
	!	integer,dimension(:),intent(in) :: map_to

	! h_vars,intent(out)
	!	real(8),dimension(num_free_global_param,num_free_global_param),intent(out) :: cov,curvature
	!	real(8),dimension(num_free_global_param) :: diag,gradient
! Local vars
	integer :: irank	
	integer :: j,k
	real(8),allocatable,dimension(:,:) :: u,v
	real(8),allocatable,dimension(:) :: s
	integer :: n
! Begin
	call calc_curvature_gradient()
	forall( j=1:num_free_global_param )
		diag(j)=curvature(j,j)
	end forall

! due to the way the curvature matrix is calculated, if a diagonal element
! is zero then the whole row will be zero; ergo the curvature matrix is singular
! This means that there is no functional dependence on a particular parameter.

	if ( any( diag(:) == 0. ) ) call error_handler('Curvature matrix is singular.')

! *** temp

	! fill in the lower triangular part of the curvature matrix
	forall(j=2:num_free_global_param)
		forall(k=1:j-1)
			curvature(j,k)=curvature(k,j)
		end forall !k
	end forall !j

! curv2=curvature
	n=num_free_global_param
	allocate(s(n))
	allocate(u(n,n))
	allocate(v(n,n))
	call dlsvrr(n,n,curvature,n,11,default%svd_tol,irank,s,u,n,v,n)


	deallocate(s,u,v)
! normalize the curvature matrix.
! Make use of the transformation:! A'=N A N where A' is the
! unit diagonal normalized matrix and N = I*(1/sqrt(diag))
! and A = N^-1 A' N^-1; so the equation A x = b is given by
! N^-1 A' N^-1 x = b => A' y = b' where y = N^-1 x and b' = N b
! solve for y = (A')^-1 b' => x = N y
	forall( j=1:num_free_global_param )
		forall( k=j+1:num_free_global_param)
			curvature(j,k)=curvature(j,k)/sqrt( diag(j)	* diag(k) )
		end forall
		curvature(j,j)=1.0
	end forall	
	! fill in the lower triangular part of the curvature matrix
	forall(j=2:num_free_global_param)
		forall(k=1:j-1)
			curvature(j,k)=curvature(k,j)
		end forall !k
	end forall !j



	call dlsgrr(num_free_global_param,num_free_global_param,&
		curvature,num_free_global_param,default%svd_tol,irank,cov,&
		num_free_global_param)
! cov_temp=cov

! so now we have the inverse of the preconditioned 
! curvature matrix.  We need the inverse of the non-preconditioned
! curvature matrix.  We use the identity: (A B C)^-1 = C^-1 B^-1 A^-1
! applied to: A^-1=(N^-1 A' N^-1)^-1 which gives: A^-1 = N (A')^-1 N

	forall( j=1:num_free_global_param )
		forall( k=j:num_free_global_param)
			cov(j,k)=cov(j,k)/sqrt( diag(j)	* diag(k) )
		end forall
	end forall	

! we won't be using the lower triangular part. But let's calculate
! it anyway just in case we want to plot cov in the debugger.

	! fill in the lower triangular part of the curvature matrix
	forall(j=2:num_free_global_param)
		forall(k=1:j-1)
			cov(j,k)=cov(k,j)
		end forall !k
	end forall !j
 
! pause

return

end subroutine calc_covariance