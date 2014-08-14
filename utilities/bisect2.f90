FUNCTION bisect2(func,current_model,extra_params,x1,x2)
! Lifted from Numerical Recipies and modified to include current_model and extra_params
!	USE nrtype; USE nrutil, ONLY : nrerror
	use h_params ! X_TOL
	use h_struct
!	use h_models
	IMPLICIT NONE
	type(model_struct), pointer :: current_model
	real,dimension(:),intent(in) :: extra_params
	REAL, INTENT(IN) :: x1,x2
	REAL :: bisect2
!	real,external :: func
	INTERFACE
		FUNCTION func(current_model,x,extra_params)
			use h_struct
			IMPLICIT NONE
			type(model_struct), pointer :: current_model
			real,dimension(:),intent(in) :: extra_params
			REAL, INTENT(IN) :: x
			REAL :: func
		END FUNCTION func
	END INTERFACE

! Local Params
	INTEGER, PARAMETER :: MAXIT=40
	INTEGER :: j
	REAL :: dx,f,fmid,xmid

! Begin
	fmid=func(current_model,x2,extra_params)
	f=func(current_model,x1,extra_params)
	if (f*fmid >= 0.0) write(*,*)'bisect: root must be bracketed'
	!call nrerror('bisect: root must be bracketed')
	if (f < 0.0) then
		bisect2=x1
		dx=x2-x1
	else
		bisect2=x2
		dx=x1-x2
	end if
	do j=1,MAXIT
		dx=dx*0.5
		xmid=bisect2+dx
		fmid=func(current_model,xmid,extra_params)
		if (fmid <= 0.0) bisect2=xmid
		if (abs(dx) < X_TOL .or. fmid == 0.0) RETURN
	end do
	write(*,*)'bisect: too many bisections'
!	call nrerror('bisect: too many bisections')
	END FUNCTION bisect2
