SUBROUTINE odeint_time(ystart,x1,x2,eps,hmin,num_pts,species,f,g,x_pts)
	USE nrtype; USE nrutil, ONLY : nrerror
	use h_physiology_models, &
		only : three_state_derivs,three_state_jacobn,stiff2
	IMPLICIT NONE
	REAL(SP), DIMENSION(3), INTENT(in) :: ystart
	REAL(SP), INTENT(IN) :: x1,x2,eps,hmin
	integer,intent(inout) :: num_steps
	real,dimension(num_pts,3),intent(out) :: species
	real,dimension(num_pts),intent(out) :: f,g ! not setting f right now
	


!	Local Variables

!	REAL(SP), PARAMETER :: TINY=1.0e-30_sp
	INTEGER(I4B), PARAMETER :: MAXSTP=10000
	INTEGER(I4B) :: nstp
	REAL(SP) :: h,hdid,hnext,x,xsav
	REAL(SP), DIMENSION(2) :: dydx,y,yscal
	INTEGER(I4B) :: nok,nbad


! Begin
	! here, we choose to solve the ODE's for species 1 and 3.  Species 2 is known
	! from concervation of mass.  Choosing 1 and 3 has the nice property that the
	! Jacobian matrix is diagonal.
	y(1)=ystart(1)
	y(2)=ystart(3)
! next two lines to get a decent estimate for h
	call three_state_jacobn(y,dydx) ! just use dydx as scratch space
					! call to three_state_derivs below will overwrite
	h=0.1/maxval(abs(dydx))

	x=x1
!	h=sign(h1,x2-x1)
	nok=0
	nbad=0
!	kount=0

	do nstp=1,MAXSTP
		call three_state_derivs(y,dydx)
!		yscal(:)=abs(y(:))+abs(h*dydx(:))+TINY
		yscal(:) = 1.0
!		if (save_steps .and. (abs(x-xsav) > abs(dxsav))) &
!			call save_a_step
		if ((x+h-x2)*(x+h-x1) > 0.0) h=x2-x
		call stiff2(y,dydx,x,h,eps,yscal,hdid,hnext) ! take a step
		if (hdid == h) then
			nok=nok+1
		else
			nbad=nbad+1
		end if
		if ((x-x2)*(x2-x1) >= 0.0) exit
!			ystart(:)=y(:)
!			if (save_steps) call save_a_step2
!			RETURN
!		end if
		if (abs(hnext) < hmin)&
			call nrerror('stepsize smaller than minimum in odeint')
		h=hnext
	end do
	if (nstp > MAXSTP) call nrerror('too many steps in odeint')
	ystart(1)=y(1)
	ystart(3)=y(2)
	ystart(2)=1.-(y(1)+y(2))	
!	CONTAINS
!BL
!	SUBROUTINE save_a_step2
!	kount=kount+1
!	if (kount > size(xp)) then
!		xp=>reallocate(xp,2*size(xp))
!		yp=>reallocate(yp,size(yp,1),size(xp))
!	end if
!	xp(kount)=x
!	yp(:,kount)=y(:)
!	xsav=x
!	END SUBROUTINE save_a_step2
	END SUBROUTINE odeint2
