	SUBROUTINE stiff3(y,dydx,x,htry,eps,yscal,hdid,hnext)
! This routine is adapted from numerical recipies in F90.

! this subroutine attempts to take a system of coupled non-linear ODE's through
! one timestep from time = x to time = x+htry (returned).
! this is specialized for a jacobian matrix that is diagonal.

! worried about loss of precision here.  The maximum effective rate of
! kon'=kon*Ca = ~1E7 * 1E-3 = 1E4

! now if h is wicked small like 1E-7 then 1/h = 1E7 and to 
! construct a you are subtracting dfdy which is three logs less than 1/h.
! But if we are not dealing with giant pCa steps then kon' = 1E7*1E-8
! ~ 1E-1.  In this case, h can be ~ 1E1 = a second.
! so an inteligent starting value for h is 1/maxval(dfdy)*0.1

	USE nrtype; USE nrutil, ONLY : assert_eq,diagadd,nrerror
	USE nr, ONLY : lubksb,ludcmp
	use h_physiology_models, only: three_state_t_derivs,three_state_t_jacobn
	IMPLICIT NONE
	REAL(SP), DIMENSION(2), INTENT(INOUT) :: y
	REAL(SP), DIMENSION(2), INTENT(IN) :: dydx,yscal
	REAL(SP), INTENT(INOUT) :: x
	REAL(SP), INTENT(IN) :: htry,eps
	REAL(SP), INTENT(OUT) :: hdid,hnext

! Local Params
	INTEGER(I4B), PARAMETER :: MAXTRY=40
	REAL(SP), PARAMETER :: SAFETY=0.9_sp,GROW=1.5_sp,PGROW=-0.25_sp,&
		SHRNK=0.5_sp,PSHRNK=-1.0_sp/3.0_sp,ERRCON=0.1296_sp,&
		GAM=1.0_sp/2.0_sp,&
		A21=2.0_sp,A31=48.0_sp/25.0_sp,A32=6.0_sp/25.0_sp,C21=-8.0_sp,&
		C31=372.0_sp/25.0_sp,C32=12.0_sp/5.0_sp,&
		C41=-112.0_sp/125.0_sp,C42=-54.0_sp/125.0_sp,&
		C43=-2.0_sp/5.0_sp,B1=19.0_sp/9.0_sp,B2=1.0_sp/2.0_sp,&
		B3=25.0_sp/108.0_sp,B4=125.0_sp/108.0_sp,E1=17.0_sp/54.0_sp,&
		E2=7.0_sp/36.0_sp,E3=0.0_sp,E4=125.0_sp/108.0_sp ,&
		C1X=1.0_sp/2.0_sp,C2X=-3.0_sp/2.0_sp,C3X=121.0_sp/50.0_sp,&
		C4X=29.0_sp/250.0_sp,A2X=1.0_sp,A3X=3.0_sp/5.0_sp

! Local Var's
	INTEGER(I4B) :: jtry,ndum
	INTEGER(I4B), DIMENSION(size(y)) :: indx
	REAL(SP), DIMENSION(size(y)) :: dytmp,err,g1,g2,g3,g4,ysav,dfdy,a
!	REAL(SP), DIMENSION(size(y),size(y)) :: a,dfdy
	REAL(SP) :: d,errmax,h,xsav

! Begin
	ndum=2
	xsav=x
	ysav(:)=y(:)
	call three_state_t_jacobn(ysav,dfdy,xsav)
	h=htry
	do jtry=1,MAXTRY
		! calculate the "inverse" of the jacobian matrix
		a=1./(spread(1/(GAM*h),1,2)-dfdy)
!		a(:,:)=-dfdy(:,:)
!		call diagadd(a,1.0_sp/(GAM*h))
!		call ludcmp(a,indx,d)
		g1=dydx*a
!		call lubksb(a,indx,g1)
		y=ysav+A21*g1
		x=xsav+A2X*h
		call three_state_t_derivs(y,dytmp,x)
		g2=(dytmp+C21*g1/h)*a
!		call lubksb(a,indx,g2)
		y=ysav+A31*g1+A32*g2
		x=xsav+A3X*h
		call three_state_t_derivs(y,dytmp,x)
		g3=(dytmp+(C31*g1+C32*g2)/h)*a
!		call lubksb(a,indx,g3)
		g4=(dytmp+(C41*g1+C42*g2+C43*g3)/h)*a
!		call lubksb(a,indx,g4)
		y=ysav+B1*g1+B2*g2+B3*g3+B4*g4
		err=E1*g1+E2*g2+E3*g3+E4*g4
		x=xsav+h
		if (x == xsav) call &
			nrerror('stepsize not significant in stiff')
		errmax=maxval(abs(err/yscal))/eps
		if (errmax <= 1.0) then
			hdid=h
			hnext=merge(SAFETY*h*errmax**PGROW, GROW*h, &
				errmax > ERRCON)
			RETURN
		else
			hnext=SAFETY*h*errmax**PSHRNK
			h=sign(max(abs(hnext),SHRNK*abs(h)),h)
		end if
	end do
	call nrerror('exceeded MAXTRY in stiff')
	END SUBROUTINE stiff3
