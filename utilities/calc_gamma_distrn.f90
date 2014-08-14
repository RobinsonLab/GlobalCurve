! not currently used

subroutine calc_gamma_distrn(rzero,x0,s,lambda,N,rmin,dr,Nnot,rsix)
use h_params
use h_struct
use numerical_libraries
! This subroutine calculates an abcissa shifted gamma probability
! density.
! WARNING - call this subroutine with s .GE. 1. and with mean = x0+s/lambda > 0
! It also calculates the factor rsix = (rzero/r)**6 along the same grid points as
! Nnot.
implicit none
real,intent(in) :: rzero,x0,s,lambda,rmin,dr
integer,intent(in) :: N
real,dimension(N),intent(out) :: Nnot,rsix

! local vars
integer :: i
real :: r,gfn

! Begin
	x0=params(1)
	lambda=params(2)
	s=params(3)
	rmin=x0
! call IMSL routine to calculate the inverse of the cumulative gamma dist fcn
	x=gamin(0.99,a)
	rmax=x/lambda+x0
	dr=(rmax-rmin)/(N-1)




	! call IMSL routine to obtain gamma(s)
	gfn=gamma(s)
	do i=1,N
		r=rmin+(i-1)*dr
		x=lambda*(r-x0)
		Nnot(i)=lambda*exp(-x)*x**(s-1.)/gfn
		if (r > 0.) then
			rsix(i)=(rzero/r)**6
		else
			rsix(i)=1.0E+9
		endif
	enddo !i
end subroutine
