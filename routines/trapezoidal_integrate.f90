function trapezoidal_integrate(fcn,N,dr) result (res)
! calculates: integral[fcn,{r,rmin,rmax}] where fcn is an array of length N
! where fcn is evaluated at N equally spaced points in the interval rmin
! to rmax
	implicit none
	real,dimension(:) :: fcn
	real,intent(in) :: dr
	integer,intent(in) :: N
	real :: res

! Begin
 res=dr*(0.5*(fcn(1)+fcn(n))+sum(fcn(2:N-1)))
return
end function trapezoidal_integrate