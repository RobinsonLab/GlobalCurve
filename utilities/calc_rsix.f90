function calc_rsix(N,rmin,dr,rzero) result(rsix)
	implicit none
! the factor rsix = (rzero/r)**6 is calculated along the same grid points as Nnot.

	integer,intent(in) :: N
	real,intent(in) :: rmin,dr,rzero
	real,dimension(N) :: rsix

! local vars
	integer :: i
	real :: r

! Begin
	do i=1,N
		r=rmin+(i-1)*dr
!		if (r > 0.) then
			rsix(i)=(rzero/r)**6
!		else
!			rsix(i)=1.0E+9
!		endif
	enddo !i
end function calc_rsix
