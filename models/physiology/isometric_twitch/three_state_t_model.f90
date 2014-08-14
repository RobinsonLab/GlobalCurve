SUBROUTINE three_state_t_derivs(x_in,dxdt,t)
! called by stiff3

! calculate x(1)=A,x(2)=A.M.Ca; A.Ca can be calculated from conc of mass.
	USE h_utils, only : get_ca
	use h_routine_specific_vars
		! intent(in) :: tsv
	IMPLICIT NONE

	REAL, DIMENSION(2), INTENT(IN) :: x_in
	REAL, DIMENSION(2), INTENT(OUT) :: dxdt
	real,intent(in),optional :: t

! Local Var's
	real :: eta,zeta
	real :: kon,koff,f,g,Ca
	real,dimension(3) :: x
! Begin
	x(1)=x_in(1)
	x(3)=x_in(2)
	x(2)=1.-(x_in(1)+x_in(2))

	eta=x(2) ! +x(3) ! note all x's are normalized
	zeta=x(3)
	kon=tsv%kon
	if (tsv%fixed) then 
		Ca=tsv%Ca
	else ! use c-spline to calculate
		Ca=get_ca(t)
	endif
!	Ca=1/10**pCa
	koff=tsv%koff * (1.+eta*(tsv%u-1.))**2 * (1.+zeta*(tsv%x-1.))**2
! old f
!	f=tsv%f * (1.+zeta*(tsv%v-1.))**2
! new f to get the right looking ktr
	f=tsv%f * (1.+eta*(tsv%v-1.))**2 * (1.+zeta*(tsv%z-1.))**2
	g=tsv%g * (1.+zeta*(tsv%w-1.))**2
	
	dxdt(1)=-kon*Ca*x(1) + koff*x(2)
!	dxdt(2)= kon*Ca*x(1) - (koff+f)*x(2) + g*x(3)
	dxdt(2)= f*x(2) - g*x(3)


	end subroutine


	SUBROUTINE three_state_t_jacobn(x,dfdx,t)
	USE h_utils, only: get_ca
	use h_routine_specific_vars
	IMPLICIT NONE

	REAL, DIMENSION(2), INTENT(IN) :: x
!	REAL(SP), DIMENSION(:), INTENT(OUT) :: dfdt
	REAL, DIMENSION(2), INTENT(OUT) :: dfdx
	REAL, INTENT(IN),optional :: t

! note the jacobian matrix is diagonal so store it in band form
! Local Var's
!	real :: eta,zeta
	real :: kon,g,Ca !,koff,f
	
! Begin

! all this is probably duplicating things.  Should define kon,koff,f,g in a module
! and save them off because they have been calculated in three_step_derivs above
!	eta=x(2)+x(3) ! note all x's are normalized
!	zeta=x(3)

	kon=tsv%kon
	if (tsv%fixed) then 
		Ca=tsv%Ca
	else
		Ca=get_ca(t)
	endif
!Ca=1/10**pCa
!	koff=tsv%koff * (1.+eta*(tsv%u-1.))**2 * (1.+zeta*(tsv%x-1.))**2
!	f=tsv%f * (1.+zeta*(tsv%v-1.))**2
	! note here zeta = x(2) bacause x(2) corresponds to CaAM
	g=tsv%g * (1.+x(2)*(tsv%w-1.))**2
!	g=tsv%g * (1.+zeta*(tsv%w-1.))**2
!	dfdt(:)=0.0

!	dfdx(1,1)= -kon*Ca
!	dfdx(1,2)= koff
!	dfdx(1,2)= 0.0

!	dfdx(2,1)= kon*Ca
!	dfdx(2,2)= -(koff+f)
!	dfdx(2,3)= g

!	dfdx(2,1)= 0.0
!	dfdx(3,2)= f
!	dfdx(2,2)=-g

	dfdx(1)= -kon*Ca
	dfdx(2)=-g

	END SUBROUTINE 