function three_state_SS(current_model,current_data,g_save) result(species)
	use h_struct
	use h_routine_specific_vars
!	USE ode_path
!	use numerical_libraries, only: neqnf
!	use nr, only: newt
	implicit none	

	type(model_struct), pointer :: current_model
	type (data_struct),pointer :: current_data	
	real,dimension(current_data%len),optional :: g_save
	real,dimension(current_data%len,3) :: species

!interface
!subroutine three_state_SS_model(y, funcv, n) 
!	use h_routine_specific_vars
!	implicit none
!	real,intent(in) :: y(*)
!	real,intent(out) :: funcv(*)
!	integer, intent(in) :: n !n=2
!end subroutine
!end interface

! params
	real,parameter :: err_tol=1E-4
	real,parameter :: y3=1.0
	real,parameter :: max_it=100
	real,parameter :: lim=1E-3
	integer,parameter :: n=2

! local variables

	REAL, DIMENSION(n) :: y
	real :: pCa
	integer :: i
!	real :: kon,koff,f,g,u,v,w,x,z,nu
!	real :: koff_p,f_p,g_p
!	real :: norm
!	real :: eta,zeta,sumy
	real :: sumy
	real :: u,v,w,x,z
!	LOGICAL :: check
	
! vars for IMSL routine

	real :: errel = 0.0001	
	real,dimension(n) :: yguess,f
	integer :: itmax=200
	real :: fnorm
	real :: xold
!	real,dimension(n) :: yguess !,fvec,yscale,fscale
	!integer,dimension(n+1) :: iparam
	!real,dimension(n) :: rparam
!	real,dimension(n) :: fvec,xscale,fscale

!	integer,dimension(6) :: iparam
!	real,dimension(5) :: rparam




! begin

! if you want to save off the time course then set: save_steps = .true.
	!	save_steps = .true. 
	tsv%kon=	param_list(current_model%model%param(1)%param_basis(1))%val
	tsv%koff=	param_list(current_model%model%param(1)%param_basis(2))%val
	tsv%f=		param_list(current_model%model%param(1)%param_basis(3))%val
	tsv%g=		param_list(current_model%model%param(1)%param_basis(4))%val
! made changes to u,v,w,x,z on 3/13/02


	u=		param_list(current_model%model%param(1)%param_basis(5))%val
	v=		param_list(current_model%model%param(1)%param_basis(6))%val
	w=		param_list(current_model%model%param(1)%param_basis(7))%val
	x=		param_list(current_model%model%param(1)%param_basis(8))%val
	z=		param_list(current_model%model%param(1)%param_basis(9))%val
	tsv%u=exp(-u)
	tsv%v=exp(-v)
	tsv%w=exp(-w)
	tsv%x=exp(-x)
	tsv%z=exp(-z)

!	nu=	param_list(current_model%model%param(3)%param_basis(1))%val

! get an initial guess for the y's
! just calculate the nullspace of the rate matrix (eliminating eqn 2 of 3)
! using mathematica

! set up imsl

!xscale(:)=1
!fscale(:)=1.0


!			y(3)=1 

	do i=1,current_data%len
		pCa=current_data%x_pts(i)
		tsv%Ca=10.**-pCa ! Ca is a global variable
		if (i==1 ) then
			y(1)=tsv%g*tsv%koff/(tsv%f*tsv%kon*tsv%Ca)
			y(2)=tsv%g/tsv%f
!		else
!			yguess=y
		endif
!		call three_state_SS_model(yguess,f,n)
!		if (sqrt(dot_product(f,f)) < 0.01) then
!			write (*,*) 'He threre'
!			y=yguess
!		else
!			CALL NEQNF (three_state_SS_model, errel, n, itmax, yguess, y, fnorm)
!		endif


		call find_zero(y)





! 		call neqbf(three_state_SS_model,n,yguess,xscale,fscale,iparam,rparam,y,fvec)
	
!		call newt(y,check)

!		call broydn(y,check)

		sumy=y(1)+y(2)+y3
		species(i,1)=y(1)/sumy 
		species(i,2)=y(2)/sumy
		species(i,3)=y3/sumy
		if (present(g_save)) g_save(i)=tsv%g*(1.+tsv%nu*tsv%nu2*species(i,3)*(tsv%w-1.))**2
	end do !i



end function



function funcv(y) 
	use h_routine_specific_vars
	implicit none
	real,dimension(2),intent(in) :: y
	real,dimension(2) :: funcv
!	integer, intent(in) :: n !n=2

! Params

	real,parameter :: y3 = 1.0 ! arbitrarily set y(3)=1

! Var's

	real :: Ca,koff,f,g

	real :: eta,zeta
	real :: sumy

! Begin

			Ca=tsv%Ca
			sumy=y(1)+y(2)+y3
			eta=tsv%nu*(y(2) + y3)/sumy
			zeta=tsv%nu*tsv%nu2*y3/sumy
			koff=tsv%koff * (1.+eta*(tsv%u-1.))**2 * (1.+zeta*(tsv%x-1.))**2
			f=tsv%f * (1.+eta*(tsv%v-1.))**2 * (1.+zeta*(tsv%z-1.))**2
			g=tsv%g	* (1.+zeta*(tsv%w-1.))**2
!koff=tsv%koff
!f=tsv%f
!g=tsv%g
			funcv(1)=-tsv%kon*Ca*y(1)+koff*y(2)
			funcv(2)=f*y(2)-g*y3
			
end function
 

subroutine find_zero(x)
	use h_params !default
	use h_utils
	use numerical_libraries, only: lsgrr
!	use nr, only: fdjac
	implicit none
	real,dimension(2),intent(inout) :: x

interface
	function funcv(y) 
		implicit none
		real,dimension(2),intent(in) :: y
		real,dimension(2) :: funcv
	end function
	SUBROUTINE fdjac(x,fvec,df)
		IMPLICIT NONE
		REAL, DIMENSION(2), INTENT(IN) :: fvec
		REAL, DIMENSION(2), INTENT(INOUT) :: x
		REAL, DIMENSION(2,2), INTENT(OUT) :: df
	end subroutine
end interface



! params
	integer,parameter :: maxit=200
	real,parameter :: tol=1E-3
	integer,parameter :: n=2

! vars
	integer :: i
	real,dimension(n) :: f,dx,xold
	real,dimension(n,n) :: df,ginva
	real :: lambda
	real :: val
	integer :: rank
! Begin
	do i=1,maxit
		f=funcv(x)
! check for convergence
		val=sqrt(dot_product(f,f))
		if (val < tol) return
		call fdjac(x,f,df) 
!		CALL LSLRG (n, df, n, -f, 1, dx)
! calculate the generalized inverse of df using SVD		
!		CALL LSGRR (NRA, NCA, A, LDA, TOL, IRANK, GINVA, LDGINV)
		CALL LSGRR (n, n, df, n,real(default%svd_tol), rank, ginva, n)
		dx=matmul(ginva,-f)
		xold=x
		lambda=1.
		do
			x=xold+lambda*dx
			if (any(x<0)) then
				lambda=lambda/2.
			else
				exit
			endif
		enddo
	enddo
	write (*,*) 'Maxit exceeded in find_zero',val

end subroutine find_zero


SUBROUTINE fdjac(x,fvec,df)
	use nrtype
	IMPLICIT NONE
	REAL, DIMENSION(2), INTENT(IN) :: fvec
	REAL, DIMENSION(2), INTENT(INOUT) :: x
	REAL, DIMENSION(2,2), INTENT(OUT) :: df
	INTERFACE
		FUNCTION funcv(x)
		IMPLICIT NONE
		REAL, DIMENSION(2), INTENT(IN) :: x
		REAL, DIMENSION(2) :: funcv
		END FUNCTION funcv
	END INTERFACE
	REAL, PARAMETER :: EPS=1.0e-4
	INTEGER(I4B) :: j,n
	REAL(SP), DIMENSION(size(x)) :: xsav,xph,h
!	n=assert_eq(size(x),size(fvec),size(df,1),size(df,2),'fdjac')
	n=size(x)
	xsav=x
	h=EPS*abs(xsav)
	where (h == 0.0) h=EPS
	xph=xsav+h
	h=xph-xsav
	do j=1,n
		x(j)=xph(j)
		df(:,j)=(funcv(x)-fvec(:))/h(j)
		x(j)=xsav(j)
	end do
	END SUBROUTINE fdjac

