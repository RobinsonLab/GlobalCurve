subroutine rig_err_anal()

! The task of rigorous error analysis can be distilled down to
! finding the "stream-beds" in the hyperspace of the chi_sqr error
! surface.  In other words, find where the gradient is a minimum
! on a given iso-contour.  The approach will be to calculate the eigenvectors/
! eigenvalues and take a step in the direction of of the eigenvector.
! Fall down the error surface until we reach the constraint imposed
! by fixing parameter 1.  Recalculate the eigenvectors and move another
! step up the stream-bed.