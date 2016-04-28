!============================================================
!   phi functions adapted to fortran90 
!   from Rita Ribeiro package uba 
!   http://www.dcc.fc.up.pt/~rpribeiro/uba/uba_0.7.5.tar.gz
!============================================================

subroutine rtophi(n, y, method, npts, lparms, phiParms, yPhi, ydPhi, yddPhi)
!----------------------------------------------------------------
! Subroutine rtophi is used for obtaining and evaluating the relevance&
! function for a set of points of the target variable provided.
!----------------------------------------------------------------
!                n :: input, integer, the nr of points in y.
!              y(n):: input, real values, the target values.
!           method :: input, integer, the method used for obtaining &
!                     the relevance (0:extremes, 1:range)
!             npts :: input, integer, the number of points in the &
!                     relevance matrix
!           lparms :: input, integer, the length of phiParms argument
! phiParms(lparms) :: input, real values, the information on phi, &
!                     i.e., the successive y, phi(y) phi'(y)
!          yPhi(n) :: input/output, real values, evaluation of phi(y)
!         ydPhi(n) :: input/output, real values, evaluation of phi'(y)
!        yddPhi(n) :: input/output, real values, evaluation of phi''(y)
!----------------------------------------------------------------
! Authors:: P.Branco, R.Ribeiro 2016.04.22.
!----------------------------------------------------------------
! Dependences:: subroutine phiInit, subroutine phiEval ----------
!----------------------------------------------------------------
! References:: Ribeiro, R., 2011. Utility-based regression 
!              (Doctoral dissertation, PhD thesis, 
!              Dep. Computer Science, Faculty of Sciences - 
!              University of Porto).
!----------------------------------------------------------------
	implicit none
	! Arguments
	integer (kind=4), intent(in) :: n, lparms, method, npts
	real   (kind=8), intent (in) :: y(n)
	real   (kind=8), intent (in) :: phiParms(lparms)
	real(kind=8), intent (inout) :: yPhi(n), ydPhi(n), yddPhi(n)
	
	!Local Variables
	real   (kind=8) :: x(npts), a(npts), b(npts), c(npts), d(npts) !,&
!						y_phi, yd_phi, ydd_phi
	integer(kind=4) :: i
	
	x=0.0d0
	a=0.0d0
	b=0.0d0
	c=0.0d0
	d=0.0d0
	i=1
	call phiSplInit(lparms, phiParms, npts, x, a, b, c, d)

	call phiEval(n, y, method, npts, x, a, b, c, d, yPhi, ydPhi, yddPhi)

contains

subroutine phiEval(n, y, method, npts, x, a, b, c, d, yPhi, ydPhi, yddPhi)
	!Arguments
	integer(kind=4), intent(in) :: n, method, npts
	real   (kind=8), intent(in) :: y(n)
	real(kind=8), intent(inout) :: yPhi(n), ydPhi(n), yddPhi(n)
	real   (kind=8), intent(in) :: x(npts), a(npts), b(npts), c(npts), d(npts)

	!Local Variables 
	integer(kind=4) :: i
	real   (kind=8) :: yval, yvald, yvaldd
	do i=1, n
		call phiSplValue(y(i), method, n, npts, x, a, b, c, d, yval, yvald, yvaldd)
		yPhi(i) = yval
		ydPhi(i) = yvald
		yddPhi(i) = yvaldd
	end do

end subroutine phiEval


subroutine phiSplInit(lparms, phiParms, npts, x, a, b, c, d)
	!Arguments
	integer (kind=4), intent(in) :: lparms, npts
	real    (kind=8), intent(in) :: phiParms(lparms)
	real (kind=8), intent(inout) :: x(npts), a(npts), b(npts), c(npts), d(npts)
	
	!Local Variables
	integer (kind=4) :: i, n
	real    (kind=8) :: newm(npts), y(npts), m(npts)
	
	do i=1, npts
		x(i) = phiParms(3 * i - 2)
		y(i) = phiParms(3 * i - 1)
		m(i) = phiParms(3 * i)
	end do
	
	newm=m
	call pchipSet(npts, x, y, m, newm, c, d)

	a=y
	b=newm

end subroutine phiSplInit


subroutine phiSplValue( y, method, n, npts, x, a, b, c, d, yval, yvald, yvaldd)
	!Arguments
	integer(kind=4), intent(in) :: method, n, npts
	real   (kind=8), intent(in) :: y, x(npts), a(npts), b(npts), c(npts), d(npts) 
	real(kind=8), intent(inout) :: yval, yvald, yvaldd

	!Local Variables
	integer(kind=4) :: extrapol ! //linear
	
	extrapol = 0
	call pchipVal(npts, x, a, b, c, d, y, extrapol, yval, yvald, yvaldd)

end subroutine phiSplValue



!============================================================
!                                                             
!    pchip: Piecewice Cubic Hermite Interpolating Polynomial  
!                                                            
!============================================================
subroutine pchipVal(npts, x, a, b, c, d, xval, extrapol, yval, yvald, yvaldd)
!----------------------------------------------------------------
! Subroutine pchipVal is used to evaluate the cubic polynomial.
! It finds, from the left, which is the interval that contains
! or is nearest to xval. 
! Check for linear extrapolation. 
! Use cubic Hermite polynomials, even for extrapolation.
!----------------------------------------------------------------
!    npts :: input, integer, nr of points in the reevance matrix
!  x(npts):: input, real values, pchip coeficients
! a(npts) :: input, real values, pchip coeficients
! b(npts) :: input, real values, pchip coeficients
! c(npts) :: input, real values, pchip coeficients
! d(npts) :: input, real values, pchip coeficients
!    xval :: input, real value, x value where the relevance function &
!            will be evaluated
!extrapol :: input, integer, used for linear extrapolation
!    yval :: input/output, real value, relevance value obtained in &
!            xval ( phi(xval) )
!   yvald :: input/output, real value, relevance derivative evaluated &
!            in xval ( phi'(xval) )
!  yvaldd :: input/output, real value, relevance second derivative &
!            in xval ( phi''(xval) )
!----------------------------------------------------------------
! Authors:: P.Branco, R.Ribeiro 2016.04.22.
!----------------------------------------------------------------
! Dependence:: subroutine findInterval.--------------------------
!----------------------------------------------------------------
! References:: Ribeiro, R., 2011. Utility-based regression 
!              (Doctoral dissertation, PhD thesis, 
!              Dep. Computer Science, Faculty of Sciences - 
!              University of Porto).
!----------------------------------------------------------------
	implicit none
	!Arguments
	integer (kind=4), intent(in) :: npts, extrapol
	real    (kind=8), intent(in) :: x(npts), a(npts), b(npts), c(npts), d(npts), xval
	real (kind=8), intent(inout) :: yval, yvald, yvaldd
	
	!Local Variables
	integer(kind=4) :: i, rightmostClosed, allInside, mfl, left
	real   (kind=8) :: s

	i = 1
	left = 1
	rightmostClosed = 0
	allInside = 0
	mfl = 0
	s=0.0d0
	! result of findInterval in variable left
	call findInterval(x, npts, xval, rightmostClosed, allInside, i,&
						mfl, left)
	i = left

	!// if extrapol is linear
	if (extrapol == 0 .and. (i == 0 .or. i == npts))then
		if (i == 0) then
			i = i + 1
		end if
		yval = a(i) + b(i) * (xval - x(i))
		yvald = b(i)
		yvaldd = 0 ! // This is the case where ydd does not exist
		return
	end if

	s = xval -x(i)
	yval = a(i) + s * (b(i) + s * (c(i) + s * d(i)))
	yvald = b(i) + s * (2 * c(i) + s * (3 * d(i)))
	yvaldd = 2 * c(i) + s * 6 * d(i)

end subroutine pchipVal


!============================================================
!                                                             
!    pchip: Piecewice Cubic Hermite Interpolating Polynomial  
!                                                            
!============================================================
subroutine pchipSet(npts, x, y, m, newm, c, d)
!----------------------------------------------------------------
! Subroutine pchip_set() is used for 
!----------------------------------------------------------------
!       npts :: input, integer, the number of points in the 
!               relevance matrix.
!    x(npts) :: input/output, real values, 
!    y(npts) :: input/output, real values, 
!    m(npts) :: input, real values, initial slopes.
! newm(npts) :: input/output, real values, adjusted slopes.
!    c(npts) :: input/output, real values, coeficients 
!    d(npts) :: input/output, real values,
!----------------------------------------------------------------
! Authors:: P.Branco, R.Ribeiro 2016.04.22.
!----------------------------------------------------------------
! Dependence:: subroutine pchipSlopeMonoFC.----------------------
!----------------------------------------------------------------
! References:: Ribeiro, R., 2011. Utility-based regression
!              (Doctoral dissertation, PhD thesis,
!              Dep. Computer Science, Faculty of Sciences
!              University of Porto).
!----------------------------------------------------------------
	implicit none
	! Arguments
	integer(kind=4), intent(in) :: npts
	real(kind=8), intent(inout) :: x(npts), y(npts), newm(npts), c(npts), d(npts)
	real   (kind=8), intent(in) :: m(npts)
	
	! Local Variables
	integer(kind=4) :: i
	real   (kind=8) :: h(npts), delta(npts)
	
	
	do i=1, npts-1
		h(i) = x(i+1) - x(i)
		delta(i) = (y(i+1) - y(i))/ h(i)
	end do
	
	call pchipSlopeMonoFC(npts, newm, delta) ! adjusts newm values
	
	do i=1, npts-1
		c(i) = (3*delta(i) - 2 * newm(i) - newm(i+1)) / h(i)
		d(i) = (newm(i) - 2 * delta(i) + newm(i+1)) / (h(i) * h(i))
	end do
	

end subroutine pchipSet


subroutine pchipSlopeMonoFC(npts, newm, delta)
!----------------------------------------------------------------
! Subroutine pchipSlopeMonoFC obtains slopes for shape-preserving
! Hermite cubic polynomials. Modify the slopes  m_k := s'(x_k)
! using Fritsch & Carlson (1980)'s algorithm.
! Adapted
!----------------------------------------------------------------
!        npts :: input, integer, the number of points in the 
!                relevance matrix.
! delta(npts) :: input, real values, pchip auxiliar coeficients.
!  newm(npts) :: input/output, real values, the slope adjusted values.
!----------------------------------------------------------------
! Authors:: P.Branco R.Ribeiro 2016.04.22.
!----------------------------------------------------------------
! Dependence:: none. --------------------------------------------
!----------------------------------------------------------------
! References:: Fritsch, F.N. and Carlson, R.E., 1980. 
!              Monotone piecewise cubic interpolation. 
!              SIAM Journal on Numerical Analysis, 17(2), pp.238-246.
!              and
!              Ribeiro, R., 2011. Utility-based regression
!              (Doctoral dissertation, PhD thesis,
!              Dep. Computer Science, Faculty of Sciences
!              University of Porto).
!----------------------------------------------------------------
	implicit none
	! Arguments
	integer(kind=4), intent(in) :: npts
	real   (kind=8), intent(in) :: delta(npts)
	real(kind=8), intent(inout) :: newm(npts)
	
	! Local Variables
	integer(kind=4) :: k, k1
	real   (kind=8) :: Sk, alpha, beta, a2b3, ab23, tauS

	do k=1,npts-1 ! modify both newm(i) and newm(i+1) if needed
		Sk = delta(k)
		k1 = k + 1
		
		if (abs(Sk) == 0) then
			newm(k) = 0.0d0
			newm(k1) = 0.0d0
		else
			alpha = newm(k) / Sk
			beta = newm(k1) / Sk
			if (abs(newm(k)) /= 0 .and. alpha < 0) then
				newm(k) = - newm(k)
				alpha = newm(k) / Sk
			end if
			
			if (abs(newm(k1)) /= 0 .and. beta < 0) then
				newm(k1) = -newm(k1)
				beta = newm(k1) / Sk
			end if
			
			a2b3 = 2 * alpha + beta - 3
			ab23 = alpha + 2 * beta - 3
			
			if (a2b3 > 0 .and. ab23 > 0 .and. &
				alpha * (a2b3 + ab23) < a2b3 * a2b3) then
				!we are outside the monotonocity region ==> fix slopes
				tauS = 3 * Sk / sqrt(alpha * alpha + beta * beta)
				newm(k) = tauS * alpha
				newm(k1) = tauS * beta
			end if
		end if
	end do

end subroutine pchipSlopeMonoFC


subroutine findInterval(xt, npts, x, rightmostClosed, allInside, ilo,&
						mflag, left)
!----------------------------------------------------------------
! Subroutine findInterval determines in which interval a case is.
! Adaptation of findInterval function in Applic.h in c to fortran 90
!----------------------------------------------------------------
!        xt(npts) :: input, real values, numeric vector of length n,&
!                    assumed to be nondecreasing
!            npts :: input, integer, length of xt
!               x :: input, real value, the point whose location with&
!                    respect to the sequence  xt  is to be determined.
! rightmostClosed :: input, integer, binary parameter setting if right&
!                    interval is closed
!       allInside :: input, integer, binary parameter setting is all &
!                    points should be inside the extremes
!             ilo :: input, integer, last iteration returned interval
!           mflag :: input/output, integer, flag for signalling if the&
!                    point is outside or outside the given points
!            left :: input/output, integer, the result obtained for the&
!                    interval index
!----------------------------------------------------------------
! Authors:: P.Branco 2016.04.22.
!----------------------------------------------------------------
! Dependences:: subroutine gotoL50, subroutine leftBoundary, subroutine&
!               rightBoundary.
!----------------------------------------------------------------
! References:: 
!----------------------------------------------------------------
	implicit none
	!Arguments
	integer    (kind=4), intent(in) :: npts, rightmostClosed, allInside
	integer (kind=4), intent(inout) :: mflag, left, ilo
	real       (kind=8), intent(in) :: xt(npts), x

	!Local Variables
	integer(kind=4) :: istep, ihi

	if (ilo <= 0) then
		if (x < xt(1)) then
			call leftBoundary(mflag, allInside, left)
			return
		end if
		ilo = 1
	end if

	ihi = ilo + 1

	if (ihi >= npts) then
		if (x >= xt(npts)) then
			call rightBoundary(xt, npts, x, mflag, allInside, &
								rightmostClosed, left)
			return
		end if
		if (npts <= 1) then
			call leftBoundary(mflag, allInside, left)
			return
		end if
		ilo = npts - 1
		ihi = npts
	end if

	if (x < xt(ihi)) then
		if (x >= xt(ilo)) then ! /* `lucky': same interval as last time */
			mflag = 0
			left = ilo
			return
		end if
		!/* **** now x < xt[ilo] .   decrease  ilo  to capture  x */
		istep = 1
		do while (ilo > 1)
			ihi = ilo
			ilo = ihi - istep
			if (x >= xt(ilo)) then
				call gotoL50(xt, npts, x, ilo, ihi, mflag, left)
				return
			end if
			istep = istep * 2
		end do
		ilo = 1
		if (x < xt(1)) then
			call leftBoundary(mflag, allInside, left)
			return
		end if

	else
		!/* **** now x >= xt[ihi] .  increase  ihi  to capture  x */
		istep = 1
		do while (ihi < npts)
			ilo = ihi
			ihi = ilo + istep
			if (ihi >= npts) then
				exit
			end if
			if (x < xt(ihi)) then
				call gotoL50(xt, npts, x, ilo, ihi, mflag, left)
				return
			end if
			istep = istep * 2
		end do
		
		if (x >= xt(npts)) then
			call rightBoundary(xt, npts, x, mflag, allInside, &
								rightmostClosed, left)
			return
		end if
		ihi = npts
	end if
	
	call gotoL50(xt, npts, x, ilo, ihi, mflag, left)

end subroutine findInterval

subroutine leftBoundary(mflag, allInside, left)
	implicit none
	!Arguments
	integer(kind=4), intent(inout) :: mflag, left
	integer   (kind=4), intent(in) :: allInside

	mflag = -1
	if (allInside == 1) then
		left = 1
	else
		left = 0
	end if
	return
end subroutine leftBoundary


subroutine rightBoundary(xt, npts, x, mflag, allInside, rightmostClosed, left)
	implicit none
	!Arguments
	integer   (kind=4), intent(in) :: npts, allInside, rightmostClosed
	integer(kind=4), intent(inout) :: mflag, left
	real      (kind=8), intent(in) :: xt(npts), x

	mflag = +1
	if (allInside == 1 .or. (rightmostClosed == 1 .and. x == xt(npts))) then
		left = npts-1
	else
		left = npts
	end if
	return
end subroutine rightBoundary

subroutine gotoL50(xt, npts, x, ilo, ihi, mflag, left)
	implicit none
	!Arguments
	integer   (kind=4), intent(in) :: npts
	integer(kind=4), intent(inout) :: mflag, left, ilo, ihi
	real      (kind=8), intent(in) :: xt(npts), x

	!Local Variables
	integer (kind=4) :: middle
	
    !* **** now xt[ilo] <= x < xt[ihi] . narrow the interval. */
	do
		middle = (ilo + ihi) / 2
		if (middle == ilo) then
			mflag = 0
			left = ilo
			return
		end if
		!/* note. it is assumed that middle = ilo in case ihi = ilo+1 . */
		if (x >= xt(middle)) then
			ilo = middle
		else
			ihi = middle
		end if
	end do
end subroutine gotoL50

end subroutine rtophi
