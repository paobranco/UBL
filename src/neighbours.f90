!============================================================
!                                                            
!   Neighbours evaluation for a selected distance metric     
!                                                            
!============================================================

subroutine neighbours(tgtData, numData, nomData, p, k, n, nnum,&
					nnom, Cl, distm, numD, res)
!----------------------------------------------------------------
! Subroutine neighbours is used for obtaining the nearest 
! neighbours of a set of examples using a specified distance metric.
!----------------------------------------------------------------
!    tgtData(1,n) :: input, real values, the target variable values
! numData(nnum,n) :: input, real values, the nnum numeric features 
!                    values of the n cases.
! nomData(nnom,n) :: input, integer values, the nnom nominal features
!                    values (coded as integers) of the n cases.
!               p :: input, integer, code for the distance metric:
!                           p=-1 -> Canberra
!                           p=-2 -> Overlap
!                           p=-3 -> HEOM
!                           p=-4 -> HVDM
!                           p=0 -> Chebyshev
!                           p=1 -> Manhattan or 1-norm
!                           p=2 -> Euclidean or 2-norm
!                           p>2 -> p-norm
!               k :: input, integer, number of nearest neighbours
!               n :: input, integer, number of examples in the data
!            nnum :: input, integer, number of numeric attributes
!            nnom :: input, integer, number of nominal attributes
!              Cl :: input, integer, number of different classes in 
!                    the target variable values
!      distm(n,n) :: input/output, real values, distance matrix
!    numD(nnum,n) :: input/output, real values, copy of numData
!        res(k,n) :: input/output, integer, 
!----------------------------------------------------------------
! Author:: P.Branco 2015.06.01 and 2016.04.10 -------------------
!----------------------------------------------------------------
! Dependence:: Inner subroutine callpnorm; 
!              Inner subroutine callChebyshev;
!              Inner subroutine callHVDM;
!              Inner subroutine callHEOM;
!              Inner subroutine callCanberra;
!              Inner subroutine callOverlap;
!----------------------------------------------------------------
! References:: for HVDM distance:
!              Wilson, D.R. and Martinez, T.R., 1997. 
!              Improved heterogeneous distance functions.
!              Journal of artificial intelligence research, pp.1-34.
!----------------------------------------------------------------

	implicit none
	!Arguments
	integer   (kind=4), intent(in) :: p, k, n, nnum, nnom, Cl
	real      (kind=8), intent(in) :: tgtData(1,n)
	real      (kind=8), intent(in) :: numData(nnum,n)
	integer   (kind=4), intent(in) :: nomData(nnom,n)
	integer(kind=4), intent(inout) :: res(k,n)
	real   (kind=8), intent(inout) :: distm(n,n)
	real   (kind=8), intent(inout) :: numD(nnum,n)

	!Local Variables
	integer(kind=4) :: i,j,l,bestIndex
	integer(kind=4) :: used(n)
	real   (kind=8) :: bestDist
	real   (kind=8) :: ranges(nnum,2)
	real   (kind=8) :: mean(nnum), sd(nnum)

	distm=0.0d0
	numD=numData
	

	if (p >= 1) then    ! p-norm
		call callpNorm(p, distm, numD, nnum, n)
	else if (p == 0) then
		call callChebyshev(distm, numD, nnum, n)
	else if (p == -1) then
		call callCanberra(distm, numD, nnum, n)
	else if (p == -2) then ! overlap metric: only for nominal attributes
		call callOverlap(distm, nomData, nnom, n)
	else if (p == -3) then
		call callHEOM(distm, numD, nnum, nomData, nnom, n)
	else if (p == -4) then
		call callHVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
! still to be implemented
!	else if(p==-5) then
!		call callDVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
!	else if(p==-6) then
!		call callIVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
!	else if(p==-7) then
!		call callWVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
!	else if(p==-8) then
!		call callMVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
	end if

	
	
	! calculate k nearest neighbours with dist matrix
	do i=1,n
		used = 0
		used(i) = 1 ! the example is not a neighbour of himself
		do j=1,k
			bestDist = huge(bestDist)
			bestIndex = -1
			do l=1,n
				if (distm(l,i) < bestDist .and. used(l) == 0) then
					bestDist = distm(l,i)
					bestIndex = l
				end if
			end do
			used(bestIndex) = 1
			res(j,i) = bestIndex
		end do
	end do
	
	
	contains
	
subroutine callpNorm(p, distm, numD, nnum, n)
	implicit none
	!Arguments
	integer (kind=4), intent(in) :: n, nnum, p
	real    (kind=8), intent(in) :: numD(nnum,n)
	real (kind=8), intent(inout) :: distm(n,n)

	!Local Variables
	integer(kind=4) :: i,j

	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j) + pNorm(p, numD(:,j), numD(:,i), nnum)
			distm(j,i) = distm(i,j)
		end do
	end do
end subroutine callpNorm

subroutine callChebyshev(distm, numD, nnum, n)
	implicit none
	!Arguments
	integer (kind=4), intent(in) :: n, nnum
	real    (kind=8), intent(in) :: numD(nnum,n)
	real (kind=8), intent(inout) :: distm(n,n)

	!Local Variables
	integer(kind=4) :: i, j
	
	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j) + chebyshev(numD(:,j), numD(:,i), nnum)
			distm(j,i) = distm(i,j)
		end do
	end do
end subroutine callChebyshev

subroutine callHVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
	implicit none
	!Arguments
	integer(kind=4), intent(in) :: nnum, nnom, n, Cl
	real(kind=8), intent(inout) :: distm(n,n)
	real   (kind=8), intent(in) :: numD(nnum,n)
	integer(kind=4), intent(in) :: nomData(nnom,n)
	real   (kind=8), intent(in) :: tgtData(n)

	!Local Variables
	integer(kind=4) :: i,j
	real    (kind=8):: sd(nnum), mean(nnum)
	
	sd = 0.0d0
	mean = 0.0d0
	if (nnum /= 0) then
		do i=1,nnum
			mean(i) = sum(numD(i,:))/n
			sd(i) = sqrt(sum((numD(i,:)-mean(i))**2) / (n-1)) 
		end do
	end if

	do j=1,n-1
		do i=j,n
			distm(i,j) = distm(i,j) + &
						HVDM(numD(:,j), numD(:,i), sd, nomData(:,:),&
						nnum, nnom,  tgtData, n, i, j, Cl)
			distm(j,i) = distm(i,j)
		end do
	end do

end subroutine callHVDM

subroutine callOverlap(distm, nomData, nnom, n)
	implicit none
	!Arguments
	integer(kind=4), intent(in) :: n, nnom
	integer(kind=4), intent(in) :: nomData(nnom,n)
	real(kind=8), intent(inout) :: distm(n,n)

	!Local Variables
	integer :: i, j
	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j) +&
						overlap(nomData(:,j), nomData(:,i), nnom)
			distm(j,i)=distm(i,j)
		end do
	end do
end subroutine callOverlap

subroutine callCanberra(distm, numD, nnum, n)
	implicit none
	!Arguments
	integer(kind=4), intent(in) :: n, nnum
	real   (kind=8), intent(in) :: numD(nnum,n)
	real(kind=8), intent(inout) :: distm(n,n)

	!Local Variables
	integer(kind=4) :: i, j
	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j) + canberra(numD(:,j), numD(:,i), nnum)
			distm(j,i) = distm(i,j)
		end do
	end do
end subroutine callCanberra

! subroutines still to be implemented
!subroutine callWVDM(distm)
!end subroutine callWVDM

!subroutine callIVDM(distm)
!end subroutine callIVDM

!subroutine callMVDM(distm)
!end subroutine callMVDM

subroutine callHEOM(distm, numD, nnum, nomData, nnom, n)
	implicit none
	!Arguments
	integer(kind=4), intent(in) :: n, nnum, nnom
	real   (kind=8), intent(in) :: numD(nnum,n)
	integer(kind=4), intent(in) :: nomData(nnom,n)
	real(kind=8), intent(inout) :: distm(n,n)

	!Local Variables
	integer(kind=4) :: i, j
	real   (kind=8) :: ranges(nnum)

	do i=1,nnum
		ranges(i) = maxval(numD(i,:)) - minval(numD(i,:))
	end do

	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j) + sqrt(HEOMnum(numD(:,j), &
							numD(:,i), nnum, ranges) + &
							overlap(nomData(:,j), nomData(:,i), nnom))
			distm(j,i) = distm(i,j)
		end do
	end do

end subroutine callHEOM

!!functions definitions
double precision function pNorm(p,a,b,d)
	implicit none
	integer(kind=4), intent(in) :: p, d
	real   (kind=8), intent(in) :: a(d), b(d)

	pNorm = (sum((abs(a-b))**p))**(1.0/dble(p))

end function pNorm

double precision function HEOMnum(a,b,d,ranges)
	implicit none
	integer(kind=4), intent(in) :: d
	real   (kind=8), intent(in) :: a(d), b(d), ranges(d)


	HEOMnum = sum((abs(a-b)/ranges)**2)

end function HEOMnum

double precision function chebyshev(a,b,d)
	implicit none
	integer(kind=4), intent(in) :: d
	real   (kind=8), intent(in) :: a(d), b(d)


	chebyshev = maxval(abs(a-b))

end function chebyshev

double precision function overlap(a,b,d)
	implicit none
	integer(kind=4), intent(in) :: d
	integer(kind=4), intent(in) :: a(d), b(d)
	integer            (kind=4) :: i,s
	
	s = 0
	do i=1,d
		if(a(i) /= b(i))then
			s = s + 1
		end if
	end do

	overlap = s

end function overlap

double precision function canberra(a,b,d)
	implicit none
	integer(kind=4), intent(in) :: d
	real   (kind=8), intent(in) :: a(d), b(d)
	
	canberra = sum(abs(a-b)/(abs(a)+abs(b)))

end function canberra

! Heterogenous Value Difference Metric
double precision function HVDM(numa, numb, sd, nom, dimnum, dimnom, tgtData, n, i, j, Cl)
	implicit none
	integer(kind=4), intent(in) :: dimnum, dimnom, n, i, j, Cl
	real   (kind=8), intent(in) :: numa(dimnum), numb(dimnum)
	integer(kind=4), intent(in) :: nom(dimnom, n)
	real   (kind=8), intent(in) :: tgtData(n)
	real   (kind=8), intent(in) :: sd(dimnum)

	real               (kind=8) :: resnum, resnom


	if (dimnom == 0) then
		resnum = HVDMnum(numa, numb, dimnum, sd)
		resnom = 0.0d0
	else if (dimnum == 0) then
		resnum = 0.0d0 
		resnom = HVDMnom(nom, dimnom, tgtData, n, i, j, Cl)
	else
		resnum = HVDMnum(numa, numb, dimnum, sd)
		resnom = HVDMnom(nom, dimnom, tgtData, n, i, j, Cl)
	end if
	
	HVDM = sqrt(resnum + resnom)

end function HVDM

double precision function HVDMnum(numa, numb, dimnum, sd)
	implicit none
	integer(kind=4), intent(in) :: dimnum
	real   (kind=8), intent(in) :: numa(dimnum), numb(dimnum), sd(dimnum)


	integer            (kind=4) :: i

	HVDMnum = sum((abs(numa-numb)/(4*sd))**2)

end function HVDMnum

double precision function HVDMnom(nomdata, dimnom, tgtData, n, i, j, Cl)
	implicit none
	integer(kind=4), intent(in) :: dimnom, n, i, j, Cl
	integer(kind=4), intent(in) :: nomdata(dimnom, n)
	real   (kind=8), intent(in) :: tgtData(n)

	real               (kind=8) :: res
	integer            (kind=4) :: att, k, l, ci, cj, p, NCli, NClj
	real               (kind=8) :: resAt, finalresAt
	
	finalresAt = 0
	
	do att=1,dimnom
		resAt = 0
		ci = 0
		cj = 0
		do p=1,n 
			if (nomdata(att,p)==nomdata(att,i)) then
				ci = ci + 1
			end if
			if (nomdata(att,p)==nomdata(att,j)) then
				cj = cj + 1
			end if
		end do
		do k=1,Cl
			NCli = 0
			NClj = 0
			do l=1,n
				if (nomdata(att,l)==nomdata(att,i) .and. tgtData(l)==tgtData(i)) then
					NCli = NCli + 1
				end if
				if (nomdata(att,l)==nomdata(att,j) .and. tgtData(l)==tgtData(j)) then
					NClj = NClj + 1
				end if
			end do
			resAt = resAt + (abs((NCli/ci) - (NClj/cj)))**2
		end do
		finalresAt = finalresAt + resAt
	end do

	HVDMnom = finalresAt

end function HVDMnom

end subroutine neighbours
