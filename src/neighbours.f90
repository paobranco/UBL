
subroutine neighbours(tgtData, numData, nomData, p, k, n, nnum, nnom, Cl, distm, numD, res)
	implicit none
	integer, intent(in) :: p, k, n, nnum, nnom, Cl
	double precision, dimension(1,n), intent(in) :: tgtData
	double precision, dimension(nnum,n), intent(in) :: numData
	integer, dimension(nnom,n), intent(in) :: nomData
	integer, dimension(k,n), intent(inout) :: res

	integer :: i,j,l,bestIndex
	double precision, dimension(n,n), intent(inout) :: distm
	integer, dimension(n) :: used
	double precision :: bestDist
	double precision, dimension(nnum,2) :: ranges
	double precision, dimension (nnum, n), intent(inout) :: numD
	double precision, dimension (nnum) :: mean, sd
	distm=0.0d0
	numD=numData
	
!	! always normalize numeric attributes using the range if the metric 
!	! selected is not one with another type of normalization
!	if(p /= -2 .and. p < -3 .and. nnum > 0)then
!		do i=1,nnum
!			ranges(i,1) = maxval(numData(:,i))
!			ranges(i,2) = minval(numData(:,i))
!		end do
!		do i=1,nnum
!			numD(i,:)=numData(i,:)/(ranges(i,1)-ranges(i,2))
!		end do
!	end if


	if (p >= 1) then    ! p-norm
		call callpNorm(p, distm, numD, nnum, n)
	else if(p == 0) then
		call callChebyshev(distm, numD, nnum, n)
	else if(p == -1) then
		call callCanberra(distm, numD, nnum, n)
	else if(p==-2) then ! overlap metric suitable only for nominal attributes
		call callOverlap(distm, nomData, nnom, n)
	else if(p==-3)then
		call callHEOM(distm, numD, nnum, nomData, nnom, n)
	else if(p==-4) then
		call callHVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
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
		used=0
		used(i)=1 ! the example is not a neighbour of himself
		do j=1,k
			bestDist=huge(bestDist)
			bestIndex=-1
			do l=1,n
				if (distm(l,i)<bestDist .and. used(l)==0) then
					bestDist = distm(l,i)
					bestIndex=l
				end if
			end do
			used(bestIndex)=1
			res(j,i)=bestIndex
		end do
	end do
	
	
	
contains
! subroutines definitions

subroutine callpNorm(p, distm, numD, nnum, n)
	implicit none
	integer, intent(in) :: n, nnum, p
	double precision, dimension(nnum, n), intent(in) :: numD
	double precision, dimension(n,n), intent(inout) :: distm
	integer :: i,j
	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j)+pNorm(p, numD(:,j), numD(:,i), nnum)
			distm(j,i) = distm(i,j)
		end do
	end do
end subroutine callpNorm

subroutine callChebyshev(distm, numD, nnum, n)
	implicit none
	integer, intent(in) :: n, nnum
	double precision, dimension(nnum, n), intent(in) :: numD
	double precision, dimension(n,n), intent(inout) :: distm
	integer :: i, j
	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j)+chebyshev(numD(:,j), numD(:,i), nnum)
			distm(j,i) = distm(i,j)
		end do
	end do
end subroutine callChebyshev

subroutine callHVDM(distm, numD, nomData, nnum, nnom, tgtData, n, Cl)
	implicit none
	double precision, dimension(n,n), intent(inout) :: distm
	double precision, dimension(nnum, n), intent(in) :: numD
	integer, dimension(nnom, n), intent(in) :: nomData
	integer, intent(in) :: nnum, nnom, n, Cl
	double precision, dimension(n), intent(in) :: tgtData
	
	integer :: i,j
	double precision, dimension(nnum) :: sd, mean
	
	sd = 0.0d0
	mean = 0.0d0
	if (nnum /=0) then
		do i=1,nnum
			mean(i) = sum(numD(i,:))/n
			sd(i) = sqrt(sum((numD(i,:)-mean(i))**2) / (n-1)) 
		end do
	end if

	
	do j=1,n-1
		do i=j,n
			distm(i,j)=distm(i,j)+HVDM(numD(:,j), numD(:,i), sd, nomData(:,:), nnum, nnom,  tgtData, n, i, j, Cl)
			distm(j,i) = distm(i,j)
		end do
	end do

end subroutine callHVDM

subroutine callOverlap(distm, nomData, nnom, n)
	implicit none
	integer, intent(in) :: n, nnom
	integer, dimension(nnom, n), intent(in) :: nomData
	double precision, dimension(n,n), intent(inout) :: distm
	integer :: i, j
	do j=1,n-1
		do i=j+1,n
			distm(i,j)=distm(i,j)+overlap(nomData(:,j), nomData(:,i), nnom)
			distm(j,i)=distm(i,j)
		end do
	end do
end subroutine callOverlap

subroutine callCanberra(distm, numD, nnum, n)
	implicit none
	integer, intent(in) :: n, nnum
	double precision, dimension(nnum, n), intent(in) :: numD
	double precision, dimension(n,n), intent(inout) :: distm
	integer :: i, j
	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j)+canberra(numD(:,j), numD(:,i), nnum)
			distm(j,i) = distm(i,j)
		end do
	end do
end subroutine callCanberra

!subroutine callWVDM(distm)


!end subroutine callWVDM

!subroutine callIVDM(distm)


!end subroutine callIVDM

!subroutine callMVDM(distm)


!end subroutine callMVDM

subroutine callHEOM(distm, numD, nnum, nomData, nnom, n)
	implicit none
	integer, intent(in) :: n, nnum, nnom
	double precision, dimension(nnum, n), intent(in) :: numD
	integer, dimension(nnom, n), intent(in) :: nomData
	double precision, dimension(n,n), intent(inout) :: distm
	integer :: i, j
	
	double precision, dimension(nnum) :: ranges

	do i=1,nnum
		ranges(i) = maxval(numD(:,i))- minval(numD(:,i))
	end do

	do j=1,n-1
		do i=j+1,n
			distm(i,j) = distm(i,j) + sqrt(HEOMnum(numD(:,j), numD(:,i), nnum, ranges) + overlap(nomData(:,j), nomData(:,i), nnom))
			distm(j,i) = distm(i,j)
		end do
	end do

end subroutine callHEOM

!!functions definitions
double precision function pNorm(p,a,b,d)
	implicit none
	double precision, dimension(d), intent(in) :: a, b
	integer, intent(in) :: p, d
	pNorm = (sum((abs(a-b))**p))**(1.0/dble(p))
end function pNorm

double precision function HEOMnum(a,b,d,ranges)
	implicit none
	double precision, dimension(d), intent(in) :: a, b, ranges
	integer, intent(in) :: d

	HEOMnum = sum((abs(a-b)/ranges)**2)

end function HEOMnum

double precision function chebyshev(a,b,d)
	implicit none
	double precision, dimension(d), intent(in) :: a, b
	integer, intent(in) :: d
	chebyshev = maxval(abs(a-b))
end function chebyshev

double precision function overlap(a,b,d)
	implicit none
	integer, dimension(d), intent(in) :: a, b
	integer, intent(in) :: d
	integer i,s
	s=0
	do i=1,d
		if(a(i) /= b(i))then
			s=s+1
		end if
	end do
	overlap=s
end function overlap

double precision function canberra(a,b,d)
	implicit none
	double precision, dimension(d), intent(in) :: a, b
	integer, intent(in) :: d
	canberra = sum(abs(a-b)/(abs(a)+abs(b)))
end function canberra

! Heterogenous Value Difference Metric
double precision function HVDM(numa, numb, sd, nom, dimnum, dimnom, tgtData, n, i, j, Cl)
	implicit none
	double precision, dimension(dimnum), intent(in) :: numa, numb
	integer, dimension(dimnom, n), intent(in) :: nom
	integer, intent(in) :: dimnum, dimnom, n, i, j, Cl
	double precision, dimension(n), intent(in) :: tgtData
	double precision, dimension(dimnum), intent(in) :: sd
	
	double precision :: resnum, resnom
	

	if(dimnom==0) then
		resnum = HVDMnum(numa, numb, dimnum, sd)
		resnom=0.0d0
	else if (dimnum==0) then
		resnum =0.0d0 
		resnom = HVDMnom(nom, dimnom, tgtData, n, i, j, Cl)
	else
		resnum = HVDMnum(numa, numb, dimnum, sd)
		resnom = HVDMnom(nom, dimnom, tgtData, n, i, j, Cl)
	end if
	
	HVDM= sqrt(resnum+resnom)
end function HVDM

double precision function HVDMnum(numa, numb, dimnum, sd)
	implicit none
	double precision, dimension(dimnum), intent(in) :: numa, numb, sd
	integer, intent(in) :: dimnum
	
	integer :: i

	HVDMnum=sum((abs(numa-numb)/(4*sd))**2)
end function HVDMnum

double precision function HVDMnom(nomdata, dimnom, tgtData, n, i, j, Cl)
	implicit none
	integer, intent(in) :: dimnom, n, i, j, Cl
	integer, dimension(dimnom, n), intent(in) :: nomdata
	double precision, dimension(n), intent(in) :: tgtData
	double precision :: res
	
	integer :: att, k, l, ci, cj, p, NCli, NClj
	double precision :: resAt, finalresAt
	
	finalresAt=0
	
	do att=1,dimnom
		resAt=0
		ci=0
		cj=0
		do p=1,n 
			if(nomdata(att,p)==nomdata(att,i))then
				ci=ci+1
			end if
			if(nomdata(att,p)==nomdata(att,j))then
				cj=cj+1
			end if
		end do
		do k=1,Cl
			NCli = 0
			NClj = 0
			do l=1,n
				if(nomdata(att,l)==nomdata(att,i) .and. tgtData(l)==tgtData(i))then
					NCli=NCli+1
				end if
				if(nomdata(att,l)==nomdata(att,j) .and. tgtData(l)==tgtData(j))then
					NClj=NClj+1
				end if
			end do
			resAt=resAt+(abs((NCli/ci)-(NClj/cj)))**2
		end do
		finalresAt=finalresAt+resAt
	end do
	
	HVDMnom=finalresAt
end function HVDMnom



end subroutine neighbours

