
subroutine neighbours(flag, codeAt, tgtData, numData, nomData, p, k, n, nnum, nnom, Cl, dist, numD, res)
	implicit none
	integer, intent(in) :: flag, codeAt, p, k, n, nnum, nnom, Cl
	double precision, dimension(1,n), intent(in) :: tgtData
	double precision, dimension(nnum,n), intent(in) :: numData
	integer, dimension(nnom,n), intent(in) :: nomData
	integer, dimension(k,n), intent(inout) :: res

	integer :: i,j,l,bestIndex
	double precision, dimension(n,n), intent(inout) :: dist
	integer, dimension(n) :: used
	double precision :: bestDist
	double precision, dimension(nnum,2) :: ranges
	double precision, dimension (nnum, n), intent(inout) :: numD
	double precision, dimension (nnum) :: mean, sd
	dist=0
	numD=0
	
	! normalize numeric attributes
	do i=1,nnum
		ranges(i,1) = maxval(numData(:,i))
		ranges(i,2) = minval(numData(:,i))
	end do
	
	do i=1,nnum
		numD(i,:)=numData(i,:)/(ranges(i,1)-ranges(i,2))
	end do
	
	if(codeAt==0 .or. codeAt==2)then
		!compute the required distance
		if (p >= 1) then    ! p-norm
			do j=1,n-1
				do i=j+1,n
					dist(i,j) = pNorm(p, numD(:,j), numD(:,i),nnum)
					dist(j,i) = dist(i,j)
				end do
			end do
		else if(p == 0) then    ! Chebyshev
			do j=1,n-1
				do i=j+1,n
					dist(i,j) = chebyshev(numD(:,j), numD(:,i),nnum)
					dist(j,i) = dist(i,j)
				end do
			end do
		else    ! p==-1 HVDM
			do i=1,nnum
				mean(i) = sum(numD(i,:))/n
				sd(i) = sqrt(sum((numD(i,:)-mean(i))**2) / n) 
			end do
			do j=1,n-1
				do i=j,n
					dist(i,j)=HVDM(numD(:,j), numD(:,i), nomData(:,:), nnum, nnom, sd, tgtData, n, i, j, Cl)
				end do
			end do
		end if
	end if
	
	

	if(codeAt == 1 .or. codeAt==2) then
		! overlap metric for nominal attributes
		do j=1,n-1
			do i=j+1,n
				dist(i,j)=dist(i,j)+overlap(nomData(:,j), nomData(:,i),nnom)
				dist(j,i)=dist(i,j)
			end do
		end do
	end if
	
	
	
	! calculate k nearest neighbours with dist matrix
	do i=1,n
		used=0
		used(i)=1 ! the example is not a neighbour of himself
		do j=1,k
			bestDist=huge(bestDist)
			bestIndex=-1
			do l=1,n
				if (dist(l,i)<bestDist .and. used(l)==0) then
					bestDist = dist(l,i)
					bestIndex=l
				end if
			end do
			used(bestIndex)=1
			res(j,i)=bestIndex
		end do
	end do
	
	
	
contains
double precision function pNorm(p,a,b,d)
	implicit none
	double precision, dimension(d), intent(in) :: a, b
	integer, intent(in) :: p, d
	pNorm = (sum((abs(a-b))**p))**(1.0/dble(p))
end function pNorm


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

! Heterogenous Value Difference Metric
double precision function HVDM(numa, numb, nom, dimnum, dimnom, sd, tgtData, n, i, j, Cl)
	implicit none
	double precision, dimension(dimnum), intent(in) :: numa,numb, sd
	integer, dimension(dimnom, n), intent(in) :: nom
	integer, intent(in) :: dimnum, dimnom, n, i, j, Cl
	double precision, dimension(n), intent(in) :: tgtData
	double precision :: resnum, resnom
	
	resnum = HVDMnum(numa, numb,dimnum,sd)
	resnom = HVDMnom(nom, dimnom, tgtData, n, i, j, Cl)
	
	HVDM= sqrt(resnum+resnom)
end function HVDM


double precision function HVDMnum(numa, numb, dimnum, sd)
	implicit none
	double precision, dimension(dimnum), intent(in) :: numa, numb, sd
	integer, intent(in) :: dimnum
	HVDMnum=sum((abs(numa-numb)/(4*sd))**2)
end function HVDMnum


double precision function HVDMnom(nomdata, dimnom, tgtData, n, i, j, Cl)
	implicit none
	integer, intent(in) :: dimnom, n, i, j, Cl
	integer, dimension(dimnom, n), intent(in) :: nomdata
	double precision, dimension(n), intent(in) :: tgtData
	double precision :: res
	
	integer :: att,k, ci, cj, p, NCli, NClj
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


!TODO: implement the following metrics in the paper Wilson & Martinez, 1997, Improved Heterogeneous Distance Functions):
!HEOM(Heterogenous Euclide-Overlap Metric) 
!DVDM(Discretized Value Difference Metric)
!IVDM(Interpolated Value Difference Metric)
!WVDM(Windowed Value Difference Metric)


end subroutine neighbours

