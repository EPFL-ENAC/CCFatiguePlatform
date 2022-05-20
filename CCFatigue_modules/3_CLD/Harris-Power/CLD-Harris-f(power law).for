	Program Harris
      
	USE NUMERICAL_LIBRARIES
	USE IMSLF90
	integer NOR,i,j,t
	real k1,k2,k3,p1,p2,p3,p4,Rin,Rp,CL,m,Sm,Sa,uu,ff,vv,UTS,UCS
      real maxsigma (100,10000),asigma(100,10000)
	real msigma(100,10000),N(100,10000),R(100),oldNOD(10000),ONC(100)
	real f(10000),u(10000),v(10000)
	real y(100),x1(100),x2(100),XM(100,3), YM(100,1), XMT(3,100)
	real XTX(3,3),INVXTX(3,3),XTY(3,1), Bet(3,1)

	CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
	!####################################################################
      OPEN (10, FILE = 'input.txt') 
	OPEN (20, FILE = 'output.txt') 		
	OPEN (30, FILE = 'output-1.txt')
      
	!"GIVE THE CRITICAL STRESS RATIO, static values and N "
	OPEN(100, FILE = 'staticvalue.txt') 
	READ(100,*) UCS,UTS,Rcr
	READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7)
	CLOSE(UNIT=100)

	m=0
	k=0
	NOR=1
	
	do i=1,12
	read(10,*)
	end do

	do while (.NOT.EOF (10)) 
	    m=m+1
		read(10,*) R(NOR),N(NOR,m),maxsigma(NOR,m)
		
		if (R(NOR).EQ.-1)
			R(NOR),N(NOR,m),maxsigma(NOR,m)



		if (R(NOR).EQ.0) then
			R(NOR)=PreSR
			oldNOD(NOR)=m-1
			m=0
			NOR=NOR+1
			do i=1,9
			read(10,*)
			end do
		go to 20
		end if
	PreSR=R(NOR)
20	end do 
	oldNOD(NOR)=m

	!do i=1,m
	!		WRITE(20,*) R(3),N(3,i),maxsigma(3,i)
	!end do

	if (NOR.LT.3) then
		WRITE(20,*) "Input data is not enough to apply Haris' method."
		WRITE(20,*) NOR
		do i=1,NOR
			WRITE(20,*) R(i)
		end do

		go to 10
	end if
	!####################################################################
	do j=1,NOR
	do i=1,oldNOD(j)
		if (abs(R(j)).GT.1) then
		asigma(j,i)=(1-(1/R(j)))*maxsigma(j,i)/2
		msigma(j,i)=-(1+(1/R(j)))*maxsigma(j,i)/2
		else 
		asigma(j,i)=(1-R(j))*maxsigma(j,i)/2
		msigma(j,i)=(1+R(j))*maxsigma(j,i)/2
		end if
	end do
	end do
	!Multiple Linear Regression to calculate u,v and f
	!####################################################################

	NOD=oldNOD(1)
	Apar=0.71 
	Ppar=1.05
	!Apar=0.94+0.03*log10(ONC(t))-0.016*log10(ONC(t))
	!Ppar=0.938+(8.4e-4)*exp(log10(ONC(t))/1.016)
	ff=Apar*(UCS/UTS)**(-Ppar)
	do j=1,NOD
	do i=1,NOR
	y(i)=log10(asigma(i,j)/UTS)-log10(ff)
	if ((msigma(i,j)).GT.0) then
	if ((msigma(i,j)).GT.UTS) then
	!msigma(i,j)=0.99*UTS
	end if 
	end if
	if ((msigma(i,j)).LT.0) then
	if (abs(msigma(i,j)).GT.UCS) then
	!msigma(i,j)=-0.99*UCS
	end if 
	end if
	x1(i)=log10(1-(msigma(i,j)/UTS))
	x2(i)=log10((UCS/UTS)+(msigma(i,j)/UTS))
	!XM(i,1)=1
	XM(i,1)=x1(i)
	XM(i,2)=x2(i)
	YM(i,1)=y(i)
	end do
	
	!WRITE(20,*) 'X & Y     #############################'
	!WRITE(20,*) XM(1,1),XM(1,2),XM(1,3),XM(2,1),XM(2,2)
	!WRITE(20,*) XM(2,3),XM(3,1), XM(3,2),XM(3,3)
	!WRITE(20,*) YM(1,1),YM(2,1),YM(3,1)						
	
	CALL MXTXF(100,2,XM,100,2,XTX,2)
	
	!WRITE(20,*) 'XTX     #############################'
	!WRITE(20,*) XTX(1,1),XTX(1,2),XTX(1,3),XTX(2,1),XTX(2,2)
	!WRITE(20,*) XTX(2,3),XTX(3,1), XTX(3,2),XTX(3,3)

	
	TOL=10*AMACH(6)
	CALL LSGRR(2,2,XTX,2,TOL,IRANK,INVXTX,2)
	
	!WRITE(20,*) 'INVXTX     #############################'
	!WRITE(20,*) INVXTX(1,1),INVXTX(1,2),INVXTX(1,3),INVXTX(2,1),INVXTX(2,2)
	!WRITE(20,*) INVXTX(2,3),INVXTX(3,1), INVXTX(3,2),INVXTX(3,3)
	
	CALL TRNRR(100,2,XM,100,2,100,XMT,2)
	
	!WRITE(20,*) 'XT     #############################'
	!WRITE(20,*) XMT(1,1),XMT(1,2),XMT(1,3),XMT(2,1),XMT(2,2)
	!WRITE(20,*) XMT(2,3),XMT(3,1), XMT(3,2),XMT(3,3)
	
	CALL MRRRR(2,100,XMT,2,100,1,YM,100,2,1,XTY,2)
	CALL MRRRR(2,2,INVXTX,2,2,1,XTY,2,2,1,Bet,2)
	
	!f(j)=10**Bet(1,1)
	u(j)=Bet(1,1)
	v(j)=Bet(2,1)
	WRITE(30,*) N(1,j),f(j),u(j),v(j)
	end do
	
	!Linear Regression to calculate A and B
	!####################################################################
	Xb=0
	fb=0
	ub=0
	vb=0
	Do j=1,NOD
	Xb=Xb+log10(N(1,j))
	fb=fb+f(j)
	ub=ub+u(j)
	vb=vb+v(j)
	end do
	Xb=Xb/NOD
	fb=fb/NOD
	ub=ub/NOD
	vb=vb/NOD
	
	p1=0
	p2=0
	p3=0
	q=0
	do j=1,NOD
	p1=p1+((log10(N(1,j))-Xb)*(f(j)-fb))
	p2=p2+((log10(N(1,j))-Xb)*(u(j)-ub))
	p3=p3+((log10(N(1,j))-Xb)*(v(j)-vb))
	q=q+(log10(N(1,j))-Xb)**2
	end do
	
	A1=p1/q
	A2=p2/q
	A3=p3/q
	B1=fb-A1*Xb
	B2=ub-A2*Xb
	B3=vb-A3*Xb
	
	
	do t=1,7
	var=log10(ONC(t))
	!ff = 0.016*var**4-0.2424*var**3+1.3534*var**2-3.2219*var+3.4222
	!uu = 0.0085*var**4-0.0766*var**3+0.4536*var**2-0.5521*var+0.8799
	!vv = 0.006*var**4-0.0544*var**3+0.2976*var**2-0.4061*var+0.2455

	Apar=0.71 
	Ppar=1.05
	!Apar=0.94+0.03*log10(ONC(t))-0.016*log10(ONC(t))
	!Ppar=0.938+(8.4e-4)*exp(log10(ONC(t))/1.016)
	ff=Apar*(UCS/UTS)**(-Ppar)

	uu=A2*log10(ONC(t))+B2
	vv=A3*log10(ONC(t))+B3
	

	do Sm=-UCS,UTS,(UTS+UCS)/40
	C2=ff*(1-Sm/UTS)**uu
	C3=((UCS/UTS)+(Sm/UTS))**vv
	Sa=C2*C3*UTS
	if (abs(UTS-Sm).LT.abs((UTS+UCS)/20)) then
	Sa=0
	WRITE(20,*) ONC(t),Sa,UTS
	go to 30
	end if
	write(20,*) ONC(t),Sa,Sm
	end do
	
30	end do
	
		
10	CLOSE(UNIT=10)
	CLOSE(UNIT=20)
	CLOSE(UNIT=30)
	
	END 
	
	