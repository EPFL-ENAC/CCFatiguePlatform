	Program MinerPNL
      
	integer i,j,ierr
	real k1,k2,k3,p1,p2,p3,p4,Rin,NOR,CL,m,Nu,Rs(10,1000),NofC,sa
      real maxsigma(10,1000),asigma(10,1000),Rtest(1000)
	real msigma(10,1000),N(10,1000),R(10,1000),X(10,1000),Y(10,1000)
	real UTS,UCS,Nmid,maxsmid,maxasmid,maxmsmid,Rmid
	real ONC(1000),NOD,Xb,Yb,P,Q,A(10),B(10),AN,NR,R1,R2,R3,RR
	real Sm1,Sinf,S0, A1,B1,A2,B2,A3,B3
	real A4,B4,Samp,Smean,jj
	
     
      CHARACTER*40 NAME, NAM

      ! Enter the input file name and open it for reading
      
      OPEN (10, FILE = 'input.txt') 
	OPEN (20, FILE = 'output.txt') 	
	OPEN (200, FILE = 'Temp-input.txt') 

	!"GIVE THE CRITICAL STRESS RATIO, static values and N "
	OPEN(100, FILE = 'staticvalue.txt') 
	READ(100,*) UCS,UTS,Rcr
	READ(100,*) NR,R1,R2,R3
	READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7)
	CLOSE(UNIT=100)
	UCS=-UCS
	
	if ((NR.NE.2).AND.(NR.NE.3)) then
		WRITE(20,*) 'Number of R-ratio is not correct!'
		go to 10
	end if
	
	if (R1.NE.-1) then
		WRITE(20,*) 'First R-ratio should be -1!'
		go to 10
	end if
	
	if ((R2.NE.1000).AND.((R2.LT.1).AND.(R2.GT.-1))) then
	WRITE(20,*) 'Second R-ratio should be 
     &               R=+-infinity or R>1 or R<-1!'
	go to 10
	end if
	
	if ((R3.NE.0).AND.((R3.LT.-1).AND.(R3.GT.1))) then
		WRITE(20,*) 'Third R-ratio should be R=0 or -1<R <1!'
		go to 10
	end if

	!Contructing the R matrix (Rtest) and temporary input
	!#####################################################
	do j=1,12
		read(10,*)
	end do
	
	i=1
	m=1
	READ(10,*) Rtest(i),Value1,Value2
	PreR=Rtest(i)
	Write(200,*) Rtest(i),Value1,Value2
	
	do
	    m=m+1
	    READ(10,*,iostat=ierr) Rtest(i),Value1,Value2
            if (ierr /= 0) then
               m=m-1
               exit
            end if

		
		if (Rtest(i).eq.0) then
			do j=1,9
				read(10,*)
			end do
			Rtest(i)=PreR
			m=0
			i=i+1
			go to 20
		end if
		PreR=Rtest(i)
		Write(200,*) Rtest(i),Value1,Value2	
20	end do
	
	NOR=i		!number of R
	NOD=m		!number of data at each R
	
	CLOSE(UNIT=10)
	CLOSE(UNIT=200)
	!#####################################################

	OPEN (10, FILE = 'Temp-input.txt') 	
      
	do i=1,NOR
			
		if (Rtest(i).EQ.R1) then	!R1=-1 
			do m=1,NOD 
				READ(10,*) SR,N(1,m),maxsigma(1,m)
				asigma(1,m)=(1-R1)*maxsigma(1,m)/2
				msigma(1,m)=(1+R1)*maxsigma(1,m)/2			
			end do
		end if

		if (Rtest(i).EQ.R2) then	!R2=+-infinity or R>1 or R<-1 
			do m=1,NOD 
				READ(10,*) SR,N(2,m),maxsigma(2,m)
				asigma(2,m)=(1-(1/R2))*maxsigma(2,m)/2
				msigma(2,m)=-(1+(1/R2))*maxsigma(2,m)/2
			end do
		end if

		
		if (Rtest(i).EQ.R3) then	!R3=0 or -1<R <1 
			do m=1,NOD 
				READ(10,*) SR,N(3,m),maxsigma(3,m)
				asigma(3,m)=(1-R3)*maxsigma(3,m)/2
				msigma(3,m)=(1+R3)*maxsigma(3,m)/2
			end do
		end if		
		
		if ((Rtest(i).NE.R1).AND.(Rtest(i).NE.R2).AND.(Rtest(i).NE.R3))
     &	then													!Other R
			do m=1,NOD				
				READ(10,*) SR,N(i+3,m),maxsigma(i+3,m)
				if (abs(Rtest(i)).GT.1) then
					asigma(i+3,m)=(1-(1/Rtest(i)))*maxsigma(i+3,m)/2
					msigma(i+3,m)=-(1+(1/Rtest(i)))*maxsigma(i+3,m)/2
				else
					asigma(i+3,m)=(1-Rtest(i))*maxsigma(i+3,m)/2
					msigma(i+3,m)=(1+Rtest(i))*maxsigma(i+3,m)/2
				end if
			end do
		end if
	end do	
		
	!##############################################
	!Calculation of constants of Log(N)=A+B*Log(S)
	 !index 1:R=-1  
	 !index 2:R2=+-infinity or R>1 or R<-1
	 !index 3:R3=0 or -1<R <1
	!##############################################
	do i=1,3
		do j=1,NOD,1
			X(i,j)=log10(asigma(i,j))
			Y(i,j)=log10(N(i,j))
			Yb=Yb+Y(i,j)
			Xb=Xb+X(i,j)
		end do
		Xb=Xb/NOD
		Yb=Yb/NOD
		do j=1,NOD
			P=P+((X(i,j)-Xb)*(Y(i,j)-Yb))
			Q=Q+(X(i,j)-Xb)**2
		end do
		B(i)=P/Q	  
		A(i)=Yb-B(i)*Xb	
	end do
	WRITE(20,*) A(1),A(2),A(3),B(1),B(2),B(3)
	!###############################################
	!Output	
	!###############################################
 	OPEN (30, FILE = 'CCinput.txt')
	OPEN (40, FILE = 'Factor.txt')
	OPEN (50, FILE = 'Miner.txt')
	READ (40,*) HSF, LSF, increment

	do Fact=HSF, LSF, -increment
		Damage=0
		jj=1.0
		Sa=10000.0
		MaxStlev=-10000
		do
		READ(30,*,iostat=ierr)	Range,Smean,rratio,NofC
                if (ierr /= 0) then
                  exit
                end if
			Smean=Fact*Smean
			Range=Fact*Range
			Stlev=Smean+Range/2
			if (MaxStlev.LT.Stlev) then
				MaxStlev=Stlev
			end if
			Samp=Smean*((1-rratio)/(1+rratio))
			
			do while ((jj.LT.1000).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+1
			end do
			
			do while ((jj.GE.1000).AND.(jj.LE.10E5).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+1
			end do			
			
			do while ((jj.GE.10e5).AND.(jj.LE.10E8).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+1000
			end do
			
			do while ((jj.GE.10e8).AND.(jj.LE.10E12).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+10e6
			end do			
			
			do while ((jj.GE.10e12).AND.(jj.LE.10E14).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+10
			end do			

			do while ((jj.GE.10e14).AND.(jj.LE.10E16).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+12
			end do

			do while ((jj.GE.10e16).AND.(jj.LE.10E20).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+14
			end do
			
			do while ((jj.GE.10e20).AND.(jj.LE.10E25).AND.(Sa.GE.Samp))
				Sm1=10**((log10(jj)-A(1))/B(1))	  !R1=-1
				Sinf=10**((log10(jj)-A(2))/B(2))  !R2=+-inf, R2>1 or R2<-1
				S0=10**((log10(jj)-A(3))/B(3))    !R3=0 or -1<R3<1			
				if (((rratio.GE.-1000).AND.(rratio.LE.-1)).OR. 
     &				((rratio.GE.1).AND.(rratio.LE.1000))) then
					if (R2.EQ.1000) then	!standard boundary conditon
						A1=-Sinf
						A4=-Sinf
						B1=Sm1/2-Sinf
						B4=UCS/2+Sinf
					else if (R2.GT.1) then	!R2>1
						A4=((Sinf/(1-R2))-(UCS/(2*R2**2)))/(1/R2-1/R2**2)
						B4=UCS/2-A4
						A1=A4
						B1=Sm1/2+A1
					else					!R2<-1
						A1=((Sinf/(1-R2))-(Sm1/(2*R2**2)))/(1/R2+1/R2**2)
						B1=Sm1/2+A1
						A4=A1
						B4=UCS/2-A4				
					end if
					if ((rratio.GE.-1000).AND.(rratio.LE.-1)) then
						Sa=(1-rratio)*(A1/rratio+B1/rratio**2)
					end if
					if ((rratio.GE.1).AND.(rratio.LE.1000)) then
						Sa=(1-rratio)*(A4/rratio+B4/rratio**2)
					end if
				end if
				if ((rratio.LT.1).AND.(rratio.GT.-1)) then
					if (NR.EQ.2) then			!2 R-ratios
						A2=1/UTS-1/Sm1
						A3=1/UTS-1/Sm1
						B2=1/UTS+1/Sm1
						B3=B2
					else					!3 R-ratios
						if (R3.EQ.0) then	!standard boundary conditon R3=0
							A2=1/S0-2/Sm1
							A3=2/UTS-1/S0
							B2=1/S0
							B3=B2	
						else if ((R3.GT.0).AND.(R3.LT.1)) then	!0<R3<1
							A3=((1-R3)/S0-2/UTS)/(R3**3-1)
							B3=2/UTS-A3
							B2=B3
							A2=B2-2/Sm1	
						else									!-1<R3<0
							A2=((1-R3)/S0-2/Sm1)/(1+R3)
							B2=2/Sm1+A2
							B3=B2
							A3=(2/UTS)-B3			
						end if			
					end if
					if ((rratio.GT.-1).AND.(rratio.LE.0)) then
						Sa=(1-rratio)/(A2*rratio+B2)
					end if
					if ((rratio.GT.0).AND.(rratio.LT.1)) then
						Sa=(1-rratio)/(A3*rratio**3+B3)
					end if
				end if
				jj=jj+10e18
			end do			
			
			Damage=Damage+NofC/jj
			!WRITE (50, *) Range/Fact,NofC, jj
			jj=1.0
			Sa=10000.0			
		end do
		WRITE(50,*) MaxStlev,1/Damage
		MaxStlev=0
		Rewind(30)
	end do
	
10	R=0
	CLOSE(UNIT=20)
	CLOSE(UNIT=10)
	CLOSE(UNIT=30)	
	CLOSE(UNIT=40)
	CLOSE(UNIT=50)

	END 
	
	