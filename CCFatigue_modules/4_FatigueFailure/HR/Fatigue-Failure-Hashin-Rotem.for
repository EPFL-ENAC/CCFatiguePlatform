	Program HR
      
!	USE NUMERICAL_LIBRARIES
!	USE IMSLF90
	integer i,j
	real Dangle,OA1,OA2,TenAxStr,ComAxStr,TenTraStr,ComTraStr,ShStr
	real TenStr1,ComStr1,TenStr2,ComStr2,TenStrDes, ComStrDes
	real PI,Theta,OAngle1,OAngle2,R1,R2,R3,Rel1,Rel2,Rel3
	real Aa,Ba,At,Bt,As,Bs,A1,B1,A2,B2,Rav,Relav,Reliability
	real Sigmat,Const1,Const2,ff,ft,fs,ff1,ff2,SN,ft2,fs2,fff,Sigmas,N



	CHARACTER*40 NAME, NAM,SNmodel

      ! Enter the input file name and open it for reading
	!####################################################################
          

	OPEN (10, FILE = 'SNA.txt') 
	OPEN (20, FILE = 'SN1.txt')
	OPEN (30, FILE = 'SN2.txt')
	
	OPEN (50, FILE = 'output.txt')
		
	OPEN(100, FILE = 'Refdata.txt') 
	READ(100,*) SNmodel
	READ(100,*) Dangle,OA1,OA2
	READ(100,*) TenAxStr,ComAxStr,TenTraStr,ComTraStr,ShStr
	READ(100,*) TenStr1, ComStr1, TenStr2, ComStr2
	READ(100,*) TenStrDes, ComStrDes !Strength at deirable off-axis angle
	CLOSE(UNIT=100)
	

	PI=3.14159265
	Theta=(Dangle/180)*PI
	OAngle1=(OA1/180)*PI
	OAngle2=(OA2/180)*PI


	read(10,*)
	read(20,*)
	read(30,*)

	read(10,*) R1
	read(20,*) R2
	read(30,*) R3

	read(10,*) Rel1
	read(20,*) Rel2
	read(30,*) Rel3
	
	!log(S)=Ax+Bx*log(N)
	READ (10,*) Aa
	READ (10,*) Ba
	
	if ((OA1.EQ.0).and.(OA2.EQ.0)) then
			READ (20,*) At
			READ (20,*) Bt
			READ (30,*) As
			READ (30,*) Bs
	endif
	
	if (OA1.EQ.90) then
			READ (20,*) At
			READ (20,*) Bt
			READ (30,*) A2
			READ (30,*) B2
	endif
	
	if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90)) then
			READ (20,*) A1
			READ (20,*) B1
			READ (30,*) A2
			READ (30,*) B2
	endif

	
	
	
	Rav=(R1+R2+R3)/3
	if (Rav.EQ.R1) then
		Rratio=Rav
	else
		Rratio=1
	endif

	Relav=(Rel1+Rel2+Rel3)/3
	if (Relav.EQ.Rel1) then
		Reliability=Relav
	else
		Reliability=0
	endif



	! Calculating and writing the predicted S-N curve based on found parameters
	! ##########################################################################
	WRITE(50,*) '0' !Seperator
	WRITE(50,*) Rratio
	WRITE(50,*) Reliability
	WRITE(50,*) A1
	WRITE(50,*) B1
	WRITE(50,*) A2 !af
	WRITE(50,*) B2 !So
	WRITE(50,*) OA1 !Pwer
	WRITE(50,*) OA2 !af
	WRITE(50,*) '0' !B
	WRITE(50,*) '0' !S
	WRITE(50,*) '0' !C

	if (SNmodel.EQ.'Log-Log') then
		do N=1,1000,50
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At*N**(-Bt)
				Sigmas=As*N**(-Bs)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
			endif

			if (OA1.EQ.90) then
				Sigmat=At*N**(-Bt)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/TenStr1)**2
					ff2=((A2*N**(-B2))/TenStr2)**2
				endif
				if (Rratio.GT.1) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/ComStr1)**2
					ff2=((A2*N**(-B2))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 10
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif			
			WRITE(50,*) Rratio,N,SN
10		end do
		
		N=1000
		SN=0
		fs=0
		ft=0
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At*N**(-Bt)
				Sigmas=As*N**(-Bs)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
			endif

			if (OA1.EQ.90) then
				Sigmat=At*N**(-Bt)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/TenStr1)**2
					ff2=((A2*N**(-B2))/TenStr2)**2
				endif
				if (Rratio.GT.1) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/ComStr1)**2
					ff2=((A2*N**(-B2))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 20
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif
			WRITE(50,*) Rratio,N,SN
		!End N=1000
		
20		do N=10000,2e6,10000
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At*N**(-Bt)
				Sigmas=As*N**(-Bs)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
			endif

			if (OA1.EQ.90) then
				Sigmat=At*N**(-Bt)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/TenStr1)**2
					ff2=((A2*N**(-B2))/TenStr2)**2
				endif
				if (Rratio.GT.1) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/ComStr1)**2
					ff2=((A2*N**(-B2))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 30
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif			
			WRITE(50,*) Rratio,N,SN
30		end do


		do N=2e6,20e6,1000000
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At*N**(-Bt)
				Sigmas=As*N**(-Bs)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
			endif

			if (OA1.EQ.90) then
				Sigmat=At*N**(-Bt)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2*N**(-B2))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/TenStr1)**2
					ff2=((A2*N**(-B2))/TenStr2)**2
				endif
				if (Rratio.GT.1) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1*N**(-B1))/ComStr1)**2
					ff2=((A2*N**(-B2))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 40
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if (Rratio.GT.1) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif			
			WRITE(50,*) Rratio,N,SN
40		end do

	endif
	
	!Lin-Log

	if (SNmodel.EQ.'Lin-Log') then
		do N=1,1000,50
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At+Bt*log10(N)
				Sigmas=As+Bs*log10(N)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
					!WRITE(50,*) Rratio,N,SN
					!WRITE(50,*) At,Bt,Sigmat,TenTraStr,ft
					!WRITE(50,*) As,Bs,Sigmas,ShStr,fs
					!WRITE(50,*) TenStrDes
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
				
			endif

			if (OA1.EQ.90) then
				Sigmat=At+Bt*log10(N)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/TenStr1)**2
					ff2=((A2+B2*log10(N))/TenStr2)**2
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/ComStr1)**2
					ff2=((A2+B2*log10(N))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 50
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif			

			
			WRITE(50,*) Rratio,N,SN
			
50		end do
		
		N=1000
		SN=0
		fs=0
		ft=0
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At+Bt*log10(N)
				Sigmas=As+Bs*log10(N)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
					!WRITE(50,*) Rratio,N,SN
					!WRITE(50,*) At,Bt,Sigmat,TenTraStr,ft
					!WRITE(50,*) As,Bs,Sigmas,ShStr,fs
					!WRITE(50,*) TenStrDes
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
				
			endif

			if (OA1.EQ.90) then
				Sigmat=At+Bt*log10(N)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/TenStr1)**2
					ff2=((A2+B2*log10(N))/TenStr2)**2
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/ComStr1)**2
					ff2=((A2+B2*log10(N))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 60
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif			

			
			WRITE(50,*) Rratio,N,SN


		
60		do N=10000,2e6,10000
			SN=0
			fs=0
			ft=0
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At+Bt*log10(N)
				Sigmas=As+Bs*log10(N)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
					!WRITE(50,*) Rratio,N,SN
					!WRITE(50,*) At,Bt,Sigmat,TenTraStr,ft
					!WRITE(50,*) As,Bs,Sigmas,ShStr,fs
					!WRITE(50,*) TenStrDes
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
				
			endif

			if (OA1.EQ.90) then
				Sigmat=At+Bt*log10(N)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/TenStr1)**2
					ff2=((A2+B2*log10(N))/TenStr2)**2
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/ComStr1)**2
					ff2=((A2+B2*log10(N))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 70
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif			

			
			WRITE(50,*) Rratio,N,SN
70		end do


		do N=2e6,20e6,1000000
			SN=0
			fs=0
			ft=0
			if ((OA1.EQ.0).and.(OA2.EQ.0)) then
				Sigmat=At+Bt*log10(N)
				Sigmas=As+Bs*log10(N)
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					fs=Sigmas/ShStr	
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))					
					!WRITE(50,*) Rratio,N,SN
					!WRITE(50,*) At,Bt,Sigmat,TenTraStr,ft
					!WRITE(50,*) As,Bs,Sigmas,ShStr,fs
					!WRITE(50,*) TenStrDes
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					fs=Sigmas/ShStr	
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))		
				endif
				
			endif

			if (OA1.EQ.90) then
				Sigmat=At+Bt*log10(N)	
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					ft=Sigmat/TenTraStr
					Const1=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/TenStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					ft=Sigmat/ComTraStr
					Const1=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff=((A2+B2*log10(N))/ComStr2)**2
					fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
			endif	

			if ((OA1.NE.0).and.(OA2.NE.0).and.(OA1.NE.90))then
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					Const1=((ShStr/TenTraStr)*tan(OAngle1))**2
					Const2=((ShStr/TenTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/TenStr1)**2
					ff2=((A2+B2*log10(N))/TenStr2)**2
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					Const1=((ShStr/ComTraStr)*tan(OAngle1))**2
					Const2=((ShStr/ComTraStr)*tan(OAngle2))**2
					ff1=((A1+B1*log10(N))/ComStr1)**2
					ff2=((A2+B2*log10(N))/ComStr2)**2					
				endif
				ft2=(ff1*ff2*(Const2-Const1))/
     &				(ff1*(1+Const2)-ff2*(1+Const1))
				fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
				if (ft2.LE.0) then
					go to 80
				else
					!WRITE(50,*) Const1,Const2,ff1,ff2
					!WRITE(50,*) ft2,fs2
					ft=sqrt(ft2)
					fs=sqrt(fs2)
				endif
				if ((Rratio.GE.0).and.(Rratio.LT.1)) then
					SN=TenStrDes*fs*sqrt((1+((ShStr/TenTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/TenTraStr)
     &				*(fs/ft)*tan(Theta))**2))
				endif
				if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
					SN=ComStrDes*fs*sqrt((1+((ShStr/ComTraStr)
     &				*tan(Theta))**2)/(1+((ShStr/ComTraStr)
     &				*(fs/ft)*tan(Theta))**2))			
				endif
			endif			

			WRITE(50,*) Rratio,N,SN
80		end do

	endif	
	
	  
	!	fs=sqrt(-fs)
	CLOSE(UNIT=10)  
      CLOSE(UNIT=20)
	CLOSE(UNIT=30)  
      CLOSE(UNIT=50)
	
	End    
