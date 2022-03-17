	Program LogLog
        
        integer k,i,j,m,NOR,NOD(100),kk,ii,f,Replicate(100,100)
        integer level(100),checknumber,Confidence,NN
        real A,B,P,Q,R(100),PN(100),slevel(100,10000)
        real asigma(100,10000),N(100,10000),X(100,10000),Y(100,10000)
        real SPN,Variance,VS,Ycar(100,10000),strlev
        real noc(1000,1000),rnos(1000),rfail(1000),S(1000)
        real sumnoc,rmean(1000),rout(1000),rno(1000),rn(1000),Noi(1000)
        real Band, SPNu,SPNl,Fp,PP,termI,termII,termIII
        real fTABLE(1000,1000,100)
        real averagesigma(100,100),FN,FD,SumN,averageN(100,100)
        real averagelogN(100,100),ttable(30,100),SSE,SST,RMSE,RSQ,Nb
        real SigmaRL(10,100,100),CycleRL(10,100,100),Linearcheck,A95,B95
        real LSSE,LSST,LRSQ,LNb
        
        CHARACTER*40 NAME, NAM
        
        ! Enter the input file name and open it for reading
        CHARACTER(len = 1024) inputFile
        call get_command_argument(1, inputFile)
        if (inputFile == '') inputFile = 'input.txt'
        
        OPEN (10, FILE = inputFile)
        OPEN (20, FILE = '/dev/stdout') 
        
        
        ! Read maximum stress, cycles and residual stress
        ! Calculate number of test data and number of censored points 
        ! ##########################################################################
        
        
        !Data from ASTM E739-91
        
        Ftable(1:25,1,95)=(/161.45,18.513,10.128,7.7086,6.6079,5.9874,
        &  5.5914,
        &	5.3177,5.1174,4.9646,4.8443,4.7472,4.6672,4.6001,4.5431, 4.4940,
        &	4.4513, 4.4139, 4.3808, 4.3513, 4.3248, 4.3009, 4.2793, 4.2597,
        &	4.2417/)
        
        Ftable(1:25,2,95)=(/199.50,19.000,9.5521,6.9443,5.7861,5.1433,
        &  4.7374,
        &	4.459,4.2565,4.1028,3.9823,3.8853,3.8056,3.7389,3.6823, 3.6337,
        &	3.5915, 3.5546, 3.5219, 3.4928, 3.4668, 3.4434, 3.4221, 3.4028,
        &	3.3852/)
        
        Ftable(1:25,3,95)=(/215.71,19.164,9.2766,6.5914,5.4095,4.7571,
        &  4.3468,
        &	4.0662,3.8626,3.7083,3.5874,3.4903,3.4105,3.3439,3.2874, 3.2389,
        &	3.1968, 3.1599, 3.1274, 3.0984, 3.0725, 3.0491, 3.028, 3.0088,
        &	2.9912/)
        
        Ftable(1:25,4,95)=(/224.58,19.247,9.1172,6.3883,5.1922,4.5337,
        &  4.1203,
        &	3.8378,3.6331,3.478,3.3567,3.2592,3.1791,3.1122,3.0556, 3.0069, 
        &	2.9647, 2.9277, 2.8951, 2.8661, 2.8401, 2.8167, 2.7955, 2.7763,
        &	2.7587/)
        
        Ftable(1:25,5,95)=(/230.16,19.296,9.0135,6.2560,5.0503,4.3874,
        &  3.9715,
        &	3.6875,3.4817,3.3258,3.2039,3.1059,3.0254,2.9582,2.9013, 2.8524,
        &	2.8100, 2.7729, 2.7401, 2.7109, 2.6848, 2.6613, 2.6400, 2.6207,
        &	2.6030/)
        
        Ftable(1:25,6,95)=(/233.99,19.330,8.9406,6.1631,4.9503,4.2839,
        &  3.8660,
        &	3.5806,3.3738,3.2172,3.0946,2.9961,2.9153,2.8477,2.7905, 2.7413,
        &	2.6987, 2.6613, 2.6283, 2.5990, 2.5727, 2.5491, 2.5277, 2.5082,
        &	2.4904/)
        
        ttable(1:28,50)=(/1.0,0.8165,0.76489,0.7407,0.72669,0.71756,
        &  0.71114,
        &	0.70639,0.70272,0.69981,0.69745,0.69548,0.69384,0.69242,0.6912,
        &	0.69013,0.68919,0.68837,0.68763,0.68696,0.68635,0.6858,0.68531,
        &	0.68485,0.68443,0.68405,0.6837,0.68335/)
        
        ttable(1:28,75)=(/2.4142,1.6036,1.4226,1.3444,1.3009,1.2733,
        &  1.2543,
        &	1.24,1.229,1.2213,1.2145,1.208,1.2041,1.2,1.196,1.1937,1.191,
        &	1.188,1.186,1.1848,1.1831,1.1816,1.18,1.1789,1.1777,1.176,
        &	1.1757,1.1748/)
        
        ttable(1:28,90)=(/6.3138,2.92,2.3534,2.1318,2.015,1.9432,1.8946,
        &	1.859,1.833,1.8125,1.795,1.782,1.7709,1.7613,1.753,1.745,1.739,
        &	1.734,1.729,1.7247,1.72,1.7171,1.7139,1.7109,1.708,1.705,1.703,
        &	1.7011/)
        
        ttable(1:28,95)=(/12.706,4.3027,3.1825,2.7764,2.5706,2.4469,
        &  2.3646,
        &	2.306,2.262,2.2281,2.201,2.1788,2.1604,2.1448,2.1315,2.1199,
        &	2.1098,2.1009,2.093,2.086,2.0796,2.0739,2.0687,2.0639,2.0595,
        &	2.0555,2.0518,2.0484/)
        
        ttable(1:28,98)=(/25.452,6.2053,4.1765,3.4954,3.1634,2.9687,
        &	2.8412,2.7515,2.685,2.6338,2.5931,2.56,2.5326,2.5096,2.4899,2.
        &	4729,2.4581,2.445,2.4334,2.4231,2.4138,2.4055,2.3979,2.391,
        &	2.3846,2.3788,2.3734,2.3685/)
        
        ttable(1:28,99)=(/63.657,9.9248,5.8409,4.6041,4.0321,3.7074,
        &  3.4995,
        &	3.3554,3.2498,3.1693,3.1058,3.0545,3.0123,2.9768,2.9467,2.9208,
        &	2.8982,2.8784,2.8609,2.8453,2.8314,2.8188,2.8073,2.7969,2.7874,
        &	2.778,2.7707,2.7633/)
        
        ttable(1:28,100)=(/127.32,14.089,7.4533,5.5976,4.7733,4.3168,
        &	4.0293,3.8325,3.6897,3.5814,3.4966,3.4284,3.3725,3.3257,3.286,
        &	3.252,3.2225,3.1966,3.1737,3.1534,3.1352,3.1188,3.104,3.0905,
        &	3.0782,3.0669,3.0565,3.0469/)
        
        m=0
        k=0
        NOR=1 ! Number of (R), used as index to different Stress ratio (R)
        ! NOD(R) = Number of data for each Stress ratio (R)
        Confidence=95
        !RROUT=1e7
        
        ! Load data
        
        ! Read input file line by line
        ! R[] = Stress_ratio
        ! PN = Reliability_level
        ! slevel = Stress_level
        ! asigma = Stress_parameter
        ! N = Number_of_cycles
        ! Residual_strength unused
        
        ! X = Log10_Stress_parameter
        ! Y = Log10_Number_of_cycles
        ! Nb = 
        do
            m=m+1
            read(10,*,iostat=ierr) R(NOR),PN(NOR),slevel(NOR,m),
            &  asigma(NOR,m),N(NOR,m)
            if (ierr /= 0) then
                m=m-1
                exit
            end if
            if (m.GE.2) then
                if (PreSR.NE.R(NOR)) then
                    OldR=PreSR
                    PreSR=R(NOR)
                    R(NOR)=OldR
                    NOR=NOR+1
                    NOD(NOR-1)=m-1
                    
                    slevel(NOR,1)=slevel(NOR-1,m)
                    asigma(NOR,1)=asigma(NOR-1,m)
                    N(NOR,1)=N(NOR-1,m) 
                    m=1
                    go to 20
                end if
            end if
            
            PreSR=R(NOR)
    20	end do 
        NOD(NOR)=m
        
        
        ! For each data of each Stress (R) => calculate log10 of asigma and N
        do j=1,NOR
            do m=1,NOD(j)
                X(j,m)=log10(asigma(j,m))
                Y(j,m)=log10(N(j,m))
                WRITE(20,'X,Y=', X, Y)
            end do
        end do
        
        
        !*********************************************
        
        ! Make averages of N and Sigmas per combination of unique stress ratio and stress level 
        
        ! For each Stress(R)
        do f=1,NOR
            
            ! TODO: Check with Tassos is this part is still useful because actualy
            ! the scratch file is written but never read, the read part is commented
            OPEN (30, STATUS='SCRATCH')
            Ssigma=asigma(f,1)
            SumN=N(f,1)
            SumlogN=log10(N(f,1))
            SigmaRL(f,1,1)=asigma(f,1)
            CycleRL(f,1,1)=N(f,1)
            
            checknumber=1 ! TODO ask Tassos: Number of input data in current stress_level ?
            level(f)=0
            kk=1
            ! For each data in current Stress(R)
            do i=2,NOD(f)
                ! If Stress_level has not changed
                if (slevel(f,i).eq.slevel(f,i-1)) then
                    Ssigma=Ssigma+asigma(f,i)
                    SumN=SumN+N(f,i)
                    SumlogN=SumlogN+log10(N(f,i))
                    kk=kk+1
                    SigmaRL(f,level(f)+1,kk)=asigma(f,i)
                    CycleRL(f,level(f)+1,kk)=N(f,i)
                    checknumber=checknumber+1
                    
                    ! Stress_level has changed
                else
                    ! If more than 1 data in previous Stress_level
                    if (checknumber.GE.2) then
                        level(f)=level(f)+1 ! count Stress_level
                        Replicate(f,level(f))=checknumber
                        averagesigma(f,level(f))=Ssigma/checknumber
                        averageN(f,level(f))=SumN/checknumber
                        averagelogN(f,level(f))=SumlogN/checknumber
                        ! For each data in previous Stress_level
                        do j=i-checknumber, i-1
                            ! Write in scratch file
                            write(30,*) averagesigma(f,level(f)),N(f,j),
                            &								averageN(f,level(f))
                        end do
                    end if
                    checknumber=1
                    Ssigma=asigma(f,i)
                    SumN=N(f,i)
                    SumlogN=log10(N(f,i))
                    kk=1
                    SigmaRL(f,level(f)+1,kk)=asigma(f,i)
                    CycleRL(f,level(f)+1,kk)=N(f,i)
                end if
            end do
            
            
            !for the last stress level in each R-ratio
            level(f)=level(f)+1
            Replicate(f,level(f))=checknumber
            averagesigma(f,level(f))=Ssigma/checknumber
            averageN(f,level(f))=SumN/checknumber
            averagelogN(f,level(f))=SumlogN/checknumber
            
            
            do j=i-checknumber, i-1
                write(30,*) averagesigma(f,level(f)),N(f,j),averageN(f,level(f))
            end do
            
            !*********************************************
            
            
            ! For each Stress ratio (R) until current
            do i=1,f
                ! For each stress level
                do j=1,level(f)
                    ! ?
                    do k=1,Replicate(f,j)
                        write(30,*) i,j,k,SigmaRL(i,j,k),CycleRL(i,j,k)
                    end do
                end do
            end do
            
            
            CLOSE(UNIT=30)
            
            
            
            
            
            
            
            !m=0
            !OPEN (30, STATUS='SCRATCH')
            !do while (.NOT.EOF (10)) 
            !    m=m+1
            !	read(30,*) asigma(f,m), N(f,m)
            !end do 
            
            !strlev=0
            
            !do i=1,m,1
            
            !	checkmark=0
            !	do p=1,i-1,1
            !		if (asigma(f,i).eq.asigma(f,p)) then
            !			checkmark=1
            !		end if
            !	end do
            !	if (checkmark.NE.1) then
            !		k=0
            !		kk=1
            !		strlev=strlev+1
            !		S(strlev)=asigma(f,i)
            !		if (N(f,i).LT.rrout) then
            !			k=k+1
            !			noc(strlev,k)=N(f,i)
            !		end if
            !		do j=i+1,m,1
            !			if ((asigma(f,i)-asigma(f,j)).EQ.0) then
            !				if (N(f,j).LT.rrout) then
            !		
            !					k=k+1
            !					noc(strlev,k)=N(f,j)
            
            !				end if
            !				kk=kk+1
            !			end if
            !		end do
            !		rnos(strlev)=kk
            !		rfail(strlev)=k
            
            !	end if
            !end do
            
            !Nostrlev=strlev
            ! calculate average number of cycles at each stress level
            ! ##########################################################################
            !sumnoc=0
            !do i=1, Nostrlev
            !	do j=1, rfail(i)
            !		sumnoc=sumnoc+noc(i,j)
            !	end do
            !	rmean (i)=sumnoc/rnos(i)
            !	rout(i)=rrout /rmean(i)
            !	sumnoc=0
            !WRITE(20,*) rmean(i)
            !end do
            
            ! Linear regression
            ! Find Slope A and Intercept B
            ! ##########################################################################	
            
            Yb=0
            Xb=0
            P=0
            Q=0
            Variance=0
            VS=0
            
            ! For each data in current Stress ratio (R)
            ! X(j,m)=log10(asigma(j,m))
            ! Y(j,m)=log10(N(j,m))
            ! Calculate average of Y, X, N and log10(N)
            do i=1,NOD(f)
                Yb=Yb+Y(f,i) ! 
                Xb=Xb+X(f,i)
                Nb=Nb+N(f,i)
                LNb=LNb+log10(N(f,i))
            end do
            Xb=Xb/NOD(f)
            Yb=Yb/NOD(f)
            Nb=Nb/NOD(f)
            LNb=LNb/NOD(f)	!average of all Log(N)
            
            ! For each data in current Stress ratio (R)
            do i=1,NOD(f),1
                P=P+((X(f,i)-Xb)*(Y(f,i)-Yb))
                Q=Q+(X(f,i)-Xb)**2
            end do
            
            ! 
            B=P/Q     ! Intercept            ! Log(N)=A+B*Log(S)
            A=Yb-B*Xb ! Slope
            
            ! For each data in current Stress ratio (R)
            do i=1,NOD(f)
                Ycar(f,i)=A+B*X(f,i)
                VS=VS+(Y(f,i)-Ycar(f,i))**2 !Sum of squares due to error
                SSE=SSE+(10**Y(f,i)-10**Ycar(f,i))**2
                LSSE=LSSE+(Y(f,i)-Ycar(f,i))**2
                
                SST=SST+(10**Y(f,i)-Nb)**2 !squares about mean
                LSST=LSST+(Y(f,i)-LNb)**2
                !WRITE(20,*) X(f,i),Ycar(f,i),Y(f,i),VS
            end do
            
            Variance=sqrt(VS/(NOD(f)-2))
            RMSE=sqrt(SSE/NOD(f))		!Root mean square error
            RSQ=1-SSE/SST
            LRSQ=1-LSSE/LSST
            
            Fp=Ftable((NOD(f)-level(f)),(level(f)-2),Confidence)
            PP=2*Fp*Variance**2
            
            !CIA=Variance*(1/NOD(f)+Xb**2/Q)**(0.5)
            !CIB=Variance*Q**(-0.5)
            
            !A95P=A+ttable(NOD(f)-2,99.5)*CIA
            !B95P=B+ttable(NOD(f)-2,99.5)*CIB
            !A95M=A-ttable(NOD(f)-2,99.5)*CIA
            !B95M=B-ttable(NOD(f)-2,99.5)*CIB
            
            
            FN=0
            FD=0
            !Testing the adequacy of the Linear model
            !***************************************************************
            ! For each stress level in current Stress ratio (R)
            do i=1,level(f)
                FN=FN+(Replicate(f,i)*((A+B*log10(averagesigma(f,i)))
                &			-averagelogN(f,i))**2)
                
                ! TODO ask Tasso: For each data in current stress_level
                do ii=1,Replicate(f,i)
                    FD=FD+(log10(CycleRL(f,i,ii))-averagelogN(f,i))**2
                    !WRITE(20,*) FN,FD
                    !WRITE(20,*) ((A+B*log10(averagesigma(f,i)))-averagelogN(f,i))**2
                    !WRITE(20,*)	(log10(CycleRL(f,i,ii))-averagelogN(f,i))**2
                end do
                
            end do
            
            !WRITE(20,*)	(level(f)-2),(NOD(f)-level(f))
            Linearcheck=(FN/(level(f)-2))/(FD/(NOD(f)-level(f)))
            
            !***************************************************************
            !if (R(f).Eq.99.5) then
            !	A=A95
            !end if
            
            AA=10**(-A/B) 
            BB=-1/B       
            
            !do i=1, level(f)
            !	WRITE(20,*) level(f),Replicate(f,i)
            !end do	
            
            !do i=1, 15
            !WRITE(20,*) Ftable(i,2,95)
            !end do	
            
            
            ! Calculating and writing the fitted S-N curve based on found parameters
            ! ##########################################################################
            WRITE(20,*) '0' !Seperator
            WRITE(20,*) R(f)
            WRITE(20,*) 50
            WRITE(20,*) AA
            WRITE(20,*) BB
            WRITE(20,*) LRSQ
            WRITE(20,*) Fp				!Linearity criterion
            WRITE(20,*) Linearcheck		!Linearity index
            WRITE(20,*) RMSE			!Root mean square error
            WRITE(20,*) SSE				!Sum of squares due to errors
            WRITE(20,*) SST				!Sum of squares about the mean
            WRITE(20,*) RSQ				!R-square
            
            
            !"Data for 50% Reliability level"
            do NN=1,1000,50
                SPN=(AA)*(NN**(-BB))
                termI= (NOD(f)*Q*A*B)-(NOD(f)*Q*log10(real(NN))*B)+(PP*NOD(f)*Xb) 
                termII=sqrt(NOD(f)*PP*Q*(2*NOD(f)*A*B*Xb - 
                #	  2*NOD(f)*log10(real(NN))*B*Xb +  
                #      Q*B**2 + NOD(f)*B**2*Xb**2 + NOD(f)*log10(real(NN))**2 -  
                #      2*NOD(f)*log10(real(NN))*A - PP + NOD(f)*A**2))
                termIII=NOD(f)*(B ** 2 * Q - PP)
                SPNu=10**(-(termI+termII)/termIII)
                SPNl=10**(-(termI-termII)/termIII)
                WRITE(20,*) R(f),NN,SPN,SPNu,SPNl
            end do
            
            NN=1000
            SPN=(AA)*(NN**(-BB))
            termI= (NOD(f)*Q*A*B)-(NOD(f)*Q*log10(real(NN))*B)+(PP*NOD(f)*Xb) 
            termII=sqrt(NOD(f)*PP*Q*(2*NOD(f)*A*B*Xb - 
            #	  2*NOD(f)*log10(real(NN))*B*Xb +  
            #      Q*B**2 + NOD(f)*B**2*Xb**2 + NOD(f)*log10(real(NN))**2 -  
            #      2*NOD(f)*log10(real(NN))*A - PP + NOD(f)*A**2))
            termIII=NOD(f)*(B ** 2 * Q - PP)
            SPNu=10**(-(termI+termII)/termIII)
            SPNl=10**(-(termI-termII)/termIII)
            WRITE(20,*) R(f),NN,SPN,SPNu,SPNl
            
            
            do NN=10000,2000000,10000
                SPN=(AA)*(NN**(-BB))
                termI= (NOD(f)*Q*A*B)-(NOD(f)*Q*log10(real(NN))*B)+(PP*NOD(f)*Xb) 
                termII=sqrt(NOD(f)*PP*Q*(2*NOD(f)*A*B*Xb - 
                #	  2*NOD(f)*log10(real(NN))*B*Xb +  
                #      Q*B**2 + NOD(f)*B**2*Xb**2 + NOD(f)*log10(real(NN))**2 -  
                #      2*NOD(f)*log10(real(NN))*A - PP + NOD(f)*A**2))
                termIII=NOD(f)*(B ** 2 * Q - PP)
                SPNu=10**(-(termI+termII)/termIII)
                SPNl=10**(-(termI-termII)/termIII)
                WRITE(20,*) R(f),NN,SPN,SPNu,SPNl
            end do
            
            do NN=2000000,20000000,1000000
                SPN=(AA)*(NN**(-BB))
                termI= (NOD(f)*Q*A*B)-(NOD(f)*Q*log10(real(NN))*B)+(PP*NOD(f)*Xb) 
                termII=sqrt(NOD(f)*PP*Q*(2*NOD(f)*A*B*Xb - 
                #	  2*NOD(f)*log10(real(NN))*B*Xb +  
                #      Q*B**2 + NOD(f)*B**2*Xb**2 + NOD(f)*log10(real(NN))**2 -  
                #      2*NOD(f)*log10(real(NN))*A - PP + NOD(f)*A**2))
                termIII=NOD(f)*(B ** 2 * Q - PP)
                SPNu=10**(-(termI+termII)/termIII)
                SPNl=10**(-(termI-termII)/termIII)
                WRITE(20,*) R(f),NN,SPN,SPNu,SPNl
            end do    
            
            do NN=30000000,1400000000,100000000
                SPN=(AA)*(NN**(-BB))
                termI= (NOD(f)*Q*A*B)-(NOD(f)*Q*log10(real(NN))*B)+(PP*NOD(f)*Xb) 
                termII=sqrt(NOD(f)*PP*Q*(2*NOD(f)*A*B*Xb - 
                #	  2*NOD(f)*log10(real(NN))*B*Xb +  
                #      Q*B**2 + NOD(f)*B**2*Xb**2 + NOD(f)*log10(real(NN))**2 -  
                #      2*NOD(f)*log10(real(NN))*A - PP + NOD(f)*A**2))
                termIII=NOD(f)*(B ** 2 * Q - PP)
                SPNu=10**(-(termI+termII)/termIII)
                SPNl=10**(-(termI-termII)/termIII)
                WRITE(20,*) R(f),NN,SPN,SPNu,SPNl
            end do
            
            !		NN=1273000
            !		  SPN=(AA)*(NN**(-BB))
            !	termI= (NOD(f)*Q*A*B)-(NOD(f)*Q*log10(real(NN))*B)+(PP*NOD(f)*Xb) 
            !            termII=sqrt(NOD(f)*PP*Q*(2*NOD(f)*A*B*Xb - 
            !     #	  2*NOD(f)*log10(real(NN))*B*Xb +  
            !     #      Q*B**2 + NOD(f)*B**2*Xb**2 + NOD(f)*log10(real(NN))**2 -  
            !     #      2*NOD(f)*log10(real(NN))*A - PP + NOD(f)*A**2))
            !            termIII=NOD(f)*(B ** 2 * Q - PP)
            !    		  SPNu=10**(-(termI+termII)/termIII)
            !		  SPNl=10**(-(termI-termII)/termIII)
            !		  WRITE(20,*) R(f),NN,SPN,SPNu,SPNl		
            
        end do
        
        CLOSE(UNIT=10)  
        CLOSE(UNIT=20)
        
    end    
        
        ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        