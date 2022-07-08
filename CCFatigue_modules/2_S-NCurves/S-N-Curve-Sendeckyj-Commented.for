	Program SENDECKYJ
        
        integer k,p,q,i,j,NOR,NOD(100),NN,e,m,ierr
        real pstar,qstar,R(100)
        real S1,S2,SI,C1,C2,CI,S,C,sles,G,Xmax,Xmin,strlev,PN(100)
        real Sstar,Cstar,Beta,MaximumAlpha,alpha,Dalpha,maxalpha,A,B,D
        real asigma(100,10000),rs(100,10000)
        real N(100,10000),esigma(100,10000)
        real K1,K2,K3,K4,X(100,10000),sl(100,10000),SI0,CI0
        real alphaold,Cold,Sold,CCold,maxalpha1,Sstar1,Cstar1,counter
        real SIGe,Nest,MSE,RMSE,Nb,SST,RSQ,SSE,LSSE,LSST,LRSQ,LNb
        
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
        
        ! R = stress_ratio
        ! PN = reliability_level
        ! sl = stress_level
        ! asigma = stress_parameter
        ! N = number_of_cycles
        ! rs = residual_strength
        
        m=0
        NOR=1
        do
            m=m+1
            read(10,*,iostat=ierr) R(NOR),PN(NOR),sl(NOR,m),
            &	                           asigma(NOR,m),N(NOR,m),rs(NOR,m)
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
                    
                    sl(NOR,1)=sl(NOR-1,m)
                    asigma(NOR,1)=asigma(NOR-1,m)
                    N(NOR,1)=N(NOR-1,m)
                    rs(NOR,1)=rs(NOR-1,m) 
                    m=1
                    go to 20
                end if
            end if
            
            PreSR=R(NOR)
    20	end do 
        NOD(NOR)=m
        
        
        !Calculation of S-N curve for all R-ratios
        !############################################################################
        do j=1,NOR
            
            Nb=0.
            LNb=0.
            do i=1,NOD(j)
                Nb=Nb+N(j,i)
                LNb=LNb+log10(N(j,i))
            end do
            Nb=Nb/real(NOD(j)) ! Average(number_of_cycles)
            LNb=LNb/real(NOD(j)) ! Average(log10(number_of_cycles))
            
            !***********************************************************
            !Incremnet and the upper and the lower boundaries for S & C
            
            
            S1=0.001
            S2=0.5
            SI0=0.0005
            S=S1
            SI=SI0
            
            C1=0.000001
            C2=10
            CI0=0.000001
            C=C1
            CI=CI0
            
            counter=0
            
            ! Count each (stress_parameter != residual_strength)
            ! TODO: ask Tassos an input file where stress_parameter != residual_strength
            ! to test this part
            k=0
            do m=1,NOD(j)
                if (asigma(j,m).NE.rs(j,m)) then
                    k=k+1
                end if
                
            end do 

            !Find the optimum solution
            ! ##########################################################################	
            maxalpha=0
            p=1
            q=1
            alphaold=0
            Cold=0
            Sold=0
            AlphaCold=0
            do while (C.LT.C2)
                do while (S.LT.S2)
                    ! ++++++++++++++++++++++++ solving the Alpha equation +++++++++++++++++++++++++++++++++++	
                    ! Eq 1 (p248)
                    do i=1,NOD(j)
                        K1=rs(j,i)/asigma(j,i)
                        K2=K1**(1/S)
                        K3=(N(j,i)-1)*C
                        K4=K3+K2
                        esigma(j,i)=asigma(j,i)*(K4**S)
                    end do
                    
                    ! Eq 17
                    sles=0;
                    do i=1,NOD(j)-k
                        sles=sles+log(esigma(j,i))
                    end do
                    G=exp((1/(real(NOD(j))-k))*sles)
                    
                    ! Eq 16
                    do i=1,NOD(j)
                        X(j,i)=esigma(j,i)/G
                    end do
                    
                    ! Get Xmax and Xmin for Eq 20
                    Xmax=X(j,1)
                    Xmin=X(j,1)
                    do e=2,NOD(j)
                        if (Xmax.LT.X(j,e)) then
                            Xmax=X(j,e)
                        else if (Xmin.GT.X(j,e)) then
                            Xmin=X(j,e)
                        end if
                        
                    end do
                    ! Eq 20
                    alpha=log((log(real(NOD(j))/(real(NOD(j))+1)))/
                    &        (log(1/(real(NOD(j))+1))))
                    alpha=alpha/(log(Xmin/Xmax)) ! TODO ask Tassos why not Xmax / Xmin like in Eq 20
                    
                    t=0
                    
                    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++	
                    do while (t.LT.100)
                        
                        ! Eq 21 = S1
                        A=0;
                        do i=1,NOD(j)
                            A=A+(X(j,i)**alpha)
                        end do
                        
                        ! Eq 22 = S2
                        B=0
                        do i=1,NOD(j)
                            B=B+log(X(j,i))*(X(j,i)**alpha)
                        end do
                        
                        ! Eq 23 = S3
                        D=0
                        do i=1,NOD(j)
                            D=D+(log(X(j,i))**2)*(X(j,i)**alpha)
                        end do
                        
                        ! Eq 24
                        Dalpha=(A-alpha*B)/(alpha*D)
                        
                        ! Eq 25
                        if ((abs(Dalpha/alpha)).GT.0.000001) then
                            alpha=alpha+Dalpha
                        else
                            ! TODO ask Tassos: from there, regarding the Eq 25, the computations are completed
                            ! Could you describe shortly what happens next?
                            !WRITE(20,*) C,S,alpha
                            if (alpha.GT.maxalpha) then
                                maxalpha=alpha
                                !WRITE(20,*) S,Sold,alpha,alphaold,p,q
                                !alphacount(q,p)=alpha
                                pstar=p
                                qstar=q
                                Sstar=S
                                Cstar=C
                            end if
                            
                            if (p.GT.1) then
                                !WRITE(20,*) S,Sold,alpha,alphaold

                                if ((alpha-alphaold)/(S-Sold).GE.0) then
                                    ! TODO ask Tassos: SI is never changed, always 0.0005, that block of
                                    ! code seems useless
                                    if ((alpha-alphaold)
                                    &	.LT.0.00001*alpha) then
                                        SI=0.0005 
                                    else
                                        SI=SI0
                                    end if
                                    !WRITE(20,*) C,S,alpha,alphaold,p,q,SI,CI
                                    alphaold=alpha
                                    Sold=S
                                    Cold=C
                                    
                                else
                                    goto 30
                                    
                                    
                                end if
                                
                                
                            end if
                            
                            t=101
                            
                        end if
                        
                    end do
                    
                    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                    p=p+1
                    S=S+SI
                end do
                
                
    30			if (q.GT.1) then
                    !	!WRITE(20,*) '*****************'
                    !	!WRITE(20,*) alpha,alphaCold
                    if ((alphaold-alphaCold)/(C-CCold).GE.0) then
                        if ((alphaold-alphaCold).LT.0.01*maxalpha) then
                            CI=0.000001
                        end if					
                        if ((alphaold-alphaCold).LT.0.001*maxalpha) then
                            CI=0.00001
                        end if
                        if ((alphaold-alphaCold).LT.0.0001*maxalpha) then
                            CI=0.0001
                        end if
                        if ((alphaold-alphaCold).LT.0.00001*maxalpha) then
                            CI=0.001
                        end if
                        if ((alphaold-alphaCold).LT.0.000001*maxalpha) then
                            CI=0.01
                        end if
                        if ((alphaold-alphaCold).LT.0.0000001*maxalpha) then
                            CI=0.1
                        end if
                        counter=0
                        
                    else
                        counter=counter+1
                        if (counter.GT.500) then
                            goto 40
                        end if
                        
                    end if
                end if	
                p=1
                q=q+1
                alphaCold=maxalpha
                CCold=C
                !WRITE(20,*) q,p,C,S
                alphaold=0
                Sold=0
                Cold=0
                SI=SI0
                C=C+CI
                S=S1
            end do
            
            ! Calculation of Beta based on optimum Alpha and corresponding C and S
            ! ##########################################################################
            
    40		MaximumAlpha=maxalpha
            
            ! Eq 1 (p248)
            do i=1,NOD(j)
                K1=rs(j,i)/asigma(j,i)
                K2=K1**(1/Sstar)
                K3=(N(j,i)-1)*Cstar
                K4=K3+K2
                esigma(j,i)=asigma(j,i)*(K4**Sstar)
                !WRITE(20,*) esigma(j,i)
            end do
            
            ! Eq 17
            sles=0 ! sles: Sum log(esigma)
            do i=1,NOD(j)-k
                sles=sles+log(esigma(j,i))
            end do
            G=exp((1/(real(NOD(j))-k))*sles)
            
            ! Eq 19
            do i=1,NOD(j)
                X(j,i)=esigma(j,i)/G
            end do
            A=0
            do i=1,NOD(j)
                A=A+(X(j,i)**maxalpha)
            end do
            Beta=G*((1/(real(NOD(j))-k))*A)**(1/maxalpha)
            
            ! Eq ??? TODO ask Tassos which Equation
            ! TODO ask Tassos description or explicit name for these variable:
            ! SIGe, Nest, SSE, LSSE, SST, LSST, LRSQ
            SIGe=Beta*((-log(PN(j)/100))**(1/MaximumAlpha))
            do i=1,NOD(j)
                Nest=(1/Cstar)*((SIGe/asigma(j,i))**(1/Sstar)+Cstar-1)
                if (Nest.LT.1) then
                    Nest=1
                end if
                SSE=SSE+(N(j,i)-Nest)**2
                LSSE=LSSE+(log10(N(j,i))-log10(Nest))**2
                
                SST=SST+(N(j,i)-Nb)**2
                LSST=LSST+(log10(N(j,i))-log10(Nb))**2
            end do
            RMSE=sqrt(SSE/NOD(j))		!Root Mean Square Error
            RSQ=1-SSE/SST				!R-squared
            LRSQ=1-LSSE/LSST
            
            ! Calculating and writing the fitted S-N curve based on found parameters
            ! ##########################################################################
            A=-(1-Cstar)/Cstar;
            WRITE(20,*) '0'				!Seperator
            WRITE(20,*) R(j)			!R-ratio
            WRITE(20,*) PN(j)			!Reliability level
            WRITE(20,*) MaximumAlpha	!af (shape parameter)
            WRITE(20,*) Beta			!Beta (scale paprameter)
            WRITE(20,*) Sstar			!S (wearout model parameter)
            WRITE(20,*) Cstar			!C (wearout model parameter)
            WRITE(20,*) LRSQ
            WRITE(20,*) RMSE			!Root mean square error
            WRITE(20,*) SSE				!Sum of squares due to errors
            WRITE(20,*) SST				!Sum of squares about the mean
            WRITE(20,*) RSQ				!R-square
            
            !	"Data for 50% Reliability level"	
            
            !if (PN(j).eq.50) then
            
            ! TODO ask Tassos: which Eq?
            do NN=1,1000,50
                K1=(-log(PN(j)/100))**(1/MaximumAlpha)
                K2=1/((NN-A)*Cstar)
                K3=K2**(Sstar)
                K4=K1*K3
                SPN=Beta*K4
                WRITE(20,*) R(j),NN,SPN
            end do
            
            NN=1000
            K1=(-log(PN(j)/100))**(1/MaximumAlpha)
            K2=1/((NN-A)*Cstar)
            K3=K2**(Sstar)
            K4=K1*K3
            SPN=Beta*K4
            WRITE(20,*) R(j),NN,SPN
            
            
            do NN=10000,2000000,10000
                K1=(-log(PN(j)/100))**(1/MaximumAlpha)
                K2=1/((NN-A)*Cstar)
                K3=K2**(Sstar)
                K4=K1*K3
                SPN=Beta*K4
                WRITE(20,*) R(j),NN,SPN
            end do
            
            do NN=3000000,20000000,1000000
                K1=(-log(PN(j)/100))**(1/MaximumAlpha)
                K2=1/((NN-A)*Cstar)
                K3=K2**(Sstar)
                K4=K1*K3
                SPN=Beta*K4
                WRITE(20,*) R(j),NN,SPN
            end do     
            
            do NN=30000000,1400000000,100000000
                K1=(-log(PN(j)/100))**(1/MaximumAlpha)
                K2=1/((NN-A)*Cstar)
                K3=K2**(Sstar)
                K4=K1*K3
                SPN=Beta*K4
                WRITE(20,*) R(j),NN,SPN
            end do     
            
            !end if
            
            !	"Data for 95% Reliability level"
            !if (PN(j).eq.95) then
            !	do NN=1,1000,50
            !		K1=(-log(0.95))**(1/MaximumAlpha)
            !		K2=1/((NN-A)*Cstar)
            !		K3=K2**(Sstar)
            !		K4=K1*K3
            !		SPN=Beta*K4
            !		WRITE(20,*) R(j),NN,SPN
            !	end do
            !
            !	NN=1000
            !	K1=(-log(0.95))**(1/MaximumAlpha)
            !	K2=1/((NN-A)*Cstar)
            !	K3=K2**(Sstar)
            !	K4=K1*K3
            !	SPN=Beta*K4
            !	WRITE(20,*) R(j),NN,SPN
            
            !	do NN=10000,2e6,10000
            !		K1=(-log(0.95))**(1/MaximumAlpha)
            !		K2=1/((NN-A)*Cstar)
            !		K3=K2**(Sstar)
            !		K4=K1*K3
            !		SPN=Beta*K4
            !		WRITE(20,*) R(j),NN,SPN
            !	end do
            
            !	do NN=2e6,20e6,1000000
            !		K1=(-log(0.95))**(1/MaximumAlpha)
            !		K2=1/((NN-A)*Cstar)
            !		K3=K2**(Sstar)
            !		K4=K1*K3
            !		SPN=Beta*K4
            !		WRITE(20,*) R(j),NN,SPN
            !w=sqrt(-Beta)
            !	end do    
            !end if
            
        end do
        CLOSE(UNIT=10)  
        CLOSE(UNIT=20)
        
        end    
        
        ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        