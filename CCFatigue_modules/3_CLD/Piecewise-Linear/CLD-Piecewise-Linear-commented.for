      Program LINEAR CLD

         integer i,j,ierr
         real k1,k2,k3,p1,p2,p3,p4,Rin,NOR,CL,m,Nu,Rs(10,1000),NofC,sa
         real maxsigma(10,1000,1000),asigma(10,1000,1000),Rtest(1000)
         real msigma(10,1000,1000),N(10,1000,1000),R(10,1000)
         real UTS,UCS,Nmid,maxsmid,maxasmid,maxmsmid,Rmid,X(10000),Y(1000)
         real ONC(1000),NOD,Xb,Yb,P,Q,A,B,AN


         CHARACTER*40 NAME, NAM

         ! Enter the input file name and open it for reading

         OPEN (10, FILE = 'input.txt')
         OPEN (20, FILE = 'output.txt')
         OPEN (200, FILE = 'Temp-input.txt')

         !"GIVE THE CRITICAL STRESS RATIO, static values and N "
         OPEN(100, FILE = 'staticvalue.txt')
         READ(100,*) UCS,UTS,Rcr ! = 27.1, 27.7, 0.1
         READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7) ! = 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9
         CLOSE(UNIT=100)

         ! Ignore input file 12 parameter lines
         do j=1,12
            read(10,*)
         end do

         i=1
         m=1
         READ(10,*) Rtest(i),Value1,Value2
         PreR=Rtest(i)
         Write(200,*) Rtest(i),Value1,Value2

         ! Copy samples (only stress_ratio, cycles_to_failure, stress) to temp file
         do
            m=m+1
            READ(10,*,iostat=ierr) Rtest(i),Value1,Value2
            if (ierr /= 0) then
               m=m-1
               exit
            end if

            ! If line = "0" => new stress ratio
            if (Rtest(i).eq.0) then
               ! Ignore param lines
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
         20 end do

         NOR=i ! Number of stress ratio
         NOD=m ! Number of data in a stress ratio

         CLOSE(UNIT=10)
         CLOSE(UNIT=200)


         OPEN (10, FILE = 'Temp-input.txt')

         p1=0 ! Count (0 <= stress ratio < 1)
         p2=0 ! Count (stress ratio < 0)
         p3=0 ! Count (stress ratio > 1)
         p4=0 ! unused

         ! For each stress ratio
         do i=1,NOR

            ! If 0 <= Stress ratio < 1
            if (Rtest(i).LT.1) then
               if (Rtest(i).GE.0) then

                  p1=p1+1
                  R(1,p1)=Rtest(i)

                  do m=1,NOD
                     READ(10,*) Nu,N(1,p1,m),maxsigma(1,p1,m)
                     ! Nu = stress_ratio
                     ! N() = cycles_to_failure
                     ! maxsigma() = stress
                     if (abs(R(1,p1)).GT.1) then
                        ! asigma = (1 - (1 / R)) * maxsigma / 2
                        asigma(1,p1,m)=(1-(1/R(1,p1)))*maxsigma(1,p1,m)/2
                        ! msigma = -(1 + (1 / R)) * maxsigma / 2
                        msigma(1,p1,m)=-(1+(1/R(1,p1)))*maxsigma(1,p1,m)/2
                     else
                        ! asigma = (1 - R) * maxsigma / 2
                        asigma(1,p1,m)=(1-R(1,p1))*maxsigma(1,p1,m)/2
                        ! msigma = (1 + R)  * maxsigma / 2
                        msigma(1,p1,m)=(1+R(1,p1))*maxsigma(1,p1,m)/2
                     end ifN

                  end do
               end if
            end if


            ! If stress ratio < 0
            if (Rtest(i).LT.0) then
               p2=p2+1
               R(2,p2)=Rtest(i)
               do m=1,NOD
                  READ(10,*) Nu,N(2,p2,m),maxsigma(2,p2,m)
                  if (abs(R(2,p2)).GT.1) then
                     asigma(2,p2,m)=(1-(1/R(2,p2)))*maxsigma(2,p2,m)/2
                     msigma(2,p2,m)=-(1+(1/R(2,p2)))*maxsigma(2,p2,m)/2
                  else
                     asigma(2,p2,m)=(1-R(2,p2))*maxsigma(2,p2,m)/2
                     msigma(2,p2,m)=(1+R(2,p2))*maxsigma(2,p2,m)/2
                  end if
               end do
            end if

            ! If stress ratio > 1
            if (Rtest(i).GT.1) then
               p3=p3+1
               R(3,p3)=Rtest(i)
               do m=1,NOD
                  READ(10,*)	Nu,N(3,p3,m),maxsigma(3,p3,m)
                  if (abs(R(3,p3)).GT.1) then
                     asigma(3,p3,m)=(1-(1/R(3,p3)))*maxsigma(3,p3,m)/2
                     msigma(3,p3,m)=-(1+(1/R(3,p3)))*maxsigma(3,p3,m)/2
                  else
                     asigma(3,p3,m)=(1-R(3,p3))*maxsigma(3,p3,m)/2
                     msigma(3,p3,m)=(1+R(3,p3))*maxsigma(3,p3,m)/2
                  end if
               end do
            end if
         end do


         ! Sorting each
         !#####################################################################
         do i=1,p1 ! for each stress_ratio in p1
            do j=i+1,p1
               if (R(1,i).LT.R(1,j)) then

                  Rmid=R(1,i)
                  R(1,i)=R(1,j)
                  R(1,j)=Rmid

                  do m=1,NOD
                     Nmid=N(1,i,m)
                     maxsmid=maxsigma(1,i,m)
                     maxasmid=asigma(1,i,m)
                     maxmsmid=msigma(1,i,m)

                     N(1,i,m)=N(1,j,m)
                     maxsigma(1,i,m)=maxsigma(1,j,m)
                     asigma(1,i,m)=asigma(1,j,m)
                     msigma(1,i,m)=msigma(1,j,m)

                     N(1,j,m)=Nmid
                     maxsigma(1,j,m)=maxsmid
                     asigma(1,j,m)=maxasmid
                     msigma(1,j,m)=maxasmid
                  end do

               end if

            end do
         end do

         ! Sorting of part 2
         !#####################################################################
         do i=1,p2
            do j=i+1,p2
               if (R(2,i).LT.R(2,j)) then
                  Rmid=R(2,i)
                  R(2,i)=R(2,j)
                  R(2,j)=Rmid

                  do m=1,NOD
                     Nmid=N(2,i,m)
                     maxsmid=maxsigma(2,i,m)
                     maxasmid=asigma(2,i,m)
                     maxmsmid=msigma(2,i,m)

                     N(2,i,m)=N(2,j,m)
                     maxsigma(2,i,m)=maxsigma(2,j,m)
                     asigma(2,i,m)=asigma(2,j,m)
                     msigma(2,i,m)=msigma(2,j,m)

                     N(2,j,m)=Nmid
                     maxsigma(2,j,m)=maxsmid
                     asigma(2,j,m)=maxasmid
                     msigma(2,j,m)=maxasmid
                  end do
               end if

            end do
         end do

         ! Sorting of part 3
         !#####################################################################
         do i=1,p3
            do j=i+1,p3
               if (R(3,i).LT.R(3,j)) then
                  Rmid=R(3,i)
                  R(3,i)=R(3,j)
                  R(3,j)=Rmid

                  do m=1,NOD
                     Nmid=N(3,i,m)
                     maxsmid=maxsigma(3,i,m)
                     maxasmid=asigma(3,i,m)
                     maxmsmid=msigma(3,i,m)

                     N(3,i,m)=N(3,j,m)
                     maxsigma(3,i,m)=maxsigma(3,j,m)
                     asigma(3,i,m)=asigma(3,j,m)
                     msigma(3,i,m)=msigma(3,j,m)

                     N(3,j,m)=Nmid
                     maxsigma(3,j,m)=maxsmid
                     asigma(3,j,m)=maxasmid
                     msigma(3,j,m)=maxasmid
                  end do
               end if

            end do
         end do
         ! ##########################################################################

         do t=1,7,1 ! for ONC in [1e3, 1e4, ..., 1e9]
            WRITE(20,*) ONC(t),0,UTS
            ! ##########################################################################
            do i=1,p1 ! for each stress_ratio in p1 group
               Yb=0
               Xb=0
               P=0
               Q=0
               do j=1,NOD,1
                  X(j)=log10(asigma(1,i,j))
                  Y(j)=log10(N(1,i,j))
                  Yb=Yb+Y(j)
                  Xb=Xb+X(j)
               end do
               Xb=Xb/NOD
               Yb=Yb/NOD
               do j=1,NOD
                  P=P+((X(j)-Xb)*(Y(j)-Yb))
                  Q=Q+(X(j)-Xb)**2
               end do
               B=P/Q
               A=Yb-B*Xb
               if (ONC(t).GT.0) then
                  sa=10**(-(A/B)+(1/B)*log10(ONC(t)))
                  sm=(1+R(1,i))*sa/(1-R(1,i))
                  !WRITE(20,*) R(1,i),ONC(t),sa,sm
                  WRITE(20,*) ONC(t),sa,sm, A, B
               end if
            end do
            ! ##########################################################################
            ! ##########################################################################
            do i=1,p2
               Yb=0
               Xb=0
               P=0
               Q=0
               do j=1,NOD,1
                  X(j)=log10(asigma(2,i,j))
                  Y(j)=log10(N(2,i,j))
                  Yb=Yb+Y(j)
                  Xb=Xb+X(j)
               end do
               Xb=Xb/NOD
               Yb=Yb/NOD
               do j=1,NOD
                  P=P+((X(j)-Xb)*(Y(j)-Yb))
                  Q=Q+(X(j)-Xb)**2
               end do
               B=P/Q
               A=Yb-B*Xb

               if (ONC(t).GT.0) then
                  sa=10**(-(A/B)+(1/B)*log10(ONC(t)))
                  sm=(1+R(2,i))*sa/(1-R(2,i))
                  !WRITE(20,*) R(2,i),ONC(t),sa,sm
                  WRITE(20,*) ONC(t),sa,sm
               end if
            end do
            ! ##########################################################################
            ! ######################################################################
            do i=1,p3
               Yb=0
               Xb=0
               P=0
               Q=0
               do j=1,NOD,1
                  X(j)=log10(asigma(3,i,j))
                  Y(j)=log10(N(3,i,j))
                  Yb=Yb+Y(j)
                  Xb=Xb+X(j)
               end do
               Xb=Xb/NOD
               Yb=Yb/NOD
               do j=1,NOD
                  P=P+((X(j)-Xb)*(Y(j)-Yb))
                  Q=Q+(X(j)-Xb)**2
               end do
               B=P/Q
               A=Yb-B*Xb

               if (ONC(t).GT.0) then
                  sa=10**(-(A/B)+(1/B)*log10(ONC(t)))
                  sm=(1+R(3,i))*sa/(1-R(3,i))
                  WRITE(20,*) ONC(t),sa,sm
               end if
            end do
            ! ##########################################################################
            WRITE(20,*) ONC(t),0,-UCS
         end do

         CLOSE(UNIT=20)
         CLOSE(UNIT=10)


      END


