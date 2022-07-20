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
         real Range,Smean,RR,nn,NNN,Cumulative,Fact,HSF,LSF,STEP
         real(8) jj,Damage

         CHARACTER*40 NAME, NAM

         ! Enter the input file name and open it for reading
         !####################################################################
         OPEN (10, FILE = 'input.txt')

         !"GIVE THE CRITICAL STRESS RATIO, static values and N "
         OPEN(100, FILE = 'staticvalue.txt')
         READ(100,*) UCS,UTS,Rcr ! = 27.1, 27.7, 0.1
         READ(100,*) ONC(1),ONC(2),ONC(3),ONC(4),ONC(5),ONC(6),ONC(7) ! = 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9
         CLOSE(UNIT=100)

         m=0
         k=0
         NOR=1		!number of R-ratios

         !####################################################################
         !Reading the input file and counting of number of R-ratios

         ! Ignore the 12 param lines
         do i=1,12
            read(10,*)
         end do
         do while (.NOT.EOF (10))
            m=m+1
            read(10,*) R(NOR),N(NOR,m),maxsigma(NOR,m)

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

         !####################################################################
         !Calculate the amplitude and mean stress
         do j=1,NOR ! For each stress_ratios
            do i=1,oldNOD(j) ! For each sample in stress_ratio
               if (abs(R(j)).GT.1) then
                  asigma(j,i)=(1-(1/R(j)))*maxsigma(j,i)/2
                  msigma(j,i)=-(1+(1/R(j)))*maxsigma(j,i)/2
               else
                  asigma(j,i)=(1-R(j))*maxsigma(j,i)/2
                  msigma(j,i)=(1+R(j))*maxsigma(j,i)/2
               end if
            end do
         end do

         !####################################################################
         !Multiple Linear Regression to calculate u,v and f
         !Harris model: a=f*(1-m)^u*(c+m)^v
         !Log(a)=Log(f)+uLog(1-m)+vLog(c+m) or in the following formulation y=Log(f)+u*x1+v*x2

         NOD=oldNOD(1)
         do j=1,NOD ! For each sample in first stress_ratio !!!
            do i=1,NOR ! For each stress_ratio

               y(i) = log10(asigma(i,j) / UTS)

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
               XM(i,1)=1
               XM(i,2)=x1(i)
               XM(i,3)=x2(i)
               YM(i,1)=y(i)
            end do

            !WRITE(20,*) 'X & Y     #############################'
            !WRITE(20,*) XM(1,1),XM(1,2),XM(1,3),XM(2,1),XM(2,2)
            !WRITE(20,*) XM(2,3),XM(3,1), XM(3,2),XM(3,3)
            !WRITE(20,*) YM(1,1),YM(2,1),YM(3,1)

            ! https://help.imsl.com/fortran/6.0/math/default.htm?turl=mxtxf.htm
            ! XM(100, 3)
            ! XTX(3, 3)
            ! Fortran 77 => CALL MXTXF (NRA, NCA, A, LDA, NB, B, LDB)
            CALL MXTXF(100,3,XM,100,3,XTX,3)	!Computes the transpose product of a matrix, ATA. For inversion matrix has to be a squared matrix!

            !WRITE(20,*) 'XTX     #############################'
            !WRITE(20,*) XTX(1,1),XTX(1,2),XTX(1,3),XTX(2,1),XTX(2,2)
            !WRITE(20,*) XTX(2,3),XTX(3,1), XTX(3,2),XTX(3,3)


            ! https://help.imsl.com/fortran/6.0/math/default.htm?turl=lsgrr.htm
            ! Fortran 77 => CALL LSGRR (NRA, NCA, A, LDA, TOL, IRANK, GINVA, LDGINV)
            TOL=10*AMACH(6)
            CALL LSGRR(3,3,XTX,3,TOL,IRANK,INVXTX,3)	!Computes the generalized inverse of a real matrix

            !WRITE(20,*) 'INVXTX     #############################'
            !WRITE(20,*) INVXTX(1,1),INVXTX(1,2),INVXTX(1,3),INVXTX(2,1),INVXTX(2,2)
            !WRITE(20,*) INVXTX(2,3),INVXTX(3,1), INVXTX(3,2),INVXTX(3,3)


            ! https://help.imsl.com/fortran/current/html/fnlmath/index.html#page/FNLMath/mch9.11.19.html
            ! Fortran 77 => 	CALL TRNRR (NRA, NCA, A, LDA, NRB, NCB, B, LDB)
            CALL TRNRR(100,3,XM,100,3,100,XMT,3)		!Transposes a rectangular matrix

            !WRITE(20,*) 'XT     #############################'
            !WRITE(20,*) XMT(1,1),XMT(1,2),XMT(1,3),XMT(2,1),XMT(2,2)
            !WRITE(20,*) XMT(2,3),XMT(3,1), XMT(3,2),XMT(3,3)

            ! https://help.imsl.com/fortran/6.0/math/default.htm?turl=mrrrr.htm
            ! Fortran 77 => CALL MRRRR (NRA, NCA, A, LDA, NRB, NCB, B, LDB, NRC, NCC, C, LDC)
            CALL MRRRR(3,100,XMT,3,100,1,YM,100,3,1,XTY,3)		!Multiplies two real rectangular matrices, AB.
            CALL MRRRR(3,3,INVXTX,3,3,1,XTY,3,3,1,Bet,3)

            f(j)=10**Bet(1,1)
            u(j)=Bet(2,1)
            v(j)=Bet(3,1)
            !WRITE(20,*) f(j),u(j),v(j)
         end do

         !Linear Regression to calculate A and B
         !f=ALog(N)+B and ...
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

         A1=p1/q		!for f
         A2=p2/q		!for u
         A3=p3/q		!for v
         B1=fb-A1*Xb
         B2=ub-A2*Xb
         B3=vb-A3*Xb


         10	CLOSE(UNIT=10)


         !Miner part
         !####################################################################
         OPEN (30, FILE = 'CCInput.txt')
         OPEN (40, FILE = 'Factor.txt')
         OPEN (50, FILE = 'Miner.txt')
         READ (40,*) HSF, LSF, STEP
         do Fact=HSF, LSF, -STEP

            Damage=0
            jj=1.0
            Sa=10000.0
            do while (.NOT.EOF (30))
               READ (30,*) Range, Smean, RR, nn, Cumulative

               Smean=Fact*Smean
               Range=Fact*Range
               Samp=Smean*((1-RR)/(1+RR))
               Stlev=Smean+Range/2
               if (MaxStlev.LT.Stlev) then
                  MaxStlev=Stlev
               end if
               do while ((jj.LT.1000).AND.(Sa.GE.Samp))

                  ff=A1*log10(jj)+B1
                  uu=A2*log10(jj)+B2
                  vv=A3*log10(jj)+B3
                  C2=ff*(1-Smean/UTS)**uu
                  C3=((UCS/UTS)+(Smean/UTS))**vv
                  Sa=C2*C3*UTS
                  jj=jj+1
               end do

               do while ((jj.GE.1000).AND.(jj.LE.10E5).AND.(Sa.GE.Samp))

                  ff=A1*log10(jj)+B1
                  uu=A2*log10(jj)+B2
                  vv=A3*log10(jj)+B3
                  C2=ff*(1-Smean/UTS)**uu
                  C3=((UCS/UTS)+(Smean/UTS))**vv
                  Sa=C2*C3*UTS
                  jj=jj+10
               end do

               do while ((jj.GE.10E5).AND.(jj.LT.10E8).AND.(Sa.GE.Samp))

                  ff=A1*log10(jj)+B1
                  uu=A2*log10(jj)+B2
                  vv=A3*log10(jj)+B3
                  C2=ff*(1-Smean/UTS)**uu
                  C3=((UCS/UTS)+(Smean/UTS))**vv
                  Sa=C2*C3*UTS
                  jj=jj+10000
               end do

               do while ((jj.GE.10E8).AND.(jj.LT.10E12).AND.(Sa.GE.Samp))

                  ff=A1*log10(jj)+B1
                  uu=A2*log10(jj)+B2
                  vv=A3*log10(jj)+B3
                  C2=ff*(1-Smean/UTS)**uu
                  C3=((UCS/UTS)+(Smean/UTS))**vv
                  Sa=C2*C3*UTS
                  jj=jj+10E7
               end do

               do while ((jj.GE.10E12).AND.(jj.LT.10E16).AND.(Sa.GE.Samp))

                  ff=A1*log10(jj)+B1
                  uu=A2*log10(jj)+B2
                  vv=A3*log10(jj)+B3
                  C2=ff*(1-Smean/UTS)**uu
                  C3=((UCS/UTS)+(Smean/UTS))**vv
                  Sa=C2*C3*UTS
                  jj=jj+10E10
               end do

               do while ((jj.GT.10E16).AND.(jj.LT.10E25).AND.(Sa.GE.Samp))

                  ff=A1*log10(jj)+B1
                  uu=A2*log10(jj)+B2
                  vv=A3*log10(jj)+B3
                  C2=ff*(1-Smean/UTS)**uu
                  C3=((UCS/UTS)+(Smean/UTS))**vv
                  Sa=C2*C3*UTS
                  jj=jj+10E12
               end do
               Damage=Damage+nn/jj
               !WRITE (50, *) Range/Fact,nn/jj
               !WRITE (50, *) Sa,Samp
               jj=1.0
               Sa=10000.0

            end do
            WRITE (50, *) MaxStlev, (1/Damage)
            Rewind(30)
            MaxStlev=0
         end do

         CLOSE(UNIT=30)
         CLOSE(UNIT=40)
         CLOSE(UNIT=50)



      END


