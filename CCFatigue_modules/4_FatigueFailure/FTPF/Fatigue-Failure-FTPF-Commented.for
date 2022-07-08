      Program FTPF

!	USE NUMERICAL_LIBRARIES
!	USE IMSLF90
         integer i,j
         real m,nc,mn,Rx,Ry,Rs,Theta,Dangle,Relx,Rely,Rels,Ax,Ay,As,Bx,By,Bs
         real Rav,Rratio,Relav,Reliability,X,Y,S,SNP,ThirdSN,Tempangle
         real At,Bt,Tm,Tnc,Tmn,T,N,SSQR


         CHARACTER * 40 NAME, NAM,SNmodel

         ! Enter the input file name and open it for reading
         !####################################################################


         OPEN (10, FILE = 'inputX.txt')
         OPEN (20, FILE = 'inputY.txt')
         OPEN (30, FILE = 'inputF.txt')

         OPEN (50, FILE = 'output.txt')

         OPEN(100, FILE = 'Refdata.txt')
         READ(100, * ) SNmodel      ! "Log-Log" or "Lin-Log"
         ! Dangle = Desirable angle [degree]
         ! ThirdSN = Off-axis angle [degree]
         ! (when inputF.txt contains the shear fatigue data, set this parameter to zero)
         READ(100, * ) Dangle,ThirdSN ! 30, 60
         CLOSE(UNIT = 100)

         ! Degree to radian
         Theta = (Dangle  / 180) * 3.14159265
         Tempangle = (ThirdSN  / 180) * 3.14159265

         read(10, * )
         read(20, * )
         read(30, * )

         ! Read stress ratios
         read(10, * ) Rx
         read(20, * ) Ry
         read(30, * ) Rs

         ! Read reliability
         read(10, * ) Relx
         read(20, * ) Rely
         read(30, * ) Rels

         if ((SNmodel.EQ.'Log-Log').OR.(SNmodel.EQ.'Lin-Log')) then
            ! S = Ax * N ** (-Bx) or S = Ax + Bx * log10(N)
            READ (10, * ) Ax
            READ (10, * ) Bx
            READ (20, * ) Ay
            READ (20, * ) By
            if (ThirdSN.EQ.0) then
               READ (30, * ) As
               READ (30, * ) Bs
            else
               READ (30, * ) At
               READ (30, * ) Bt
            endif
         endif

         Tm = sin(Tempangle) ** 4
         Tnc = cos(Tempangle) ** 4
         Tmn = sin(Tempangle) ** 2 * cos(Tempangle) ** 2

         m = sin(Theta) ** 4
         nc = cos(Theta) ** 4
         mn = sin(Theta) ** 2 * cos(Theta) ** 2

         Rav = (Rx + Ry + Rs) / 3
         if (Rav.EQ.Rx) then
            Rratio = Rav
         else
            Rratio = 1
         endif

         Relav = (Relx + Rely + Rels) / 3
         if (Relav.EQ.Relx) then
            Reliability = Relav
         else
            Reliability = 0
         endif

         ! Calculating and writing the predicted S-N curve based on found parameters
         ! ##########################################################################
         WRITE(50, * ) '0' !Seperator
         WRITE(50, * ) Rratio
         WRITE(50, * ) Reliability
         WRITE(50, * ) '0'
         WRITE(50, * ) '0'
         WRITE(50, * ) '0' !af
         WRITE(50, * ) '0' !So
         WRITE(50, * ) '0' !Pwer
         WRITE(50, * ) '0' !af
         WRITE(50, * ) '0' !B
         WRITE(50, * ) '0' !S
         WRITE(50, * ) '0' !C

         if (SNmodel.EQ.'Log-Log') then
            do N = 1,1000,50
               X = Ax * N ** (-Bx)	!Longitudinal
               Y = Ay * N ** (-By)	!Transverse
               if (ThirdSN.NE.0) then
                  T = At * N ** (-Bt)
                  SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
                  if (SSQR.GE.0) then
                     S = 1 / sqrt(SSQR)
                  else
                     goto 10 ! Ignore current line in output file
                  endif
               else
                  S = As * N ** (-Bs)
               endif
               if (ThirdSN.EQ.22.5) then
                  S = T / 2.2
               endif
               SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * ((1 / S ** 2)-(1 / (X * Y))))) ** (-0.5)

               WRITE(50, * ) Rratio,N,SN,S
   10       end do

            N = 1000
            X = Ax * N ** (-Bx)
            Y = Ay * N ** (-By)
            if (ThirdSN.NE.0) then
               T = At * N ** (-Bt)
               SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
               if (SSQR.GE.0) then
                  S = 1 / sqrt(SSQR)
               else
                  goto 20
               endif
            else
               S = As * N ** (-Bs)
            endif
            if (ThirdSN.EQ.22.5) then
               S = T / 2.2
            endif
            SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * (1 / S ** 2-1 / (X * Y)))) ** (-0.5)
            WRITE(50, * ) Rratio,N,SN,S

   20       do N = 10000,2e6,10000
               X = Ax * N ** (-Bx)
               Y = Ay * N ** (-By)
               if (ThirdSN.NE.0) then
                  T = At * N ** (-Bt)
                  SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
                  if (SSQR.GE.0) then
                     S = 1 / sqrt(SSQR)
                  else
                     goto 30
                  endif
               else
                  S = As * N ** (-Bs)
               endif
               if (ThirdSN.EQ.22.5) then
                  S = T / 2.2
               endif
               SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * (1 / S ** 2-1 / (X * Y)))) ** (-0.5)
               WRITE(50, * ) Rratio,N,SN,S
   30       end do

            do N = 2e6,20e6,1000000
               X = Ax * N ** (-Bx)
               Y = Ay * N ** (-By)
               if (ThirdSN.NE.0) then
                  T = At * N ** (-Bt)
                  SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
                  if (SSQR.GE.0) then
                     S = 1 / sqrt(SSQR)
                  else
                     goto 40
                  endif
               else
                  S = As * N ** (-Bs)
               endif
               if (ThirdSN.EQ.22.5) then
                  S = T / 2.2
               endif
               SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * (1 / S ** 2-1 / (X * Y)))) ** (-0.5)
               WRITE(50, * ) Rratio,N,SN,S
   40       end do
         endif

         if (SNmodel.EQ.'Lin-Log') then
            do N = 1,1000,50
               X = Ax + Bx * log10(N)	!Longitudinal
               Y = Ay + By * log10(N)	!Transverse
               if (ThirdSN.NE.0) then
                  T = At + Bt * log10(N)
                  SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
                  if (SSQR.GE.0) then
                     S = 1 / sqrt(SSQR)
                  else
                     goto 50
                  endif
               else
                  S = As + Bs * log10(N)
               endif
               if (ThirdSN.EQ.22.5) then
                  S = T / 2.2
               endif
               SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * ((1 / S ** 2)-(1 / (X * Y))))) ** (-0.5)
               WRITE(50, * ) Rratio,N,SN
   50       end do

            N = 1000
            X = Ax + Bx * log10(N)	!Longitudinal
            Y = Ay + By * log10(N)	!Transverse
            if (ThirdSN.NE.0) then
               T = At + Bt * log10(N)
               SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
               if (SSQR.GE.0) then
                  S = 1 / sqrt(SSQR)
               else
                  goto 60
               endif
            else
               S = As + Bs * log10(N)
            endif
            if (ThirdSN.EQ.22.5) then
               S = T / 2.2
            endif
            SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * ((1 / S ** 2)-(1 / (X * Y))))) ** (-0.5)
            WRITE(50, * ) Rratio,N,SN

   60       do N = 10000,2e6,10000
               X = Ax + Bx * log10(N)	!Longitudinal
               Y = Ay + By * log10(N)	!Transverse
               if (ThirdSN.NE.0) then
                  T = At + Bt * log10(N)
                  SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
                  if (SSQR.GE.0) then
                     S = 1 / sqrt(SSQR)
                  else
                     goto 70
                  endif
               else
                  S = As + Bs * log10(N)
               endif
               if (ThirdSN.EQ.22.5) then
                  S = T / 2.2
               endif
               SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * ((1 / S ** 2)-(1 / (X * Y))))) ** (-0.5)
               WRITE(50, * ) Rratio,N,SN
   70       end do

            do N = 2e6,20e6,1000000
               X = Ax + Bx * log10(N)	!Longitudinal
               Y = Ay + By * log10(N)	!Transverse
               if (ThirdSN.NE.0) then
                  T = At + Bt * log10(N)
                  SSQR = (-((Tnc / X ** 2) + (Tm / Y ** 2)-(Tmn / (X * Y))-(1 / T ** 2)) / Tmn)
                  if (SSQR.GE.0) then
                     S = 1 / sqrt(SSQR)
                  else
                     goto 80
                  endif
               else
                  S = As + Bs * log10(N)
               endif
               if (ThirdSN.EQ.22.5) then
                  S = T / 2.2
               endif
               SN = ((nc / X ** 2) + (m / Y ** 2) + (mn * ((1 / S ** 2)-(1 / (X * Y))))) ** (-0.5)
               WRITE(50, * ) Rratio,N,SN
   80       end do
         endif


         CLOSE(UNIT = 10)
         CLOSE(UNIT = 20)
         CLOSE(UNIT = 30)
         CLOSE(UNIT = 50)

      End
