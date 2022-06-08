      Program HR

!	USE NUMERICAL_LIBRARIES
!	USE IMSLF90
         integer i, j
         real desirable_angle, off_axis_angle1, off_axis_angle2, tensile_axial_strength, compressive_axial_strength, tensile_transverse_strength, compressive_transverse_strength, shear_strength
         real tensile_strength1, compressive_strength1, tensile_strength2, compressive_strength2, tensile_strength_at_desirable_angle, compressive_strength_at_desirable_angle
         real PI, Theta, OAngle1, OAngle2, R1, R2, R3, Rel1, Rel2, Rel3
         real Aa, Ba, At, Bt, As, Bs, A1, B1, A2, B2, Rav, Relav, Reliability
         real Sigmat, Const1, Const2, ff, ft, fs, ff1, ff2, SN, ft2, fs2, fff, Sigmas, N



         CHARACTER*40 NAME, NAM, sn_model

         ! Enter the input file name and open it for reading
         !####################################################################


         OPEN (10, FILE = 'SNA.txt')
         OPEN (20, FILE = 'SN1.txt')
         OPEN (30, FILE = 'SN2.txt')

         OPEN (50, FILE = 'output.txt')

         OPEN(100, FILE = 'Refdata.txt')
         READ(100, *) sn_model
         READ(100, *) desirable_angle, off_axis_angle1, off_axis_angle2
         READ(100, *) tensile_axial_strength, compressive_axial_strength, tensile_transverse_strength, compressive_transverse_strength, shear_strength
         READ(100, *) tensile_strength1, compressive_strength1, tensile_strength2, compressive_strength2
         READ(100, *) tensile_strength_at_desirable_angle, compressive_strength_at_desirable_angle !Strength at deirable off-axis angle
         CLOSE(UNIT=100)


         PI=3.14159265

         ! Degree to rad
         Theta=(desirable_angle/180)*PI
         OAngle1=(off_axis_angle1/180)*PI
         OAngle2=(off_axis_angle2/180)*PI


         read(10, *)
         read(20, *)
         read(30, *)

         read(10, *) R1
         read(20, *) R2
         read(30, *) R3

         read(10, *) Rel1
         read(20, *) Rel2
         read(30, *) Rel3

         !log(S)=Ax+Bx*log(N)
         READ (10, *) Aa
         READ (10, *) Ba

         if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
            READ (20, *) At
            READ (20, *) Bt
            READ (30, *) As
            READ (30, *) Bs
         endif

         if (off_axis_angle1.EQ.90) then
            READ (20, *) At
            READ (20, *) Bt
            READ (30, *) A2
            READ (30, *) B2
         endif

         if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90)) then
            READ (20, *) A1
            READ (20, *) B1
            READ (30, *) A2
            READ (30, *) B2
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
         WRITE(50, *) '0' !Seperator
         WRITE(50, *) Rratio
         WRITE(50, *) Reliability
         WRITE(50, *) A1
         WRITE(50, *) B1
         WRITE(50, *) A2 !af
         WRITE(50, *) B2 !So
         WRITE(50, *) off_axis_angle1 !Pwer
         WRITE(50, *) off_axis_angle2 !af
         WRITE(50, *) '0' !B
         WRITE(50, *) '0' !S
         WRITE(50, *) '0' !C

         if (sn_model.EQ.'Log-Log') then
            do N=1, 1000, 50
               if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
                  Sigmat=At*N**(-Bt)
                  Sigmas=As*N**(-Bs)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     ft=Sigmat/compressive_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if (off_axis_angle1.EQ.90) then
                  Sigmat=At*N**(-Bt)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff=((A2*N**(-B2))/tensile_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     ft=Sigmat/compressive_transverse_strength
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff=((A2*N**(-B2))/compressive_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1*N**(-B1))/tensile_strength1)**2
                     ff2=((A2*N**(-B2))/tensile_strength2)**2
                  endif
                  if (Rratio.GT.1) then
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1*N**(-B1))/compressive_strength1)**2
                     ff2=((A2*N**(-B2))/compressive_strength2)**2
                  endif
                  ft2=(ff1*ff2*(Const2-Const1))/
     &            (ff1*(1+Const2)-ff2*(1+Const1))
                  fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
                  if (ft2.LE.0) then
                     go to 10
                  else
                     !WRITE(50, *) Const1, Const2, ff1, ff2
                     !WRITE(50, *) ft2, fs2
                     ft=sqrt(ft2)
                     fs=sqrt(fs2)
                  endif
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif
               WRITE(50, *) Rratio, N, SN
   10       end do

            N=1000
            SN=0
            fs=0
            ft=0
            if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
               Sigmat=At*N**(-Bt)
               Sigmas=As*N**(-Bs)
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  ft=Sigmat/tensile_transverse_strength
                  fs=Sigmas/shear_strength
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
               if (Rratio.GT.1) then
                  ft=Sigmat/compressive_transverse_strength
                  fs=Sigmas/shear_strength
                  SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
            endif

            if (off_axis_angle1.EQ.90) then
               Sigmat=At*N**(-Bt)
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  ft=Sigmat/tensile_transverse_strength
                  Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                  ff=((A2*N**(-B2))/tensile_strength2)**2
                  fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
               if (Rratio.GT.1) then
                  ft=Sigmat/compressive_transverse_strength
                  Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                  ff=((A2*N**(-B2))/compressive_strength2)**2
                  fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
            endif

            if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                  Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                  ff1=((A1*N**(-B1))/tensile_strength1)**2
                  ff2=((A2*N**(-B2))/tensile_strength2)**2
               endif
               if (Rratio.GT.1) then
                  Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                  Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                  ff1=((A1*N**(-B1))/compressive_strength1)**2
                  ff2=((A2*N**(-B2))/compressive_strength2)**2
               endif
               ft2=(ff1*ff2*(Const2-Const1))/
     &         (ff1*(1+Const2)-ff2*(1+Const1))
               fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
               if (ft2.LE.0) then
                  go to 20
               else
                  !WRITE(50, *) Const1, Const2, ff1, ff2
                  !WRITE(50, *) ft2, fs2
                  ft=sqrt(ft2)
                  fs=sqrt(fs2)
               endif
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
               if (Rratio.GT.1) then
                  SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
            endif
            WRITE(50, *) Rratio, N, SN
            !End N=1000

   20       do N=10000, 2e6, 10000
               if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
                  Sigmat=At*N**(-Bt)
                  Sigmas=As*N**(-Bs)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     ft=Sigmat/compressive_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if (off_axis_angle1.EQ.90) then
                  Sigmat=At*N**(-Bt)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff=((A2*N**(-B2))/tensile_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     ft=Sigmat/compressive_transverse_strength
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff=((A2*N**(-B2))/compressive_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1*N**(-B1))/tensile_strength1)**2
                     ff2=((A2*N**(-B2))/tensile_strength2)**2
                  endif
                  if (Rratio.GT.1) then
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1*N**(-B1))/compressive_strength1)**2
                     ff2=((A2*N**(-B2))/compressive_strength2)**2
                  endif
                  ft2=(ff1*ff2*(Const2-Const1))/
     &            (ff1*(1+Const2)-ff2*(1+Const1))
                  fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
                  if (ft2.LE.0) then
                     go to 30
                  else
                     !WRITE(50, *) Const1, Const2, ff1, ff2
                     !WRITE(50, *) ft2, fs2
                     ft=sqrt(ft2)
                     fs=sqrt(fs2)
                  endif
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif
               WRITE(50, *) Rratio, N, SN
   30       end do


            do N=2e6, 20e6, 1000000
               if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
                  Sigmat=At*N**(-Bt)
                  Sigmas=As*N**(-Bs)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     ft=Sigmat/compressive_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if (off_axis_angle1.EQ.90) then
                  Sigmat=At*N**(-Bt)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff=((A2*N**(-B2))/tensile_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     ft=Sigmat/compressive_transverse_strength
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff=((A2*N**(-B2))/compressive_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1*N**(-B1))/tensile_strength1)**2
                     ff2=((A2*N**(-B2))/tensile_strength2)**2
                  endif
                  if (Rratio.GT.1) then
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1*N**(-B1))/compressive_strength1)**2
                     ff2=((A2*N**(-B2))/compressive_strength2)**2
                  endif
                  ft2=(ff1*ff2*(Const2-Const1))/
     &            (ff1*(1+Const2)-ff2*(1+Const1))
                  fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
                  if (ft2.LE.0) then
                     go to 40
                  else
                     !WRITE(50, *) Const1, Const2, ff1, ff2
                     !WRITE(50, *) ft2, fs2
                     ft=sqrt(ft2)
                     fs=sqrt(fs2)
                  endif
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if (Rratio.GT.1) then
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif
               WRITE(50, *) Rratio, N, SN
   40       end do

         endif

         !Lin-Log

         if (sn_model.EQ.'Lin-Log') then
            do N=1, 1000, 50
               if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
                  Sigmat=At+Bt*log10(N)
                  Sigmas=As+Bs*log10(N)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                     !WRITE(50, *) Rratio, N, SN
                     !WRITE(50, *) At, Bt, Sigmat, tensile_transverse_strength, ft
                     !WRITE(50, *) As, Bs, Sigmas, shear_strength, fs
                     !WRITE(50, *) tensile_strength_at_desirable_angle
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     ft=Sigmat/compressive_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif

               endif

               if (off_axis_angle1.EQ.90) then
                  Sigmat=At+Bt*log10(N)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff=((A2+B2*log10(N))/tensile_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     ft=Sigmat/compressive_transverse_strength
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff=((A2+B2*log10(N))/compressive_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1+B1*log10(N))/tensile_strength1)**2
                     ff2=((A2+B2*log10(N))/tensile_strength2)**2
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1+B1*log10(N))/compressive_strength1)**2
                     ff2=((A2+B2*log10(N))/compressive_strength2)**2
                  endif
                  ft2=(ff1*ff2*(Const2-Const1))/
     &            (ff1*(1+Const2)-ff2*(1+Const1))
                  fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
                  if (ft2.LE.0) then
                     go to 50
                  else
                     !WRITE(50, *) Const1, Const2, ff1, ff2
                     !WRITE(50, *) ft2, fs2
                     ft=sqrt(ft2)
                     fs=sqrt(fs2)
                  endif
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif


               WRITE(50, *) Rratio, N, SN

   50       end do

            N=1000
            SN=0
            fs=0
            ft=0
            if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
               Sigmat=At+Bt*log10(N)
               Sigmas=As+Bs*log10(N)
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  ft=Sigmat/tensile_transverse_strength
                  fs=Sigmas/shear_strength
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
                  !WRITE(50, *) Rratio, N, SN
                  !WRITE(50, *) At, Bt, Sigmat, tensile_transverse_strength, ft
                  !WRITE(50, *) As, Bs, Sigmas, shear_strength, fs
                  !WRITE(50, *) tensile_strength_at_desirable_angle
               endif
               if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                  ft=Sigmat/compressive_transverse_strength
                  fs=Sigmas/shear_strength
                  SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif

            endif

            if (off_axis_angle1.EQ.90) then
               Sigmat=At+Bt*log10(N)
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  ft=Sigmat/tensile_transverse_strength
                  Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                  ff=((A2+B2*log10(N))/tensile_strength2)**2
                  fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
               if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                  ft=Sigmat/compressive_transverse_strength
                  Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                  ff=((A2+B2*log10(N))/compressive_strength2)**2
                  fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
            endif

            if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                  Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                  ff1=((A1+B1*log10(N))/tensile_strength1)**2
                  ff2=((A2+B2*log10(N))/tensile_strength2)**2
               endif
               if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                  Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                  Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                  ff1=((A1+B1*log10(N))/compressive_strength1)**2
                  ff2=((A2+B2*log10(N))/compressive_strength2)**2
               endif
               ft2=(ff1*ff2*(Const2-Const1))/
     &         (ff1*(1+Const2)-ff2*(1+Const1))
               fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
               if (ft2.LE.0) then
                  go to 60
               else
                  !WRITE(50, *) Const1, Const2, ff1, ff2
                  !WRITE(50, *) ft2, fs2
                  ft=sqrt(ft2)
                  fs=sqrt(fs2)
               endif
               if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                  SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
               if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                  SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &            *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &            *(fs/ft)*tan(Theta))**2))
               endif
            endif


            WRITE(50, *) Rratio, N, SN



   60       do N=10000, 2e6, 10000
               SN=0
               fs=0
               ft=0
               if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
                  Sigmat=At+Bt*log10(N)
                  Sigmas=As+Bs*log10(N)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                     !WRITE(50, *) Rratio, N, SN
                     !WRITE(50, *) At, Bt, Sigmat, tensile_transverse_strength, ft
                     !WRITE(50, *) As, Bs, Sigmas, shear_strength, fs
                     !WRITE(50, *) tensile_strength_at_desirable_angle
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     ft=Sigmat/compressive_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif

               endif

               if (off_axis_angle1.EQ.90) then
                  Sigmat=At+Bt*log10(N)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff=((A2+B2*log10(N))/tensile_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     ft=Sigmat/compressive_transverse_strength
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff=((A2+B2*log10(N))/compressive_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1+B1*log10(N))/tensile_strength1)**2
                     ff2=((A2+B2*log10(N))/tensile_strength2)**2
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1+B1*log10(N))/compressive_strength1)**2
                     ff2=((A2+B2*log10(N))/compressive_strength2)**2
                  endif
                  ft2=(ff1*ff2*(Const2-Const1))/
     &            (ff1*(1+Const2)-ff2*(1+Const1))
                  fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
                  if (ft2.LE.0) then
                     go to 70
                  else
                     !WRITE(50, *) Const1, Const2, ff1, ff2
                     !WRITE(50, *) ft2, fs2
                     ft=sqrt(ft2)
                     fs=sqrt(fs2)
                  endif
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif


               WRITE(50, *) Rratio, N, SN
   70       end do


            do N=2e6, 20e6, 1000000
               SN=0
               fs=0
               ft=0
               if ((off_axis_angle1.EQ.0).and.(off_axis_angle2.EQ.0)) then
                  Sigmat=At+Bt*log10(N)
                  Sigmas=As+Bs*log10(N)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                     !WRITE(50, *) Rratio, N, SN
                     !WRITE(50, *) At, Bt, Sigmat, tensile_transverse_strength, ft
                     !WRITE(50, *) As, Bs, Sigmas, shear_strength, fs
                     !WRITE(50, *) tensile_strength_at_desirable_angle
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     ft=Sigmat/compressive_transverse_strength
                     fs=Sigmas/shear_strength
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif

               endif

               if (off_axis_angle1.EQ.90) then
                  Sigmat=At+Bt*log10(N)
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     ft=Sigmat/tensile_transverse_strength
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff=((A2+B2*log10(N))/tensile_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     ft=Sigmat/compressive_transverse_strength
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff=((A2+B2*log10(N))/compressive_strength2)**2
                     fs=sqrt((ff*ft**2)/((1+Const1)*ft**2-(ff*Const1)))
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               if ((off_axis_angle1.NE.0).and.(off_axis_angle2.NE.0).and.(off_axis_angle1.NE.90))then
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     Const1=((shear_strength/tensile_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/tensile_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1+B1*log10(N))/tensile_strength1)**2
                     ff2=((A2+B2*log10(N))/tensile_strength2)**2
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     Const1=((shear_strength/compressive_transverse_strength)*tan(OAngle1))**2
                     Const2=((shear_strength/compressive_transverse_strength)*tan(OAngle2))**2
                     ff1=((A1+B1*log10(N))/compressive_strength1)**2
                     ff2=((A2+B2*log10(N))/compressive_strength2)**2
                  endif
                  ft2=(ff1*ff2*(Const2-Const1))/
     &            (ff1*(1+Const2)-ff2*(1+Const1))
                  fs2=(-ff1*ft2)/(ff1*Const1-ft2*(1+Const1))
                  if (ft2.LE.0) then
                     go to 80
                  else
                     !WRITE(50, *) Const1, Const2, ff1, ff2
                     !WRITE(50, *) ft2, fs2
                     ft=sqrt(ft2)
                     fs=sqrt(fs2)
                  endif
                  if ((Rratio.GE.0).and.(Rratio.LT.1)) then
                     SN=tensile_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/tensile_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/tensile_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
                  if ((Rratio.GT.1).OR.(Rratio.EQ.-1)) then
                     SN=compressive_strength_at_desirable_angle*fs*sqrt((1+((shear_strength/compressive_transverse_strength)
     &               *tan(Theta))**2)/(1+((shear_strength/compressive_transverse_strength)
     &               *(fs/ft)*tan(Theta))**2))
                  endif
               endif

               WRITE(50, *) Rratio, N, SN
   80       end do

         endif


         !	fs=sqrt(-fs)
         CLOSE(UNIT=10)
         CLOSE(UNIT=20)
         CLOSE(UNIT=30)
         CLOSE(UNIT=50)

      End
