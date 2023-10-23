      SUBROUTINE UMAT(STRESS,STATEV,DDSDDE,SSE,SPD,SCD,
     1 RPL,DDSDDT,DRPLDE,DRPLDT,
     2 STRAN,DSTRAN,TIME,DTIME,TEMP,DTEMP,PREDEF,DPRED,CMNAME,
     3 NDI,NSHR,NTENS,NSTATV,PROPS,NPROPS,COORDS,DROT,PNEWDT,
     4 CELENT,DFGRD0,DFGRD1,NOEL,NPT,LAYER,KSPT,JSTEP,KINC)
C
      INCLUDE 'ABA_PARAM.INC'
C
      CHARACTER*80 CMNAME
      DIMENSION STRESS(NTENS),STATEV(NSTATV),
     1 DDSDDE(NTENS,NTENS),DDSDDT(NTENS),DRPLDE(NTENS),
     2 STRAN(NTENS),DSTRAN(NTENS),TIME(2),PREDEF(1),DPRED(1),
     3 PROPS(NPROPS),COORDS(3),DROT(3,3),DFGRD0(3,3),DFGRD1(3,3),
     4 JSTEP(4)

      
C------------------------------------------------------------------------      
      real*8 :: X1_norm(2),X2_norm(2),epsilon1(2),sigma(2),D(4),
     $ Alpha1(10),Alpha2(10),Alpha3(10),Alpha4(2),
     $ A1(10),A2(10),A3(10),
     $ Beta1(10),Beta2(10),Beta3(10),Beta4(10),Beta5(4),
     $ B1(10),B2(10),B3(10),B4(10)
      
      real*8,save :: W1(2,10),W2(10,10),W3(10,10),W4(10,2),
     $ Theta1(10),Theta2(10),Theta3(10),Theta4(2),
     $ Y1_max(2),Y1_min(2),X1_max(2),X1_min(2),
     $ sigma0(2)
      
      real*8,save :: V1(2,10),V2(10,10),V3(10,10),V4(10,10),V5(10,4),
     $ gamma1(10),gamma2(10),gamma3(10),gamma4(10),gamma5(4),
     $ Y2_max(4),Y2_min(4),X2_max(2),X2_min(2)
      
      integer,save :: d1,e1,e2,e3,h1
      integer,save :: d2,f1,f2,f3,f4,h2
      
      integer i,j,k,l,m,kk,mm,ll
      
      real*8  epsilonXX,epsilonYY,epsilonXY
      
      real*8,parameter::PI=3.1415926D0
      
C------------------------------------------------------------------------     
      
      d1=2
      e1=10
      e2=10
      e3=10
      h1=2
      
      d2=2
      f1=10
      f2=10
      f3=10
      f4=10
      h2=4
      
C------------------------------------------------------------------------
C     获取神经网络参数
      
      if (KINC==0 .or. KINC==1) then  
          open(unit=101,file='C:\Users\15430\Desktop\200000.txt',
     $ status='old')
          do i=1,d1
              read(101,*) W1(i,:)           
          end do
          do i=1,e1
              read(101,*) W2(i,:)           
          end do
          do i=1,e2
              read(101,*) W3(i,:)           
          end do
          do i=1,e3
              read(101,*) W4(i,:)           
          end do   
          read(101,*) Theta1
          read(101,*) Theta2
          read(101,*) Theta3
          read(101,*) Theta4
          read(101,*) X1_max
          read(101,*) X1_min
          read(101,*) Y1_max
          read(101,*) Y1_min
          read(101,*) sigma0
          close(101)
          
          open(unit=102,file='C:\Users\15430\Desktop\c200000.txt',
     $ status='old')
          do i=1,d2
              read(102,*) V1(i,:)           
          end do
          do i=1,f1
              read(102,*) V2(i,:)           
          end do
          do i=1,f2
              read(102,*) V3(i,:)           
          end do
          do i=1,f3
              read(102,*) V4(i,:)           
          end do 
          do i=1,f4
              read(102,*) V5(i,:)           
          end do 
          read(102,*) gamma1
          read(102,*) gamma2
          read(102,*) gamma3
          read(102,*) gamma4
          read(102,*) gamma5
          read(102,*) X2_max
          read(102,*) X2_min
          read(102,*) Y2_max
          read(102,*) Y2_min
          close(102)
      end if  


C------------------------------------------------------------------------
C     将正应变转换为主应变
      epsilonXX=STRAN(1)+DSTRAN(1)
      epsilonYY=STRAN(2)+DSTRAN(2)
      epsilonXY=STRAN(4)+DSTRAN(4)
      
      
      if (abs(epsilonXY)==0)then
          angle=0.0
      else
          if (abs(epsilonXX-epsilonYY)==0) then
              angle=PI/4.0
          else
              angle=atan(epsilonXY/(epsilonXX-epsilonYY))/2.0
          end if
      end if

      
      epsilon1(1)=cos(angle)*cos(angle)*epsilonXX+sin(angle)*sin(angle)
     $ *epsilonYY+sin(angle)*cos(angle)*epsilonXY
      
      epsilon1(2)=sin(angle)*sin(angle)*epsilonXX+cos(angle)*cos(angle)
     $ *epsilonYY-sin(angle)*cos(angle)*epsilonXY     

C------------------------------------------------------------------------
C     神经网络一

      do i=1,e1
          Alpha1(i)=0
      end do
      
      do j=1,e1
          do k=1,d1
              Alpha1(j)=Alpha1(j)+X1_norm(k)*W1(k,j)
          end do
      end do
      
      do l=1,e1
          A1(l)=1/(1+EXP(-(Alpha1(l)-Theta1(l))))
      end do 
      
      
      do i=1,e2
          Alpha2(i)=0
      end do
      
      do j=1,e2
          do k=1,e1
              Alpha2(j)=Alpha2(j)+A1(k)*W2(k,j)
          end do
      end do
      
      do l=1,e2
          A2(l)=1/(1+EXP(-(Alpha2(l)-Theta2(l))))
      end do 
            

      do i=1,e3
          Alpha3(i)=0
      end do

      do j=1,e3
          do k=1,e2
              Alpha3(j)=Alpha3(j)+A2(k)*W3(k,j)
          end do
      end do
      
      do l=1,e3
          A3(l)=1/(1+EXP(-(Alpha3(l)-Theta3(l))))
      end do
      

      do i=1,h1
          Alpha4(i)=0
      end do

      do j=1,h1
          do k=1,e3
              Alpha4(j)=Alpha4(j)+A3(k)*W4(k,j)
          end do
      end do
      
      do l=1,h1
          sigma(l)=((Alpha4(l)-Theta4(l))+1)*(Y1_max(l)-Y1_min(l))/2
     $ +Y1_min(l)
      end do
      
C------------------------------------------------------------------------
      
      sigma(1)=sigma(1)-sigma0(1)
      sigma(2)=sigma(2)-sigma0(2)
      
      STRESS(1)=cos(angle)*cos(angle)*sigma(1)
     $ +sin(angle)*sin(angle)*sigma(2)
      STRESS(2)=sin(angle)*sin(angle)*sigma(1)
     $ +cos(angle)*cos(angle)*sigma(2)
      STRESS(3)=0.3*(STRESS(1)+STRESS(2))
      STRESS(4)=sin(angle)*cos(angle)*sigma(1)
     $ -sin(angle)*cos(angle)*sigma(2)
        
C------------------------------------------------------------------------
C     神经网络二
      do i=1,d2
        X2_norm(i)=2*(epsilon1(i)-X2_min(i))/(X2_max(i)-X2_min(i))-1
      end do

      
      do i=1,f1
          Beta1(i)=0
      end do
      
      do j=1,f1
          do k=1,d2
              Beta1(j)=Beta1(j)+X2_norm(k)*V1(k,j)
          end do
      end do
      
      do l=1,f1
          B1(l)=1/(1+EXP(-(Beta1(l)-gamma1(l))))
      end do 
      

      do i=1,f2
          Beta2(i)=0
      end do
      
      do j=1,f2
          do k=1,f1
              Beta2(j)=Beta2(j)+B1(k)*V2(k,j)
          end do
      end do
      
      do l=1,f2
          B2(l)=1/(1+EXP(-(Beta2(l)-gamma2(l))))
      end do 
            

      do i=1,f3
          Beta3(i)=0
      end do

      do j=1,f3
          do k=1,f2
              Beta3(j)=Beta3(j)+B2(k)*V3(k,j)
          end do
      end do
      
      do l=1,f3
          B3(l)=1/(1+EXP(-(Beta3(l)-gamma3(l))))
      end do
 

      do i=1,f4
          Beta4(i)=0
      end do

      do j=1,f4
          do k=1,f3
              Beta4(j)=Beta4(j)+B3(k)*V4(k,j)
          end do
      end do
      
      do l=1,f4
          B4(l)=1/(1+EXP(-(Beta4(l)-gamma4(l))))
      end do
      

      do i=1,h2
          Beta5(i)=0
      end do

      do j=1,h2
          do k=1,f4
              Beta5(j)=Beta5(j)+B4(k)*V5(k,j)
          end do
      end do
      
      do l=1,h2
      D(l)=((Beta5(l)-gamma5(l))+1)*(Y2_max(l)-Y2_min(l))/2+Y2_min(l)
      end do
     
     
      DDSDDE(1,1)=cos(angle)*cos(angle)*(D(1)*cos(angle)*cos(angle)
     $ +D(3)*sin(angle)*sin(angle))+sin(angle)*sin(angle)
     $ *(D(2)*cos(angle)*cos(angle)+D(4)*sin(angle)*sin(angle))+4*
     $ cos(angle)*cos(angle)*sin(angle)*sin(angle)*(D(1)-D(2))/2.0
      
      DDSDDE(1,2)=cos(angle)*cos(angle)*(D(2)*cos(angle)*cos(angle)
     $ +D(4)*sin(angle)*sin(angle))+sin(angle)*sin(angle)
     $ *(D(1)*cos(angle)*cos(angle)+D(3)*sin(angle)*sin(angle))-4*
     $ cos(angle)*cos(angle)*sin(angle)*sin(angle)*(D(1)-D(2))/2.0
      
      DDSDDE(1,3)=0.0
      
      DDSDDE(1,4)=cos(angle)*sin(angle)*sin(angle)*sin(angle)*
     $ (D(1)-D(2)+D(3)-D(4))
      
      DDSDDE(2,1)=cos(angle)*cos(angle)*(D(3)*cos(angle)*cos(angle)
     $ +D(1)*sin(angle)*sin(angle))+sin(angle)*sin(angle)
     $ *(D(4)*cos(angle)*cos(angle)+D(2)*sin(angle)*sin(angle))-4*
     $ cos(angle)*cos(angle)*sin(angle)*sin(angle)*(D(1)-D(2))/2.0
      
      DDSDDE(2,2)=cos(angle)*cos(angle)*(D(4)*cos(angle)*cos(angle)
     $ +D(2)*sin(angle)*sin(angle))+sin(angle)*sin(angle)
     $ *(D(3)*cos(angle)*cos(angle)+D(1)*sin(angle)*sin(angle))+4*
     $ cos(angle)*cos(angle)*sin(angle)*sin(angle)*(D(1)-D(2))/2.0
      
      DDSDDE(2,3)=0.0
      
      DDSDDE(2,4)=cos(angle)*cos(angle)*cos(angle)*sin(angle)*
     $ (D(1)-D(2)+D(3)-D(4))
      
      DDSDDE(3,1)=0.0
      DDSDDE(3,2)=0.0
      DDSDDE(3,3)=0.0
      DDSDDE(3,4)=0.0
      
      DDSDDE(4,1)=cos(angle)*sin(angle)*(D(2)*cos(angle)*cos(angle)
     $ -D(3)*cos(angle)*cos(angle)+D(1)*sin(angle)*sin(angle)
     $ -D(4)*sin(angle)*sin(angle))
      
      DDSDDE(4,2)=cos(angle)*sin(angle)*(D(1)*cos(angle)*cos(angle)
     $ -D(4)*cos(angle)*cos(angle)+D(2)*sin(angle)*sin(angle)
     $ -D(3)*sin(angle)*sin(angle))
      
      DDSDDE(4,3)=0.0
      
      DDSDDE(4,4)=(D(1)*cos(angle)*cos(angle)*cos(angle)*cos(angle))/2.0
     $ -(D(2)*cos(angle)*cos(angle)*cos(angle)*cos(angle))/2.0+
     $ (D(1)*sin(angle)*sin(angle)*sin(angle)*sin(angle))/2.0-
     $ (D(2)*sin(angle)*sin(angle)*sin(angle)*sin(angle))/2.0-
     $ (D(3)*cos(angle)*cos(angle)*sin(angle)*sin(angle))+
     $ (D(4)*cos(angle)*cos(angle)*sin(angle)*sin(angle))
      
C-------------------------------------------------------------------	   
	RETURN
	END