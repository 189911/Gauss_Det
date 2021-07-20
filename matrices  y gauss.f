      subroutine gauss(c,s,f,r,n,b,a,l,i,j,k,z,d)
      real*8, dimension (100,101):: mat1,mat2,mat3
      write(*,*) "ingrese la matriz del sistema de ecuaciones que desea"
      write(*,*) "resolver, entiendase que las variables x,y,z,a,c son"
      write(*,*)"las variables correspondientes a los valores ingresado"
      write(*,*)"s en cada columna"
      write(*,*) "ingrese numero de variables "
      read(*,*) a
      b=a+1
      do i=1,a
      do j=1,b
      write(*,*) "dame el valor ",i,j,"de la matriz"
      read(*,*) mat1(i,j)
      end do
      end do
      do i=1,a
      write(*,*) (mat1(i,j),j=1,b)
      end do
      write(*,*)" si desea conocer el procedimiento ingrese el numero 1"
      write(*,*)" si no marque 2"
      read(*,*) z
      if (z.eq.1) then
      l=1
      do while(l.le.(a-1))
      if (mat1(l,l).eq.0)then
      write(*,*)"como necesitamos que la diagonal principal de la matri"
      write(*,*)"este llena de numeros 1 sumomos una de las filas de"
      write(*,*)"abajo a la fila", l ,"esto es:"
      do while (mat1(l,l).eq.0)
      do i=l+1,a
      do j=1,b
      mat1(l,j)=mat1(l,j)+mat1(i,j)
      end do
      end do
      end do

      write(*,*)"en este caso se suma la fila",i
      write(*,*)"por conveniencia esto quedaria de la siguinte manera:"

      do i=1,a
      write(*,*) (mat1(i,j),j=1,b)
      end do
      end if


      r=1/(mat1(l,l))
      write(*,*)"para hacer que en en la posicion",l,l
      write(*,*)"halla un uno debemos multiplicar por el inverso de"
      write(*,*) mat1(l,l),"a toda la fila",l,"entonces multiplicamos"
      write(*,*) "por",r

      do j=1,b
      mat1(l,j)= mat1(l,j)*r
      end do
       write(*,*)"entonces tendriamos:"
       do i=1,a
      write(*,*) (mat1(i,j),j=1,b)
      end do


      write(*,*)"teniendo un 1 en la posicion",l,l
      write(*,*)"ahora podemos sumar los valores de la fila ",l,"a "
      write(*,*)"las demas filas con el fin de que en debajo"
      write(*,*)" de la posicion",l,l

      write(*,*)"a lo largo de la columna",l,"solamente cerosden como "
      write(*,*)"resultado para esto multiplicaremos la fila",l,"por el"
      write(*,*)"valor que se encuentra en la poscion en la que "
      write(*,*)"quremos obtener los ceros"


      do i=l+1,a

      f=mat1(i,l)*mat1(l,l)
      write(*,*)"para la fila ",i," multiplicamos la fila",l," por",f
       write(*,*)"y posteriormente restamos"

      do j=1,b


      mat1(i,j)=mat1(i,j)-mat1(l,j)*f
      end do
      write(*,*)"entonces la columna quedaria asi"
      do k=1,a
      write(*,*) (mat1(k,j),j=1,b)
      end do

      end do
      l=l+1

      end do
      write(*,*)"para obtener el ultimo uno de la diagonal principal"
      write(*,*)"dividiremos el valor que se encuentra en",l,l
      write(*,*)"sobre si mismo y a suves a cada termino de su fila"
      write(*,*)"asi tendriamos los 0s que estandebajo de la diagonal"


      r=1/(mat1(l,l))
      do j=1,b
      mat1(l,j)= mat1(l,j)*r
      end do
      do i=1,a
      write(*,*) (mat1(i,j),j=1,b)
      end do
      write(*,*) "los nuevos valores son"


      write(*,*)"aqui se termina los primeros 0s vamos por los segundos"

      do while(l.ge.2)
      i=l-1
      do while(i.ge.1)
      f=mat1(i,l)*mat1(l,l)
      write(*,*)"para obtener ceros en la parte superior a",l,l
      write(*,*)"debemos multiplcar la columna por el valor que se"
      write(*,*)" encuentra en",l-1,l
      write(*,*)" que seria",f
      write(*,*) "y posteriormente sumar a la fila",i
      write(*,*) "de la siguiente manera"




       j=b
      do while(j.ge.l)
      mat1(i,j)=mat1(i,j)-mat1(l,j)*f

      j=j-1
      end do


      do k=1,a
      write(*,*) (mat1(k,j),j=1,b)
      end do
      i=i-1

      end do
      l=l-1

      end do

      write(*,*)"como podemos observar obtuvimos el valor asignado para"
      write(*,*)"cada variable, los resultados son"

      do n=1,a
      write(*,*) n,"x=",mat1(n,b)
      end do
      end if
      if(z.ne.1)then
       l=1
      do while(l.le.(a-1))
      if (mat1(l,l).eq.0)then
      do while (mat1(l,l).eq.0)
      do i=l+1,a
      do j=1,b
      mat1(l,j)=mat1(l,j)+mat1(i,j)
      end do
      end do
      end do
      end if


      r=1/(mat1(l,l))
      do j=1,b
      mat1(l,j)= mat1(l,j)*r
      end do


      do i=l+1,a
      f=mat1(i,l)*mat1(l,l)

      do j=1,b
      mat1(i,j)=mat1(i,j)-mat1(l,j)*f
      end do

      end do
      l=l+1

      end do


      r=1/(mat1(l,l))
      do j=1,b
      mat1(l,j)= mat1(l,j)*r
      end do

      write(*,*) "los nuevos valores son"


      write(*,*)"aqui se termina los primeros 0s vamos por los segundos"

      do while(l.ge.2)
      i=l-1
      do while(i.ge.1)
      f=mat1(i,l)*mat1(l,l)


       j=b
      do while(j.ge.l)
      mat1(i,j)=mat1(i,j)-mat1(l,j)*f

      j=j-1
      end do
      i=i-1

      end do
      l=l-1

      end do

      write(*,*) "los nuevos valores son"
      do n=1,a
      write(*,*) n,"x=",mat1(n,b)
      end do
      end if
      end subroutine







       program todo en uno


       real:: c,s,f,r
      integer:: n,b,a,l,i,j,k,z,d,w,v,q
      real*8, dimension (100,101):: mat1,mat2,mat3
      Q=2


         DO WHILE(Q.NE.1)
       WRITE (*,*)"si desea usar este programa marque 1, de no ser asi"
       write(*,*)"marque 2"
       READ(*,*) v
       IF(v.EQ.2)THEN
       Q=1
       END IF

       IF (v.EQ.1) THEN
       write(*,*)"resolver sistema de ecuaciones lineales 1 "
       write(*,*)"encontrar el determinante de una matriz 2"


      read(*,*) w
      if (w.eq.1)then
      call gauss(c,s,f,r,n,b,a,l,i,j,k,z,d,w)
      end if
      if(w.eq.2)then
      call determinantes(c,s,f,r, n,b,a,l,i,j,k,z,d)
      end if
      END IF
      end do

      pause
      end program




      subroutine determinantes(c,s,f,r, n,b,a,l,i,j,k,z,d)
      real*8, dimension (100,101):: mat1,mat2,mat3
      write(*,*)"escribe la dimension n de la matriz "
      read(*,*) a
      b=a
      d=1
      do i=1,a
      do j=1,b
      write(*,*) "dame el valor ",i,j,"de la matriz"
      read(*,*) mat1(i,j)
      end do
      end do
      do i=1,a
      write(*,*) (mat1(i,j),j=1,b)
      end do


      write(*,*)"dhj"
      l=1
      do while (a.ge.3)
      s=0
      j=1
      r=0
      do while (s.eq.0)
      r=r+1


      s=mat1(r,j)
      end do

      !todo teine que estar dentro de esto
      d=s*d*((-1)**(R+1))



      s=1/mat1(r,1)


      do i= r+1,a
      f=mat1(i,1)
      do j=1,a
      mat1(i,j)=mat1(i,j)-mat1(r,j)*f*s
      end do
      end do





      if (r.ne.1)then
      z=1
      do while (z.ne.r)
      do j=1,a-1
      mat1(z,j)=mat1(z,j+1)
      end do
      z=z+1
      end do
      do z=r+1,a
      do j=1,a-1
      mat1(z-1,j)=mat1(z,j+1)
      end do
      end do
      end if
      if (r.eq.1)then
      do i=r+1,a
      do j=2,a
      mat1(i-1,j-1)=mat1(i,j)
      end do
      end do
      end if

      a=a-1



      end do
      write(*,*) "ahi quedo"


      d=d*(mat1(1,1)*mat1(2,2)-mat1(2,1)*mat1(1,2))
      write(*,*) d
      end subroutine
