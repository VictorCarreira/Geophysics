C     PROGRAMA - geração de dados sintéicos de poços
C     VARIAVEIS UTILIZADAS

C	DEFINICAO DO TIPO DAS VARIAVEIS

	implicit real*8(a-h,o-z)

      real*8 ds(10000,3),x1(10000),x2(10000)


	character*11 L(4),Lito(1000)
	character*80 cab


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	ARQUIVOS 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	open(1,file='dados_sint.txt')

c	isemen=6   						
c	do j=1,isemen
c	xaa=rand()
c	end do


cc	nd - número de dados
cc	vp - valor da propriedade física
cc	delta - despersão da valor medidos (desvio padrão)

cccccc  camada 1ccccccccccccccccccccccccccccccccc
	vp=2.8d0
	delta=0.02d0
	nd=100

	ic=0
	do i=1,nd
	ic=ic+1
	call n_rand(vp,delta,vc)
	ds(ic,1)=dfloat(ic)
	ds(ic,2)=vc
	write(1,*) ds(ic,1),ds(ic,2)
	end do

cccccccc camada 2   cccccccccccccccccccccccccccc
	vp=2.3d0
	delta=0.04d0
	nd=150


	do i=1,nd
	ic=ic+1
	call n_rand(vp,delta,vc)
	ds(ic,1)=dfloat(ic)
	ds(ic,2)=vc
	write(1,*) ds(ic,1),ds(ic,2)
	end do
cccccccccc  camada 3   ccccccccccccccccccccccc
	vp=2.1d0    ! propriedade fisica
	delta=0.01d0
	nd=50


	do i=1,nd
	ic=ic+1
	call n_rand(vp,delta,vc)
	ds(ic,1)=dfloat(ic)
	ds(ic,2)=vc
	write(1,*) ds(ic,1),ds(ic,2)
	end do
ccccccccccc   camada 4 cccccccccccccccccccccc

	vp=2.5d0    ! propriedade fisica
	delta=0.015d0
	nd=75


	do i=1,nd
	ic=ic+1
	call n_rand(vp,delta,vc)
	ds(ic,1)=dfloat(ic)
	ds(ic,2)=vc
	write(1,*) ds(ic,1),ds(ic,2)
	end do

ccccccccccccccccccccccccccccccccccccccccc

	call media_desvio(x1,nd,xmedia,desvio_padrao)



	write(6,*) 'media1=',xmedia
	write(6,*)'desp padrao1=',desvio_padrao


	call media_desvio(x2,nd,xmedia,desvio_padrao)

	write(6,*) 'media2=',xmedia
	write(6,*)'desp padrao2=',desvio_padrao





11	format(4(ES12.4E3,2x))
12	format(I3,2x,3(f6.2,2x))
13	format(4(ES12.4E3,2x))
14	format(4(ES9.2E2,2x))
15	format(A71)
16	format(A11,8(ES9.2E3))





	print*,' ************ FIM *************'
	print*,''

c	pause
	stop
	end

ccccccccccccccccc
ccccccccccccccccc

	subroutine media_desvio(x,n,media,desvio_padrao)

	implicit real*8(a-h,o-z)

	real*8 x(n),media,desvio_padrao

c	n=i-1
	 
	soma = 0d0
	do i=1,n	
	soma = soma + x(i)
	end do
	media=soma/dfloat(n)

	soma2 = 0d0
	do i=1,n
	   soma2 = soma2 + (x(i)-media)**2
	end do
	
	soma2=soma2/(dfloat(n)-1d0)
	
	desvio_padrao=dsqrt(soma2)

	return
	end


c###########################################################################
	subroutine soma(e,x,w)
	integer:: e,i
	real*8 :: x(e),w

	w=0
	do i=1,e
		w= w + x(i)
	end do

	end subroutine soma

cccccccccccccccccccccccccccc
	subroutine n_rand(vp,dp,vc)
	real*8 vp,dp,vc,tq(100),vmax,vmin
	integer i

	do i=1,38
		vmax=vp+0.5d0*dp
		vmin=vp-0.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=39,62
		vmax=vp+1.5d0*dp
		vmin=vp+0.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=63,86
		vmax=vp-0.5d0*dp
		vmin=vp-1.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=87,92
		vmax=vp+2.5d0*dp
		vmin=vp+1.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=93,98
		vmax=vp-1.5d0*dp
		vmin=vp-2.5d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	vmax=vp+3.5d0*dp
	vmin=vp+2.5d0*dp
	tq(99)=rand()*(vmax-vmin)+vmin
	
	vmax=vp-2.5d0*dp
	vmin=vp-3.5d0*dp
	tq(100)=rand()*(vmax-vmin)+vmin
	
	vc=tq(int(100.d0*rand()+1.d0))
	
	return
	end

cccccccccccccccccccccccccc

	subroutine n_rand2(vp,dp,vc)
	real*8 vp,dp,vc,tq(100),vmax,vmin
	integer i

	do i=1,22
		vmax=vp+0.28d0*dp
		vmin=vp-0.28d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=23,28
		vmax=vp+0.44d0*dp
		vmin=vp+0.28d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=29,34
		vmax=vp-0.28d0*dp
		vmin=vp-0.44d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=35,44
		vmax=vp+0.74d0*dp
		vmin=vp+0.44d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=45,54
		vmax=vp-0.44d0*dp
		vmin=vp-0.74d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=55,58
		vmax=vp+0.88d0*dp
		vmin=vp+0.74d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=59,62
		vmax=vp-0.74d0*dp
		vmin=vp-0.88d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=63,70
		vmax=vp+1.23d0*dp
		vmin=vp+0.88d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=71,78
		vmax=vp-0.88d0*dp
		vmin=vp-1.23d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=79,83
		vmax=vp+1.56d0*dp
		vmin=vp+1.23d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=84,88
		vmax=vp-1.23d0*dp
		vmin=vp-1.56d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=89,90
		vmax=vp+1.76d0*dp
		vmin=vp+1.56d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=91,92
		vmax=vp-1.56d0*dp
		vmin=vp-1.76d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=93,94
		vmax=vp+2.06d0*dp
		vmin=vp+1.76d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=95,96
		vmax=vp-1.76d0*dp
		vmin=vp-2.06d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do


	do i=97,98
		vmax=vp+4.51d0*dp
		vmin=vp+2.06d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	do i=99,100
		vmax=vp-2.06d0*dp
		vmin=vp-4.51d0*dp
		tq(i)=rand()*(vmax-vmin)+vmin
	end do

	
cccccccccccccccc
cccccccccccccccc	
	vc=tq(int(100.d0*rand()+1.d0))

	
	return
	end










