program main
   !
   ! Function: add perturbation to ensemble parameters files based on prescribed range.
   ! 
   ! Usage: ./ptrb_param_bio nens perr styp
   !    ex) ./ptrb_param_bio 10 10 NORM
   !
   !        nens: number of ensemble members [integer]
   !        perr: size of perturbation [0-100: integer]
   !        styp: type of distribution ["NORM": string]
   !
   ! Note: mem000 : default value
   !       mem001-: perturbed values
   !
   ! Author: tsuyoshi.wakamatsu@nersc.no
   !            

   implicit none 
   
   type gen_params 
        character(len=9) :: pnam
        real             :: pval, pmin, pmax
   end type gen_params

   integer(kind=4), parameter :: npar=8     !number of parameters
   type(gen_params)           :: bio(npar)

   integer(kind=4)   :: narg       ! number of command line arguments
   integer(kind=4)   :: nens,perr
   character(len=10) :: styp

   integer(kind=4)    :: ip,ie
   real               :: pstd
   real               :: rng(1)
   real, allocatable  :: prm(:,:)
   character(len=255) :: filename
   character(len=10)  :: tmpchar,tmp1,tmp2,tmp3,tmp4

   !--- define parameter set

   bio(1)%pnam = "grPl" ; bio(1)%pmin = 0.8  ; bio(1)%pmax = 3.0  ! growth rate of Pl               [1/day]
   bio(2)%pnam = "grPs" ; bio(2)%pmin = 0.5  ; bio(2)%pmax = 2.0  ! growth rate of Ps               [1/day]
   bio(3)%pnam = "mrPl" ; bio(3)%pmin = 0.02 ; bio(3)%pmax = 0.25 ! mortality rate of Pl            [1/day]
   bio(4)%pnam = "mrPs" ; bio(4)%pmin = 0.02 ; bio(4)%pmax = 0.25 ! mortality rate of Ps            [1/day]
   bio(5)%pnam = "mrZl" ; bio(5)%pmin = 0.05 ; bio(5)%pmax = 0.4  ! mortality rate of Zl            [1/day]
   bio(6)%pnam = "mrZs" ; bio(6)%pmin = 0.05 ; bio(6)%pmax = 0.4  ! mortality rate of Zs            [1/day]
   bio(7)%pnam = "srDO" ; bio(7)%pmin = 0.5  ; bio(7)%pmax = 20.0 ! sinking rate of Detritus & Opal [m/day]
   bio(8)%pnam = "CrSi" ; bio(8)%pmin = 3.0  ; bio(8)%pmax = 13.0 ! Carbon to Silicate ratio        [mol C/mol Si]

   !--- read the command line arguments

   narg = command_argument_count()  ! number of the command line arguments
   
   if ( narg.lt.3 ) then 
      write ( *,'(a)' ) "Usage: Ensemble size, Error size [%], Sampling method [GAUSS/LOGN]"
      print *,"Error: reading environment variables"
      stop 
   endif

   call get_command_argument(1,tmpchar) ; read(tmpchar,'(i3)') nens
   call get_command_argument(2,tmpchar) ; read(tmpchar,'(i3)') perr
   call get_command_argument(3,styp) 

   write ( *,'(a,i3)' ) "Ensemble size                 : ", nens 
   write ( *,'(a,i3)' ) "Initial parameter variance [%]: ", perr
   write ( *,'(a,a4)' ) "Sampling method [NORM or LOGN]: ", TRIM(styp)

   write ( *,'(a)' ) "PARAMETERS:"
   do ip = 1,npar
      write ( *,'(x,5a)' ) "-"//TRIM(bio(ip)%pnam)
   enddo

   !--- generate sets of perturbed parameters

   allocate( prm(nens,npar) )
   
   if (nens == 1) then
      prm(1,:) = bio(:)%pval
   else
      call set_random_seed ! set a seed for random number generator

      do ie = 1,nens

         !--- read ensemble member

         write( tmp1,'(i0.3)' ) ie
         filename="Parameter_bio_mem"//trim(adjustl(tmp1))//".txt"  

         open( unit=10,file=filename,status='old',form='formatted' )
         do ip = 1,npar
            read(10,'(7x,f7.3)') bio(ip)%pval
         enddo
         close( unit=10 )

         !--- add perturbation

         do ip = 1,npar
            call random(rng(1),1)
                  
            if ( styp.eq."LOGN" ) then
               print *,"Error: sampling method:"//TRIM(styp)//" not supported yet"
               stop 
               !prm(ie,ip) = bio(ip)%pval*exp(0.01*perr*rng(1))
            elseif ( styp.eq."NORM" ) then
               pstd = 0.01*perr*(bio(ip)%pmax-bio(ip)%pmin)
               prm(ie,ip) = max( 0.0, bio(ip)%pval + pstd*rng(1) )
            else
               print *,"Error: sampling method:"//TRIM(styp)//" not supported"
               stop 
            endif
                  
            if (prm(ie,ip).gt.bio(ip)%pmax) then
               prm(ie,ip) = bio(ip)%pmax
            elseif (prm(ie,ip).lt.bio(ip)%pmin) then
               prm(ie,ip) = bio(ip)%pmin
            endif
            !print *, rng(1), bio(ip)%pval, bio(ip)%pmin, bio(ip)%pmax, prm(ie,ip)
         enddo
      enddo
   endif

   ! rewrite parameter files

   do ie = 1,nens
      write( tmp1,'(i0.3)' ) ie
      filename="Parameter_bio_mem"//trim(adjustl(tmp1))//".txt"  

      open( unit=10,file=filename,status='replace',form='formatted' )
      do ip = 1,npar
        write(10,'(a4,1x,a1,1x,f7.3)') bio(ip)%pnam,"=",prm(ie,ip)
      enddo
      close( unit=10 )
   enddo

   ! store parameter ensemble in column wise

   write(tmp2,'(i3)') npar
   tmp3 = '('//trim(adjustl(tmp2))//'f10.5)'
   tmp4 = '('//trim(adjustl(tmp2))//'a10)'
   
   open( unit=10,file='Parameter_bio_mem_all.txt',status='replace' )
   do ie = 1,nens 
      write( 10,tmp3 ) ( prm(ie,ip), ip = 1,npar )
   enddo
   close( unit=10 )
   
end program main

subroutine set_random_seed 
   implicit none
   integer :: i, n, clock
   integer, dimension(:), allocatable :: seed
          
   call random_seed(size = n)
   allocate(seed(n))
          
   call system_clock(count=clock)
          
   seed = clock + 37 * (/ (i - 1, i = 1, n) /)
   call random_seed(put = seed)
   deallocate(seed)
end subroutine set_random_seed

subroutine random(work1,n)
!
!  Returns a vector of normally distributed (variance=1,mean=0) random value
!  generated with the Box-Muller transformation
!
   implicit none
   integer, intent(in)  :: n
   real,    intent(out) :: work1(n)
   real,    allocatable :: work2(:)
   real,    parameter   :: pi=3.141592653589

   allocate (work2(n))

   call random_number(work1)
   call random_number(work2)
   work1= sqrt(-2.0*log(work1))*cos(2.0*pi*work2)

   deallocate(work2)
end subroutine random
