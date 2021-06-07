        ! This program creates the prog1.hops file for use
        ! with the APPSPACK optimizer - includes lower and upper bounds

        subroutine setappsfile(numparms,scale1,parms,lbound,ubound,nbnodes)

        implicit none
  
        integer numparms,jj, nbnodes
        real(8) parms(numparms)
        real(8) scale1(numparms),lbound(numparms),ubound(numparms)
 write(*,*) "got to here"
        
        open(unit=200,file='prog1.hops',position='rewind')
        write(200,22) '@ "Problem Definition"          '	
        write(200,32) '"Objective Target" double 0.001'
        write(200,19) '"Scaling" vector',numparms, (scale1(jj),jj=1,numparms) 
        write(200,25) '"Number Unknowns" int ', numparms
        write(200,26) '"Display" int 1'
        write(200,20) '"Initial X" vector',numparms,' ',(parms(jj),jj=1,numparms)
!        write(200,17) '@@                  '	
!        write(200,22) '@ "Linear Constraints"          '	
        write(200,42,advance='no') '"Lower Bounds" vector',numparms, ' '
        jj3: do jj = 1,numparms
           if (lbound(jj).eq.-99.0d-0) then
             write(200,41,advance='no') 'DNE ' 
           else 
             write(200,40,advance='no') lbound(jj),' '
           end if
        end do jj3
        write(200,41) '   '
        write(200,42,advance='no') '"Upper Bounds" vector',numparms, ' '
        jj4: do jj = 1,numparms
           if (ubound(jj).eq.-99.0d-0) then
             write(200,41,advance='no') 'DNE ' 
           else 
             write(200,40,advance='no') ubound(jj),' '
           end if
        end do jj4
        write(200,41) '    '
        write(200,17) '@@                  '	
        write(200,17) '@ "Evaluator"       '	
        write(200,28) '"Evaluator Type" string "System Call" '	
        write(200,21) '"Executable Name" string "./a.out"'
        write(200,24) '"Input Prefix" string "infile.asc"  '
        write(200,24) '"Output Prefix" string "outfile.asc"'
        write(200,29) '"Debug Eval Worker" bool false'
        write(200,30) '"Save IO Files" bool false'
        write(200,17) '@@                  '
        write(200,17) '@ "Mediator"          '
        write(200,27) '"Citizen Count" int 7'
        write(200,51) '"Number Processors" int ', nbnodes
	write(200,45) '"Cache enabled" bool true'
	write(200,45) '"Cache Input File" string "./Evaluated.txt"'
	write(200,45) '"Cache Output File" string "./Evaluated.txt"'
	write(200,45) '"Solution File" string "./bests.txt"'
        write(200,27) '"Number Threads" int 8'
        write(200,35) '"Maximum Evaluations" int 30000'
        write(200,26) '"Display" int 3'
        write(200,17) '@@                  '
        write(200,17) '@ "Citizen 1"          '
        write(200,17) '"Type" string "GSS"'
        write(200,32) '"Step Tolerance" double 0.0001'
        write(200,45) '"Sufficient Improvement Factor" double 0.01'
        write(200,34) '"Display" int 2'
        write(200,35) '"Use Random Order" bool true'
        write(200,33) '"Precision" int 8'
        write(200,30) '"Initial Step" double 1.0'
        write(200,24) '"Ignore Other Points" bool true'
        write(200,24) '"Contraction Factor" double 0.5'
        write(200,17) '@@                  '
        write(200,17) '@ "Citizen 2"          '
        write(200,17) '"Type" string "GSS"'
        write(200,32) '"Step Tolerance" double 0.0001'
        write(200,45) '"Sufficient Improvement Factor" double 0.01'
        write(200,34) '"Display" int 1'
        write(200,35) '"Use Random Order" bool true'
        write(200,33) '"Precision" int 8'
        write(200,30) '"Initial Step" double 0.85'
        write(200,24) '"Ignore Other Points" bool true'
        write(200,24) '"Contraction Factor" double 0.5'
        write(200,17) '@@                  '
        write(200,17) '@ "Citizen 3"          '
        write(200,17) '"Type" string "GSS"'
        write(200,32) '"Step Tolerance" double 0.0001'
        write(200,45) '"Sufficient Improvement Factor" double 0.01'
        write(200,34) '"Display" int 1'
        write(200,35) '"Use Random Order" bool true'
        write(200,33) '"Precision" int 8'
        write(200,30) '"Initial Step" double 0.65'
        write(200,24) '"Ignore Other Points" bool true'
        write(200,24) '"Contraction Factor" double 0.5'
        write(200,17) '@@                  '
        write(200,17) '@ "Citizen 4"          '
        write(200,17) '"Type" string "GSS"'
        write(200,32) '"Step Tolerance" double 0.0001'
        write(200,45) '"Sufficient Improvement Factor" double 0.01'
        write(200,34) '"Display" int 1'
        write(200,35) '"Use Random Order" bool true'
        write(200,33) '"Precision" int 8'
        write(200,30) '"Initial Step" double 0.80'
        write(200,24) '"Ignore Other Points" bool false'
        write(200,24) '"Contraction Factor" double 0.5'
        write(200,17) '@@                  '
        write(200,17) '@ "Citizen 5"          '
        write(200,17) '"Type" string "GSS"'
        write(200,32) '"Step Tolerance" double 0.0001'
        write(200,45) '"Sufficient Improvement Factor" double 0.01'
        write(200,34) '"Display" int 1'
        write(200,35) '"Use Random Order" bool true'
        write(200,33) '"Precision" int 8'
        write(200,30) '"Initial Step" double 1.5'
        write(200,24) '"Ignore Other Points" bool false'
        write(200,24) '"Contraction Factor" double 0.5'
        write(200,17) '@@                  '
        write(200,17) '@ "Citizen 6"          '
        write(200,17) '"Type" string "GSS"'
        write(200,32) '"Step Tolerance" double 0.0001'
        write(200,45) '"Sufficient Improvement Factor" double 0.01'
        write(200,34) '"Display" int 1'
        write(200,35) '"Use Random Order" bool true'
        write(200,33) '"Precision" int 8'
        write(200,30) '"Initial Step" double 1.0'
        write(200,24) '"Ignore Other Points" bool false'
        write(200,24) '"Contraction Factor" double 0.5'
        write(200,17) '@@                  '
        write(200,17) '@ "Citizen 7"          '
        write(200,17) '"Type" string "GSS"'
        write(200,32) '"Step Tolerance" double 0.0001'
        write(200,45) '"Sufficient Improvement Factor" double 0.01'
        write(200,34) '"Display" int 1'
        write(200,35) '"Use Random Order" bool true'
        write(200,33) '"Precision" int 8'
        write(200,30) '"Initial Step" double 0.9'
        write(200,24) '"Ignore Other Points" bool false'
        write(200,24) '"Contraction Factor" double 0.5'
        write(200,17) '@@                  '

        close(200)
27      format(a22)
26      format(a15)
28      format(a37)
17      format(a20)
25      format(a22,i4)
18      format(200e14.7)
19      format(a16,i8,200e14.7)
20      format(a18,i8,a2,200e24.8)
42      format(a21,i8,a2,200e24.8)
40      format(e14.7,a2)
41      format(a4)
21      format(a67)
22      format(a27)
23      format(a39)
24      format(a36)
29      format(a30)
30      format(a30)
31      format(a27)
32      format(a31)
33      format(a18)
34      format(a16)
35      format(a32)
45	format(a45)
51      format(a24,i3)

        open(unit=100,file='infile.asc',position='rewind')
        write(100,*) 'F'
        write(100,*) numparms
        do 100 jj = 1,numparms
           write(100,*) parms(jj)
        100 continue 

        close(100)

	return
	end subroutine setappsfile



        !------------------------------------------------------------------------------
        !       This program sets up the prog1.apps file for use in the progresa program

        PROGRAM setapps

	implicit none

        external setappsfile,ReadIn

	integer, parameter :: ncoeff=150
        character*3 NbNodesChar

        integer totp,Tbar,Sbar,kidmax
	integer nedraw,nmc,FTbar,vl
	integer simul,nsimul,ktp
	integer dnumdraw,snumdraw,nbnodes
        real(8) vseed(3)
	real(8) disc
	real(8) ftol,rtol,temp1
	integer maxit,temp2
        real(8), allocatable :: alpha(:,:)
        real(8), allocatable :: abump(:,:)
        integer, allocatable :: aiter(:,:)
        real(8), allocatable :: alphalow(:,:)
        real(8), allocatable :: alphahigh(:,:)
        integer i,j,numper,numiter
        real(8) tausm,tempvar
        integer checkp,numint,f1ind,sind,regind(75),intmat(75,75)
        integer numz,jj,kk
        real(8),allocatable :: tparms(:)
        real(8),allocatable :: parms(:)
        real(8),allocatable :: scale(:)
        real(8),allocatable :: lbound(:)
        real(8),allocatable :: ubound(:)
        real(8),allocatable :: rvart(:,:,:)
        integer,allocatable :: ivart(:,:,:)   
        integer, allocatable :: fixvar(:,:)        
        real(8), allocatable :: maxsafe(:)        
 	character*200::pathinput

        call getarg(1, NbNodesChar)
        
        read(NbNodesChar,'(I3)') nbnodes 
        write(*,*) "number of nodes", NbNodesChar 
        write(*,*) "number of nodes", nbnodes
        totp = 0
        kidmax = 0
        numper = 0
        vseed = 0
        numiter = 0
        alpha = 0.0d-0
        abump = 0.0d-0
        aiter = 0
        Tbar = 0
        Sbar = 0
        ktp = 4
        allocate(alpha(ncoeff,ktp))      
        allocate(alphalow(ncoeff,ktp))      
        allocate(alphahigh(ncoeff,ktp))      
        allocate(abump(ncoeff,ktp))      
        allocate(aiter(ncoeff,ktp))      
        
        pathinput=""

        call Readin(nedraw,totp,kidmax,ktp,numper,vseed,alpha,abump,aiter,numiter,Sbar,Tbar,nmc,&
             regind,intmat,numint,nsimul,simul, dnumdraw,snumdraw,tausm,rtol,ftol,maxit,alphalow,alphahigh)

        numz = 75+numint
 write(*,*) "got to here"

        allocate(rvart(6,totp,numper))      
        allocate(ivart(9,totp,numper))   
        allocate(fixvar(4,totp))
        allocate(maxsafe(numper))

        numiter = sum(aiter(1:ncoeff,1:ktp))

        write(*,*) 'numiter',numiter

        ALLOCATE(parms(numiter))
        ALLOCATE(scale(numiter))
        ALLOCATE(lbound(numiter))
        ALLOCATE(ubound(numiter))
	ALLOCATE(tparms(numiter))

        parms = 0.0d-0   
        scale = 0.0d-0
        lbound = 0.0d-0
        ubound = 0.0d-0


	i = 0
	if (sum(aiter).gt.0) then
	jj1: do jj = 1,ncoeff
	 kk1: do kk = 1,ktp
	    if (aiter(jj,kk).eq.1) then
	       i = i + 1
               parms(i) = alpha(jj,kk)
               lbound(i) = alphalow(jj,kk)
               ubound(i) = alphahigh(jj,kk)
               scale(i)=abs(abump(jj,kk))
	    end if
	 end do	kk1 !jj
	end do	jj1 !kk
	end if !iterating on values of avec

!        iiloop: do i = 1,numiter
!           if (scale(i).le.0.00001d-0) then
!             scale(i) = 0.1d-0
!           end if
!        end do iiloop 

 write(*,*) "got to here"
        call setappsfile(numiter,scale,parms,lbound,ubound,nbnodes)
        write(*,*) 'finished creating file needed by hopspack'

        END PROGRAM setapps 



	
