program readnewbest !First argument in the estimation directory, second argument is the number of estimated parameters

!This program reads latest minimum and store parameters in infile.asc

integer:: ct, numparms, jj,evaluated,lastrecord
real,allocatable:: newbest(:),newbest_temp(:)
real:: newmin,newmin_temp
character blah1*3, dir*20, blah3*4
character blah2*4, numparmschar*3, blah4*500

evaluated=1 !get new minimum from list of evaluated points (otherwise from file newbest.txt)
ct = 0
!Get working directory for that estimation job
	call getarg(1,dir)
	dir=trim(adjustl(dir))
	print*,"Reading new minimum from directory", dir
	call getarg(2,numparmschar)
	print*,"Number of estimated parameters is", numparmschar

	!write(dir,fmt='(A1)') dirint
	read(numparmschar(1:3),fmt='(i3)') numparms

	allocate(newbest(numparms))
	allocate(newbest_temp(numparms))

!Initialize
	newbest=0
	newmin=9999999999999999
	newbest_temp=0
	newmin_best=0
	lastrecord=0

if (evaluated.eq.1) then  ! Read in evaluated points from file Evaluated.test
print*, "the path we are using is", adjustl(trim(dir))
        open(file=adjustl(trim(dir))//"/Evaluated.test",unit=1)
        do while (1.eq.1)
                ct=ct+1
		print*,"reading best point nb",ct
		read(unit=1,fmt='(A3)',advance='NO',IOSTAT=lastrecord) blah1
                do jj=1,numparms
                        read(unit=1,fmt='(e22.17)',advance='NO',IOSTAT=lastrecord) newbest_temp(jj) 
                enddo
                read(unit=1,fmt='(A4)',advance='NO',IOSTAT=lastrecord) blah2
                read(unit=1,fmt='(A4)',advance='NO',IOSTAT=lastrecord) blah3
                read(unit=1,fmt='(e22.17)',advance='NO',IOSTAT=lastrecord) newmin_temp
                read(unit=1,fmt=*,END=200) 
	print*,"newmin is equal to",newmin
! Store the minimum in newmin                
		if (newmin_temp.lt.newmin) then
                        newbest=newbest_temp
                        newmin=newmin_temp
                endif
        enddo
	print*,"newbest is equal to", newbest
        close(1)
else

!Read in the new best parameter vector from file newbest
        open(file=adjustl(trim(dir))//"newbest.txt",unit=1)
        read(unit=1,fmt='(A3)',advance='NO') blah1
        read(unit=1,fmt='(e22.17)',advance='NO') newmin 
        read(unit=1,fmt='(A4)',advance='NO') blah2
        read(unit=1,fmt='(A4)',advance='NO') blah3
        read(unit=1,fmt='(<numparms>e22.17)',advance='NO') newbest

        close(1)
endif
200 continue

!Store minimum value in file newmin.txt

	open(file=adjustl(trim(dir))//"/newmin.txt",unit=3)
		write(3,*) newmin
	close(3)

!Store minimum in file infile.asc
	open(file=adjustl(trim(dir))//"/infile.asc",unit=2)
		write(2,*) 'F'
		write(2,*) numparms
		do 100 jj = 1,numparms
   			write(2,*) newbest(jj)
		100 continue 
	close(2)

endprogram
