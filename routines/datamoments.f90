program datamoments


call MSM(nsim,nmoments,n1,n2,criterion,simdata1,simdata2,simdiv,loss,pathoutput,estimation,counterchar)

        open(unit=4004, file=adjustl(trim(pathoutput))//'criterion'//adjustl(trim(counterchar))//'.txt',position='rewind')
        read(4004,*) 'i',' m(i) ', ' mdata(i) ', ' weights(i) ', ' loss(1,i) ','nobs(i)',' name(i) '
        do i=1,nmoments
        read(4004,'(I4,4f20.8,I6,A160)') i, msim(i), mdata(i), weights(i), loss(1,i), nobs(i), momentnames(i)
        enddo
        close(4004)

    open(unit=4002,file='./inputs/mdata9.txt',position='rewind')
    write(4002,*)
    do i=1,nmoments
    write(4002,*) bleh, mdata(i), var(i)
    close(4002)
