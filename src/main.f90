program jolify

    implicit none

    character(len=180), dimension(:), allocatable :: charArray

    call fillCharArrayWithFile ! at this step, all file is in charArray
    call removeDoubleSpaces



    block
    integer :: i
    do i= 1, size(charArray)
        print*,trim(adjustl(charArray(i)))
    end do
    end block
    
    contains
    
    subroutine removeDoubleSpaces
        logical :: continue, changeInTheLoop
        integer :: i, j, k
        continue = .true.
        do while (continue)
            changeInTheLoop = .false.
            do i= 1, size(charArray)
                do j= firstchar(charArray(i)), lastchar(charArray(i))-1
                    if(charArray(i)(j:j)==" " .and. charArray(i)(j+1:j+1)==" ") then
                        changeInTheLoop = .true.
                        do k= j, lastchar(charArray(i))
                            charArray(i)(k:k) = charArray(i)(k+1:k+1)
                        end do
                    end if
                end do
            end do
            if( changeInTheLoop .eqv. .true.) then
                continue = .true.
            else
                continue = .false.
            end if
        end do
    end subroutine
    
    integer function lastchar(line)
        character(len=*), intent(in) :: line
        integer :: i
        lastchar = 0
        do i= len(line), 1, -1
            if( line(i:i)==" " ) then
                cycle
            else if (line(i:i)/=" ") then
                lastchar = i
                exit
            end if
        end do
    end function

    integer function firstchar(line)
        character(len=*), intent(in) :: line
        integer :: i
        firstchar = 0
        do i= 1, len(line)
            if( line(i:i)==" " ) then
                cycle
            else if (line(i:i)/=" ") then
                firstchar = i
                exit
            end if
        end do
    end function
    
    subroutine getFileNameFromCommandArgument (arg)
        character(len=180), intent(out) :: arg    
        integer :: status
        if ( COMMAND_ARGUMENT_COUNT() == 0 ) then
            write(*,*),"Jolify should be executed with the name of a file as argument."
            write(*,*),"For instance $  jolify file.f90"
            stop
        else
            call get_command_argument(number=1, value=arg, status=status)
            if( status > 0 ) then
                stop "Argument retrieval failed"
            else if( status < 0 ) then
                stop "Argument is truncated. length of character arg is too short. Modify the program"
            else
                print*,"Now working on file ", trim(adjustl(arg))
            end if
        end if    
    end subroutine
    
    subroutine fillCharArrayWithFile
        character(len=180) :: arg
        call getFileNameFromCommandArgument (arg)
        call sizeCharArray ( trim(adjustl(arg)) )
        call copyLineByLine ( trim(adjustl(arg)) )
    end subroutine
    
    subroutine sizeCharArray(file)
        character(len=*), intent(in) :: file
        allocate( charArray( countLines(file) ) )
        print*,"number of lines in file is", countLines(file)
    end subroutine

    integer function countLines (file)
        character(len=*), intent(in) :: file
        integer :: iostat
        logical :: continue
        character(len=1) :: c
        countLines = 0
        open(11, file=file, iostat=iostat )
        if ( iostat /= 0 ) then
            print*,"Problem during opening of file",file
            stop
        end if
        continue = .true.
        do while (continue)
            read(11,'(a)', iostat=iostat) c
            if (iostat == 0) then
                countLines = countLines + 1
            else if (iostat > 0) then
                stop "problem while reading file"
            else if (iostat < 0) then
                select case (countLines)
                    case (0) ! file is empty
                        print*, "File is empty. Can't go further."
                        stop
                    case default ! correct end of file
                        continue = .false.
                end select
            end if
        end do
        close(11)
    end function

    subroutine copyLineByLine (file)
        character(len=*), intent(in) :: file
        character(len=180) :: line
        logical :: continue
        integer :: iostat, i
        continue = .true.
        open(11,file=file)
        do i= 1, size(charArray)
            read(11,'(a)',iostat=iostat) line
            if( iostat /= 0) exit
!~             print*,trim(adjustl(line))
            charArray(i) = trim(adjustl(line))
        end do
        close(11)
    end subroutine

!~     
!~     subroutine removeTrailingLines
!~         implicit none
!~         
!~     end subroutine
!~ 
!~     subroutine

end program
