program jolify

    implicit none

    character(len=180), dimension(:), allocatable :: charArray

    call fillCharArrayWithFile ! at this step, all file is in charArray
    call removeDoubleSpaces
    call insertSpaceBeforeAndAfter("::")
    call insertSpaceBeforeAndAfter(".eqv.")
    call insertSpaceBeforeAndAfter(".EQV.")
    call insertSpaceBeforeAndAfter(".neq.")
    call insertSpaceBeforeAndAfter(".NEQ.")
    call insertSpaceBeforeAndAfter("==")
    call insertSpaceBeforeAndAfter("/=")
    call insertSpaceBeforeAndAfter(">=")
    call insertSpaceBeforeAndAfter("<=")
    call insertSpaceBeforeAndAfter(">")
    call insertSpaceBeforeAndAfter("<")
    call removeDoubleSpaces
    call indent
    call verifyMaxLength(132)


    block
    integer :: i
    do i= 1, size(charArray)
        print*,trim(charArray(i))
    end do
    end block
    
    contains

    subroutine insertSpaceBeforeAndAfter(ch)
        character(len=*), intent(in) :: ch
        call insertSpaceBefore(ch)
        call insertSpaceAfter(ch)
    end subroutine

    subroutine moveToTheRight(ch,fromPosition,byNChar) ! fromPosition is included in the movement
        character(len=*), intent(inout) :: ch
        integer, intent(in) :: fromPosition, byNChar
        integer :: i, j
        do i = 1, byNchar
            do j = lastchar(ch), fromPosition+i-1, -1
                ch(j+1:j+1) = ch(j:j)
            end do
        end do
    end subroutine

    subroutine insertSpaceBefore(ch)
        character(len=*), intent(in) :: ch
        integer :: i, imin, imax, j, k, jp
        imin = lbound(charArray,1)
        imax = ubound(charArray,1)
        do i = imin, imax
            do j = 1, lastchar(charArray(i))
                jp = j+len(ch)-1
                if( (charArray(i)(j:jp)==ch) .and. (j>1) .and. (charArray(i)(j-1:j-1)/=" ") .and. &
                    (charArray(i)(j-1:j-1)/='"') )then
                    call moveToTheRight(charArray(i),j,1)
                    charArray(i)(j:j) = " "
                end if
            end do
        end do
    end subroutine

    subroutine insertSpaceAfter(ch)
        character(len=*), intent(in) :: ch
        integer :: i, imin, imax, j, k, jp
        imin = lbound(charArray,1)
        imax = ubound(charArray,1)
        do i = imin, imax
            do j = 1, lastchar(charArray(i))
                jp = j+len(ch)-1
                if( (charArray(i)(j:jp)==ch) .and. (j>1) .and. (charArray(i)(jp+1:jp+1)/=" ") .and. &
                    (charArray(i)(jp+1:jp+1)/='"')) then
                    call moveToTheRight(charArray(i),jp+1,1)
                    charArray(i)(jp+1:jp+1) = " "
                end if
            end do
        end do
    end subroutine

    subroutine verifyMaxLength(maxlen)
        integer, intent(in) :: maxlen
        integer :: i
        do i = lbound(charArray,1), ubound(charArray,1)
            if( lastchar(charArray(i)) > maxlen ) then
                print*, "Some lines have more than 132 characters. Not allowed in Fortran."
                print*, "Of course they may be comment lines, but that's not good practice."
                stop
            end if
        end do
    end subroutine

    subroutine indent
        integer, dimension(size(charArray)) :: level
        call levelEachLine(level)
        call indentLines(level)
    end subroutine

    subroutine indentLines(level)
        integer, dimension(size(charArray)) :: level
        integer :: i, j, k
        integer, parameter :: blankPerIndentLevel = 4
        do i = 1, size(charArray)
            if( level(i) < 0 ) stop "negative indentation not allowed"
            do j= 1, level(i)
                do k= lastchar(charArray(i)), firstchar(charArray(i)), -1
                    charArray(i)(k+blankPerIndentLevel:k+blankPerIndentLevel) = charArray(i)(k:k)
                end do
                charArray(i)(firstchar(charArray(i)):firstchar(charArray(i))+blankPerIndentLevel-1) = "    "
            end do
        end do
    end subroutine indentLines

    subroutine levelEachLine(level)
        integer, dimension(size(charArray)), intent(out) :: level
        integer :: fc, lc, fi, i, j
        character(len=len(charArray)) :: line
        level = 0
        fi= size(charArray)
        do i= 1, fi
            line=charArray(i)
            fc = firstchar(line)
            lc = lastchar(line)
            if ( &
                (line(fc:fc+6) == "program") .or. &
                (line(fc:fc+6) == "PROGRAM") .or. &
                (line(fc:fc+6) == "Program") .or. &
                (line(fc:fc+1) == "do") .or. &
                (line(fc:fc+1) == "DO") .or. &
                (line(fc:fc+1) == "Do") .or. &
                (line(fc:fc+9) == "subroutine") .or. &
                (line(fc:fc+9) == "SUBROUTINE") .or. &
                (line(fc:fc+9) == "Subroutine") .or. &
                (line(fc:fc+1) == "if" .and. line(lc-3:lc) == "then") .or. &
                (line(fc:fc+1) == "IF" .and. line(lc-3:lc) == "THEN") .or. &
                (line(fc:fc+1) == "If" .and. line(lc-3:lc) == "Then") .or. &
                (line(fc:fc+1) == "if" .and. line(lc:lc) == "&") .or. &
                (line(fc:fc+1) == "IF" .and. line(lc:lc) == "&") .or. &
                (line(fc:fc+1) == "If" .and. line(lc:lc) == "&") .or. &
                (line(fc:fc+5) == "select") .or. &
                (line(fc:fc+5) == "SELECT") .or. &
                (line(fc:fc+5) == "Select") .or. &
                (line(fc:fc+8) == "elemental") .or. &
                (line(fc:fc+8) == "ELEMENTAL") .or. &
                (line(fc:fc+8) == "Elemental") .or. &
                (line(fc:fc+3) == "pure") .or. &
                (line(fc:fc+3) == "PURE") .or. &
                (line(fc:fc+3) == "Pure") .or. &
                (line(fc:fc+4) == "block") .or. &
                (line(fc:fc+4) == "BLOCK") .or. &
                (line(fc:fc+4) == "Block") .or. &
                (line(fc:fc+15) == "integer function") .or. &
                (line(fc:fc+15) == "INTEGER FUNCTION") .or. &
                (line(fc:fc+15) == "Integer Function") .or. &
                (line(fc:fc+12) == "real function") .or. &
                (line(fc:fc+12) == "REAL FUNCTION") .or. &
                (line(fc:fc+12) == "Real Function") .or. &
                (line(fc:fc+15) == "logical function") .or. &
                (line(fc:fc+15) == "LOGICAL FUNCTION") .or. &
                (line(fc:fc+15) == "Logical Function") .or. &
                (line(fc:fc+4) == "Block") ) then
                level(i+1:fi) = level(i+1:fi) + 1
            else if(    (line(fc:fc+2) == "end") .or. &
                        (line(fc:fc+2) == "END") .or. &
                        (line(fc:fc+2) == "End") ) then
                level(i:fi) = level(i:fi) - 1
            else if(    (line(fc:fc+3) == "else") .or. &
                        (line(fc:fc+3) == "ELSE") .or. &
                        (line(fc:fc+3) == "Else") .or. &
                        (line(fc:fc+3) == "case") .or. &
                        (line(fc:fc+3) == "CASE") .or. &
                        (line(fc:fc+3) == "Case") .or. &
                        (line(fc:fc+7) == "contains") .or. &
                        (line(fc:fc+7) == "CONTAINS") .or. &
                        (line(fc:fc+7) == "Contains") ) then
                level(i) = level(i)-1
            end if
            ! pb with functions
!~             if( lc - fc > len("function") ) then
!~                 do j= fc, lc-len("function")
!~                     if(  ( line(j:j+len("function")-1) == "function") .and. 
!~                 end do
        end do
        if( level(1) /= 0 ) stop "indentation of first line should be 0"
!~         if( level(fi) /= 0) stop "indentation of last line of file should be 0"
!~         if( any(level < 0)) stop "no line should have a negative indent."
        do i = 1, size(charArray)
            print*,i,level(i)
        end do
    end subroutine

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
        character(len=180)    ::     line
        logical::continue
        integer::iostat, i
        continue = .true.
        open(11,file=file)
        do i= 1, size(charArray)
            read(11,'(a)',iostat=iostat) line
            if( iostat /= 0) exit
            charArray(i) = trim(adjustl(line))
        end do
        close(11)
    end subroutine

end program
