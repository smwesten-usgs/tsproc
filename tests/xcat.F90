      program xcat

      integer :: index_cnt
      character(len=255) :: arg
      character(len=255) :: line

      index_cnt = 1
      do
         call get_command_argument(index_cnt, arg)
         index_cnt = index_cnt + 1
         if (len_trim(arg) == 0) exit

         open(unit=11, file=trim(arg))
         do
            read(11, '(a)', end=100, err=100) line
            write(*, '(a)') line(:len_trim(line))
         end do

 100  close(11)
      end do
      end program xcat
