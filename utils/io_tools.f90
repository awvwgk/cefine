module io_tools
implicit none

intrinsic :: present,get_command_argument,allocated
character,parameter :: space = ' '
character,parameter :: colon = ':'
character,parameter :: slash = '/'

contains

subroutine get_line(unit,line,iostat)
   use iso_fortran_env, only : iostat_eor
   implicit none
   integer,intent(in) :: unit
   character(len=:),allocatable,intent(out) :: line
   integer,intent(out),optional :: iostat

   integer,parameter  :: buffersize=128
   character(len=buffersize) :: buffer
   integer :: size
   integer :: err

   line = ''
   do
      read(unit,'(a)',advance='no',iostat=err,size=size)  &
      &    buffer
      if (err.gt.0) then
         if (present(iostat)) iostat=err
         return ! an error occurred
      endif
      line = line // buffer(:size)
      if (err.lt.0) then
         if (err.eq.iostat_eor) err = 0
         if (present(iostat)) iostat=err
         return
      endif
   enddo

end subroutine get_line

subroutine read_command_argument(i,arg,iostat)
   implicit none
   integer,intent(in) :: i
   character(len=:),allocatable,intent(out) :: arg
   integer,intent(out),optional :: iostat
   integer :: l,err
   if (allocated(arg)) deallocate(arg)
   call get_command_argument(i,length=l,status=err)
   if (err.ne.0) then
      if (present(iostat)) iostat = err
      return
   endif
   allocate( character(len=l) :: arg, stat=err )
   if (err.ne.0) then
      if (present(iostat)) iostat = err
      return
   endif
   call get_command_argument(i,arg,status=err)
   if (err.ne.0) then
      if (present(iostat)) iostat = err
      return
   endif
   if (present(iostat)) iostat=0
end subroutine read_command_argument

subroutine read_environment_variable(name,var,iostat)
   character(len=*),intent(in) :: name
   character(len=:),allocatable,intent(out) :: var
   integer,intent(out),optional :: iostat
   integer :: l,err
   if (allocated(var)) deallocate(var)
   call get_environment_variable(name,length=l,status=err)
   if (err.ne.0) then
      if (present(iostat)) then
         iostat = err
         return
      endif
   endif
   allocate( character(len=l) :: var, stat=err )
   if (err.ne.0) then
      if (present(iostat)) then
         iostat = err
         return
      endif
   endif
   call get_environment_variable(name,var,status=err)
   if (err.ne.0) then
      if (present(iostat)) then
         iostat = err
         return
      endif
   endif
   if (present(iostat)) iostat=0
end subroutine read_environment_variable

subroutine read_from_path(path,arg,fname,ex)
   implicit none
   character(len=*),intent(in)  :: arg
   character(len=*),intent(in)  :: path
   character(len=:),allocatable,intent(out) :: fname
   logical,intent(out),optional :: ex

!* temporary variables
   character(len=:),allocatable :: scratch1
   character(len=:),allocatable :: scratch2
   character(len=:),allocatable :: fpath
   logical :: exist
   integer :: i

   scratch1 = path
   do
      i = index(scratch1,colon)
      if (i.eq.0) then
         scratch2 = scratch1
      else
         scratch2 = scratch1(:i-1)
         scratch1 = scratch1(i+1:)
      endif
      fpath = scratch2//slash//arg
      inquire(file=fpath,exist=exist)
      if (exist) exit
      if (i.eq.0) exit
   enddo


   if (exist) fname = fpath
   if (present(ex)) ex = exist
   
end subroutine read_from_path

end module io_tools
