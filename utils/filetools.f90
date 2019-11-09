! This file is part of xtb.
!
! Copyright (C) 2017-2019 Stefan Grimme
!
! xtb is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! xtb is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with xtb.  If not, see <https://www.gnu.org/licenses/>.

module tbmod_file_utils
   implicit none

   type, private :: tb_enum_molecule
      integer :: default = 2  ! Turbomole format is default
      integer :: xyz = 1
      integer :: tmol = 2
      integer :: molfile = 3
      integer :: vasp = 4
      integer :: pdb = 5
      integer :: sdf = 6
   end type tb_enum_molecule
   type(tb_enum_molecule), public, parameter :: p_ftype = tb_enum_molecule()

contains

subroutine file_generate_meta_info(fname, extension, basename, directory)
   character(len=*), intent(in) :: fname
   character(len=:), allocatable, intent(out) :: extension, basename, directory
   integer :: idot, islash
   idot = index(fname, '.', back=.true.)
   islash = index(fname, '/', back=.true.)
   if (idot > islash .and. idot > 0) then
      extension = fname(idot+1:)
   else ! means point is somewhere in the path or absent
      idot = len(fname)+1
   endif
   if (idot > islash .and. islash > 0) then
      basename = fname(islash+1:idot-1)
   endif
   if (islash > 0) directory = fname(:islash)
end subroutine file_generate_meta_info

subroutine file_figure_out_ftype(ftype, extension, basename)
   integer, intent(out) :: ftype
   character(len=*), intent(in) :: extension
   character(len=*), intent(in) :: basename

   ftype = p_ftype%default

   if (len(extension) > 0) then
      select case(lowercase(extension))
      case('coord', 'tmol')
         ftype = p_ftype%tmol
      case('xyz')
         ftype = p_ftype%xyz
      case('mol')
         ftype = p_ftype%molfile
      case('sdf')
         ftype = p_ftype%sdf
      case('poscar', 'contcar', 'vasp')
         ftype = p_ftype%vasp
      case('pdb')
         ftype = p_ftype%pdb
      case default
         if (len(basename) > 0) then
            select case(lowercase(basename))
            case('coord')
               ftype = p_ftype%tmol
            case('poscar', 'contcar')
               ftype = p_ftype%vasp
            end select
         endif
      end select
   endif

end subroutine file_figure_out_ftype

function lowercase(str) result(lcstr)

   character (len=*):: str
   character (len=len_trim(str)):: lcstr

   integer :: ilen,ioffset,iquote,i,iav,iqc

   ilen=len_trim(str)
   ioffset=iachar('A')-iachar('a')
   iquote=0
   lcstr=str
   do i=1,ilen
      iav=iachar(str(i:i))
      if(iquote==0 .and. (iav==34 .or.iav==39)) then
         iquote=1
         iqc=iav
         cycle
      end if
      if(iquote==1 .and. iav==iqc) then
         iquote=0
         cycle
      end if
      if (iquote==1) cycle
      if(iav >= iachar('A') .and. iav <= iachar('Z')) then
         lcstr(i:i)=achar(iav-ioffset)
      else
         lcstr(i:i)=str(i:i)
      end if
   end do
   return

end function lowercase

end module tbmod_file_utils
