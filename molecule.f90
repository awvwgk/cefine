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

!> molecular structure information
! 
!  contains information about the molecular geometry, the atom types
!  the nuclear and total charge, atomic masses and all interatomic distances
!  In periodic calculations the lattice parameters, a Wigner--Seitz cell
!  as well as fractional coordinates are attached to the data type
!
!  Additionally the molecular structure can keep a topology information
!  usually containing the bonds of the system and a list of non-overlapping
!  fragments. Both data containers are optional and need not to be filled
!  by the provider of the structure data.
!
!  Vendor specific information can be stored along with the molecular structure
!  in auxilary data objects. Data objects like PDB or SDF atomic data should
!  be kept as light-weighted as possible and the user of the structure class
!  is not required to care about it existence
module tbdef_molecule
   use iso_fortran_env, only : wp => real64
   implicit none

   public :: tb_molecule
   public :: len

   private


   !> atomic pdb data type
   !
   !  keeps information from PDB input that is currently not used by the
   !  caller program (like residues or chains) but is needed to write
   !  the PDB output eventually
   type :: pdb_data
! ATOM   2461  HA3 GLY A 153     -10.977  -7.661   2.011  1.00  0.00           H
! TER    2462      GLY A 153
! a6----i5---xa4--aa3-xai4--axxxf8.3----f8.3----f8.3----f6.2--f6.2--xxxxxxa4--a2a2
! HETATM 2463  CHA HEM A 154       9.596 -13.100  10.368  1.00  0.00           C
      logical :: het = .false.
      integer :: charge = 0
      character(len=4) :: name = ' '
      character(len=1) :: loc = ' '
      character(len=3) :: residue = ' '
      character(len=1) :: chains = ' '
      character(len=1) :: code = ' '
      character(len=4) :: segid = ' '
   end type pdb_data

   !> sdf atomic data
   !
   !  we only support some entries, the rest is simply dropped.
   !  the format is: ddcccssshhhbbbvvvHHHrrriiimmmnnneee
   type :: sdf_data
      integer :: isotope = 0   !< d field
      integer :: charge = 0    !< c field
      integer :: hydrogens = 0 !< h field
      integer :: valence = 0   !< v field
   end type sdf_data

   !> vasp input data
   !
   !  contains specific vasp keywords that modify the appearance of the
   !  input file and can be used to reproduce it in the output
   type :: vasp_info
      real(wp) :: scale = 1.0_wp
      logical :: selective = .false.
      logical :: cartesian = .false.
   end type vasp_info

   !> Turbomole input data
   !
   !  Saves preferences for cartesian vs. direct coordinates and lattice vs. cell.
   !  Also saves units of input data groups.
   type :: turbo_info
      logical :: cartesian = .true.
      logical :: lattice = .true.
      logical :: angs_lattice = .false.
      logical :: angs_coord = .false.
   end type turbo_info

   !> molecular structure information
   type :: tb_molecule
      integer  :: n = 0            !< number of atoms
      real(wp) :: chrg = 0.0_wp    !< total charge
      integer  :: uhf = 0          !< number of unpaired electrons
      logical  :: pbc(3) = .false. !< periodic dimensions
      integer  :: npbc = 0         !< periodicity of system
      character(len=2),allocatable :: sym(:) !< element symbols
      integer, allocatable :: at(:)          !< ordinal numbers
      real(wp),allocatable :: xyz(:,:)       !< cartesian coordinates in bohr
      real(wp),allocatable :: abc(:,:)       !< fractional coordinates
      real(wp),allocatable :: dist(:,:)      !< interatomic distances
      real(wp),allocatable :: atmass(:)      !< atomic masses in amu
      real(wp),allocatable :: z(:)           !< nuclear charges
      real(wp),allocatable :: cn(:)          !< coordination number
      real(wp) :: cellpar(6) = 0.0_wp        !< cell parameters
      real(wp) :: lattice(3,3) = 0.0_wp      !< direct lattice parameters
      real(wp) :: rec_lat(3,3) = 0.0_wp      !< reciprocal lattice parameters
      real(wp) :: volume = 0.0_wp            !< volume of unit cell
      integer  :: ftype = 0                  !< file type of the input
      character(len=:), allocatable :: name
      !> PDB specific information about residues and chains
      type(pdb_data), allocatable :: pdb(:)
      !> SDF specific information about atom types
      type(sdf_data), allocatable :: sdf(:)
      !> VASP specific information about input type
      type(vasp_info) :: vasp = vasp_info()
      !> Turbomole specific information about input type
      type(turbo_info) :: turbo = turbo_info()
   contains
      procedure :: allocate => allocate_molecule
      procedure :: deallocate => deallocate_molecule
      procedure :: calculate_distances => mol_calculate_distances
      procedure :: read => read_molecule_generic
      procedure :: update => mol_update
      procedure :: wrap_back => mol_wrap_back
   end type tb_molecule


   interface
      module subroutine read_molecule_generic(self, unit, format)
         class(tb_molecule), intent(out) :: self
         integer, intent(in) :: unit
         integer, intent(in), optional :: format
      end subroutine read_molecule_generic
   end interface


   interface len
      module procedure :: mol_length
   end interface


contains


!> obtain number of atoms for molecular structure
!
!  The molecular structure is assumed to be well-behaved in this respect
!  so there is no sanity check on the allocation status.
integer pure elemental function mol_length(self) result(length)
   class(tb_molecule),intent(in) :: self !< molecular structure information
   length = self%n
end function mol_length


!> constructor for molecular structure
subroutine allocate_molecule(self,n)
   implicit none
   class(tb_molecule),intent(inout) :: self !< molecular structure information
   integer,intent(in) :: n
   call self%deallocate
   self%n = n
   allocate( self%at(n),          source = 0 )
   allocate( self%sym(n),         source = '  ' )
   allocate( self%xyz(3,n),       source = 0.0_wp )
   allocate( self%abc(3,n),       source = 0.0_wp )
   allocate( self%dist(n,n),      source = 0.0_wp )
   allocate( self%atmass(n),      source = 0.0_wp )
   allocate( self%z(n),           source = 0.0_wp )
   allocate( self%cn(n),          source = 0.0_wp )
end subroutine allocate_molecule

!> deconstructor for molecular structure
subroutine deallocate_molecule(self)
   implicit none
   class(tb_molecule),intent(inout) :: self !< molecular structure information
   self%n = 0
   self%pbc = .false.
   self%chrg = 0.0_wp
   self%uhf = 0
   self%lattice = 0.0_wp
   if (allocated(self%at))     deallocate(self%at)
   if (allocated(self%sym))    deallocate(self%sym)
   if (allocated(self%xyz))    deallocate(self%xyz)
   if (allocated(self%abc))    deallocate(self%abc)
   if (allocated(self%dist))   deallocate(self%dist)
   if (allocated(self%atmass)) deallocate(self%atmass)
   if (allocated(self%z))      deallocate(self%z)
   if (allocated(self%cn))     deallocate(self%cn)
   if (allocated(self%pdb))    deallocate(self%pdb)
   if (allocated(self%sdf))    deallocate(self%sdf)
   if (allocated(self%name))   deallocate(self%name)
end subroutine deallocate_molecule

subroutine mol_update(self)
   use iso_fortran_env, wp => real64
   use pbc_tools
   implicit none
   class(tb_molecule),intent(inout) :: self  !< molecular structure information

   if (self%npbc > 0) then
      call dlat_to_cell(self%lattice,self%cellpar)
      call dlat_to_rlat(self%lattice,self%rec_lat)
      self%volume = dlat_to_dvol(self%lattice)

      call self%wrap_back
   endif

   call self%calculate_distances

end subroutine mol_update

!> calculates all distances for molecular structures and minimum
!  image distances for peridic structures
subroutine mol_calculate_distances(self)
   use iso_fortran_env, wp => real64
   use pbc_tools
   implicit none
   class(tb_molecule),intent(inout) :: self !< molecular structure information
   integer :: i,j
   if (self%npbc > 0) then
      do i = 1, self%n
         do j = 1, i-1
            self%dist(j,i) = minimum_image_distance(.false.,self%abc(:,i), &
               &              self%abc(:,j),self%lattice,self%pbc)
            self%dist(i,j) = self%dist(j,i)
         enddo
         self%dist(j,i) = minimum_image_distance(.true.,self%abc(:,i), &
            &              self%abc(:,i),self%lattice,self%pbc)
      enddo
   else
      do i = 1, self%n
         do j = 1, i-1
            self%dist(j,i) = norm2(self%xyz(:,j)-self%xyz(:,i))
            self%dist(i,j) = self%dist(j,i)
         enddo
         self%dist(i,i) = 0.0_wp
      enddo
   endif
end subroutine mol_calculate_distances

!> wrap cartesian coordinates back into cell
! 
!  This automatically done when calling @see xyz_to_abc, so we only have
!  to perform the transformation there and back again
subroutine mol_wrap_back(self)
   use iso_fortran_env, wp => real64
   use pbc_tools
   implicit none
   class(tb_molecule),intent(inout) :: self !< molecular structure information
   call xyz_to_abc(self%n,self%lattice,self%xyz,self%abc,self%pbc)
   call abc_to_xyz(self%n,self%lattice,self%abc,self%xyz)
end subroutine mol_wrap_back

end module tbdef_molecule
