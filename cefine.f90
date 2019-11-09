! This file is part of cefine.
!
! Copyright (C) 2006-2019 Stefan Grimme
!
! cefine is free software: you can redistribute it and/or modify it under
! the terms of the GNU Lesser General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! cefine is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with cefine. If not, see <https://www.gnu.org/licenses/>.

Program comand_line_define
   use, intrinsic :: iso_fortran_env
   use io_tools
   use tbdef_argparser
   implicit none
   include 'cefine-version.fh'

   type(tb_argparser) :: ap
   integer io,i,nn
   character*80 out,run1,run2,run3
   character*80 func, func_
   character*80 grid, grid_
   character*80 bas, bas_
   character*80 sym
   character*80 atmp
   character*20 hsstmp
   character*80 coord_name
   character*80 newlib, newlib_
   character*80 dbas
   character*80 homedir,cefinerc
   real*8 extol !tolerance of K integrals
   logical RI, MP2, DFT, VDW, DESY, OPT, KEEP, UHF, MODEHT,QUICK,TROLD
   logical DFPT2, FC, pr, OR, RIK, NOVDW, EX, CP1, CP2, POL, CC, ECP
   logical COORD, FOLD, MOLD, RANGST, SCS, TRUNC,LIB,ALIB,LAP,NDIFF
   logical da,FON,TS,R12,MRCI,COSMO,OPTI,ECHO,TEST,OLDMO,SOS,ZERO,FAKE
   logical strange_elem,diffuse, egrid, XMOL, MARIJ,REF,nori,BJ,ATM,D4
   logical deletion_failed
   logical cosx ! SAW: added seminumerical exchange = COSX
   logical modbas  !basis defined in input
   logical modgrid  !grid defined in input
   logical noauxg  !remove g-fkt from auxbasis
   logical modaux !aux handling defined in input
   logical hf3c ! perform HF-3c calculation
   logical donl ! perform nl non-self-consistent
   logical gcpinfo !for pbe-3c, pbe0-3c, b3-lyp-3c (echo into control)
   integer ricore, scfconv, intmem, thime, maxcor, rpacor
   integer charge, maxiter, nheavy, nopen, nat
   integer kfrag, libnr, l, ntypes, irare, att(20)
   real*8  desythr,xx(5),thize,cosmodk,dum,fp

   !     coord file handling
   real*8 xyz(3,10000)
   integer iat(10000)

   character(len=:), allocatable :: turbodir
   character(len=:), allocatable :: path
   character(len=:), allocatable :: home

   character(len=:), allocatable :: define_prog

   character(len=:), allocatable :: string

   logical :: exist
   logical :: helpme

   pr=.true.

   write(output_unit, '("*",*(1x,a))') &
      & "cefine version", version, "compiled by", author, "on", date

   call read_environment_variable('TURBODIR', turbodir)
   call read_environment_variable('PATH', path)
   call read_environment_variable('HOME', home)

   if (len(turbodir) > 0) then
      write(output_unit, '("TURBODIR:",1x,a)') turbodir
   else
      write(output_unit, '("TURBODIR not found... check Turbomole installation!")')
   endif

   call read_from_path(path, 'define', define_prog, exist)
   if (.not.exist) then
      write(output_unit, '("failed to locate program in path variable")')
      error stop 'define program not found'
   endif

   io=1

   out='def.inp'
   !      run3='define_huge<def.inp>define.out'
   run3='define<def.inp>define.out'

   ! defaults
   maxiter=125
   desythr=0.05
   scfconv=7
   grid   ='m4'
   ricore =1000
   intmem =1000
   maxcor =1000
   rpacor =1000
   thime  =4
   thize  =0.0000001
   !     func   ='tpss'
   func   ='pbeh-3c'
   bas    ='def2-TZVP'
   charge =0
   cosmodk=78.0
   fp     =-3.5
   extol  = -1.0d0      !negatice extol -> not used

   FAKE=.false.
   VDW  =.false.
   ECHO =.false.
   REF=.false.
   XMOL=.false.
   RI   =.true.
   RIK  =.false.
   COSX = .false. !SAW
   R12  =.false.
   MODEHT=.false.
   RANGST=.false.
   DFT  =.true.
   DESY =.true.
   CP1  =.false.
   CP2  =.false.
   OPT  =.true.
   MP2  =.false.
   OPTI =.false.
   KEEP =.false.
   UHF  =.false.
   DFPT2=.false.
   OR   =.false.
   NOVDW=.false.
   EX   =.false.
   POL  =.false.
   COORD=.false.
   FOLD =.false.
   MOLD =.false.
   SCS  =.false.
   SOS  =.false.
   TRUNC=.false.
   FC   =.true.
   LIB  =.false.
   ALIB =.false.
   TS   =.false.
   FON  =.false.
   MRCI =.false.
   COSMO=.false.
   diffuse=.false.
   TEST =.false.
   TROLD =.false.
   OLDMO=.false.
   LAP  =.false.
   EGRID=.false.
   MARIJ=.false.
   QUICK=.false.
   !       BJ=.false.
   !JGB BJ as default (!)
   BJ=.true.
   ZERO=.false.
   !FB ATM
   ATM=.false.
   !FB D4
   D4=.false.
   NDIFF=.false.
   ECP=.false.
   CC=.false.
   modbas=.false.
   modaux=.false.
   noauxg=.false.
   modgrid=.false.
   hf3c=.false.
   !FB NL
   donl=.false.
   !FB gcpinfo
   gcpinfo=.false.

   CALL get_environment_variable("HOME", homedir)
   cefinerc=trim(homedir)//'/.cefinerc'
   inquire(file=cefinerc,exist=da)
   if(da)then
      write(*,*) cefinerc
      open(unit=20,file=cefinerc)
      842     read(20,'(a)',end=942)atmp
      if(index(atmp,'desythr').ne.0)then
         call readl(atmp,xx,nn)
         desythr=xx(nn)
      endif
      if(index(atmp,'scfconv').ne.0)then
         call readl(atmp,xx,nn)
         scfconv=xx(nn)
      endif
      if(index(atmp,'fp').ne.0)then
         call readl(atmp,xx,nn)
         fp     =xx(nn)
      endif
      if(index(atmp,'ricore').ne.0)then
         call readl(atmp,xx,nn)
         ricore=idint(xx(nn))
      endif
      if(index(atmp,'maxcor').ne.0)then
         call readl(atmp,xx,nn)
         maxcor=idint(xx(nn))
      endif
      if(index(atmp,'twoint').ne.0)then
         call readl(atmp,xx,nn)
         intmem=idint(xx(nn))
      endif
      if(index(atmp,'func').ne.0)then
         call backstring(atmp,func,4)
      endif
      if(index(atmp,'bas').ne.0)then
         call backstring(atmp,bas,3)
         modbas=.true.
      endif
      if(index(atmp,'grid').ne.0)then
         call backstring(atmp,grid,4)
         egrid=.true.
         modgrid=.true.
      endif
      if(index(atmp,'vdw').ne.0) then
         if(index(atmp,'on').ne.0)BJ=.true.
      endif
      if(index(atmp,'marij').ne.0) then
         MARIJ=.true.
         RI=.true.
      endif
      if(index(atmp,'no-rij').ne.0) then
         NORI=.true.
      endif
      if(index(atmp,'echo').ne.0)then
         if(index(atmp,'on').ne.0)ECHO=.true.
      endif
      if(index(atmp,'nodiff').ne.0)NDIFF=.true.
      goto 842
      942     close(20)
   endif

   call ap%new
   write(output_unit, '("provided",1x,i0,1x,"arguments")') size(ap)
   call ap%write(output_unit)

   helpme = ap%has_option('h') .or. ap%has_option('help')
   if (helpme) call helpmessage(output_unit)

   ref = ap%has_option('ref')
   fake = ap%has_option('fake')
   ecp = ap%has_option('ecp')
   if (ap%has_option('d3')) bj = .true.
   if (ap%has_option('d3atm')) then
      bj = .true.
      atm = .true.
   endif
   if (ap%has_option('zero')) then
      zero = .true.
      bj = .false.
   endif
   if (ap%has_option('d4')) then
      d4 = .true.
      bj = .false.
      atm = .false.
      zero = .false.
   endif
   donl = ap%has_option('donl')
   quick = ap%has_option('quick')
   if (ap%has_option('nodiff')) ndiff = .true.
   if (ap%has_option('nolap')) lap = .false.
   if (ap%has_option('noscs')) scs = .false.
   if (ap%has_option('lap') .or. ap%has_option('lsos')) then
      LAP=.true.
      DFT=.false.
      NOVDW=.true.
      func='tpss'
   endif
   if (ap%has_option('scs')) then
      SCS=.true.
      DFT=.false.
      NOVDW=.true.
      func='tpss'
   endif
   if (ap%has_option('marij')) marij = .true.
   fold = ap%has_option('fold')
   mold = ap%has_option('mold')
   modeht = ap%has_option('modeht')
   if (ap%has_option('nofc')) fc = .false.
   or = ap%has_option('or')
   pol = ap%has_option('pol')
   ex = ap%has_option('ex')
   if (ap%has_option('nori')) ri = .false.
   if (ap%has_option('ri')) then
      ri = .true.
   endif
   if (ap%has_option('noopt')) opt = .false.
   opti = ap%has_option('opt')
   if (ap%has_option('hf')) dft = .false.
   if (ap%has_option('mp2')) then
      MP2=.true.
      DFT=.false.
      NOVDW=.true.
      func='tpss'
   endif
   if (ap%has_option('vdw')) vdw = .true.
   if (ap%has_option('novdw')) novdw = .true.
   if (ap%has_option('cp1')) cp1 = .true.
   if (ap%has_option('cp2')) cp2 = .true.
   if (ap%has_option('rijk')) rik = .true.
   if (ap%has_option('rijcosx')) cosx = .true.
   if (ap%has_option('trunc')) trunc = .true.
   if (ap%has_option('fon')) FON = .true.
   if (ap%has_option('ts')) TS = .true.
   if (ap%has_option('r12')) R12 = .true.
   if (ap%has_option('dftmrci')) then
      MRCI = .true.
      NOVDW = .true.
      func = 'bh-lyp'
   endif
   if (ap%has_option('echo'))  ECHO=.true.
   if (ap%has_option('diff'))  diffuse=.true.
   if (ap%has_option('test'))  TEST=.true.
   if (ap%has_option('oldmo')) OLDMO=.true.
   if (ap%has_option('pmos')) OLDMO=.true.
   if (ap%has_option('trold')) TROLD=.true. ! cbann: enable use of old hessian for TS search
   if (ap%has_option('cc')) then
      CC=.true.
      DFT=.false.
      NOVDW=.true.
      func='tpss'
   endif
   !JGB include hf-3c compound key
   if (ap%has_option('hf3c')) then
      hf3c=.true.
      dft=.false.
      RI=.false.
   endif
   !JGB remove g functions from auxbasis
   if (ap%has_option('noauxg')) then
      noauxg=.true.
      modaux=.true.
   endif
   !JGB do not remove g functions in auxbasis
   if (ap%has_option('auxg')) then
      noauxg=.false.
      modaux=.true.
   endif
   call ap%get_option('chrg', i, exist)
   if (exist) charge = i
   call ap%get_option('cosmo', dum, exist)
   if (exist) then
      if (dum > 0.999) cosmodk = dum
      cosmo = .true.
   endif
   call ap%get_option('scfconv', i, exist)
   if (exist) scfconv = i
   !JGB K tolerance
   call ap%get_option('ktol', dum, exist)
   if (exist) extol = dum
   call ap%get_option('uhf', i, exist)
   if (exist) then
      nopen = i
      uhf = .true.
   endif
   call ap%get_option('sym', sym, exist)
   if (exist) desy = .false.
   if (ap%has_option('abel')) call susy(sym, desy)
   !c keep outputs for debuging purposes
   if (ap%has_option('keep')) KEEP=.true.
   call ap%get_option('grid', grid_, exist)
   if (exist) then
      grid = grid_
      modgrid = .true.
   endif
   call ap%get_option('bas', bas_, exist)
   if (exist) then
      bas = bas_
      modbas = .true.
   endif
   call ap%get_option('func', func_, exist)
   if (exist) then
      func = func_
      dft = .true.
   endif
   if (ap%has_option('angst'))RANGST=.true.
   if (ap%has_option('lib')) then  ! ... hok
      LIB=.true.
      !  no need for more than one own basis set library?
      !          call readl(arg(i+1),xx,nn)
      !            libnr=idint(xx(1))
      libnr=3
   endif
   call ap%get_option('auxlib', newlib_, exist)
   if (exist) then
      alib = .true.
      newlib = newlib_
   endif

   if (uhf) write(output_unit, '("*** UHF switched ON ***")')

   if (len(ap) > 0) then
      do i = 1, len(ap)
         call ap%get_argument(string)
         write(output_unit, '("unknown argument: ''",a,"''")') string
      enddo
      error stop 'unknown command line arguments provided'
   endif

   !c read possible file .SYM and .UHF

   inquire(file='.SYM',exist=da)
   if(da)then
      open(unit=21,file='.SYM')
      read(21,'(a)') sym
      if(pr) write(*,'(''!!! symmetry enforced by user in <.SYM>   : '',a,'' !!!'')')trim(sym)
      DESY=.false.
   endif
   inquire(file='.UHF',exist=da)
   if(da)then
      open(unit=21,file='.UHF')
      read(21,*) nopen
      UHF=.true.
      if(pr) write(*,'(''!!! UHF enforced by user in <.UHF> . Na-Nb:'',i4,'' !!!'')')nopen
   endif
   inquire(file='.CHRG',exist=da)
   if(da)then
      open(unit=21,file='.CHRG')
      read(21,*) charge
      if(pr)write(*,'(''!!! charge in <.CHRG> :'',i4,'' !!!'')')charge
   endif

   ! cbann: if trold, check for hessian files. If present, also activate TS
   ! and save hessian
   if(TROLD) then
      inquire(file='hessian',exist=da)
      if(da) then
         call execute_command_line("exec mv hessian  hss.tmp")
         TS=.true.
      endif
      inquire(file='hessian_driver',exist=da)
      if(da) then
         call execute_command_line("cat hessian_driver|sed 's/$hessian.*/$hessian (projected)/' > hss.tmp")
         TS=.true.
      endif
   endif

   if(MOLD)call execute_command_line('mv mos mos.tmp')
   if(OLDMO) then
      call execute_command_line('rm -fr TMP.MOS')
      write(*,*) '* projecting old mos to new basis *'
      call execute_command_line("cpc TMP.MOS &> /dev/null ")
   endif


   call execute_command_line('rm -rf control basis auxbasis mos alpha beta')

   ! c why?
   !      if((.not.OPT).and.(.not.MP2))scfconv=6
   !      if(CP) scfconv=8
   if(POL) then
      scfconv=7
      grid='3'
   endif
   ! c correct nonsense options
   if(LAP) SOS=.true.
   if(MP2.and.DFT)DFT=.false.
   if(SCS.and.DFT)DFT=.false.
   if(SOS.and.DFT)DFT=.false.
   if(CC.and.DFT)DFT=.false.
   if(MP2        )VDW=.false.
   if(MP2        )BJ=.false.
   if(MP2        )ZERO=.false.
   if(MP2        )ATM=.false.
   if(MP2        )D4=.false.
   if(VDW.and.BJ)BJ=.false.
   if(MP2.or.SCS.or.SOS)OPT=.false.
   if(MP2.or.SCS) LAP=.false.
   if(rik.and.cosx) cosx=.false. ! SAW: don't use both

   ! c dfpt2 special
   if(func.eq.'b2-plyp'.or.func.eq.'b2gp-plyp'.or.func.eq.'ptpss'.or.MRCI.or.func.eq.'pwpb95'.or.func.eq.'dsd-blyp') then
      MP2  =.true.
      SCS  =.false.
      DFPT2=.true.
      FC   =.false.
      OPT  =.false.
      if(ex)FC=.true.
      if(.not.EGRID) grid='m5'
      scfconv=7
   endif
   !      if(func.eq.'b97-d'.and.DFT)VDW=.true.
   !      if(func.eq.'ptpss'.or.func.eq.'pwpb95')then
   !         SOS=.true.
   !         SCS=.false.
   !      endif
   if(NOVDW                  )then
      VDW=.false.
      BJ=.false.
      ATM=.false.
      ZERO=.false.
      D4=.false.
      donl=.false.
   endif
   if(donl)then
      VDW=.false.
      BJ=.false.
      ATM=.false.
      ZERO=.false.
      D4=.false.
      sym='c1'
      DESY=.false.
   endif


   !JGB define PBEh-3c defaults
   if(func.eq.'pbeh3c') func='pbeh-3c'
   if(func.eq.'pbeh-3c'.and.DFT) then
      !FB needs .and.DFT else D4 turned off for HF
      if(.not.modbas) bas='def2-mSVP'
      if(.not.modgrid) grid='m4'
      if(extol.lt.0) extol= 2.5d0
      if(.not.novdw) bj=.true.
      if(.not.modaux) noauxg=.true.
      D4=.false.
      donl=.false.
   endif
   !FB define B97-3c defaults
   if(func.eq.'b97-3c') then
      if(.not.modbas) bas='def2-mTZVP'
      if(.not.modgrid) grid='m4'
      if(.not.novdw) then
         BJ=.true.
         ATM=.true.
         D4=.false.
         donl=.false.
      endif
      ! not sure about noauxg (ask stefan)
   elseif(func.eq.'b973c') then
      write(*,*) "Using the slower b973c because of XCFun e.g. for hessian calculation"
      ! b973c uses XCFun for hessian calculation
      if(.not.modbas) bas='def2-mTZVP'
      if(.not.modgrid) grid='m4'
      if(.not.novdw) then
         BJ=.true.
         ATM=.true.
         D4=.false.
      endif
   endif

   !JGB define HF-3c defaults
   if(hf3c) then
      write(*,*)'Setting up HF-3c calculation!'    !FB
      if(.not.modbas) bas='minix'
      if(.not.novdw) bj=.true.
      if(.not.modaux.and.RI) noauxg=.true.
      D4=.false.
      donl=.false.
   endif
   !FB define PBE-3c defaults
   if(func.eq.'pbe3c') func='pbe-3c'
   if(func.eq.'pbe-3c')then
      write(*,*) 'Setting up PBE-3c calculation (NOT PBEh-3c)!'
      func='pbe'
      gcpinfo=.true.
      if(.not.novdw)then
         BJ=.true.
         ATM=.true.
         D4=.false.
         donl=.false.
      endif
      if(.not.modgrid)then
         grid='m3'
      endif
      if(.not.modbas) then
         bas='def2-mSVP'
      endif
      if(.not.modaux) noauxg=.true.
      if(extol.lt.0) extol= 2.5d0
   endif
   !FB define B3-LYP-3c defaults
   if(func.eq.'b3-lyp-3c'.or.func.eq.'b3lyp-3c')then
      write(*,*) 'Setting up B3-LYP-3c calculation!'
      func='b3-lyp'
      gcpinfo=.true.
      if(.not.novdw)then
         BJ=.true.
         ATM=.true.
         D4=.false.
         donl=.false.
      endif
      if(.not.modgrid)then
         grid='m4'
      endif
      if(.not.modbas) then
         bas='def2-mSVP'
      endif
      if(.not.modaux) noauxg=.true.
      if(extol.lt.0) extol= 2.5d0
   endif
   !FB define PBE0-3c defaults
   if(func.eq.'pbe0-3c'.or.func.eq.'pbe03c')then
      write(*,*) 'Setting up PBE0-3c calculation!'
      func='pbe0'
      gcpinfo=.true.
      if(.not.novdw)then
         BJ=.true.
         ATM=.true.
         D4=.false.
         donl=.false.
      endif
      if(.not.modgrid)then
         grid='m4'
      endif
      if(.not.modbas) then
         bas='def2-mSVP'
      endif
      if(.not.modaux) noauxg=.true.
      if(extol.lt.0) extol= 2.5d0
   endif



   ! c do reference single point?
   if(REF) then
      OPT=.false.
      if (scfconv.lt.7) then
         scfconv=7
         write(*,*) 'increasing scfconv to 7'
      endif
   endif

   ! Quick&Dirty

   if(QUICK) then
      func='pwlda'
      bas='SVP'
      grid='1'
      OPT=.false.
   endif

   ! c dft/mrci
   if(MRCI)then
      func='bh-lyp'
      FC   =.false.
      VDW  =.false.
      BJ   =.false.
      ZERO =.false.
      D4   =.false.
   endif
   ! c override the default or derived setting via input
   if(OPTI) OPT=.true.
   if(OPTI.and.MP2) RI=.false.
   ! c check for hybrid
   if(NORI) then
      if( index(func,'b3-lyp').ne.0 &
         .or.index(func,'bh-lyp').ne.0 &
         .or.index(func,'b2-plyp' ).ne.0 &
         .or.index(func,'b2gp-plyp' ).ne.0 &
         .or.index(func,'ptpss' ).ne.0 &
         .or.index(func,'dsd-blyp' ).ne.0 &
         .or.index(func,'tpssh' ).ne.0 &
         .or.index(func,'pbe38' ).ne.0 &
         .or.index(func,'pw6b95' ).ne.0 &
         .or.index(func,'pwpb95' ).ne.0 &
         .or.index(func,'pbe0'  ).ne.0 &
         .or.index(func,'pwb95'  ).ne.0 &
         .or.index(func,'mpw1b95'  ).ne.0 &
         .or.index(func,'mpwb1k'  ).ne.0 &
         .or.index(func,'pbe0'  ).ne.0 &
         ) then
         RI=.false.
      elseif(.not.DFT) then
         RI=.false.
      endif
   endif
   if(RIK) RI=.true.
   if (COSX) RI=.true.
   ! c how many different heavy atoms
   call atoms(nheavy,nat,ntypes,strange_elem,irare,att)
   if(nat.eq.1) OPT=.false.

   if(io.ne.6)open(unit=io,file=out)
   ! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   ! c coord
   write(io,*)'    '
   write(io,*)'    '
   if(COORD)then
      if(RANGST)then
         write(io,'(''aa '',a)')trim(coord_name)
      else
         write(io,'(''a '',a)')trim(coord_name)
      endif
   else
      if(RANGST)then
         write(io,*)'aa coord'
      else
         write(io,*)'a coord'
      endif
   endif
   if(DESY) then
      write(io,'('' desy '',f6.2)')desythr
   else
      write(io,'('' sy '',a)')trim(sym)
   endif
   if(OPT ) write(io,'('' ired '')')
   write(io,*)'*'
   if(.not.OPT ) write(io,'('' no '')')
   ! c basis
   ! hok
   ! own basis library in /$HOME/.definerc
   if(LIB) then
      write(io,*) 'lib'
      write(io,*) libnr
   endif
   write(io,'('' b all '',a20)') bas
   ! ECPs
   if(ECP) then
      write(*,*) 'WARNING: Check BASIS/ECPs !!'
      write(io,*)'ecp  all ecp-2-sdf'
      do l=1,ntypes
         write(io,'('' '')')
         write(io,'('' '')')
      enddo
      write(io,*)'ecp  all ecp-10-sdf'
      do l=1,ntypes
         write(io,'('' '')')
         write(io,'('' '')')
      enddo
   endif
   ! c add diffuse
   if(diffuse)then
      open(unit=142,file='dbas')
      do i=1,ntypes
         read(142,'(a)')dbas
         ! c        if(att(i).le.2)dbas='1s1p'
         if(pr)write(*,*) 'adding functions ',dbas
         write(io,'('' bm'')')
         if(ntypes.gt.1)write(io,'('' #'',i1)') i
         write(io,'('' flat'')')
         write(io,'(a)') dbas
         write(io,'('' '')')
         write(io,'('' '')')
         write(io,'('' '')')
         write(io,'('' '')')
      enddo
      close(142)
   endif
   ! c CP correction
   if(CP1.or.CP2)then
      call execute_command_line('splitmol > splitmol.tmp')
      open(unit=43,file='splitmol.tmp')
      read(43,'(a)')atmp
      read(43,'(a)')atmp
      read(43,'(a)')atmp
      read(43,'(a)')atmp
      if(CP2)then
         read(43,'(a)')atmp
         read(43,'(a)')atmp
      endif
      write(io,*)'c'
      write(io,*)trim(atmp)
      write(io,*)'0.0'
      close(43,status='delete')
      close(43)
   endif
   write(io,*)'*'
   ! c HUECKEL
   if(OLDMO)then
      write(io,*)'use TMP.MOS/control'
   else
      write(io,*)'eht '
      if(MODEHT)then
         write(io,*)'n'
         write(io,*)'y'
         write(io,*)'1.1'
         write(io,*)'n'
         write(io,*)'n'
      endif
      ! cts
      if(strange_elem) write(io,*)'    '
      write(io,*)'    '
      ! c     do i=1,1+nheavy
      ! c        write(io,*)'    '
      ! c     enddo
      write(io,*)charge
      if(nat.eq.1)write(io,*)'    '
      if(UHF)then
         write(io,*)'n'
         if(nopen.ne.0)then
            write(io,*)'uf ',nopen
         else
            ! we want maybe triplett cases
            write(io,*)'s'
         endif
         ! ?? why list?? the list
         !         write(io,*)'l '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         write(io,*)' '
         if(irare.eq.0)write(io,*)'*'
      endif !uhf
   endif ! mos-setup
   write(io,*)'    '
   write(io,*)'    '
   write(io,*)'    '
   write(io,*)'    '
   write(io,*)'    '
   write(io,*)'    '
   write(io,*)'    '
   ! c DFT
   if(DFT.or.DFPT2)then
      write(io,*)'dft'
      write(io,*)'on '
      write(io,*)'func'
      write(io,*)'b-p'
      ! c         write(io,'(a20)') func
      write(io,*)'grid'
      write(io,'(a20)') grid
      write(io,*)'q'
   endif
   if(RI)then
      write(io,*)'ri '
      write(io,*)'on '
      if(ALIB) then
         do l=1,ntypes     ! hok
            write(io,*)'newlib '
            write(io,*) trim(newlib)
            write(io,*) 'lib'
         enddo
      endif
      write(io,*)'m  '
      write(io,*)ricore
      write(io,*)'q'
   endif
   if(RIK)then
      write(io,*)'rijk'
      write(io,*)'on'
      write(io,*)'m'
      write(io,*)ricore
      write(io,*)'q'
      ! SAW added COSX support
   elseif(COSX) then
      write(io,*)'senex'
      write(io,*)'on'
      write(io,*)'y'
      write(io,*)'g'
      write(io,'(a20)') grid
      write(io,*)'y'
      write(io,*)'q'
   endif

   ! c special
   write(io,*)'scf'
   write(io,*)'iter'
   write(io,*)maxiter
   write(io,*)'conv'
   write(io,*)scfconv
   write(io,*)'thi'
   write(io,*)thize
   write(io,*)thime
   write(io,*)'ints'
   write(io,*)'y'
   write(io,*)intmem,' twoint'
   write(io,*)'*'
   write(io,*)'    '
   ! c mp2
   if(MP2.or.SCS.or.SOS)then
      write(io,*)'mp2'
      write(io,*)'cbas'
      if(ALIB) then
         do l=1,ntypes     ! hok
            write(io,*)'newlib '
            write(io,*) trim(newlib)
            write(io,*) 'lib'
         enddo
      endif
      write(io,*)'*'
      if(FC)then
         write(io,*)'freeze'
         write(io,*)'fp ',fp
         write(io,*)'*'
      endif
      write(io,*)'other'
      if(.not.OPT)write(io,*)'emp2'
      write(io,*)'*'
      write(io,*)'*'
   endif
   ! c CCSD(T)
   if(CC) then
      write(io,*)'cc2'
      write(io,*)'cbas'
      if(ALIB) then
         do l=1,ntypes     ! hok
            write(io,*)'newlib '
            write(io,*) trim(newlib)
            write(io,*) 'lib'
         enddo
      endif
      write(io,*)'*'
      if(FC)then
         write(io,*)'freeze'
         write(io,*)'fp ',fp
         write(io,*)'*'
      endif
      write(io,*) 'ricc2'
      write(io,*) 'ccsd(t)'
      write(io,*) '*'
      write(io,*)'*'
   endif
   ! c R12
   if(R12)then
      write(io,*)'cc2'
      write(io,*)'mp2-f12'
      write(io,*)'*'
      write(io,*)'cabs'
      write(io,*)'*'
      write(io,*)'*'
   endif

   if(RIK.and.TRUNC)then
      write(io,*)'trunc'
   endif
   ! c end
   write(io,*)'q'
   ! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   if(io.ne.6)close(io)

   run1='HF'
   if(DFT)run1='DFT'
   if(MP2)run1='RI-MP2'
   if(SCS)run1='RI-SCS-MP2'
   if(SOS)run1='RI-SOS-MP2'
   if(DFT.and.RI)run1='RI-DFT'
   if(DFPT2)run1='DHDF'
   if(DFPT2.and.RI)run1='RI-DHDF'
   if(DFT.or.DFPT2)then
      write(run2,'(a,''('',a2,'')'')')trim(run1),trim(grid)
   else
      func=''
      run2=run1
   endif
   if(VDW)write(run2,'(a,''-D'')')trim(run2)

   if(pr)write(*,'('' * '',a,''-'',a,''/'',a,'' * '')')trim(run2),trim(func),trim(bas)

   ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   if(.not.TEST) then
      call execute_command_line('rm -f control basis auxbasis mos alpha beta')

      deletion_failed=.false.
      INQUIRE(FILE="auxbasis", EXIST=da)
      if(da)deletion_failed=.true.
      INQUIRE(FILE="basis", EXIST=da)
      if(da)deletion_failed=.true.
      INQUIRE(FILE="control", EXIST=da)
      if(da)deletion_failed=.true.
      INQUIRE(FILE="mos", EXIST=da)
      if(da)deletion_failed=.true.
      INQUIRE(FILE="alpha", EXIST=da)
      if(da)deletion_failed=.true.
      INQUIRE(FILE="beta", EXIST=da)
      if(da)deletion_failed=.true.

      if(deletion_failed) stop 'files remaining from previous setup, exiting'

      call flush(6) ! cbannwarth 21.08.13: use unit=6 (stdout) to prevent crash when executing ifort-compiled binary
      !         call flush
      !         call execute_command_line('sync')
      ! FB use old $tmole script to keep standartized define, else error with Pd and charge in TM.7.2.1
      !call execute_command_line('echo ''$tmole'' > control; echo ''$end'' >> control')
      call execute_command_line(run3)
   else
      stop 'def.inp written.'
   endif
   ! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
   ! c     call execute_command_line('sdg symmetry')
   if(pr)write(*,'(''#atoms='',i3)')nat
   ! c     call execute_command_line('grep "nbf(AO)" control')
   call execute_command_line('eiger > tmp.eiger')
   call execute_command_line('grep "Number of MOs" tmp.eiger')

   if(MRCI) then
      ! c       call freeze(UHF)
      if(pr)then
         write(*,*)'!!! SET DFT/MRCI FROZEN ORBS AFTER SCF MANUALLY !!!'
         write(*,*)'    (e.g. freeze script: freeze -3.0 2.0)'
         write(*,*)'writing mrci example input <dftmrci.inp> '
      endif
      open(unit=33,file='dftmrci.inp')
      write(33,*)'$method dftci'
      write(33,*)'$roots 2 0 0 0 0 0 0 0'
      write(33,*)'$mul=1'
      write(33,*)'$ciref 4 4 2'
      write(33,*)'$esel=0.8'
      close(33)
   endif

   if(EX)then
      call execute_command_line("echo '$scfinstab rpas' >>control")
      call execute_command_line("echo '$soes' >>control")
      call execute_command_line("echo ' <irrep> <# states>' >>control")
      call freeze(UHF)
      if(pr)then
         write(*,*)'!!! SETTING MP2 FROZEN CORE !!!'
         write(*,*)'!!! PLEASE ADJUST $soes     !!!'
      endif
   endif

   ! c     if( index(func,'fith').ne.0)then
   ! c         call execute_command_line('kdg dft')
   ! c         call execute_command_line('kdg end')
   ! c         call execute_command_line("echo '$dft' >>control")
   ! c         call execute_command_line("echo ' functional fith' >>control")
   ! c         call execute_command_line("echo ' gridsize m4'     >>control")
   ! c         call execute_command_line("echo '$end' >>control")
   ! c     elseif(index(func,'fit').ne.0)then
   ! c         call execute_command_line('kdg dft')
   ! c         call execute_command_line('kdg end')
   ! c         call execute_command_line("echo '$dft' >>control")
   ! c         call execute_command_line("echo ' functional fit' >>control")
   ! c         call execute_command_line("echo ' gridsize m4'    >>control")
   ! c         call execute_command_line("echo '$end' >>control")
   ! c     endif

   ! c specials
   if(POL)call execute_command_line('kdg dft')
   call execute_command_line('kdg maxcor')
   call execute_command_line('kdg rpacor')
   call execute_command_line('kdg end')
   write(atmp,1001)maxcor
   call execute_command_line(atmp)
   1001 format(' echo ''$maxcor ',i5,' ''>>control')
   write(atmp,1002)rpacor
   call execute_command_line(atmp)
   1002 format(' echo ''$rpacor ',i5,' ''>>control')

   call execute_command_line("echo '$pop' >>control")
   if(RI.and.(.not.MP2)) call execute_command_line("echo '$jbas file=auxbasis'>>control")

   if(VDW) call execute_command_line("echo '$vdwx' >>control")
   ! c     if(DFPT2) call execute_command_line("kdg denconv")
   if(FOLD)  call execute_command_line("kdg forceinit")
   if(OR)then
      call execute_command_line("echo '$scfinstab dynpol nm'>>control")
      call execute_command_line("echo ' 589.3'>>control")
      call execute_command_line("echo '$rpacor 1000'>>control")
      call execute_command_line("echo '$rpaconv 4 '>>control")
      call execute_command_line("echo '$velocity gauge '>>control")
   endif
   if(FON)then
      call execute_command_line("echo '$fermi tmstrt=5000.0 tmend=5000.0 tmfac=1.000' &
         ' hlcrt=1.0 stop=0.001' >>control")
   endif
   if(TS) then
      call execute_command_line("echo '$statpt'   >>control")
      call execute_command_line("echo '   itrvec 1' >>control")
      call execute_command_line("echo '   tradius 0.05 ' >>control")
      call execute_command_line("echo '   radmax  0.05 ' >>control")
      call execute_command_line("echo '   threchange  5.0d-7' >>control")
      call execute_command_line("echo '   thrrmsgrad  5.0d-5' >>control")
      call execute_command_line("echo '   thrmaxdispl 1.0d-1' >>control")
      call execute_command_line("echo '   thrrmsdispl 1.0d-1' >>control")
   else
      call execute_command_line("echo '$statpt'   >>control")
      call execute_command_line("echo '   itrvec 0' >>control")
      call execute_command_line("echo '   tradius 0.3 ' >>control")
      call execute_command_line("echo '   threchange  5.0d-7' >>control")
      call execute_command_line("echo '   thrrmsgrad  5.0d-5' >>control")
      call execute_command_line("echo '   thrmaxdispl 1.0d-1' >>control")
      call execute_command_line("echo '   thrrmsdispl 1.0d-1' >>control")
   endif
   ! specify where to find hessian
   if(TROLD) then
      call execute_command_line('mv hss.tmp hessian')
      open(file='hessian',unit=44,status='old')
      rewind(44)
      hsstmp='                    '
      read(44,'(a20)')hsstmp
      close(44)
      write(atmp,'(a,a,a)')"echo '",hsstmp," file=hessian' >> control"
      call execute_command_line(atmp)
   endif
   if(COSMO) then
      call execute_command_line("echo '$cosmo'   >>control")
      write(atmp,1003)cosmodk
      1003    format(' echo '' epsilon= ',f6.2,' ''>>control')
      call execute_command_line(atmp)
   endif
   if(POL)then
      call execute_command_line("echo '$scfinstab dynpol a.u.'>>control")
      call execute_command_line("echo ' 0.000001 i'>>control")
      call execute_command_line("echo ' .05 i'>>control")
      call execute_command_line("echo ' 0.1 i'>>control")
      call execute_command_line("echo ' 0.2 i'>>control")
      call execute_command_line("echo ' 0.3 i'>>control")
      call execute_command_line("echo ' 0.4 i'>>control")
      call execute_command_line("echo ' 0.5 i'>>control")
      call execute_command_line("echo ' 0.6 i'>>control")
      call execute_command_line("echo ' 0.7 i'>>control")
      call execute_command_line("echo ' 0.8 i'>>control")
      call execute_command_line("echo ' 0.9 i'>>control")
      call execute_command_line("echo ' 1.0 i'>>control")
      call execute_command_line("echo ' 1.2 i'>>control")
      call execute_command_line("echo ' 1.4 i'>>control")
      call execute_command_line("echo ' 1.6 i'>>control")
      call execute_command_line("echo ' 1.8 i'>>control")
      call execute_command_line("echo ' 2.0 i'>>control")
      call execute_command_line("echo ' 2.5 i'>>control")
      call execute_command_line("echo ' 3.0 i'>>control")
      !        call execute_command_line("echo ' 4.0 i'>>control")
      !        call execute_command_line("echo ' 5.0 i'>>control")
      !        call execute_command_line("echo ' 7.5 i'>>control")
      !        call execute_command_line("echo '10.0 i'>>control")
      call execute_command_line("echo '$rpacor 1000'>>control")
      call execute_command_line("echo '$rpaconv 4 '>>control")
      call execute_command_line("echo '$escfiterlimit 250'>>control")
      call execute_command_line("echo '$dft'>>control")
      call execute_command_line("echo '  functional pbe38'>>control")
      call execute_command_line("echo '  gridsize 4'>>control")
   endif

   if(pr)then
      call execute_command_line('grep "HOMO/LUMO-SEPARATION" define.out')
      if(pr)then
         if(MP2.and.OPT.or.SCS.and.OPT) then
            write(*,*)'!!! MP2 OPTIMIZATION REQUESTED !!!'
         endif
      endif
   endif

   if(FOLD)then
      call execute_command_line("echo '$forceinit off'>>control")
      call execute_command_line("rm gradient")
   endif

   ! c   handle SCS-MP2 SOS-MP2 LP-SOS-MP2 MP2 settings
   if(MP2.or.SCS.or.SOS)then
      call execute_command_line("echo '$ricc2'>>control")
      if(SOS)then
         call execute_command_line("echo ' sos'>>control")
      elseif(SCS) then
         call execute_command_line("echo ' scs cos=1.2  css=0.333333333'>>control")
      else
         if(.not.dfpt2) call execute_command_line("echo ' scs cos=1.0  css=1.0'>>control")
      endif


      if(.not.OPT)then
         call execute_command_line("echo ' mp2 energy only'>>control")
      else
         call execute_command_line("echo ' geoopt model=mp2'>>control")
      endif
      if(LAP) then

         call execute_command_line("echo '$laplace' >>control")
         call execute_command_line("echo 'conv=4' >>control")
      endif
   endif


   ! handle double-hybrids
   !PT2
   !B2PLYP 0.27
   !B2GPPLYP
   !PWPB95
   !PTPSS
   if(func.eq.'b2-plyp') then
      call execute_command_line("echo ' scs cos=0.27  css=0.27 '>>control")
   elseif (func.eq.'dsd-blyp') then
      call execute_command_line("echo ' scs cos=0.460  css=0.370 '>>control")
   elseif (func.eq.'b2gp-plyp') then
      call execute_command_line("echo ' scs cos=0.36  css=0.36 '>>control")
   elseif (func.eq.'ptpss') then
      call execute_command_line("echo ' scs cos=0.375  css=0.0'>>control")
   elseif (func.eq.'pwpb95') then
      call execute_command_line("echo ' scs cos=0.269  css=0.0 '>>control")
   endif


   if(marij) then
      call execute_command_line("echo '$marij' >> control")
   endif
   if(BJ.and.ZERO) then
      write(*,*) '** WARNING: unclear D3 options **'
      write(*,*) '** cefine -zero OR cefine -d3  **'
   endif
   if(BJ.and.D4) then
      write(*,*) '** WARNING: unclear D3 /D4 option! **'
      write(*,*) '   found BJ and D4 '
   endif
   !FB D3 Dispersion edited
   if (BJ.and.hf3c) then
      call execute_command_line("echo '$disp3 -bj -func hf3c' >> control")
   elseif(BJ.and.ATM) then
      call execute_command_line("echo '$disp3 -bj -abc' >> control")
   elseif(BJ.and..not.hf3c) then
      call execute_command_line("echo '$disp3 -bj' >> control")
      !FB D4 dispersion
   elseif(D4)then
      call execute_command_line("echo '$disp4 ' >> control")
      write(*,*) 'D4 selected'
   endif
   if(ZERO) call execute_command_line("echo '$disp3 ' >> control")
   !FB NL
   if(donl) call execute_command_line("echo '$donl ' >> control")
   ! c      if(func.eq.'b2gp-plyp'.or.func.eq.'ptpss') then
   ! c       call execute_command_line('kdg dft')
   ! c       call execute_command_line("echo '$dft'>>control")
   ! c       if(func.eq.'ptpss')
   ! c     . call execute_command_line("echo '  functional ptpss' >>control")
   ! c       if(func.eq.'b2gp-plyp')
   ! c     . call execute_command_line("echo '  functional b2gp-plyp' >>control")
   ! c       call execute_command_line("echo ' gridsize m5' >> control")
   ! c      endif
   !FB gcp in case of pbe-3c, pbe0-3c, b3-lyp-3c
   if(gcpinfo)then
      call execute_command_line("echo '$gcp dft/sv(p)' >> control")
   endif

   !JGB modify K tolerance
   if(extol.gt.0.0d0) then
      write (atmp, "(a,a,f6.3,a)") "echo ", "'$extol ",extol," ' >> control"
      call execute_command_line(atmp)
   endif

   !JGB mulipole accelerated RI for GGA, PBEh-3c, HF-3c if natoms>200
   if(nat.gt.200)then
      if( index(func,'b3-lyp').ne.0 &
         .or.index(func,'bh-lyp').ne.0 &
         .or.index(func,'b2-plyp' ).ne.0 &
         .or.index(func,'b2gp-plyp' ).ne.0 &
         .or.index(func,'ptpss' ).ne.0 &
         .or.index(func,'dsd-blyp' ).ne.0 &
         .or.index(func,'tpssh' ).ne.0 &
         .or.index(func,'pbe38' ).ne.0 &
         .or.index(func,'pw6b95' ).ne.0 &
         .or.index(func,'pwpb95' ).ne.0 &
         .or.index(func,'pbe0'  ).ne.0 &
         .or.index(func,'pwb95'  ).ne.0 &
         .or.index(func,'mpw1b95'  ).ne.0 &
         .or.index(func,'mpwb1k'  ).ne.0 &
         .or.index(func,'pbe0'  ).ne.0 &
         ) then
      else
         atmp="echo '$marij' >> control"
         call execute_command_line(trim(atmp))
         atmp="echo '   precision   0.100D-06' >> control"
         call execute_command_line(trim(atmp))
         atmp="echo '   lmaxmom            12' >> control"
         call execute_command_line(trim(atmp))
      endif
   endif

   ! chok  set functional
   atmp='sed -i s/b-p/'//trim(func)//'/ control'
   if(KEEP) write(*,*) 'sed functional' ,atmp
   call execute_command_line(trim(atmp))
   ! c hok set reference gridsize
   if (REF) then
      atmp='sed -i s/gridsize.*$/reference/ control'
      call execute_command_line("echo '$denconv 1d-4' >> control")
      call execute_command_line("echo '$scftol 1e-13' >> control")
      !      atmp='sed -i s/gridsize/reference/ control'
      if(KEEP) write(*,*) 'sed grid' ,atmp
      call execute_command_line(trim(atmp))
   endif

   ! c use old mos file
   if(MOLD)call execute_command_line('mv mos.tmp mos')

   ! set difference densities expansion to zero
   if(NDIFF) call execute_command_line("echo '$scfdenapproxl 0' >> control")

   ! "close" control file
   call execute_command_line("echo '$end' >>control")
   ! c print some control file settings
   if(ECHO)then
      write(*,'(''==============================================='')')
      write(*,*)'IMPORTANT FINAL CONTROL SETTINGS:'
      call execute_command_line('echo $TURBODIR')
      call execute_command_line('sdg symmetry')
      call execute_command_line('sdg atoms')
      call execute_command_line('sdg rij')
      call execute_command_line('sdg rik')
      call execute_command_line('sdg dft')
      call execute_command_line('sdg vdwx')
      call execute_command_line('sdg closed')
      call execute_command_line('sdg alpha shells')
      call execute_command_line('sdg beta shells')
      call execute_command_line('sdg ricc2')
      write(*,'(''==============================================='')')
   endif


   !JGB modify auxbasis
   if(noauxg) then
      INQUIRE(FILE="auxbasis", EXIST=da)
      if(da) then
         do i =1,9
            write (atmp, "(a,i1,a)") "sed -i '/",i,"\ \ g/,+1 d' auxbasis"
            call execute_command_line(atmp)
         enddo
      endif
   endif


   if(.not.KEEP) call execute_command_line('rm -rf define.out def.inp prep.inp tmp.eiger')
   if(OLDMO.and..not.keep) call execute_command_line("rm -r TMP.MOS")
end Program comand_line_define


! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! c     integer n
! c     call atoms(n)
! c     write(*,*) n
! c     end
!
! c returns the number of heavy (n>10) atoms in a coord file, and the number of diff atom types
! c sets cu_pd true for some special elements that need more input
subroutine atoms(n,nat,nt,cu_pd,irare,att)
   implicit none
   integer n,i,j,nn,nat,irare,att(20)
   logical cu_pd
   character*80 a80
   real*8 xx(10)
   integer na(110),nt,na2(110)

   cu_pd=.false.
   na = 0
   irare = 0
   na2=0
   n=0
   nat=0
   j=0
   nt=0
   open(unit=1,file='coord')
   read(1,'(a)',end=100) a80
   10   read(1,'(a)',end=100) a80
   call readl(a80,xx,nn)
   if(index(a80,'$').ne.0)goto 100
   if(nn.eq.3)then
      nat=nat+1
      j=j+1
      call elem(a80,i)
      ! select rare gas and
      if(i.eq.2.or.i.eq.10.or.i.eq.18.or. &
         i.eq.36.or.i.eq.54.or.i.eq.86)irare=1
      na2(i)=na2(i)+1
      if(na2(i).eq.1)then
         nt=nt+1
         att(nt)=i
      endif
      if(i.gt.10) na(i)=na(i)+1
      ! cts check for Cu/Pd problem
      if(i.eq.29.or.i.eq.46) cu_pd=.true.
   endif
   goto 10
   100   close(1)

   n=0
   do i=1,110
      if(na(i).gt.0)n=n+1
   enddo

end subroutine

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

SUBROUTINE ELEM(KEY1, NAT)
   IMPLICIT DOUBLE PRECISION (A-H,O-Z)
   CHARACTER*(*) KEY1
   CHARACTER*2 ELEMNT(107),E

   DATA ELEMNT/'h ','he', &
      'li','be','b ','c ','n ','o ','f ','ne',  &
      'na','mg','al','si','p ','s ','cl','ar',  &
      'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',  &
      'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn',  &
      'fr','ra','ac','th','pa','u ','np','pu','am','cm','bk','cf','xx',  &
      'fm','md','cb','xx','xx','xx','xx','xx'/

   nat=0
   e='  '
   k=1
   DO J=1,len(key1)
      if (k.gt.2)exit
      N=ICHAR(key1(J:J))
      if(n.ge.ichar('A') .and. n.le.ichar('Z') )then
         call lower(key1(j:j))
         N=ICHAR(key1(J:J))
      endif
      if(n.ge.ichar('a') .and. n.le.ichar('z') )then
         e(k:k)=key1(j:j)
         k=k+1
      endif
   enddo

   DO I=1,107
      if(e.eq.elemnt(i))then
         NAT=I
         RETURN
      ENDIF
   ENDDO

end subroutine

! C     *****************************************************************

SUBROUTINE lower(AS)
   CHARACTER*1 AS
   AS=CHAR(ICHAR(AS)-ICHAR('A')+ICHAR('a'))
END subroutine

! C     *****************************************************************

SUBROUTINE backstring(A1,A2,lena2)
   CHARACTER*(*) A1
   CHARACTER*(*) A2
   integer n,lena2
   n=0
   DO J=1,len(a1)
      if(a1(j:j).ne.' ')then
         n=n+1
         a2(n:n)=a1(j:j)
      endif
   enddo
   DO J=1,lena2
      a2(j:j)=' '
   enddo
   a1=a2
   a2='                                                            '
   n=0
   DO J=1,len(a1)
      if(a1(j:j).ne.' ')then
         n=n+1
         a2(n:n)=a1(j:j)
      endif
   enddo

END subroutine


! C     *****************************************************************

SUBROUTINE READL(A1,X,N)
   IMPLICIT REAL*8 (A-H,O-Z)
   CHARACTER*(*) A1
   DIMENSION X(*)
   I=0
   IS=1
   10  I=I+1
   X(I)=READAA(A1,IS,IB,IE)
   IF(IB.GT.0 .AND. IE.GT.0) THEN
      IS=IE
      GOTO 10
   ENDIF
   N=I-1
   RETURN
END subroutine


FUNCTION READAA(A,ISTART,IEND,IEND2)
   IMPLICIT REAL*8 (A-H,O-Z)
   REAL*8 READAA
   CHARACTER*(*) A
   NINE=ICHAR('9')
   IZERO=ICHAR('0')
   MINUS=ICHAR('-')
   IDOT=ICHAR('.')
   ND=ICHAR('D')
   NE=ICHAR('E')
   IBL=ICHAR(' ')
   IEND=0
   IEND2=0
   IDIG=0
   C1=0
   C2=0
   ONE=1.D0
   X = 1.D0
   NL=LEN(A)
   DO J=ISTART,NL-1
      N=ICHAR(A(J:J))
      M=ICHAR(A(J+1:J+1))
      IF(N.LE.NINE.AND.N.GE.IZERO .OR.N.EQ.IDOT)GOTO 20
      IF(N.EQ.MINUS.AND.(M.LE.NINE.AND.M.GE.IZERO &
         .OR. M.EQ.IDOT)) GOTO 20
   enddo
   READAA=0.D0
   RETURN
   20 CONTINUE
   IEND=J
   DO I=J,NL
      N=ICHAR(A(I:I))
      IF(N.LE.NINE.AND.N.GE.IZERO) THEN
         IDIG=IDIG+1
         IF (IDIG.GT.10) GOTO 60
         C1=C1*10+N-IZERO
      ELSEIF(N.EQ.MINUS.AND.I.EQ.J) THEN
         ONE=-1.D0
      ELSEIF(N.EQ.IDOT) THEN
         GOTO 40
      ELSE
         GOTO 60
      ENDIF
   enddo
   40 CONTINUE
   IDIG=0
   DO II=I+1,NL
      N=ICHAR(A(II:II))
      IF(N.LE.NINE.AND.N.GE.IZERO) THEN
         IDIG=IDIG+1
         IF (IDIG.GT.10) GOTO 60
         C2=C2*10+N-IZERO
         X = X /10
      ELSEIF(N.EQ.MINUS.AND.II.EQ.I) THEN
         X=-X
      ELSE
         GOTO 60
      ENDIF
   enddo
   ! C
   ! C PUT THE PIECES TOGETHER
   ! C
   60 CONTINUE
   READAA= ONE * ( C1 + C2 * X)
   DO J=IEND,NL
      N=ICHAR(A(J:J))
      IEND2=J
      IF(N.EQ.IBL)RETURN
   enddo
   IF(N.EQ.ND .OR. N.EQ.NE)GOTO 57
   RETURN

   57 C1=0.0D0
   ONE=1.0D0
   DO I=J+1,NL
      N=ICHAR(A(I:I))
      IEND2=I
      IF(N.EQ.IBL)GOTO 70
      IF(N.LE.NINE.AND.N.GE.IZERO) C1=C1*10.0D0+N-IZERO
      IF(N.EQ.MINUS)ONE=-1.0D0
   enddo
   61 CONTINUE
   70 READAA=READAA*10**(ONE*C1)
   RETURN
END function

! C     *****************************************************************

subroutine freeze(uhf)
   implicit real*8 (a-h,o-z)
   character*60 a,line(2000),dum
   character*5  lab(8)
   dimension xx(10),irf(8,4)
   logical uhf,ex

   irf=0
   inquire(file='~/.freeze',exist=ex)
   if(ex)then
      open(unit=11,file='~/.freeze')
      read(11,*)thr1,thr2
      close(11)
   else
      thr1=-5.0
      thr2= 1.d+9
   endif

   line=' '

   if(uhf)then
      call execute_command_line('grep eigenvalue alpha > freeze.tmp')
   else
      call execute_command_line('grep eigenvalue mos > freeze.tmp')
   endif

   open(unit=1,file='freeze.tmp')

   ir=1
   n=0
   imem=0
   i=1
   10   read(1,'(a)',end=100)line(i)
   i=i+1
   goto 10
   100  continue
   nl=i-1
   close(1)

   do i=1,nl
      a=line(i)
      call readl(a,xx,nn)
      n=n+1
      if(xx(nn-1).gt.thr1.and.imem.eq.0)then
         irf(ir,2)=n-1
         imem=1
      endif
      lab(ir)=a(7:11)
      dum    =line(i+1)
      if(lab(ir).ne.dum(7:11))then
         irf(ir,1)=1
         lab(ir)=a(7:11)
         ir=ir+1
         n=0
         imem=0
      endif
   enddo

   open(unit=1,file='freeze.tmp')
   ir=1
   n=0
   imem=0
   do i=1,nl
      a=line(i)
      call readl(a,xx,nn)
      n=n+1
      if(xx(nn-1).gt.thr2.and.imem.eq.0)then
         irf(ir,3)=n
         imem=1
      endif
      lab(ir)=a(7:11)
      dum    =line(i+1)
      if(lab(ir).ne.dum(7:11))then
         ! c        irf(ir,4)=nao
         lab(ir)=a(7:11)
         ir=ir+1
         n=0
         imem=0
      endif
   enddo

   ir=ir-1

   rewind 1
   write(1,'(''$freeze'')')
   do i=1,ir
      if(irf(i,2).gt.0.and.irf(i,3).gt.0) &
         write(1,500)lab(i),irf(i,1),irf(i,2),irf(i,3),irf(i,4)
      if(irf(i,2).eq.0.and.irf(i,3).gt.0) &
         write(1,501)lab(i),irf(i,3),irf(i,4)
      if(irf(i,3).eq.0.and.irf(i,2).ne.0) &
         write(1,501)lab(i),irf(i,1),irf(i,2)
   enddo
   write(1,'(''$end'')')
   close(1)

   call execute_command_line('kdg freeze')
   call execute_command_line('kdg end')
   call execute_command_line('cat freeze.tmp >> control')

   ! c     write(*,*)
   ! c     write(*,*)'the following data group will be inserted into control'
   ! c     write(*,*)
   ! c     call execute_command_line('cat freeze.tmp')

   call execute_command_line('rm freeze.tmp')


   500  format(2x,a5,2x,i3,'-',i3,', ',i3,'-',i3)
   501  format(2x,a5,2x,i3,'-',i3)

end subroutine freeze



subroutine susy(sym,DESY)
   ! c     program    susy
   implicit none
   character*20 a
   character*80 sym
   character*3  g(5)
   integer n,i
   logical DESY

   sym=''
   write(*,*)'======= automatic Abel-subgoup adjustment ======'
   call execute_command_line('rm -rf control')
   open(unit=1,file='def.dum')
   write(1,*)
   write(1,*)
   write(1,*)'a coord'
   write(1,*)'desy 0.03'
   write(1,*)'*'
   write(1,*)'no'
   close(1)

   call execute_command_line('define_huge < def.dum > def.out')
   call execute_command_line('sdg symmetry > def.out')
   open(unit=1,file='def.out')
   read(1,'(a)') a
   close(1)

   call execute_command_line('rm control def.dum def.out tmp.input')

   n=0
   if(index(a,' c3 ').ne.0)then
      write(*,*) 'Group: C3'
      n=1
      g(1)='c1'
   endif
   if(index(a,' oh').ne.0)then
      write(*,*) 'Group: Oh'
      n=2
      g(1)='d4h'
      g(2)='d2h'
   endif
   if(index(a,' d4h').ne.0)then
      write(*,*) 'Group: D4h'
      n=1
      g(1)='d2h'
   endif
   if(index(a,' d6h').ne.0)then
      write(*,*) 'Group: D6h'
      n=1
      g(1)='d2h'
   endif
   if(index(a,' d4d').ne.0)then
      write(*,*) 'Group: D4d'
      n=2
      g(1)='c4v'
      g(2)='c2v'
   endif
   if(index(a,' d5h').ne.0)then
      write(*,*) 'Group: D5h'
      n=1
      g(1)='c2v'
   endif
   if(index(a,' c5v').ne.0)then
      write(*,*) 'Group: C5v'
      n=1
      g(1)='cs'
   endif
   if(index(a,' c6v').ne.0)then
      write(*,*) 'Group: C6v'
      n=1
      g(1)='c2v'
   endif
   if(index(a,' td').ne.0)then
      write(*,*) 'Group: Td'
      n=2
      g(1)='d2d'
      g(2)='c2v'
   endif
   if(index(a,' d2d').ne.0)then
      write(*,*) 'Group: D2d'
      n=1
      g(1)='d2'
   endif
   if(index(a,' d3h').ne.0)then
      write(*,*) 'Group: D3h'
      n=1
      g(1)='c2v'
   endif
   if(index(a,' d3d').ne.0)then
      write(*,*) 'Group: D3d'
      n=1
      g(1)='c2h'
   endif
   if(index(a,' c3v').ne.0)then
      write(*,*) 'Group: C3v'
      n=1
      g(1)='cs'
   endif
   if(index(a,' c3h').ne.0)then
      write(*,*) 'Group: C3h'
      n=1
      g(1)='cs'
   endif
   if(index(a,' c4h').ne.0)then
      write(*,*) 'Group: C4h'
      n=1
      g(1)='c2h'
   endif
   if(index(a,' c6h').ne.0)then
      write(*,*) 'Group: C6h'
      n=1
      g(1)='c2h'
   endif
   if(index(a,' d3 ').ne.0)then
      write(*,*) 'Group: D3'
      n=1
      g(1)='c2'
   endif
   if(index(a,' s6 ').ne.0)then
      write(*,*) 'Group: S6'
      n=1
      g(1)='ci'
   endif

   if(index(a,' ih ').ne.0)then
      write(*,*) 'Group: Ih'
      n=1
      g(1)='d2h'
   endif

   if(n.eq.0)then
      DESY=.true.
      return
   endif

   sym=g(n)
   DESY=.false.
   write(*,*) 'adjusted to:',g(n)

   open(unit=1,file='def.dum')
   write(1,*)
   write(1,*)
   write(1,*)'a coord'
   write(1,*)'desy 0.03'
   write(1,*)'susy'
   do i=1,n
      write(1,'(a)')g(i)
   enddo
   write(1,*)' '
   write(1,*)' '
   write(1,*)'*'
   write(1,*)'no'
   write(1,*)
   close(1)

   call execute_command_line('rm -rf control')
   call execute_command_line('define_huge < def.dum > def.out')

   call execute_command_line('rm control def.dum def.out tmp.input')
   41    continue

   write(*,*)'=======          done                     ======'
end subroutine


! c     read XYZ (xmol) files. the first two lines (#atoms, blank) can be omitted

subroutine xyzrd(xyz,iat,nat,infile)
   implicit none
   character*2 elemnt(107)
   character*80 infile, outfile,atmp
   real*8 xyz(3,10000),xx(5)
   integer iat(10000),nat,nel,i,nn
   real*8 bohr
   logical da
   DATA ELEMNT/'h ','he',&
      'li','be','b ','c ','n ','o ','f ','ne',   &
      'na','mg','al','si','p ','s ','cl','ar',  &
      'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',  &
      'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn',  &
      'fr','ra','ac','th','pa','u ','np','pu','am','cm','bk','cf','xx',  &
      'fm','md','cb','xx','xx','xx','xx','xx'/

   bohr=0.52917726
   nat=0
   inquire(file=infile,exist=da)

   if(da)then

      write(*,'(5x,''reading...'',$)')
      ! read XYZ file
      open(unit=3,file=infile)
      read(3,'(a)',end=100) atmp
      ! check for first two lines
      call readl(atmp,xx,nn)
      if(nn.gt.1) then   ! more than one argument found, assuming they are coords
         do i=1,10000   ! while loop would be better
            nat=nat+1
            read(3,'(a)',end=123) atmp
         enddo
      else
         nat=idint(xx(1))
         read(3,'(a)',end=100) atmp  !titel line
      endif
      123   if(nn.gt.1) rewind(3)
      do i=1,nat
         read(3,'(a)') atmp
         call readl(atmp,xx,nn)
         call elem(atmp,iat(i))
         xyz(1:3,i)=xx(1:3)
         !       write(*,'(a2,5x,3F18.12)') elemnt(iat(i)),xyz(1:3,i)
      enddo
      100  close(3)
      write(*,'(5x,''XYZ file : '',a)')  trim(infile)
   else
      write(*,*) ' no input file found !! '
   endif

   write(*,*) '    number of atoms:  ',nat
end subroutine

subroutine wtm(xyz,iat,nat,outfile)
   implicit none
   character*2 elemnt(107)
   character*80 outfile,atmp
   real*8 xyz(3,10000),xx(5)
   integer iat(10000),nat,nel,i,nn,io
   real*8 bohr
   logical da
   DATA ELEMNT/'h ','he',   &
      'li','be','b ','c ','n ','o ','f ','ne',  &
      'na','mg','al','si','p ','s ','cl','ar',  &
      'k ','ca','sc','ti','v ','cr','mn','fe','co','ni','cu',  &
      'zn','ga','ge','as','se','br','kr',  &
      'rb','sr','y ','zr','nb','mo','tc','ru','rh','pd','ag',  &
      'cd','in','sn','sb','te','i ','xe',  &
      'cs','ba','la','ce','pr','nd','pm','sm','eu','gd','tb','dy',  &
      'ho','er','tm','yb','lu','hf','ta','w ','re','os','ir','pt',  &
      'au','hg','tl','pb','bi','po','at','rn',  &
      'fr','ra','ac','th','pa','u ','np','pu','am','cm','bk','cf','xx',  &
      'fm','md','cb','xx','xx','xx','xx','xx'/

   bohr=0.52917726
   open(unit=4,file=outfile)
   !       write(*,*) 'writing coords'
   io=4
   write(io,'(a)')'$coord'
   do i=1,nat
      write(io,'(3F18.12,2x,a2)') xyz(1:3,i)/bohr , elemnt(iat(i))
   enddo
   write(io,'(a)')'$end'
   close(4)
end subroutine

!       subroutine setL(L,arg)
!       logical L,arg
!       L=arg
!       print *,L
!       end subroutine

subroutine helpmessage(unit)
   implicit none
   integer, intent(in) :: unit
   write(unit,*)'options:'
   write(unit,*)'   -hf (def: RI-DFT/TPSS)'
   write(unit,*)'   -func <string>'
   write(unit,*)'   -bas  <string>'
   write(unit,*)'   -grid <string>'
   write(unit,*)'   -mp2  (do RI-MP2)'
   write(unit,*)'   -scs  (do RI-SCS-MP2)'
   write(unit,*)'   -sos  (do RI-SOS-MP2)'
   write(unit,*)'   -lsos /-lap  (do RI-Laplace-SOS-MP2)'
   write(unit,*)'   -cc  (do RI-CCSD(T))'
   write(unit,*)'   -d3   ($disp3 -bj)'
   write(unit,*)'   -d3atm ($disp3 -bj -abc)' ! FB implemented
   write(unit,*)'   -zero (D3 zero damping)'
   write(unit,*)'   -d4   ($disp4)'
   write(unit,*)'   -donl   ($donl, induces c1 sym)'
   write(unit,*)'   -ref  (reference SP)'
   write(unit,*)'   -vdw (DFT-D2)'
   write(unit,*)'   -quick  (PWLDA/SVP grid 1, no ired)'
   write(unit,*)'   -chrg <integer>'
   write(unit,*)'   -angst (read coords in Angstroem)'
   write(unit,*)'   -uhf <integer> (integer=# Na-Nb)'
   write(unit,*)'   -sym <string> (def: desy 0.03)'
   write(unit,*)'   -scfconv <integer>'
   write(unit,*)'   -abel (adjust for Abelian subgroups->e.g. pmp2)'
   write(unit,*)'   -noopt (def: ired optimization)'
   write(unit,*)'   -opt (switch on opt e.g. for MP2)'
   write(unit,*)'   -novdw (switch it off for B97-D due to EDA)'
   write(unit,*)'   -nori'
   write(unit,*)'   -ri'
   write(unit,*)'   -nofc (all e-corr. for MP2)'
   write(unit,*)'   -rijk (RI for HF/hybrids)'
   write(unit,*)'   -rijcosx (seminum. exchange w/ COS alg. for hybrids)'   !SAW
   write(unit,*)'   -or (set flags for OR, escf)'
   write(unit,*)'   -ex (set flags UV/CD, escf)'
   write(unit,*)'   -fold (take forceapprox from previous run)'
   write(unit,*)'   -mold (take mos from previous run)'
   write(unit,*)'   -trold (takes hessian from previous run, activates TS)'
   write(unit,*)'   -trunc (truncated RIJK)'
   write(unit,*)'   -fon (Fermi smearing options switched on)'
   write(unit,*)'   -pol (set flags C6 computation, escf)'
   write(unit,*)'   -cp1 (counterpoise computation, frag1, calls splitmol)'
   write(unit,*)'   -cp2 (counterpoise computation, frag2, calls splitmol)'
   write(unit,*)'   -ts  (statpt TS search settings)'
   write(unit,*)'   -r12 (R12/F12 options for ricc2)'
   write(unit,*)'   -dftmrci (sets cbas, bhlyp etc)'
   write(unit,*)'   -cosmo <real> (COSMO with dk=real)'
   write(unit,*)'   -echo (write important parts of control)'
   write(unit,*)'   -keep (debuging)'
   write(unit,*)'   -co <coord_file>  (def: coord)'
   write(unit,*)'   -cox <xyz_file>  (writes "coord" file)'
   write(unit,*)'   -lib <integer> (use own basis set lib) '
   write(unit,*)'    ($HOME/.definerc, basis=PATH)'
   write(unit,*)'   -auxlib <PATH>  (own jbas/cbas basis lib)'
   write(unit,*)'   -diff (add spd/sp diffuse functions)'
   write(unit,*)'   -test (do not call define)'
   write(unit,*)'   -nodiff (turns off diff density feature of TM)'
   write(unit,*)'needs: <coord> file in TM format'
   write(unit,*)'optional files    : <.SYM> with Schoenflies symbol'
   write(unit,*)'(in <coord> dir)    <.UHF> integer number Na-Nb'
   write(unit,*)'                    <.CHRG> integer (charge)'
   write(unit,*)'possible options in .cefinerc:'
   write(unit,*)'func    STRING'
   write(unit,*)'bas     STRING'
   write(unit,*)'grid    STRING'
   write(unit,*)'desythr REAL'
   write(unit,*)'scfconv INTEGER'
   write(unit,*)'ricore  INTEGER'
   write(unit,*)'twoint  INTEGER'
   write(unit,*)'maxcor  INTEGER'
   write(unit,*)'fp REAL'
   write(unit,*)'vdw     on   #  sets DFT-D2(BJ) '
   write(unit,*)'echo    on   #  more printing'
   write(unit,*)'marij        #sets $marij '
   write(unit,*)'no-rij       #no RIJ for hybrids, if functional is known'
   write(unit,*)'nodiff       #turns off diff density feature of TM'
   stop
end subroutine helpmessage
