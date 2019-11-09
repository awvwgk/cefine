project('cefine', 'fortran',
        version: '2.2.1',
        license: 'LGPL3',
        meson_version: '>=0.49')

conf = configuration_data()
conf.set('version', meson.project_version())
conf.set('commit', run_command(find_program('git'),'show','-s','--format=%h').stdout().strip())
conf.set('date', run_command(find_program('date'),'-I').stdout().strip())
conf.set('author', run_command(find_program('whoami')).stdout().strip())
conf.set('origin', run_command(find_program('hostname')).stdout().strip())
configure_file(input : 'version.fh', output : 'cefine-version.fh',
  configuration : conf)

fsrcs = ['cefine.f90',
         'argparser.f90',
         'molecule.f90',
         'utils/units.f90',
         'utils/io_tools.f90',
         'utils/atomic_units.f90',
         'utils/resize_arrays.f90',
         'utils/constants.f90',
         'utils/pbc_tools.f90',
         'utils/filetools.f90',
         'utils/molecule_reader.f90']

executable(meson.project_name(), fsrcs)
