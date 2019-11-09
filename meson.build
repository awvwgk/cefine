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

executable(meson.project_name(), ['cefine.f90', 'io_tools.f90', 'argparser.f90'])
