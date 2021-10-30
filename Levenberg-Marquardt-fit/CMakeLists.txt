################################################################################
#  @author Oskar Wyszynski, oskar.wyszynski(at)cern.ch
#  @version $Id$
################################################################################

SET (src_dir ${CMAKE_CURRENT_SOURCE_DIR})

SET (v0fit_sources
  ${src_dir}/v0fit_main.c
  ${src_dir}/veefit.F
  ${src_dir}/connect_to_ds.F
  ${src_dir}/covsrt.F
  ${src_dir}/fvtget.F
  ${src_dir}/fvthc.F
  ${src_dir}/gaussj.F
  ${src_dir}/mqminv.F
  ${src_dir}/rz_get_event.F
  ${src_dir}/rz_output.F
  ${src_dir}/v0fit.F
  ${src_dir}/v0fit_outi.F
  ${src_dir}/vfit.F
  ${src_dir}/caldat_from_cernlib.F
  ${src_dir}/cdmon_from_cernlib.F
  ${src_dir}/cleap_from_cernlib.F
  ${src_dir}/cydiy_from_cernlib.F
  ${src_dir}/cyear4_from_cernlib.F
  ${src_dir}/datime_from_cernlib.F
  ${src_dir}/lenocc_from_cernlib.F
  ${src_dir}/lfitw_from_cernlib.F
  ${src_dir}/mtlprt_from_cernlib.F
  ${src_dir}/mtlset_from_cernlib.F
  ${src_dir}/prob_from_cernlib.F
  ${src_dir}/abend_from_cernlib.F
  #${src_dir}/timed_from_cernlib.F
  #${src_dir}/timel_from_cernlib.F
  #${src_dir}/timelcc_from_cernlib.c
  PARENT_SCOPE
)
