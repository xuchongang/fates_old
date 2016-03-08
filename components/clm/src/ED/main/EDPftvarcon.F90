module EDPftvarcon

  !-----------------------------------------------------------------------
  ! !DESCRIPTION:
  ! Module containing vegetation constants and method to
  ! read and initialize vegetation (PFT) constants.
  !
  ! !USES:
  use clm_varpar  , only : mxpft
  use shr_kind_mod, only : r8 => shr_kind_r8

  !
  ! !PUBLIC TYPES:
  implicit none
  save
  private

  !ED specific variables. 
  type, public ::  EDPftvarcon_type
     real(r8) :: max_dbh            (0:mxpft) ! maximum dbh at which height growth ceases...
     real(r8) :: freezetol          (0:mxpft) ! minimum temperature tolerance...
     real(r8) :: wood_density       (0:mxpft) ! wood density  g cm^-3  ...
     real(r8) :: alpha_stem         (0:mxpft) ! live stem turnover rate. y-1
     real(r8) :: hgt_min            (0:mxpft) ! sapling height m
     real(r8) :: cushion            (0:mxpft) ! labile carbon storage target as multiple of leaf pool.
     real(r8) :: leaf_stor_priority (0:mxpft) ! leaf turnover vs labile carbon use prioritisation. (1 = lose  leaves, 0 = use store).
     real(r8) :: leafwatermax       (0:mxpft) ! degree to which respiration is limited by btran if btran = 0
     real(r8) :: rootresist         (0:mxpft)
     real(r8) :: soilbeta           (0:mxpft)
     real(r8) :: crown              (0:mxpft)
     real(r8) :: bark_scaler        (0:mxpft)
     real(r8) :: crown_kill         (0:mxpft)
     real(r8) :: initd              (0:mxpft)
     real(r8) :: sd_mort            (0:mxpft)
     real(r8) :: seed_rain          (0:mxpft)
     real(r8) :: BB_slope           (0:mxpft)
     real(r8) :: root_long          (0:mxpft) ! root longevity (yrs)
     real(r8) :: clone_alloc        (0:mxpft) ! fraction of carbon balance allocated to clonal reproduction.
     real(r8) :: seed_alloc         (0:mxpft) ! fraction of carbon balance allocated to seeds.
     real(r8) :: sapwood_ratio      (0:mxpft) ! amount of sapwood per unit leaf carbon and m of height. gC/gC/m
     
     real(r8) :: dbh2h_m            (0:mxpft) ! allocation parameter m from dbh to height
     real(r8) :: dbh2h_c            (0:mxpft) ! allocation parameter c from dbh to height
     real(r8) :: dbh2bl_a           (0:mxpft) ! allocation parameter a from dbh to bleaf     
     real(r8) :: dbh2bl_b           (0:mxpft) ! allocation parameter b from dbh to bleaf
     real(r8) :: dbh2bl_c           (0:mxpft) ! allocation parameter c from dbh to bleaf
     real(r8) :: dbh2bl_slascaler   (0:mxpft) ! allocation parameter slascaler from dbh to bleaf
     real(r8) :: dbh2bd_a           (0:mxpft) ! allocation parameter a from dbh to bdead  
     real(r8) :: dbh2bd_b           (0:mxpft) ! allocation parameter b from dbh to bdead  
     real(r8) :: dbh2bd_c           (0:mxpft) ! allocation parameter c from dbh to bdead  
     real(r8) :: dbh2bd_d           (0:mxpft) ! allocation parameter d from dbh to bdead        
     real(r8) :: sai_scaler         (0:mxpft) ! coef that links bdead to sai   
     
     real(r8) :: b_mort             (0:mxpft) ! mortality rate 
     real(r8) :: hf_sm_threshold    (0:mxpft) ! hydraulic failure soil moisture threshold 

     real(r8) :: ed_ph_drought_threshold      ! soil moisture leads to leaf drop for drought decidious PFT
     real(r8) :: ed_ph_a                      ! phenology parameter a 
     real(r8) :: ed_ph_b                      ! phenology parameter b  
     real(r8) :: ed_ph_c                      ! phenology parameter c  
     real(r8) :: ed_ph_chiltemp               ! chilling day temperature 
     real(r8) :: ed_ph_coldtemp               ! cold day tempeture (for leaf drop)
     real(r8) :: ed_ph_ncolddayslim           ! number of cold days for leave drop off
     real(r8) :: ed_ph_mindayson              ! minimum number of days before leaf drops for cold phenology 
     real(r8) :: ed_ph_doff_time              ! minimum number of days between leaf off and leaf on for drought phenology  
     
     
          
  end type EDPftvarcon_type

  type(EDPftvarcon_type), public :: EDPftvarcon_inst

  !
  ! !PUBLIC MEMBER FUNCTIONS:
  public :: EDpftconrd ! Read and initialize vegetation (PFT) constants
  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------
  subroutine EDpftconrd( ncid )
    !
    ! !DESCRIPTION:
    ! Read and initialize vegetation (PFT) constants
    !
    ! !USES:
    use ncdio_pio , only : file_desc_t, ncd_io
    use abortutils  , only : endrun
    !
    ! !ARGUMENTS:
    implicit none
    !
    type(file_desc_t), intent(inout) :: ncid   ! pio netCDF file id

    ! !LOCAL VARIABLES:

    logical :: readv            ! read variable in or not
    character(len=32) :: subname = 'EDpftconrd'              ! subroutine name

    call ncd_io('max_dbh',EDPftvarcon_inst%max_dbh, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('freezetol',EDPftvarcon_inst%freezetol, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('wood_density',EDPftvarcon_inst%wood_density, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('alpha_stem',EDPftvarcon_inst%alpha_stem, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('hgt_min',EDPftvarcon_inst%hgt_min, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('cushion',EDPftvarcon_inst%cushion, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('leaf_stor_priority',EDPftvarcon_inst%leaf_stor_priority, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('leafwatermax',EDPftvarcon_inst%leafwatermax, 'read', ncid, readvar=readv)
    if ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('rootresist',EDPftvarcon_inst%rootresist,'read',     ncid, readvar=readv)
    if  ( .not. readv ) call endrun( trim(subname)//' ERROR: error in reading in pft data' )

    call ncd_io('soilbeta',EDPftvarcon_inst%soilbeta,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('crown',EDPftvarcon_inst%crown,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('bark_scaler',EDPftvarcon_inst%bark_scaler,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('crown_kill',EDPftvarcon_inst%crown_kill,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('initd',EDPftvarcon_inst%initd,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('sd_mort',EDPftvarcon_inst%sd_mort,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('seed_rain',EDPftvarcon_inst%seed_rain,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('BB_slope',EDPftvarcon_inst%BB_slope,'read',         ncid, readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('root_long',EDPftvarcon_inst%root_long, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('seed_alloc',EDPftvarcon_inst%seed_alloc, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('clone_alloc',EDPftvarcon_inst%clone_alloc, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')

    call ncd_io('sapwood_ratio',EDPftvarcon_inst%sapwood_ratio, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')
    
    call ncd_io('dbh2h_m',EDPftvarcon_inst%dbh2h_m, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2h_c',EDPftvarcon_inst%dbh2h_c, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bl_a',EDPftvarcon_inst%dbh2bl_a, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bl_b',EDPftvarcon_inst%dbh2bl_b, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bl_c',EDPftvarcon_inst%dbh2bl_c, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bl_slascaler',EDPftvarcon_inst%dbh2bl_slascaler, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bd_a',EDPftvarcon_inst%dbh2bd_a, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bd_b',EDPftvarcon_inst%dbh2bd_b, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bd_c',EDPftvarcon_inst%dbh2bd_c, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('dbh2bd_d',EDPftvarcon_inst%dbh2bd_d, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')  
    
    call ncd_io('sai_scaler',EDPftvarcon_inst%sai_scaler, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')  
    
    call ncd_io('b_mort',EDPftvarcon_inst%b_mort, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')
    
    call ncd_io('hf_sm_threshold',EDPftvarcon_inst%hf_sm_threshold, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_drought_threshold',EDPftvarcon_inst%ed_ph_drought_threshold, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_a',EDPftvarcon_inst%ed_ph_a, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_b',EDPftvarcon_inst%ed_ph_b, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_c',EDPftvarcon_inst%ed_ph_c, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_chiltemp',EDPftvarcon_inst%ed_ph_chiltemp, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_coldtemp',EDPftvarcon_inst%ed_ph_coldtemp, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_ncolddayslim',EDPftvarcon_inst%ed_ph_ncolddayslim, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_mindayson',EDPftvarcon_inst%ed_ph_mindayson, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
    
    call ncd_io('ed_ph_doff_time',EDPftvarcon_inst%ed_ph_doff_time, 'read', ncid,  readvar=readv)
    if   ( .not. readv) call endrun(trim(subname)// ' ERROR : error in reading in pft data')   
  
        
    
  end subroutine EDpftconrd

end module EDPftvarcon

