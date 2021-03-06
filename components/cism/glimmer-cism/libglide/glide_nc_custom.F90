!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                             
!   glide_nc_custom.F90 - part of the Community Ice Sheet Model (CISM)  
!                                                              
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!   Copyright (C) 2005-2014
!   CISM contributors - see AUTHORS file for list of contributors
!
!   This file is part of CISM.
!
!   CISM is free software: you can redistribute it and/or modify it
!   under the terms of the Lesser GNU General Public License as published
!   by the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   CISM is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   Lesser GNU General Public License for more details.
!
!   You should have received a copy of the Lesser GNU General Public License
!   along with CISM. If not, see <http://www.gnu.org/licenses/>.
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#ifdef HAVE_CONFIG_H
#include "config.inc"
#endif

#define NCO outfile%nc

module glide_nc_custom

  !module for filling in dimension variables

  use glimmer_global, only: dp
  implicit none

contains

  subroutine glide_nc_fillall(model, outfiles)

    !> fill dimension variables of all files
    use glide_types
    use glimmer_ncdf
    use glimmer_ncio
    implicit none

    type(glide_global_type) :: model
    type(glimmer_nc_output),pointer,optional :: outfiles

    ! local variables
    type(glimmer_nc_output), pointer :: oc

    if (present(outfiles)) then
       oc => outfiles
    else
       oc=>model%funits%out_first
    end if

    do while(associated(oc))
       if (.not.oc%append) then
          call glide_nc_filldvars(oc,model)
       endif
       oc=>oc%next
    end do

  end subroutine glide_nc_fillall

  subroutine glide_nc_filldvars(outfile, model)

    use parallel
    use glide_types
    use glimmer_ncdf
    use glimmer_paramets, only : len0
    implicit none

    type(glimmer_nc_output), pointer :: outfile
    type(glide_global_type) :: model

    integer i,status,varid
    real(dp),dimension(model%general%ewn-1) :: x0
    real(dp),dimension(model%general%ewn) :: x1
    real(dp),dimension(model%general%nsn-1) :: y0
    real(dp),dimension(model%general%nsn) :: y1

    ! check if we are still in define mode and if so leave it
    if (NCO%define_mode) then
       status = parallel_enddef(NCO%id)
       call nc_errorhandle(__FILE__,__LINE__,status)
       NCO%define_mode = .FALSE.
    end if

    ! horizontal dimensions
    ! (x1,y1) is the unstaggered scalar grid
    ! (x0,y0) is the staggered velocity grid

    if (associated(model%funits%in_first)) then

       status = parallel_inq_varid(NCO%id,'x1',varid)
       status = distributed_put_var(NCO%id,varid,model%general%x1)
       call nc_errorhandle(__FILE__,__LINE__,status)

       status = parallel_inq_varid(NCO%id,'y1',varid)
       status= distributed_put_var(NCO%id,varid,model%general%y1)
       call nc_errorhandle(__FILE__,__LINE__,status)

       !create the x0 and y0 grids from x1 and y1

       status = parallel_inq_varid(NCO%id,'x0',varid)
       do i=1, model%general%ewn-1
          x0(i) = (model%general%x1(i)+model%general%x1(i+1))/2.0
       end do
       status=distributed_put_var(NCO%id,varid,x0)
       call nc_errorhandle(__FILE__,__LINE__,status)

       status = parallel_inq_varid(NCO%id,'y0',varid)
       do i=1, model%general%nsn-1
          y0(i) = (model%general%y1(i)+model%general%y1(i+1))/2.0
       end do
       status = distributed_put_var(NCO%id,varid,y0)
       call nc_errorhandle(__FILE__,__LINE__,status)
    
    else if(.not. associated(model%funits%in_first)) then

       ! filling coordinate variables
       status = parallel_inq_varid(NCO%id,'x0',varid)
       do i=1, model%general%ewn-1
          x0(i) = ((i-0.5)*model%numerics%dew*len0)
       end do
       status=distributed_put_var(NCO%id,varid,x0)
       call nc_errorhandle(__FILE__,__LINE__,status)

       status = parallel_inq_varid(NCO%id,'y0',varid)
       do i=1, model%general%nsn-1
          y0(i) = (i-0.5)*model%numerics%dns*len0
       end do
       status=distributed_put_var(NCO%id,varid,y0)
       call nc_errorhandle(__FILE__,__LINE__,status)

       status = parallel_inq_varid(NCO%id,'x1',varid)
       do i=1, model%general%ewn
          x1(i) = (i-1.)*model%numerics%dew*len0
       end do
       status=distributed_put_var(NCO%id,varid,x1)
       call nc_errorhandle(__FILE__,__LINE__,status)

       status = parallel_inq_varid(NCO%id,'y1',varid)
       do i=1, model%general%nsn
          y1(i) = (i-1.)*model%numerics%dns*len0
       end do
       status=distributed_put_var(NCO%id,varid,y1)
       call nc_errorhandle(__FILE__,__LINE__,status)

    end if   ! associated(model%funits%in_first)

    ! layer interfaces

    status = parallel_inq_varid(NCO%id,'level',varid)
    status = parallel_put_var(NCO%id,varid,model%numerics%sigma)
    call nc_errorhandle(__FILE__,__LINE__,status)

    ! layer midpoints

    status = parallel_inq_varid(NCO%id,'staglevel',varid)
    status = parallel_put_var(NCO%id,varid,model%numerics%stagsigma)
    call nc_errorhandle(__FILE__,__LINE__,status)

    ! layer midpoints, plus upper and lower surfaces
    ! (e.g., temperature field in HO dycore)

    status = parallel_inq_varid(NCO%id,'stagwbndlevel',varid)
    status = parallel_put_var(NCO%id,varid,model%numerics%stagwbndsigma)
    call nc_errorhandle(__FILE__,__LINE__,status)

    ! lithosphere vertical coordinate

    if (model%options%gthf == GTHF_COMPUTE) then
       status = parallel_inq_varid(NCO%id,'lithoz',varid)
       status= parallel_put_var(NCO%id,varid,model%lithot%deltaz)
       call nc_errorhandle(__FILE__,__LINE__,status)
    end if

  end subroutine glide_nc_filldvars

end module glide_nc_custom
