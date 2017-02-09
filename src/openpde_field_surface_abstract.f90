!< Abstract class of field surface.
module openpde_field_surface_abstract
    !< Abstract class of field surface.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_field_abstract
    use openpde_kinds

    implicit none
    private
    public :: field_surface

    type, abstract, extends(field) :: field_surface
        !< Abstract class of field surface.
        contains
            ! deferred public methods
            procedure(abstract_compute_fluxes), pass(this), deferred :: compute_fluxes !< Compute fluxes of field through surfaces.
    endtype field_surface

    ! deferred public methods interfaces
    abstract interface
        !< Compute fluxes of field through surfaces.
        subroutine abstract_compute_fluxes(this, field_cell, error)
            !< Compute fluxes of field through surfaces.
            import :: field, field_surface, I_P
            class(field_surface), intent(inout)         :: this       !< Fluxex.
            class(field),         intent(in)            :: field_cell !< Field at cells center.
            integer(I_P),         intent(out), optional :: error      !< Error status.
        end subroutine abstract_compute_fluxes
    endinterface
end module openpde_field_surface_abstract
