!< opendiff: Open Fortran Library for PDE solving.
module opendiff
    !< opendiff: Open Fortran Library for PDE solving.
    use opendiff_adt_equation
    use opendiff_adt_field
    use opendiff_adt_integrator
    use opendiff_adt_mesh
    use opendiff_adt_spatial_operator
    use opendiff_adt_spatial_operator_der1
    use opendiff_field_fd_1d
    use opendiff_integrator_euler
    use opendiff_mesh_fd_1d
    use opendiff_spatial_operator_der1_fd_1d

    implicit none
    private
    public :: equation
    public :: field
    public :: field_fd_1d
    public :: integrator
    public :: integrator_euler
    public :: mesh
    public :: mesh_fd_1d
    public :: spatial_operator
    public :: spatial_operator_der1
    public :: spatial_operator_der1_fd_1d
end module opendiff
