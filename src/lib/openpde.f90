!< Open Fortran Library for PDE solving.
module openpde
    !< Open Fortran Library for PDE solving.
    ! abstract classes definition
    use openpde_equation_abstract
    use openpde_field_abstract
    use openpde_field_surface_abstract
    use openpde_integrator_abstract
    use openpde_mesh_abstract
    use openpde_spatial_operator_abstract
    use openpde_spatial_operator_d1_abstract
    use openpde_spatial_operator_d2_abstract
    ! concrete classes definition
    use openpde_field_FD_1D
    use openpde_field_FD_2D
    use openpde_field_FV_1D
    use openpde_field_surface_FV_1D
    use openpde_integrator_euler_explicit
    use openpde_mesh_FD_1D
    use openpde_mesh_FD_2D
    use openpde_mesh_FV_1D
    use openpde_spatial_operator_d1_FD_1D
    use openpde_spatial_operator_d1_FD_2D
    use openpde_spatial_operator_d1_FV_1D
    use openpde_spatial_operator_d2_FD_1D
    use openpde_spatial_operator_d2_FD_2D
    use openpde_spatial_operator_d2_FV_1D
    ! kinds definition
    use openpde_kinds

    implicit none
    private
    ! abstract classes
    public :: equation
    public :: field
    public :: field_surface
    public :: integrator
    public :: mesh
    public :: spatial_operator
    public :: spatial_operator_d1
    public :: spatial_operator_d2
    ! concrete classes
    public :: associate_field_FD_1D, field_FD_1D
    public :: associate_field_FD_2D, field_FD_2D
    public :: associate_field_FV_1D, field_FV_1D
    public :: associate_field_surface_FV_1D, field_surface_FV_1D
    public :: integrator_euler_explicit
    public :: associate_mesh_FD_1D, mesh_FD_1D
    public :: associate_mesh_FD_2D, mesh_FD_2D
    public :: associate_mesh_FV_1D, mesh_FV_1D
    public :: spatial_operator_d1_FD_1D
    public :: spatial_operator_d1_FD_2D
    public :: spatial_operator_d1_FV_1D
    public :: spatial_operator_d2_FD_1D
    public :: spatial_operator_d2_FD_2D
    public :: spatial_operator_d2_FV_1D
    ! kinds
    public :: R8P
    public :: R4P
    public :: R_P
    public :: I8P
    public :: I4P
    public :: I2P
    public :: I1P
    public :: I_P
end module openpde
