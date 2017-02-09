!< Open Fortran Library for PDE solving.
module openpde
    !< Open Fortran Library for PDE solving.
    ! abstract classes definition
    use openpde_equation_abstract
    use openpde_field_abstract
    use openpde_field_surface_abstract
    use openpde_integrator_abstract
    use openpde_integrator_adv_abstract
    use openpde_mesh_abstract
    use openpde_spatial_operator_abstract
    use openpde_spatial_operator_d1_abstract
    use openpde_spatial_operator_d2_abstract
    use openpde_vector_abstract
    use openpde_matrix_abstract
    use openpde_f2v_abstract
    use openpde_v2f_abstract
    use openpde_f2m_abstract
    use openpde_f2m_d1_abstract
    use openpde_f2m_d2_abstract
    use openpde_linsolver_abstract
    use openpde_equation_adv  ! fake concrete, acts as fake-abstract
    ! concrete classes definition
    use openpde_field_FD_1D
    use openpde_field_FD_2D
    use openpde_field_FV_1D
    use openpde_field_surface_FV_1D
    use openpde_integrator_euler_explicit
    use openpde_integrator_adv_euler_explicit
    use openpde_integrator_adv_euler_implicit
    use openpde_integrator_adv_rk_implicit
    use openpde_mesh_FD_1D
    use openpde_mesh_FD_2D
    use openpde_mesh_FV_1D
    use openpde_spatial_operator_d1_FD_1D
    use openpde_spatial_operator_d1_FD_2D
    use openpde_spatial_operator_d1_FV_1D
    use openpde_spatial_operator_d2_FD_1D
    use openpde_spatial_operator_d2_FD_2D
    use openpde_spatial_operator_d2_FV_1D
    use openpde_vector_simple
    use openpde_matrix_simple
    use openpde_f2v_FD_1D
    use openpde_v2f_FD_1D
    use openpde_f2v_FD_2D
    use openpde_v2f_FD_2D
    use openpde_f2m_d1_FD_1D
    use openpde_f2m_d2_FD_1D
    use openpde_f2m_d2_FD_2D
    use openpde_linsolver_gmlapack
    use openpde_multigrid_FD_1D
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
    public :: vector
    public :: matrix
    public :: f2v
    public :: v2f
    public :: f2m
    public :: f2m_d1
    public :: f2m_d2
    public :: linsolver
    public :: equation_adv ! fake abstract
    public :: integrator_adv
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
    public :: vector_simple
    public :: matrix_simple
    public :: f2v_FD_1D
    public :: v2f_FD_1D
    public :: f2v_FD_2D
    public :: v2f_FD_2D
    public :: f2m_d1_FD_1D
    public :: f2m_d2_FD_1D
    public :: f2m_d2_FD_2D
    public :: linsolver_gmlapack
    public :: multigrid_FD_1D
    public :: integrator_adv_euler_explicit
    public :: integrator_adv_euler_implicit
    public :: integrator_adv_rk_implicit
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
