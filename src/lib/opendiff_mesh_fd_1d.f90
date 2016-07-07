!< Concrete class of mesh for Finite Difference 1D.
module opendiff_mesh_fd_1d
    !< Concrete class of mesh for Finite Difference 1D.
    use opendiff_adt_mesh
    use opendiff_kinds

    implicit none
    private
    public :: mesh_fd_1d

    type, extends(mesh) :: mesh_fd_1d
        !< Finite difference 1D class for *mesh* handling.
        integer(I4P) :: n  !< number of points.
        integer(I4P) :: ng !< number of ghost points.
        integer(I4P) :: s  !< number of replicas for steps/stages.
        real(R8P)    :: h  !< cell size.
        contains
            procedure :: init => mesh_fd_1d_init     !< Initilize mesh.
            procedure :: output => mesh_fd_1d_output !< Output mesh data.
    endtype mesh_fd_1d
contains
    function mesh_fd_1d_init(this) result(res)
        !< Initialize finite difference 1D mesh.
        class(mesh_fd_1d), intent(inout) :: this !< The mesh.
        integer(I4P)                     :: res  !< Result (error code?).
        this%n  = 50
        this%ng = 2
        this%s  = 1
        this%h  = 0.1_R8P
        res = 0
    end function mesh_fd_1d_init

    function mesh_fd_1d_output(this) result(res)
        !< Output mesh data.
        class(mesh_fd_1d), intent(in) :: this !< The mesh.
        integer(I4P)                  :: res  !< Result (error code?).
        print*,"n: ", this%n
        print*,"ng: ", this%ng
        print*,"s: ", this%s
        print*,"h: ", this%h
        res = 0
    end function mesh_fd_1d_output
end module opendiff_mesh_fd_1d
