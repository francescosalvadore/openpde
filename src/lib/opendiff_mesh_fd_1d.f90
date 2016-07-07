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
        integer(I4P) :: n  !< Number of points.
        integer(I4P) :: ng !< Number of ghost points.
        integer(I4P) :: s  !< Number of replicas for steps/stages.
        real(R8P)    :: h  !< Cell size.
        contains
            procedure :: init   !< Initilize mesh.
            procedure :: output !< Output mesh data.
            procedure :: set    !< Set mesh.
    endtype mesh_fd_1d
contains
    pure subroutine init(this, description, error)
        !< Initialize finite difference 1D mesh.
        class(mesh_fd_1d), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description
        integer(I4P),      intent(out), optional :: error       !< Error status.
        call this%free
        if (present(description)) this%description = description
        this%n = 50
        this%ng = 2
        this%s = 1
        this%h = 0.1_R8P
        if (present(error)) error = 0
    end subroutine init

    subroutine output(this, error)
        !< Output mesh data.
        class(mesh_fd_1d), intent(in)            :: this  !< The mesh.
        integer(I4P),      intent(out), optional :: error !< Error status.
        if (allocated(this%description)) print "(A)", this%description
        print*,"n: ", this%n
        print*,"ng: ", this%ng
        print*,"s: ", this%s
        print*,"h: ", this%h
        if (present(error)) error = 0
    end subroutine output

    pure subroutine set(this, description, n, ng, s, h, error)
        !< Set mesh.
        class(mesh_fd_1d), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description
        integer(I4P),      intent(in),  optional :: n           !< Number of points.
        integer(I4P),      intent(in),  optional :: ng          !< Number of ghost points.
        integer(I4P),      intent(in),  optional :: s           !< Number of replicas for steps/stages.
        real(R8P),         intent(in),  optional :: h           !< Cell size.
        integer(I4P),      intent(out), optional :: error       !< Error status.
        if (present(description)) this%description = description
        if (present(n)) this%n = n
        if (present(ng)) this%ng = ng
        if (present(s)) this%s = s
        if (present(h)) this%h = h
        if (present(error)) error = 0
    end subroutine set
end module opendiff_mesh_fd_1d
