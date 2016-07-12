!< Concrete class of mesh for Finite Difference 2D.
module opendiff_mesh_fd_2d
    !< Concrete class of mesh for Finite Difference 2D.
    use opendiff_adt_mesh
    use opendiff_kinds
    use vtk_fortran

    implicit none
    private
    public :: mesh_fd_2d

    type, extends(mesh) :: mesh_fd_2d
        !< Finite difference 1D class for *mesh* handling.
        integer(I_P) :: nx  !< Number of points.
        integer(I_P) :: ny  !< Number of points.
        integer(I_P) :: ngx !< Number of ghost points.
        integer(I_P) :: ngy !< Number of ghost points.
        integer(I_P) :: s  !< Number of replicas for steps/stages.
        real(R_P)    :: hx  !< Cell size.
        real(R_P)    :: hy  !< Cell size.
        contains
            procedure :: init   !< Initilize mesh.
            procedure :: output !< Output data.
            procedure :: set    !< Set mesh.
    endtype mesh_fd_2d
contains
    pure subroutine init(this, description, error)
        !< Initialize mesh.
        class(mesh_fd_2d), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description.
        integer(I_P),      intent(out), optional :: error       !< Error status.
        call this%free
        if (present(description)) this%description = description
        this%nx = 10
        this%ny = 10
        this%ngx = 2
        this%ngy = 2
        this%s = 1
        this%hx = 0.05_R8P
        this%hy = 0.05_R8P
        if (present(error)) error = 0
    end subroutine init

    subroutine output(this, error)
        !< Output mesh.
        class(mesh_fd_2d), intent(in)            :: this  !< The mesh.
        integer(I_P),      intent(out), optional :: error !< Error status.
        if (allocated(this%description)) print "(A)", this%description
        print*,"nx: ", this%nx
        print*,"ny: ", this%ny
        print*,"ngx: ", this%ngx
        print*,"ngy: ", this%ngy
        print*,"s: ", this%s
        print*,"hx: ", this%hx
        print*,"hy: ", this%hy
        if (present(error)) error = 0
    end subroutine output

    pure subroutine set(this, description, nx, ny, ngx, ngy, s, hx, hy, error)
        !< Set mesh.
        class(mesh_fd_2d), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description
        integer(I_P),      intent(in),  optional :: nx           !< Number of points.
        integer(I_P),      intent(in),  optional :: ny           !< Number of points.
        integer(I_P),      intent(in),  optional :: ngx          !< Number of ghost points.
        integer(I_P),      intent(in),  optional :: ngy          !< Number of ghost points.
        integer(I_P),      intent(in),  optional :: s           !< Number of replicas for steps/stages.
        real(R_P),         intent(in),  optional :: hx           !< Cell size.
        real(R_P),         intent(in),  optional :: hy           !< Cell size.
        integer(I_P),      intent(out), optional :: error       !< Error status.
        if (present(description)) this%description = description
        if (present(nx)) this%nx = nx
        if (present(ny)) this%ny = ny
        if (present(ngx)) this%ngx = ngx
        if (present(ngy)) this%ngy = ngy
        if (present(s)) this%s = s
        if (present(hx)) this%hx = hx
        if (present(hy)) this%hy = hy
        if (present(error)) error = 0
    end subroutine set
end module opendiff_mesh_fd_2d
