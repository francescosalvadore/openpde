!< Concrete class of mesh for Finite Difference 2D methods.
module openpde_mesh_FD_2D
    !< Concrete class of mesh for Finite Difference 2D methods.
    use, intrinsic :: iso_fortran_env, only : stderr=>error_unit
    use openpde_mesh_abstract
    use openpde_kinds
    use vtk_fortran

    implicit none
    private
    public :: associate_mesh_FD_2D, mesh_FD_2D

    type, extends(mesh) :: mesh_FD_2D
        !< Concrete class of mesh for Finite Difference 2D methods.
        integer(I_P) :: nx=0      !< Number of X points.
        integer(I_P) :: ny=0      !< Number of Y points.
        integer(I_P) :: ngx=0     !< Number of X ghost points.
        integer(I_P) :: ngy=0     !< Number of Y ghost points.
        integer(I_P) :: s=0       !< Number of replicas for steps/stages.
        real(R_P)    :: hx=0._R_P !< Cell X size.
        real(R_P)    :: hy=0._R_P !< Cell Y size.
        contains
            ! deferred public methods
            procedure, pass(this) :: init   !< Initilize mesh.
            procedure, pass(this) :: output !< Output data.
            ! public methods
            procedure, pass(this) :: set !< Set mesh.
    endtype mesh_FD_2D
contains
    ! public, non TBP
    function associate_mesh_FD_2D(mesh_input, emsg) result(mesh_pointer)
        !< Check the type of the mesh passed as input and return a Finite Difference 2D mesh pointer associated to mesh.
        class(mesh),       intent(in), target   :: mesh_input    !< Input mesh.
        character(*),      intent(in), optional :: emsg          !< Auxiliary error message.
        class(mesh_FD_2D), pointer              :: mesh_pointer  !< Finite Difference 2D mesh pointer.

        select type(mesh_input)
            type is(mesh_FD_2D)
                mesh_pointer => mesh_input
            class default
               write(stderr, '(A)')'error: cast mesh to mesh_FD_2D'
               if (present(emsg)) write(stderr, '(A)') emsg
               stop
        end select
      end function associate_mesh_FD_2D

    ! deferred public methods
    pure subroutine init(this, description, filename, error)
        !< Initialize mesh.
        class(mesh_FD_2D), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description.
        character(*),      intent(in),  optional :: filename    !< Initialization file name.
        integer(I_P),      intent(out), optional :: error       !< Error status.

        call this%free
        if (present(description)) this%description = description
        this%nx = 50
        this%ny = 40
        this%ngx = 2
        this%ngy = 2
        this%s = 1
        this%hx = 0.05_R8P
        this%hy = 0.07_R8P
        if (present(error)) error = 0
    end subroutine init

    subroutine output(this, error)
        !< Output mesh.
        class(mesh_FD_2D), intent(in)            :: this  !< The mesh.
        integer(I_P),      intent(out), optional :: error !< Error status.

        if (allocated(this%description)) print "(A)", this%description
        print*, "nx: ", this%nx
        print*, "ny: ", this%ny
        print*, "ngx: ", this%ngx
        print*, "ngy: ", this%ngy
        print*, "s: ", this%s
        print*, "hx: ", this%hx
        print*, "hy: ", this%hy
        if (present(error)) error = 0
    end subroutine output

    ! public methods
    pure subroutine set(this, description, nx, ny, ngx, ngy, s, hx, hy, error)
        !< Set mesh.
        class(mesh_FD_2D), intent(inout)         :: this        !< The mesh.
        character(*),      intent(in),  optional :: description !< Mesh description
        integer(I_P),      intent(in),  optional :: nx          !< Number of X points.
        integer(I_P),      intent(in),  optional :: ny          !< Number of Y points.
        integer(I_P),      intent(in),  optional :: ngx         !< Number of X ghost points.
        integer(I_P),      intent(in),  optional :: ngy         !< Number of Y ghost points.
        integer(I_P),      intent(in),  optional :: s           !< Number of replicas for steps/stages.
        real(R_P),         intent(in),  optional :: hx          !< Cell X size.
        real(R_P),         intent(in),  optional :: hy          !< Cell Y size.
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
end module openpde_mesh_FD_2D
