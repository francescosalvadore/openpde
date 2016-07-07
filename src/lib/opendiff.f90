!< opendiff: Open Fortran Library for PDE solving (OpenFoam done right).
module opendiff
    !< opendiff: Open Fortran Library for PDE solving (OpenFoam done right).
    use opendiff_adt_field
    use opendiff_adt_mesh
    use opendiff_field_fd_1d
    use opendiff_mesh_fd_1d

    implicit none
    private
    public :: field
    public :: field_fd_1d
    public :: mesh
    public :: mesh_fd_1d
end module opendiff
