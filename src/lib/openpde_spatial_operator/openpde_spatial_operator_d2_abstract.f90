!< Abstract class of spatial operator of second derivative.
module openpde_spatial_operator_d2_abstract
    !< Abstract class of spatial operator of second derivative.
    use openpde_spatial_operator_abstract

    implicit none
    private
    public :: spatial_operator_d2

    type, abstract, extends(spatial_operator) :: spatial_operator_d2
        !< Abstract class of spatial operator of second derivative.
    endtype spatial_operator_d2
end module openpde_spatial_operator_d2_abstract
