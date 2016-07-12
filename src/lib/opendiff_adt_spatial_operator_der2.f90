!< Abstract class of spatial operator for 1D derivative.
module opendiff_adt_spatial_operator_der2
    !< Abstract class of spatial operator for 1D derivative.
    use opendiff_adt_spatial_operator

    implicit none
    private
    public :: spatial_operator_der2

    type, abstract, extends(spatial_operator) :: spatial_operator_der2
        !< Abstract class for first derivative for *spatial operator* handling.
    endtype spatial_operator_der2
end module opendiff_adt_spatial_operator_der2
