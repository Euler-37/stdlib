submodule (stdlib_linalg) stdlib_linalg_repmat

  implicit none

contains

    pure module function repmat_rsp(a, m, n) result(res)
      real(sp), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      real(sp) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_rsp

    pure module function repmat_rdp(a, m, n) result(res)
      real(dp), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      real(dp) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_rdp

    pure module function repmat_csp(a, m, n) result(res)
      complex(sp), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      complex(sp) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_csp

    pure module function repmat_cdp(a, m, n) result(res)
      complex(dp), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      complex(dp) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_cdp

    pure module function repmat_iint8(a, m, n) result(res)
      integer(int8), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      integer(int8) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_iint8

    pure module function repmat_iint16(a, m, n) result(res)
      integer(int16), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      integer(int16) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_iint16

    pure module function repmat_iint32(a, m, n) result(res)
      integer(int32), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      integer(int32) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_iint32

    pure module function repmat_iint64(a, m, n) result(res)
      integer(int64), intent(in) :: a(:,:)
      integer, intent(in) :: m,n
      integer(int64) :: res(m*size(a,1),n*size(a,2))
      integer ::i,j,k,l
      associate(ma=>size(a,1),na=>size(a,2))
         do j=1,n
            do l=1,na
               do i=1,m
                  do k=1,ma
                     res((i-1)*ma+k,(j-1)*na+l)=a(k,l)
                  end do
               end do
            end do
         end do
      end associate
    end function repmat_iint64


end submodule
