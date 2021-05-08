program transform
    implicit none
    integer :: i, l, n
    real, dimension(:), allocatable :: x1, y1, z1, x, y, z
    real, parameter :: pi = 4.0 * atan(1.0)
    real roll, pitch, yaw, height, r, x_center, y_center
    real disp_width, disp_height, disp_margin, half_disp_width, half_disp_height
    character :: a*1

    ! ========ユーザー入力欄============
    ! グラフィックウィンドウの大きさ
    disp_width = 500.0
    disp_height = 500.0
    disp_margin = 20.0

    ! 大きさrの正n角形を描画
    n = 6
    r = 100.0

    ! 正n角形の奥行き方向（z軸）高さ
    height = 100.0

    ! 正n角形の中心位置
    x_center = disp_width / 2.0
    y_center = disp_height / 2.0

    ! 回転角度 degree指定
    roll = 0.0
    pitch = 170.0
    yaw = 0.0
    ! ==============================
    

    ! display計算
    half_disp_width = disp_width / 2.0
    half_disp_height = disp_height / 2.0

    ! nの2倍の長さの配列を作成
    l = 2 * n + 2
    allocate(x1(l)); allocate(y1(l)); allocate(z1(l))
    allocate(x(l)); allocate(y(l)); allocate(z(l))

    ! 正n角形の点を計算
    do i = 1, n + 1
        x1(i) = x_center + r * cos(real(i-1)/real(n)*2.0*pi)
        y1(i) = y_center + r * sin(real(i-1)/real(n)*2.0*pi)
        z1(i) = 0.0
        ! 値を(i+n+1)番目にコピー
        x1(i+n+1) = x1(i)
        y1(i+n+1) = y1(i)
        z1(i+n+1) = height
    end do

    z1(1: n+1) = 0.0
    z1(n+2: l) = height

    ! 配列をコピー
    x = x1; y = y1; z = z1

    ! 変形させる部分
    do i = 1, l
        ! 原点に移動させる  x(i) - x1(1)
        call translate(x(i), y(i), z(i), x_center, y_center, 0.0)
        ! 原点を中心に回転させる
        call rotate(roll, pitch, yaw, x(i), y(i), z(i), x(i), y(i), z(i))
        ! もとの位置に戻す
        call translate(x(i), y(i), z(i), -x_center, -y_center, 0.0)
    end do

    ! 描画
    call xcinit(disp_width, disp_height)

    do i = 1, n
        call xccolor(2)
        call xcline(x(i), y(i), x(i+1), y(i+1))
        call xcline(x(i+n+1), y(i+n+1), x(i+n+2), y(i+n+2))
        call xcline(x(i), y(i), x(i+n+1), y(i+n+1))
    end do
    
    write (6,*) "press enter"
    read (5,'(a)') a
  
    call xcclose (1)

end program transform


subroutine translate(x, y, z, p, q, r)
    ! ====================
    ! 平行移動を行うサブルーチン
    ! p: x軸方向の変位
    ! q: y軸方向の変位
    ! r: z軸方向の変位
    ! ====================
    implicit none
    real, intent(in) :: p, q, r
    real, intent(inout) ::  x, y, z

    x = x - p
    y = y - q
    z = z - r
end subroutine translate


subroutine rotate(roll, pitch, yaw, x, y, z, return_x, return_y, return_z)
    ! ====================
    ! 回転移動を行うサブルーチン
    ! roll, pitch, yawは全てdegreeで渡すこと
    ! roll:  x軸周りの回転
    ! pitch: y軸周りの回転
    ! yaw :  z軸周りの回転
    ! yaw -> pitch -> roll の順で回転させる
    ! ====================
    implicit none
    real, intent(in) :: roll, pitch, yaw
    real, intent(inout) :: x, y, z, return_x, return_y, return_z
    real, parameter :: pi = 4.0 * atan(1.0)
    real cosine, sine, origin_x, origin_y, origin_z
    real, dimension(3, 3) :: rot_roll, rot_pitch, rot_yaw

    ! もとのx, y, zをコピー
    origin_x = x
    origin_y = y
    origin_z = z

    if (yaw /= 0.0) then
        print *, "yaw start", yaw
        cosine = cos(yaw*pi/180.0)
        sine = sin(yaw*pi/180.0)

        rot_yaw(1, :) = [real :: cosine, -sine, 0.0]
        rot_yaw(2, :) = [real :: sine, cosine, 0.0]
        rot_yaw(3, :) = [real :: 0.0, 0.0, 1.0]

        return_x = rot_yaw(1, 1) * origin_x + rot_yaw(1, 2) * origin_y + rot_yaw(1, 3) * origin_z
        return_y = rot_yaw(2, 1) * origin_x + rot_yaw(2, 2) * origin_y + rot_yaw(2, 3) * origin_z
        return_z = rot_yaw(3, 1) * origin_x + rot_yaw(3, 2) * origin_y + rot_yaw(3, 3) * origin_z

        ! todo: ここの解決　もっとスマートに
        ! もとの座標を変換後の座標に更新
        origin_x = return_x
        origin_y = return_y
        origin_z = return_z
    end if

    if (pitch /= 0.0) then
        print *, "pitch start", pitch
        cosine = cos(pitch*pi/180.0)
        sine = sin(pitch*pi/180.0)
        rot_pitch(1, :) = [real :: cosine, 0.0, sine]
        rot_pitch(2, :) = [real :: 0.0, 1.0, 0.0]
        rot_pitch(3, :) = [real :: -sine, 0.0, cosine]

        return_x = rot_pitch(1, 1) * origin_x + rot_pitch(1, 2) * origin_y + rot_pitch(1, 3) * origin_z
        return_y = rot_pitch(2, 1) * origin_x + rot_pitch(2, 2) * origin_y + rot_pitch(2, 3) * origin_z
        return_z = rot_pitch(3, 1) * origin_x + rot_pitch(3, 2) * origin_y + rot_pitch(3, 3) * origin_z
        origin_x = return_x
        origin_y = return_y
        origin_z = return_z
    end if

    if (roll /= 0.0) then
        print *, "roll start", roll
        cosine = cos(roll*pi/180.0)
        sine = sin(roll*pi/180.0)
        rot_roll(1, :) = [real :: 1.0, 0.0, 0.0]
        rot_roll(2, :) = [real :: 0.0, cosine, -sine]
        rot_roll(3, :) = [real :: 0.0, sine, cosine]

        return_x = rot_roll(1, 1) * origin_x + rot_roll(1, 2) * origin_y + rot_roll(1, 3) * origin_z
        return_y = rot_roll(2, 1) * origin_x + rot_roll(2, 2) * origin_y + rot_roll(2, 3) * origin_z
        return_z = rot_roll(3, 1) * origin_x + rot_roll(3, 2) * origin_y + rot_roll(3, 3) * origin_z
    end if

end subroutine rotate