! ==============================
! 課題3-2
! 細長体の視点の変換
! compile as, gfortran -Wconversion ex3_2.f90 XCgraphics.o -o ex3_2.x -lX11 
! ==============================
program transform
    implicit none
    integer :: i, j, l, n, m, surface
    real, dimension(:, :), allocatable :: mat, mat2, origin_mat
    real, parameter :: pi = 4.0 * atan(1.0)
    real roll, pitch, yaw, height, r, x_center, y_center
    real disp_width, disp_height, disp_margin
    character :: a*1, roll_string*6, pitch_string*6, yaw_string*6, degree*35

    ! ========ユーザー入力欄============
    ! グラフィックウィンドウの大きさ・余白
    disp_width = 500.0
    disp_height = 500.0
    disp_margin = 20.0

    ! 大きさrの正n角形を描画
    n = 10
    r = 50.0

    ! 正n角形の中心位置
    x_center = disp_width / 2.0
    y_center = disp_height / 2.0

    ! 回転角度 degree指定
    roll  = 45.0
    pitch = 45.0
    yaw = .0

    ! 面の数
    surface = 4
    ! ==============================

    !行列の列数
    m = surface * (n + 1)
    allocate(mat(m, 3), mat2(3, m), origin_mat(3, m))

    ! 座標作成
    ! 複数行 x 3列の行列．1行に対して(x, y, z)の順に入力
    mat = 0.0
    do i = 1, n + 1
        mat(i, 1) = x_center + r * 0.9 * cos(real(i-1)/real(n)*2.0*pi)
        mat(i, 2) = y_center + r * 0.6 * sin(real(i-1)/real(n)*2.0*pi)
        mat(i, 3) = -200.0

        mat(i+n+1, 1) = mat(i, 1)
        mat(i+n+1, 2) = mat(i, 2)
        mat(i+n+1, 3) = -100.0

        mat(i+2*(n+1), 1) = x_center + (r - 20.0) * 0.7 * cos(real(i-1)/real(n)*2.0*pi)
        mat(i+2*(n+1), 2) = y_center + (r - 20.0) * 0.4 * sin(real(i-1)/real(n)*2.0*pi)
        mat(i+2*(n+1), 3) = 50.0

        mat(i+3*(n+1), 1) = x_center + (r - 20.0) * 0.4 * cos(real(i-1)/real(n)*2.0*pi)
        mat(i+3*(n+1), 2) = y_center + (r - 20.0) * 0.4 * sin(real(i-1)/real(n)*2.0*pi)
        mat(i+3*(n+1), 3) = 200.0
    end do

    ! 転置する 
    do i = 1, 3
        do j = 1, m
            mat2(i, j) = mat(j, i)
        end do
    end do

    ! 配列をコピー
    origin_mat = mat2

    ! ==========変換計算==========
    ! 原点に移動させる
    call translate(mat2, 3, m, -x_center, -y_center, 0.0)
    ! 原点を中心に回転
    call rotate(0.0, 0.0, yaw, mat2, mat2, 3, m)
    call rotate(0.0, pitch, 0.0, mat2, mat2, 3, m)
    call rotate(roll, 0.0, 0.0, mat2, mat2, 3, m)
    ! もとの位置に戻す
    call translate(mat2, 3, m, x_center, y_center, 0.0)

    ! ==========描画==========
    call xcinit(disp_width, disp_height)
    call xclines(mat2(1, 1:n+1), mat2(2, 1:n+1), n+1)
    call xclines(mat2(1, n+2:2*(n+1)), mat2(2, n+2:2*(n+1)), n+1)
    call xclines(mat2(1, 2*(n+1)+1:3*(n+1)), mat2(2, 2*(n+1)+1:3*(n+1)), n+1)
    call xclines(mat2(1, 3*(n+1)+1:4*(n+1)), mat2(2, 3*(n+1)+1:4*(n+1)), n+1)
    
    ! ========稜線を描く========
    do i = 1, n
        call xcline(mat2(1, i), mat2(2, i), mat2(1, i+n+1), mat2(2, i+n+1))
        call xcline(mat2(1, i+n+1), mat2(2, i+n+1), mat2(1, i+2*(n+1)), mat2(2, i+2*(n+1)))
        call xcline(mat2(1, i+2*(n+1)), mat2(2, i+2*(n+1)), mat2(1, i+3*(n+1)), mat2(2, i+3*(n+1)))
    end do

    ! ========面を塗りつぶす========
    call xccolor(5)
    call xcpolygon(mat2(1, 1:n), mat2(2, 1:n), n)
    call xccolor(4)
    call xcpolygon(mat2(1, 1*(n+1)+1 : 2*(n+1)-1), mat2(2, 1*(n+1)+1 : 2*(n+1)-1), n)
    call xccolor(2)
    call xcpolygon(mat2(1, 2*(n+1)+1 : 3*(n+1)-1), mat2(2, 2*(n+1)+1 : 3*(n+1)-1), n)
    call xccolor(0)
    call xcpolygon(mat2(1, 3*(n+1)+1 : 4*(n+1)-1), mat2(2, 3*(n+1)+1 : 4*(n+1)-1), n)

    ! ========角度情報の表示========
    print '(f6.1)', roll
    write(roll_string, '(f6.1)') roll
    write(pitch_string, '(f6.1)') pitch
    write(yaw_string, '(f6.1)') yaw
    degree = "roll:" // roll_string // " "// "pitch:" // pitch_string // " " // "yaw:" // yaw_string
    print *, degree
    call xccolor(0)
    call xcstring(10.0, 100.0, degree, 35)
    
    write (6,*) "press enter"
    read (5,'(a)') a
  
    call xcclose (1)

end program transform


subroutine translate(matrix, line_dim, col_dim, p, q, r)
    ! ====================
    ! 平行移動を行うサブルーチン
    ! p: x軸方向の変位
    ! q: y軸方向の変位
    ! r: z軸方向の変位
    ! ====================
    implicit none
    real, intent(in) :: p, q, r
    integer, intent(in) :: line_dim, col_dim    
    real, dimension(line_dim, col_dim) ,intent(inout) :: matrix

    matrix(1, :) = matrix(1, :) + p
    matrix(2, :) = matrix(2, :) + q
    matrix(3, :) = matrix(3, :) + r
end subroutine translate


subroutine scale(matrix, line_dim, col_dim, a, b, c)
    ! ====================
    ! 拡大・縮小を行うサブルーチン
    ! a: x軸方向の拡大率
    ! b: y軸方向の拡大率
    ! c: z軸方向の拡大率
    ! ====================
    implicit none
    real, intent(in) :: a, b, c
    integer, intent(in) :: line_dim, col_dim    
    real, dimension(line_dim, col_dim) ,intent(inout) :: matrix

    matrix(1, :) = matrix(1, :) * a
    matrix(2, :) = matrix(2, :) * b
    matrix(3, :) = matrix(3, :) * c
end subroutine scale


subroutine rotate(roll, pitch, yaw, matrix, return_mat, line_dim, col_dim)
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
    integer, intent(in) :: line_dim, col_dim
    real, intent(inout) :: matrix(line_dim, col_dim), return_mat(line_dim, col_dim)
    real, parameter :: pi = 4.0 * atan(1.0)
    real cos_r, sin_r, cos_p, sin_p, cos_y, sin_y, rad_roll, rad_pitch, rad_yaw, origin_x, origin_y, origin_z
    real rot_matrix(4, 4), origin(line_dim, col_dim)
    integer i

    ! もとの行列をコピー
    origin = matrix

    if (roll /= 0.0 .or. pitch /= 0.0 .or. yaw /= 0.0) then
        rad_roll = roll * pi / 180.0
        rad_pitch = pitch * pi / 180.0
        rad_yaw = yaw * pi / 180.0

        cos_r = cos(rad_roll);  sin_r = sin(rad_roll)
        cos_p = cos(rad_pitch); sin_p = sin(rad_pitch)
        cos_y = cos(rad_yaw);   sin_y = sin(rad_yaw)

        ! 計算後の回転行列を使用 R_z(y)*R_y(p)*R_x(r)
        rot_matrix(1, :) = [real :: cos_y*cos_p, cos_y*sin_p*sin_r-sin_y*cos_r, cos_y*sin_p*cos_r+sin_y*sin_r, 0.0]
        rot_matrix(2, :) = [real :: sin_y*cos_p, sin_y*sin_p*sin_r+cos_y*cos_r, sin_y*sin_p*cos_r-cos_y*sin_r, 0.0]
        rot_matrix(3, :) = [real :: -sin_p, cos_p*sin_r, cos_p*cos_r, 0.0]
        rot_matrix(4, :) = [real :: 0.0, 0.0, 0.0, 1.0]

        print *, "rotation start! roll:", roll, "pitch:", pitch, "yaw:", yaw
        do i = 1, col_dim
            return_mat(1, i) = rot_matrix(1, 1) * origin(1, i) + rot_matrix(1, 2) * origin(2, i) + rot_matrix(1, 3) * origin(3, i)
            return_mat(2, i) = rot_matrix(2, 1) * origin(1, i) + rot_matrix(2, 2) * origin(2, i) + rot_matrix(2, 3) * origin(3, i)
            return_mat(3, i) = rot_matrix(3, 1) * origin(1, i) + rot_matrix(3, 2) * origin(2, i) + rot_matrix(3, 3) * origin(3, i)
        end do
    else
        print *, "no rotation"
    end if
end subroutine rotate