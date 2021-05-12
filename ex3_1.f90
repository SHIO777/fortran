! ==============================
! 課題3-1
! 1. 2次元座標系に多角形を表示する
! 2. 多角形を平行移動，回転移動，拡大縮小の順に変換する
! 3. 合成変換の結果の確認 -> 回転角度，拡大率を計算する
! compile as, gfortran -Wconversion ex3_1.f90 XCgraphics.o -o ex3_1.x -lX11 
! ==============================
program transform
    implicit none
    integer :: i, j, n, m, surface
    character :: a*1
    ! 図形描画のための変数
    real, parameter :: pi = 4.0 * atan(1.0)
    real, dimension(:, :), allocatable :: mat, mat2, mat_translate, mat_rotate, mat_scale, origin_mat
    real height, r, x_center, y_center
    real dx, dy, dz, rate_x, rate_y, rate_z, roll, pitch, yaw
    ! ディスプレイ設定
    real disp_width, disp_height, disp_margin
    ! 合成変換の結果の確認のための変数
    real scale_rev_x, scale_rev_y
    real p_x, p_y, q_x, q_y, c_x, c_y, p_dot_q, p_cross_q
    real norm_p, norm_q, norm, c_theta, s_theta, theta
    ! 変換情報書き出しのための変数
    character :: dx_string*6, dy_string*6, dz_string*6, translate_info*28
    character :: roll_string*6, pitch_string*6, yaw_string*6, degree_info*35
    character :: rate_x_string*6, rate_y_string*6, rate_z_string*6, rate_info*26

    ! ========ユーザー入力欄============
    ! グラフィックウィンドウの大きさ・余白
    disp_width = 500.0
    disp_height = 500.0
    disp_margin = 20.0

    ! 大きさrの正n角形を描画
    n = 5
    r = 50.0

    ! 正n角形の中心位置
    x_center = disp_width / 2.0
    y_center = disp_height / 2.0

    ! 平行移動量
    dx = 100.0
    dy = 100.0
    dz = 0.0

    ! 回転角度 degree指定
    ! roll  = 0.0
    ! pitch = 0.0
    yaw = 54.0

    ! 拡大率
    rate_x = 2.0
    rate_y = 2.0
    rate_z = 0.0

    ! 面の数
    surface = 1
    ! ==============================

    !行列の列数
    m = surface * (n + 1)    
    allocate(mat(m, 3), mat2(3, m), origin_mat(3, m))
    allocate(mat_translate(3, m), mat_rotate(3, m), mat_scale(3, m))

    ! 座標作成
    ! 複数行 x 3列の行列．1行に対して(x, y, z)の順に入力
    mat = 0.0
    do i = 1, n + 1
        mat(i, 1) = x_center + r * cos(real(i-1)/real(n)*2.0*pi)
        mat(i, 2) = y_center + r * sin(real(i-1)/real(n)*2.0*pi)
        mat(i, 3) = 0.0
    end do

    ! 転置する 
    do i = 1, 3
        do j = 1, m
            mat2(i, j) = mat(j, i)
        end do
    end do

    ! ==========1. 平行移動==========
    mat_translate = mat2        ! 配列をコピー
    call translate(mat_translate, 3, m, dx, dy, dz)

    ! ==========2. 回転移動==========
    mat_rotate = mat_translate
    ! 原点に移動させる
    call translate(mat_rotate, 3, m, -x_center-dx, -y_center-dy, 0.0)
    ! 原点中心に回転させる
    call rotate(0.0, 0.0, yaw, mat_rotate, mat_rotate, 3, m) 
    ! もとの位置に戻す
    call translate(mat_rotate, 3, m, x_center+dx, y_center+dy, 0.0)
    
    ! ==========3. 拡大・縮小==========
    mat_scale = mat_rotate
    ! 原点に移動させる
    call translate(mat_scale, 3, m, -x_center-dx, -y_center-dy, 0.0)
    ! 原点を中心に拡大させる
    call scale(mat_scale, 3, m, rate_x, rate_y, rate_z)
    ! もとの位置に戻す
    call translate(mat_scale, 3, m, x_center+dx, y_center+dy, 0.0)

    ! ==========4-1. 拡大率を求める==========
    scale_rev_x = (mat_scale(1, 2) - mat_scale(1, 1)) / (mat_rotate(1, 2) - mat_rotate(1, 1))
    scale_rev_y = (mat_scale(2, 2) - mat_scale(2, 1)) / (mat_rotate(2, 2) - mat_rotate(2, 1))

    ! 軸に平行だった場合の例外
    if (scale_rev_x == 0.0) then
        scale_rev_x = (mat_scale(1, 3) - mat_scale(1, 2)) / (mat_rotate(1, 3) - mat_rotate(1, 2))
    end if
    if (scale_rev_y == 0.0) then
        scale_rev_y = (mat_scale(2, 3) - mat_scale(2, 2)) / (mat_rotate(2, 3) - mat_rotate(2, 2))
    end if
    
    ! ==========4-2. 回転角度を求める==========
    ! p -> before rotate, q -> after rotate, c -> center of the polygon
    ! 内積，外積の定義よりcos_theta, sin_thetaを求める -> atan2で角度を求める
    p_x = mat_translate(1, 1)
    p_y = mat_translate(2, 1)
    q_x = mat_rotate(1, 1)
    q_y = mat_rotate(2, 1)
    c_x = x_center + dx
    c_y = y_center + dy

    p_dot_q = (p_x-c_x) * (q_x-c_x) + (p_y-c_y) * (q_y-c_y)
    p_cross_q = (p_x-c_x) * (q_y-c_y) - (p_y-c_y) * (q_x-c_x)
    norm_p = sqrt((p_x-c_x)*(p_x-c_x) + (p_y-c_y)*(p_y-c_y))
    norm_q = sqrt((q_x-c_x)*(q_x-c_x) + (q_y-c_y)*(q_y-c_y))
    norm = norm_p * norm_q

    c_theta = p_dot_q / norm
    s_theta = p_cross_q / norm
    ! Note: atan2(y, x)
    theta = atan2(s_theta, c_theta) * 180 / pi
    
    print *, "拡大率:", "x方向=", scale_rev_x, "y方向=", scale_rev_y
    print *, "回転角:", theta, "[deg]"

    ! 描画
    call xcinit(disp_width, disp_height)
    call xclines(mat2(1, 1:n+1), mat2(2, 1:n+1), n+1)
    call xccolor(5)
    call xclines(mat_translate(1, 1:n+1), mat_translate(2, 1:n+1), n+1)
    call xccolor(4)
    call xclines(mat_rotate(1, 1:n+1), mat_rotate(2, 1:n+1), n+1)
    call xccolor(2)
    call xclines(mat_scale(1, 1:n+1), mat_scale(2, 1:n+1), n+1)

    ! 変換情報を表示する
    call xccolor(5)
    call xcstring(10.0, 80.0, "translation", 11)
    write(dx_string, '(f6.1)') dx
    write(dy_string, '(f6.1)') dy
    write(dz_string, '(f6.1)') dz
    translate_info = "x:" // dx_string // " " // "y:" // dy_string // " " // "z:" // dz_string
    call xccolor(0)
    call xcstring(10.0, 90.0, translate_info, 28)

    call xccolor(4)
    call xcstring(10.0, 110.0, "rotation angle [deg]", 20)
    write(roll_string, '(f6.1)') roll
    write(pitch_string, '(f6.1)') pitch
    write(yaw_string, '(f6.1)') yaw
    degree_info = "x:" // roll_string // " "// "y:" // pitch_string // " " // "z:" // yaw_string
    call xccolor(0)
    call xcstring(10.0, 120.0, degree_info, 35)
    
    call xccolor(2)
    call xcstring(10.0, 140.0, "expansion rate", 14)
    write(rate_x_string, '(f6.1)') rate_x
    write(rate_y_string, '(f6.1)') rate_y
    write(rate_z_string, '(f6.1)') rate_z
    rate_info = "x:" // rate_x_string // " " // "y:" // rate_y_string // " " // "z:" // rate_z_string
    call xccolor(0)
    call xcstring(10.0, 150.0, rate_info, 26)

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