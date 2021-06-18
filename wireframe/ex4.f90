! compile as, f95 ex4_test.f90 XCgraphics4.o -o 4.x -lX11

program transform
    implicit none
    real, dimension(:, :), allocatable :: mat, mat2, wing, wing2, engine, engine2, prop, prop2, mat_for_trans
    real, parameter :: pi = 4.0 * atan(1.0)
    integer i, j, m, n, p, r, col, col1, col2, col3, col4, w_point
    real x, y, z
    real dx, dy, dz
    real disp_width, disp_height, disp_margin, x_center, y_center
    real roll, pitch, yaw
    character a*1, roll_string*6, pitch_string*6, yaw_string*6, degree*29

    ! ========ユーザー入力欄============
    ! グラフィックウィンドウの大きさ・余白
    disp_width = 1000.0
    disp_height = 1000.0
    disp_margin = 20.0

    ! dx = 500.0
    dx = 250.0
    dy = 250.0
    dz = .0

    ! 正n角形の中心位置
    x_center = disp_width / 2.0
    y_center = disp_height / 2.0

    ! ! 回転角度 degree指定
    roll  = .0
    pitch = 0.0
    yaw = .0
    ! ==============================
    ! 座標作成
    ! 複数行 x 3列の行列．1行に対して(x, y, z)の順に入力

    m = 0
    open(10, file="ex4_2.dat")
    do
        read(10, *, end=100)
        m = m + 1
    end do

    100 continue
        allocate(mat(m, 3), mat2(3, m))
        rewind(10)  ! ファイルの先頭に戻る
        do i = 1, m
            read(10, *) mat(i, 1), mat(i, 2), mat(i, 3)
            print *, mat(i, 1), mat(i, 2), mat(i, 3)      
        end do
        ! 転置する 
        do i = 1, 3
            do j = 1, m
                mat2(i, j) = mat(j, i)
            end do
        end do
    close(10)

    open(11, file="wing.dat")
    print *, "wing.dat******************************************************************"
    n = 0
    do
        read(11, *, end=200)
        n = n + 1
    end do

    200 continue
        allocate(wing(n, 3), wing2(3, n))
        rewind(11)  ! ファイルの先頭に戻る
        do i = 1, n
            read(11, *) wing(i, 1), wing(i, 2), wing(i, 3)
            ! print *, wing(i, 1), wing(i, 2), wing(i, 3)      
        end do
        ! 転置する 
        do i = 1, 3
            do j = 1, n
                wing2(i, j) = wing(j, i)
            end do
        end do
    close(11)

    open(12, file="engine.dat")
    p = 0
    do
        read(12, *, end=300)
        p = p + 1
    end do

    300 continue
        allocate(engine(p, 3), engine2(3, p))
        rewind(12)  ! ファイルの先頭に戻る
        do i = 1, p
            read(12, *) engine(i, 1), engine(i, 2), engine(i, 3)     
        end do
        ! 転置する 
        do i = 1, 3
            do j = 1, p
                engine2(i, j) = engine(j, i)
            end do
        end do
    close(12)

    open(13, file="propellar.dat")
    r = 0
    do
        read(13, *, end=400)
        r = r + 1
    end do
    400 continue
        allocate(prop(r, 3), prop2(3, r))
        rewind(13)  ! ファイルの先頭に戻る
        do i = 1, r
            read(13, *) prop(i, 1), prop(i, 2), prop(i, 3)     
        end do
        ! 転置する 
        do i = 1, 3
            do j = 1, r
                prop2(i, j) = prop(j, i)
            end do
        end do
    close(13)

    x_center = 0.0
    y_center = 206.0
    
    ! ==========変換計算==========
    ! 配列を追加する場合はcol と，for文を書き直すこと！
    ! 全配列を結合
    col1 = ubound(mat2, 2)
    col2 = ubound(wing2, 2)     ! *****変更
    col3 = ubound(engine2, 2)
    col4 = ubound(prop2, 2)
    col = col1 + col2 + col3 + col4        ! *****変更
    allocate(mat_for_trans(3, col))
    do i = 1, 3
        ! *****配列追加の時は以下を変更
        mat_for_trans(i, 1:col1) = mat2(i, 1:col1)
        mat_for_trans(i, col1+1:col1+col2) = wing2(i, 1:col2)
        mat_for_trans(i, col1+col2+1:col1+col2+col3) = engine2(i, 1:col3)
        mat_for_trans(i, col1+col2+col3+1:col1+col2+col3+col4) = prop2(i, 1:col4)
    end do
    

    ! 変換実行
    call translate(mat_for_trans, ubound(mat_for_trans, 1), ubound(mat_for_trans, 2), dx, dy, dz)
    ! 原点に移動させる
    call translate(mat_for_trans, ubound(mat_for_trans, 1), ubound(mat_for_trans, 2), -x_center-dx, -y_center-dy, 0.0)
    ! ! 原点を中心に回転
    call rotate(0.0, 0.0, yaw, mat_for_trans, mat_for_trans, ubound(mat_for_trans, 1), ubound(mat_for_trans, 2))
    call rotate(0.0, pitch, 0.0, mat_for_trans, mat_for_trans, ubound(mat_for_trans, 1), ubound(mat_for_trans, 2))
    call rotate(roll, 0.0, 0.0, mat_for_trans, mat_for_trans, ubound(mat_for_trans, 1), ubound(mat_for_trans, 2))
    ! ! もとの位置に戻す
    call translate(mat_for_trans, ubound(mat_for_trans, 1), ubound(mat_for_trans, 2), x_center+dx, y_center+dy, 0.0) 

    do i = 1, 3
        ! *****配列追加の時は以下を変更
        mat2(i, 1:col1) = mat_for_trans(i, 1:col1)
        wing2(i, 1:col2) = mat_for_trans(i, col1+1:col1+col2)
        engine2(i, 1:col3) = mat_for_trans(i, col1+col2+1:col1+col2+col3)
        prop2(i, 1:col4) = mat_for_trans(i, col1+col2+col3+1:col1+col2+col3+col4)
    end do
    print *, wing2(:, 1)
    print *, wing2(:, 2)


    ! print *, "trans_mat.dat******************************************************************"
    ! do i = 1, col
    !     print *, mat_for_trans(:, i)
    ! end do

    ! ==========描画==========
    call xcinit(disp_width, disp_height)

    ! ==========線の描画==========
    call xcfillrect(0, 0, disp_width, disp_height)
    call xccolor(1)
    call xclines_close(mat2(1, 60:69), mat2(2, 60:69), 10)
    call xclines_close(mat2(1, 70:79), mat2(2, 70:79), 10)
    call xclines_close(mat2(1, 80:89), mat2(2, 80:89), 10)
    call xclines_close(mat2(1, 90:99), mat2(2, 90:99), 10)
    call xclines_close(mat2(1, 100:109), mat2(2, 100:109), 10)
    call xclines_close(mat2(1, 110:119), mat2(2, 110:119), 10)
    call xclines_close(mat2(1, 120:129), mat2(2, 120:129), 10)
    call xclines_close(mat2(1, 130:139), mat2(2, 130:139), 10)      
    call xclines_close(mat2(1, 140:149), mat2(2, 140:149), 10)      ! 水平尾翼前
    call xclines_close(mat2(1, 150:159), mat2(2, 150:159), 10)      ! 水平尾翼
    call xclines_close(mat2(1, 160:169), mat2(2, 160:169), 10)      ! 垂直尾翼上端
    call xclines_close(mat2(1, 170:179), mat2(2, 170:179), 10)
    call xclines_close(mat2(1, 180:189), mat2(2, 180:189), 10)
    call xclines_close(mat2(1, 190:199), mat2(2, 190:199), 10)

    ! 200 ~ 227はwing付け根
    ! 以下は不要
    ! call xclines_close(mat2(1, 228:262), mat2(2, 228:262), 35)  ! 主翼断面1
    ! call xclines_close(mat2(1, 263:297), mat2(2, 263:297), 35)
    ! call xclines_close(mat2(1, 298:332), mat2(2, 298:332), 35)
    ! call xclines_close(mat2(1, 333:367), mat2(2, 333:367), 35)
    ! call xclines_close(mat2(1, 368:402), mat2(2, 368:402), 35)
    ! 主翼断面
    call xclines_close(wing2(1, 1:35), wing2(2, 1:35), 35)     ! 主翼断面
    call xclines_close(wing2(1, 36:70), wing2(2, 36:70), 35)
    call xclines_close(wing2(1, 71:105), wing2(2, 71:105), 35)
    call xclines_close(wing2(1, 106:140), wing2(2, 106:140), 35)
    call xclines_close(wing2(1, 141:175), wing2(2, 141:175), 35)

    ! ! wing.datのline176は区切り
    ! ! 主翼とエンジンの間
    call xclines_close(wing2(1, 177:186), wing2(2, 177:186), 10)
    ! ここはxcline_closeにしないこと! 
    call xclines(wing2(1, 187:196), wing2(2, 187:196), 10)

    ! ! エンジン
    call xclines_close(engine2(1, 1:14), engine2(2, 1:14), 14)
    call xclines_close(engine2(1, 15:28), engine2(2, 15:28), 14)
    call xclines_close(engine2(1, 29:42), engine2(2, 29:42), 14)
    call xclines_close(engine2(1, 43:56), engine2(2, 43:56), 14)
    call xclines_close(engine2(1, 57:70), engine2(2, 57:70), 14)
    call xclines_close(engine2(1, 71:84), engine2(2, 71:84), 14)
    ! ! 円柱
    call xclines_close(engine2(1, 85:98), engine2(2, 85:98), 14)
    call xclines_close(engine2(1, 99:112), engine2(2, 99:112), 14)
    ! ! 円錐
    call xclines_close(engine2(1, 113:126), engine2(2, 113:126), 14)
    call xclines_close(engine2(1, 127:140), engine2(2, 127:140), 14)
    call xclines_close(engine2(1, 141:154), engine2(2, 141:154), 14)
    call xclines_close(engine2(1, 155:168), engine2(2, 155:168), 14)

    ! ! ! プロペラ
    call xclines_close(prop2(1, 1:4), prop2(2, 1:4), 4)
    call xclines_close(prop2(1, 5:8), prop2(2, 5:8), 4)
    ! プロペラ2
    call xclines_close(prop2(1, 9:12), prop2(2, 9:12), 4)
    call xclines_close(prop2(1, 13:16), prop2(2, 13:16), 4)
    ! ! プロペラ3
    call xclines_close(prop2(1, 17:20), prop2(2, 17:20), 4)
    call xclines_close(prop2(1, 21:24), prop2(2, 21:24), 4)

    



    ! 稜線
    do i = 0, 4
        call xcline(mat2(1, 60+i*2), mat2(2, 60+i*2), mat2(1, 70+i*2), mat2(2, 70+i*2))
        call xcline(mat2(1, 70+i*2), mat2(2, 70+i*2), mat2(1, 80+i*2), mat2(2, 80+i*2))
        call xcline(mat2(1, 80+i*2), mat2(2, 80+i*2), mat2(1, 90+i*2), mat2(2, 90+i*2))
        call xcline(mat2(1, 90+i*2), mat2(2, 90+i*2), mat2(1, 100+i*2), mat2(2, 100+i*2))
        call xcline(mat2(1, 100+i*2), mat2(2, 100+i*2), mat2(1, 110+i*2), mat2(2, 110+i*2))
        call xcline(mat2(1, 110+i*2), mat2(2, 110+i*2), mat2(1, 120+i*2), mat2(2, 120+i*2))
        call xcline(mat2(1, 120+i*2), mat2(2, 120+i*2), mat2(1, 130+i*2), mat2(2, 130+i*2))
    end do

    ! 胴体
    do i = 0, 9
        call xcline(mat2(1, 60+i), mat2(2, 60+i), mat2(1, 70+i), mat2(2, 70+i))
        call xcline(mat2(1, 70+i), mat2(2, 70+i), mat2(1, 80+i), mat2(2, 80+i))
        call xcline(mat2(1, 80+i), mat2(2, 80+i), mat2(1, 90+i), mat2(2, 90+i))
        call xcline(mat2(1, 90+i), mat2(2, 90+i), mat2(1, 100+i), mat2(2, 100+i))
        call xcline(mat2(1, 100+i), mat2(2, 100+i), mat2(1, 110+i), mat2(2, 110+i))
        call xcline(mat2(1, 110+i), mat2(2, 110+i), mat2(1, 120+i), mat2(2, 120+i))
        call xcline(mat2(1, 120+i), mat2(2, 120+i), mat2(1, 130+i), mat2(2, 130+i))
        ! call xcline(mat2(1, 140+i), mat2(2, 140+i), mat2(1, 150+i), mat2(2, 150+i))     ! 水平尾翼
        call xcline(mat2(1, 160+i), mat2(2, 160+i), mat2(1, 170+i), mat2(2, 170+i))       ! 垂直尾翼
        call xcline(mat2(1, 170+i), mat2(2, 170+i), mat2(1, 180+i), mat2(2, 180+i))
        call xcline(mat2(1, 180+i), mat2(2, 180+i), mat2(1, 190+i), mat2(2, 190+i))
    end do

    ! 機上の稜線 208 ~ 283までは翼
    do i = 0, 3
        do j = 0, 22
            call xcline(mat2(1, 200+4*j+i), mat2(2, 200+4*j+i), mat2(1, 204+4*j+i), mat2(2, 204+4*j+i))
        end do
    end do

    ! ! 主翼稜線
    do i = 0, 3
        w_point = 35  ! 主翼ポイント数
        call xcline(wing2(1, w_point*i+1), wing2(2, w_point*i+1), wing2(1, w_point*(i+1)+1), wing2(2, w_point*(i+1)+1))
        call xcline(wing2(1, w_point*i+5), wing2(2, w_point*i+5), wing2(1, w_point*(i+1)+5), wing2(2, w_point*(i+1)+5))
        call xcline(wing2(1, w_point*i+9), wing2(2, w_point*i+9), wing2(1, w_point*(i+1)+9), wing2(2, w_point*(i+1)+9))
        call xcline(wing2(1, w_point*i+18), wing2(2, w_point*i+18), wing2(1, w_point*(i+1)+18), wing2(2, w_point*(i+1)+18))
        call xcline(wing2(1, w_point*i+27), wing2(2, w_point*i+27), wing2(1, w_point*(i+1)+27), wing2(2, w_point*(i+1)+27))
        call xcline(wing2(1, w_point*i+31), wing2(2, w_point*i+31), wing2(1, w_point*(i+1)+31), wing2(2, w_point*(i+1)+31))
               
        ! call xcline(wing2(1, w_point*i+3), wing2(2, w_point*i+3), wing2(1, w_point*(i+1)+3), wing2(2, w_point*(i+1)+3))
        ! call xcline(wing2(1, w_point*i+7), wing2(2, w_point*i+7), wing2(1, w_point*(i+1)+7), wing2(2, w_point*(i+1)+7))
        ! call xcline(wing2(1, w_point*i+18), wing2(2, w_point*i+18), wing2(1, w_point*(i+1)+18), wing2(2, w_point*(i+1)+18))
    end do

    ! line 176 は区切り
    ! 主翼とエンジンの間描画
    do i = 0, 9
        call xcline(wing2(1, 177+i), wing2(2, 177+i), wing2(1, 177+10+i), wing2(2, 177+10+i))
    end do   

    ! ! エンジン稜線
    do i = 1, 14
        call xcline(engine2(1, i), engine2(2, i), engine2(1, i+14), engine2(2, i+14))
        call xcline(engine2(1, i+14), engine2(2, i+14), engine2(1, 2*14+i), engine2(2, 2*14+i))
        call xcline(engine2(1, 2*14+i), engine2(2, 2*14+i), engine2(1, 3*14+i), engine2(2, 3*14+i))
        ! ! 3 to 4 の稜線はなし
        ! 円柱部
        call xcline(engine2(1, 4*14+i), engine2(2, 4*14+i), engine2(1, 5*14+i), engine2(2, 5*14+i))
        call xcline(engine2(1, 5*14+i), engine2(2, 5*14+i), engine2(1, 6*14+i), engine2(2, 6*14+i))
        ! 円錐部
        call xcline(engine2(1, 6*14+i), engine2(2, 6*14+i), engine2(1, 7*14+i), engine2(2, 7*14+i))
        call xcline(engine2(1, 7*14+i), engine2(2, 7*14+i), engine2(1, 8*14+i), engine2(2, 8*14+i))
        call xcline(engine2(1, 8*14+i), engine2(2, 8*14+i), engine2(1, 9*14+i), engine2(2, 9*14+i))
        call xcline(engine2(1, 9*14+i), engine2(2, 9*14+i), engine2(1, 10*14+i), engine2(2, 10*14+i))
        call xcline(engine2(1, 10*14+i), engine2(2, 10*14+i), engine2(1, 11*14+i), engine2(2, 11*14+i))
    end do

    ! ! プロペラ稜線
    do i = 1, 4
        call xcline(prop2(1, i), prop2(2, i), prop2(1, i+4), prop2(2, i+4))
        call xcline(prop2(1, 2*4+i), prop2(2, 2*4+i), prop2(1, 3*4+i), prop2(2, 3*4+i))
        call xcline(prop2(1, 4*4+i), prop2(2, 4*4+i), prop2(1, 5*4+i), prop2(2, 5*4+i))
    end do



    call xccolor(1)
    ! do i = -3, 3
    !     call xcpoints(mat2(1, 1:m), mat2(2, 1:m)+i, m)
    ! end do

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