! compile as, f95 osprey.f90 XCgraphics4.o -o 4.x -lX11
! compile as, gfortran -Wconversion osprey.f90 XCgraphics4.o -o osprey.x -lX11
! 塗りつぶしありver.

program transform
    implicit none
    real, dimension(:, :), allocatable :: body, body2, wing, wing2, engine, engine2, prop, prop2, mat_for_trans
    real, parameter :: pi = 4.0 * atan(1.0)
    integer i, j, k, m, n, p, r, col, col1, col2, col3, col4, w_point
    real x, y, z
    real dx, dy, dz
    real disp_width, disp_height, disp_margin, x_center, y_center
    real roll, pitch, yaw
    real square, sin_theta, norm1, norm2, dot
    character a*1

    ! ========ユーザー入力欄============
    ! グラフィックウィンドウの大きさ・余白
    disp_width = 1000.0
    disp_height = 1000.0
    disp_margin = 20.0

    dx = 250.0
    dy = 500.0
    dz = .0

    x_center = disp_width / 2.0
    y_center = disp_height / 2.0

    x_center = .0
    ! y_center = 206.0
    y_center = 0.0

    ! ! 回転角度 degree指定
    roll  = 45.0
    pitch = 45.0
    yaw = .0
    ! ==============================
    ! 座標作成
    ! 複数行 x 3列の行列．1行に対して(x, y, z)の順に入力

    m = 0
    open(10, file="osprey_body.dat")
    do
        read(10, *, end=100)
        m = m + 1
    end do

    100 continue
        allocate(body(m, 3), body2(3, m))
        rewind(10)  ! ファイルの先頭に戻る
        do i = 1, m
            read(10, *) body(i, 1), body(i, 2), body(i, 3)
            print *, body(i, 1), body(i, 2), body(i, 3)      
        end do
        ! 転置する 
        do i = 1, 3
            do j = 1, m
                body2(i, j) = body(j, i)
            end do
        end do
    close(10)

    open(11, file="osprey_wing.dat")
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
        end do
        ! 転置する 
        do i = 1, 3
            do j = 1, n
                wing2(i, j) = wing(j, i)
            end do
        end do
    close(11)

    open(12, file="osprey_engine.dat")
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

    open(13, file="osprey_propeller.dat")
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

    
    
    ! ==========変換計算==========
    ! 配列を追加する場合はcol と，for文を書き直すこと！
    ! 全配列を結合
    col1 = ubound(body2, 2)
    col2 = ubound(wing2, 2)     ! *****変更
    col3 = ubound(engine2, 2)
    col4 = ubound(prop2, 2)
    col = col1 + col2 + col3 + col4        ! *****変更
    allocate(mat_for_trans(3, col))
    do i = 1, 3
        ! *****配列追加の時は以下を変更
        mat_for_trans(i, 1:col1) = body2(i, 1:col1)
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
        body2(i, 1:col1) = mat_for_trans(i, 1:col1)
        wing2(i, 1:col2) = mat_for_trans(i, col1+1:col1+col2)
        engine2(i, 1:col3) = mat_for_trans(i, col1+col2+1:col1+col2+col3)
        prop2(i, 1:col4) = mat_for_trans(i, col1+col2+col3+1:col1+col2+col3+col4)
    end do

    ! ==========描画==========
    call xcinit(disp_width, disp_height)

    ! ==========線の描画==========
    call xcfillrect(0, 0, disp_width, disp_height)
    ! call xccolor(0)
    call xccolor(1)

    ! 最初はもともと三角形なのでこちら
    do i = 0, 8
        if (i-2*int(i/2) == 0) then
            call xccolor(3)
            call xcpolygon([body2(1, 1+i), body2(1, 20+i), body2(1, 21+i)], &
                            [body2(2, 1+i), body2(2, 20+i), body2(2, 21+i)], 3)
        else
            call xccolor(1)
            call xcpolygon([body2(1, 1+i), body2(1, 20+i), body2(1, 21+i)], &
                            [body2(2, 1+i), body2(2, 20+i), body2(2, 21+i)], 3)
        end if
        ! print *, i-2*int(i/2)       ! 剰余を求める　i %% 2みたいな
        
        norm1 = sqrt((body2(1, 20+i)-body2(1, 1+i))**2+(body2(2, 20+i)-body2(2, 1+i))**2+(body2(3, 20+i)-body2(3, 1+i))**2)
        norm2 = sqrt((body2(1, 21+i)-body2(1, 1+i))**2+(body2(2, 21+i)-body2(2, 1+i))**2+(body2(3, 21+i)-body2(3, 1+i))**2)
        print *, norm1, norm2
        print *, body2(1, 1+i)!, body2(1:3, 20+i)
        dot = dot_product(body2(1:3, 1+i), body2(1:3, 20+i))
        print *, dot
        ! sin_theta = sqrt(1-)
        ! square = 0.5 * 
    end do
    call xccolor(1)
    
    ! このループはi = 1 からスタート i = 1, 6 / j = 0, 8
    do i = 0, 6
        do j = 0, 8
            if (i == 6) then
                if (j == 0 .or. j == 1 .or. j == 7 .or. j == 8) then
                    call xccolor(1)
                    call xcpolygon([body2(1, 2+19*i+j), body2(1, 20+19*i+j), body2(1, 21+19*i+j)], &
                            [body2(2, 2+19*i+j), body2(2, 20+19*i+j), body2(2, 21+19*i+j)], 3)
                    call xccolor(3)
                    call xcpolygon([body2(1, 1+19*i+j), body2(1, 2+19*i+j), body2(1, 20+19*i+j)], &
                            [body2(2, 1+19*i+j), body2(2, 2+19*i+j), body2(2, 20+19*i+j)], 3)
                else
                    ! 何もしない
                end if
            else
                call xccolor(1)
                call xcpolygon([body2(1, 2+19*i+j), body2(1, 20+19*i+j), body2(1, 21+19*i+j)], &
                                [body2(2, 2+19*i+j), body2(2, 20+19*i+j), body2(2, 21+19*i+j)], 3)
                call xccolor(3)
                call xcpolygon([body2(1, 1+19*i+j), body2(1, 2+19*i+j), body2(1, 20+19*i+j)], &
                                [body2(2, 1+19*i+j), body2(2, 2+19*i+j), body2(2, 20+19*i+j)], 3)
            end if
        end do
    end do
    call xccolor(1)


    ! 胴体断面
    ! osprey_body.dat line 1 to 190
    ! i = 0-7 胴体断面, i = 8-9 尾翼断面
    do i = 0, 9
        call xclines(body2(1, 1+19*i:19+i*19), body2(2, 1+19*i:19+i*19), 19)
    end do

    ! 胴体断面稜線
    ! do i = 0, 6
    !     do j = 0, 18
    !         call xcline(body2(1, 1+19*i+j), body2(2, 1+19*i+j), body2(1, 20+19*i+j), body2(2, 20+19*i+j))
    !     end do
    ! end do

    

    ! 水平尾翼 稜線 i = 0, 18
    do i = 0, 17
        call xcline(body2(1, 153+i), body2(2, 153+i), body2(1, 172+i), body2(2, 172+i))
    end do

    ! ! 水平尾翼塗りつぶし 胴体と重なる部分は除く i = 0, 17(i = 18は胴体とかぶる)
    ! ! i = 3, 4, 5, 9, 12, 13, 14, 18除外
    ! do i = 0, 8
    !     if (i == 3 .or. i == 4 .or. i == 5) then
    !             ! 何もしない
    !     else
    !         call xccolor(3)
    !         call xcpolygon([body2(1, 153+i), body2(1, 172+i), body2(1, 173+i)], &
    !         [body2(2, 153+i), body2(2, 172+i), body2(2, 173+i)], 3)
    !         call xccolor(1)
    !         call xcpolygon([body2(1, 153+i), body2(1, 154+i), body2(1, 173+i)], &
    !         [body2(2, 153+i), body2(2, 154+i), body2(2, 173+i)], 3)
    !     end if
    ! end do

    ! ! 水平尾翼側面
    ! do i = 0, 2
    !     call xccolor(3)
    !     call xcpolygon([body2(1, 153+i), body2(1, 154+i), body2(1, 162-i)], &
    !                     [body2(2, 153+i), body2(2, 154+i), body2(2, 162-i)], 3)
    !     call xccolor(1)
    !     call xcpolygon([body2(1, 154+i), body2(1, 161-i), body2(1, 162-i)], &
    !                     [body2(2, 154+i), body2(2, 161-i), body2(2, 162-i)], 3)

    !     ! 奥側
    !     call xccolor(3)
    !     call xcpolygon([body2(1, 172+i), body2(1, 173+i), body2(1, 181-i)], &
    !                     [body2(2, 172+i), body2(2, 173+i), body2(2, 181-i)], 3)
    !     call xccolor(1)
    !     call xcpolygon([body2(1, 173+i), body2(1, 180-i), body2(1, 181-i)], &
    !                     [body2(2, 173+i), body2(2, 180-i), body2(2, 181-i)], 3)
    ! end do

    ! ! 垂直尾翼
    ! ! osprey_body.dat line 191 to 270
    ! do i = 0, 7
    !     call xclines_close(body2(1, 191+i*10:200+i*10), body2(2, 191+i*10:200+i*10), 10)
    ! end do


    ! 垂直尾翼　稜線 i = 0, 2, j = 0, 9
    ! do i = 0, 2
    !     do j = 0, 8
    !         ! 左側
    !         call xcline(body2(1, 191+20*i+j), body2(2, 191+20*i+j), body2(1, 191+20+20*i+j), body2(2, 191+20+20*i+j))
    !         call xccolor(3)
    !         call xcpolygon([body2(1, 191+20*i+j), body2(1, 192+20*i+j), body2(1, 211+20*i+j)], &
    !         [body2(2, 191+20*i+j), body2(2, 192+20*i+j), body2(2, 211+20*i+j)], 3)
    !         call xccolor(1)
    !         call xcpolygon([body2(1, 192+20*i+j), body2(1, 211+20*i+j), body2(1, 212+20*i+j)], &
    !         [body2(2, 192+20*i+j), body2(2, 211+20*i+j), body2(2, 212+20*i+j)], 3)
    !         ! 右側
    !         ! call xcline(body2(1, 201+20*i+j), body2(2, 201+20*i+j), body2(1, 201+20+20*i+j), body2(2, 201+20+20*i+j))
    !     end do
    ! end do
    

    ! ! 胴体の膨らみ 
    ! ! osprey_body.dat line 463 to 572
    ! ! 注意　最初は曲面なので，xclines_closeを使用しないこと
    ! call xclines(body2(1, 463:473), body2(2, 463:473), 11)
    ! call xclines(body2(1, 474:484), body2(2, 474:484), 11)
    ! do i = 0, 7
    !     call xclines_close(body2(1, 485+11*i:485+11+i*11), body2(2, 485+11*i:485+11+i*11), 11)
    ! end do

    ! ! 胴体の膨らみ 稜線, i = 0, 3 / j = 0, 10
    ! ! j = 1, 7がサイド稜線
    ! do i = 0, 3
    !     do j = 0, 10
    !         ! 左側
    !         call xcline(body2(1, 463+22*i+j), body2(2, 463+22*i+j), body2(1, 463+22+22*i+j), body2(2, 463+22+22*i+j))
    !         ! 右側
    !         call xcline(body2(1, 474+22*i+j), body2(2, 474+22*i+j), body2(1, 474+22+22*i+j), body2(2, 474+22+22*i+j))
    !     end do
    ! end do

    ! do i = 0, 3
    !     ! 本来はj = 9までだが，9は接合部分（内面）なのでj = 8まで
    !     do j = 0, 8 
    !         ! 左側
    !         call xccolor(3)
    !         call xcpolygon([body2(1, 463+22*i+j), body2(1, 464+22*i+j), body2(1, 485+22*i+j)], &
    !                         [body2(2, 463+22*i+j), body2(2, 464+22*i+j), body2(2, 485+22*i+j)], 3)
    !         ! 右側
    !         call xccolor(1)
    !         call xcpolygon([body2(1, 464+22*i+j), body2(1, 485+22*i+j), body2(1, 486+22*i+j)], &
    !         [body2(2, 464+22*i+j), body2(2, 485+22*i+j), body2(2, 486+22*i+j)], 3)
    !     end do
    ! end do

    ! 機上の機軸方向稜線
    ! i = 0, 3 / j = 0, 22
    do i = 0, 3
        do j = 0, 22
            ! 左側
            call xcline(body2(1, 271+4*j+i), body2(2, 271+4*j+i), body2(1, 271+4+4*j+i), body2(2, 271+4+4*j+i))
            ! 右側
            call xcline(body2(1, 367+4*j+i), body2(2, 367+4*j+i), body2(1, 367+4+4*j+i), body2(2, 367+4+4*j+i))
        end do
    end do

    ! do i = 0, 2
    !     do j = 0, 21
    !         ! 左側
    !         call xccolor(3)
    !         call xcpolygon([body2(1, 271+4*j+i), body2(1, 272+4*j+i), body2(1, 275+4*j+i)], &
    !                        [body2(2, 271+4*j+i), body2(2, 272+4*j+i), body2(2, 275+4*j+i)], 3)
    !         call xccolor(1)
    !         call xcpolygon([body2(1, 272+4*j+i), body2(1, 275+4*j+i), body2(1, 276+4*j+i)], &
    !                         [body2(2, 272+4*j+i), body2(2, 275+4*j+i), body2(2, 276+4*j+i)], 3)
    !     end do
    ! end do

    ! 引く面積
    ! do i = 0, 2
    !     ! 左側
    !     call xccolor(3)
    !     call xcpolygon([body2(1, 271+i), body2(1, 272+i), body2(1, 359+i)], &
    !                     [body2(2, 271+i), body2(2, 272+i), body2(2, 359+i)], 3)
    !     call xccolor(1)
    !     call xcpolygon([body2(1, 272+i), body2(1, 359+i), body2(1, 360+i)], &
    !                     [body2(2, 272+i), body2(2, 359+i), body2(2, 360+i)], 3)
    ! end do


    ! 主翼断面
    ! do i = 0, 4
    !     ! 左側
    !     call xclines_close(wing2(1, 1+70*i:1+70*i+34), wing2(2, 1+70*i:1+70*i+34), 35)
    !     ! 右側
    !     call xclines_close(wing2(1, 36+70*i:36+70*i+34), wing2(2, 36+70*i:36+70*i+34), 35)
    ! end do

    ! ! *******メモ
    ! ! ! 主翼断面この書き方もあった
    ! ! ! osprey_wing.dat line 1 to 350
    ! ! ! do i = 0, 9
    ! ! !     call xclines_close(wing2(1, 1+35*i:35+35*i), wing2(2, 1+35*i:35+35*i), 35)
    ! ! ! end do
    ! ! ***********
    
    ! 主翼稜線 i = 0, 3 / j = 0, 34
    ! do i = 0, 3
    !     do j = 0, 34
    !         ! 左側
    !         call xcline(wing2(1, 1+70*i+j), wing2(2, 1+70*i+j), wing2(1, 71+70*i+j), wing2(2, 71+70*i+j))
    !         ! 右側
    !         call xcline(wing2(1, 36+70*i+j), wing2(2, 36+70*i+j), wing2(1, 106+70*i+j), wing2(2, 106+70*i+j))
    !     end do
    ! end do


    ! ! 主翼表面
    ! do i = 0, 3
    !     do j = 0, 33
    !         ! 左
    !         call xccolor(3)
    !         call xcpolygon([wing2(1, 1+70*i+j), wing2(1, 2+70*i+j), wing2(1, 71+70*i+j)], &
    !                        [wing2(2, 1+70*i+j), wing2(2, 2+70*i+j), wing2(2, 71+70*i+j)], 3)
    !         call xccolor(1)
    !         call xcpolygon([wing2(1, 2+70*i+j), wing2(1, 71+70*i+j), wing2(1, 72+70*i+j)], &
    !                        [wing2(2, 2+70*i+j), wing2(2, 71+70*i+j), wing2(2, 72+70*i+j)], 3)
    !     end do
    ! end do

    ! 翼端断面 i = 0, 32
    ! do i = 0, 32
    !     call xccolor(1)
    !     call xcpolygon([wing2(1, 281), wing2(1, 282+1*i), wing2(1, 283+1*i)], &
    !                     [wing2(2, 281), wing2(2, 282+1*i), wing2(2, 283+1*i)], 3)
    ! end do
    

    ! !  翼根x方向稜線 ! i = 21に変更
    ! do i = 0, 21
    !     do j = 0, 2
    !         ! 左側
    !         call xcline(body2(1, 279+4*i+j), body2(2, 279+4*i+j), body2(1, 280+4*i+j), body2(2, 280+4*i+j))
    !         ! 右側
    !         call xcline(body2(1, 375+4*i+j), body2(2, 375+4*i+j), body2(1, 376+4*i+j), body2(2, 376+4*i+j))
    !     end do
    ! end do

    ! ! 主翼とエンジンの間
    ! osprey_wing.dat line 351 to 390
    call xclines_close(wing2(1, 351:360), wing2(2, 351:360), 10)
    call xclines_close(wing2(1, 361:370), wing2(2, 361:370), 10)
    ! 以下はxclines_close使用しないこと！
    call xclines(wing2(1, 371:380), wing2(2, 371:380), 10)
    call xclines(wing2(1, 381:390), wing2(2, 381:390), 10)

    ! ! 主翼とエンジンの間　塗りつぶし
    ! do i = 0, 7
    !     if (i-2*int(i/2)==0) then
    !         call xccolor(1)
    !     else 
    !         call xccolor(3)
    !     end if
    !     call xcpolygon([wing2(1, 351), wing2(1, 352+1*i), wing2(1, 353+1*i)], &
    !                     [wing2(2, 351), wing2(2, 352+1*i), wing2(2, 353+1*i)], 3)
    ! end do
    ! call xccolor(1)

    ! 主翼とエンジンの間の線を描画
    do i = 0, 9
        ! 左側
        call xcline(wing2(1, 351+i), wing2(2, 351+i), wing2(1, 351+20+i), wing2(2, 351+20+i))
        ! 右側
        call xcline(wing2(1, 361+i), wing2(2, 361+i), wing2(1, 361+20+i), wing2(2, 361+20+i))
    end do

    ! ! 主翼とエンジンの間　側面 i = 0, 8
    ! do i = 0, 8
    !     call xccolor(1)
    !     call xcpolygon([wing2(1, 351+i), wing2(1, 352+i), wing2(1, 371+i)], &
    !                     [wing2(2, 351+i), wing2(2, 352+i), wing2(2, 371+i)], 3)
    !     call xccolor(3)
    !     call xcpolygon([wing2(1, 352+i), wing2(1, 371+i), wing2(1, 372+i)], &
    !                     [wing2(2, 352+i), wing2(2, 371+i), wing2(2, 372+i)], 3)
    ! end do
    ! 最後の面
    call xccolor(1)
        call xcpolygon([wing2(1, 351), wing2(1, 360), wing2(1, 380)], &
                        [wing2(2, 351), wing2(2, 360), wing2(2, 380)], 3)
    call xccolor(3)
    call xcpolygon([wing2(1, 351), wing2(1, 371), wing2(1, 380)], &
                    [wing2(2, 351), wing2(2, 371), wing2(2, 380)], 3)
    call xccolor(1)

    
    ! エンジン
    ! osprey_engine.dat line 1 to 168 - surface
    ! osprey_engine.dat line 169 to 224 - 円柱部
    ! osprey_engine.dat line 225 to 336 - 円錐部
    ! i = 0, 23
    do i = 0, 23
        call xclines_close(engine2(1, 1+i*14:14+i*14), engine2(2, 1+i*14:14+i*14), 14)
    end do

    

    ! エンジン　稜線
    do i = 0, 10
        do j = 0, 13
            ! 左側
            call xcline(engine2(1, 1+28*i+j), engine2(2, 1+28*i+j), engine2(1, 29+28*i+j), engine2(2, 29+28*i+j))
            ! 右側
            call xcline(engine2(1, 15+28*i+j), engine2(2, 15+28*i+j), engine2(1, 43+28*i+j), engine2(2, 43+28*i+j))
        end do
    end do

    ! ! エンジン塗りつぶし i = 0, 9 / j = 0, 12
    ! do i = 0, 9
    !     do j = 0, 12
    !         ! 左側
    !         call xccolor(1)
    !         call xcpolygon([engine2(1, 1+28*i+j), engine2(1, 2+28*i+j), engine2(1, 29+28*i+j)], &
    !                         [engine2(2, 1+28*i+j), engine2(2, 2+28*i+j), engine2(2, 29+28*i+j)], 3)
    !         call xccolor(3)
    !         call xcpolygon([engine2(1, 2+28*i+j), engine2(1, 29+28*i+j), engine2(1, 30+28*i+j)], &
    !                         [engine2(2, 2+28*i+j), engine2(2, 29+28*i+j), engine2(2, 30+28*i+j)], 3)
    !     end do
    ! end do
    ! call xccolor(1)

    ! ! エンジン最低面　塗りつぶし
    ! do i = 0, 11
    !     if (i-2*int(i/2)==0) then
    !         call xccolor(1)
    !     else 
    !         call xccolor(3)
    !     end if
    !     call xcpolygon([engine2(1, 1), engine2(1, 2+i), engine2(1, 3+i)], &
    !                    [engine2(2, 1), engine2(2, 2+i), engine2(2, 3+i)], 3)
    ! end do
    ! call xccolor(1)

    ! ! プロペラ
    ! ! osprey_propeller.dat line 1 to 16 左右プロペラ1
    ! ! osprey_propeller.dat line 17 to 32 左右プロペラ2
    ! ! osprey_propeller.dat line 33 to 48 左右プロペラ3
    ! do i = 0, 11
    !     call xclines_close(prop2(1, 1+4*i:4+i*4), prop2(2, 1+4*i:4+i*4), 4)
    ! end do

    ! ! プロペラ稜線 i = 0, 2 / j = 0, 3
    ! do i = 0, 2
    !     do j = 0, 3
    !         ! 左側
    !         call xcline(prop2(1, 1+16*i+j), prop2(2, 1+16*i+j), prop2(1, 9+16*i+j), prop2(2, 9+16*i+j))
    !         ! 右側
    !         call xcline(prop2(1, 5+16*i+j), prop2(2, 5+16*i+j), prop2(1, 13+16*i+j), prop2(2, 13+16*i+j))
    !     end do
    ! end do

    ! ! プロペラ塗りつぶし i = 0, 2 / j = 0, 2
    ! ! pattern 9 = 12 and 10 = 11
    ! ! 1, 2, 9  | 9, 10, 2
    ! ! 3, 4, 9  | 9, 10, 3
    ! ! 1, 4, 9  | 2, 3,  10
    ! do i = 0, 2
    !     call xccolor(1)
    !     ! call xcpolygon([prop2(1, 1+16*i), prop2(1, 2+16*i), prop2(1, 9+16*i)], &
    !     !                 [prop2(2, 1+16*i), prop2(2, 2+16*i), prop2(2, 9+16*i)], 3)
    !     ! call xcpolygon([prop2(1, 3+16*i), prop2(1, 4+16*i), prop2(1, 9+16*i)], &
    !     !                 [prop2(2, 3+16*i), prop2(2, 4+16*i), prop2(2, 9+16*i)], 3)
    !     call xcpolygon([prop2(1, 2+16*i), prop2(1, 3+16*i), prop2(1, 10+16*i)], &
    !                     [prop2(2, 2+16*i), prop2(2, 3+16*i), prop2(2, 10+16*i)], 3)
    !     call xccolor(3)
    !     ! call xcpolygon([prop2(1, 2+16*i), prop2(1, 9+16*i), prop2(1, 10+16*i)], &
    !     !                 [prop2(2, 2+16*i), prop2(2, 9+16*i), prop2(2, 10+16*i)], 3)
    !     ! call xcpolygon([prop2(1, 3+16*i), prop2(1, 9+16*i), prop2(1, 10+16*i)], &
    !     !                 [prop2(2, 3+16*i), prop2(2, 9+16*i), prop2(2, 10+16*i)], 3)
    !     call xcpolygon([prop2(1, 1+16*i), prop2(1, 4+16*i), prop2(1, 9+16*i)], &
    !                     [prop2(2, 1+16*i), prop2(2, 4+16*i), prop2(2, 9+16*i)], 3)
    ! end do
    ! call xccolor(1)
  

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