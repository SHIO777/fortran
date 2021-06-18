! compile as, gfortran -Wconversion osprey_calc.f90 -o osprey_calc.x

program calc
    implicit none
    real, dimension(:, :), allocatable :: body, body2, wing, wing2, engine, engine2, prop, prop2
    ! real, parameter :: pi = 4.0 * atan(1.0)
    integer i, j, k, m, n, p, r
    real :: s_fuselage, s_h_tail, s_v_tail, s_wing, s_engine, s_prop, sum_square = 0.0
    character a*1, process*10
    real :: debug = 0.0

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
            ! print *, body(i, 1), body(i, 2), body(i, 3)      
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


    print *, "fuselage"
    ! 先端 - 胴体表面1 (三角形のみなので別処理)
    do i = 0, 8
        call square_calc(body2(1:3, 1+i), body2(1:3, 20+i), body2(1:3, 21+i), 1, 2.0, s_fuselage)
    end do
        
    ! 胴体表面1 - 胴体表面6 (四角形) i = 1, 6 / j = 0, 8
    ! i = 6 / j = 2-6は，水平尾翼との接合面なので除外
    do i = 1, 6
        do j = 0, 8
            if (i == 6) then
                if (j == 0 .or. j == 1 .or. j == 7 .or. j == 8) then
                    call square_calc(body2(1:3, 1+19*i+j), body2(1:3, 2+19*i+j), body2(1:3, 20+19*i+j), 1, 2.0, s_fuselage)
                    call square_calc(body2(1:3, 2+19*i+j), body2(1:3, 20+19*i+j), body2(1:3, 21+19*i+j), 1, 2.0, s_fuselage)
                else
                    ! 何もしない
                end if
            else
                call square_calc(body2(1:3, 1+19*i+j), body2(1:3, 2+19*i+j), body2(1:3, 20+19*i+j), 1, 2.0, s_fuselage)
                call square_calc(body2(1:3, 2+19*i+j), body2(1:3, 20+19*i+j), body2(1:3, 21+19*i+j), 1, 2.0, s_fuselage)
            end if
        end do
    end do


    ! 水平尾翼
    print *, "horizontal tail"
    do i = 0, 8
        if (i == 3 .or. i == 4 .or. i == 5) then
                ! 何もしない
        else
            call square_calc(body2(1:3, 153+i), body2(1:3, 172+i), body2(1:3, 173+i), 1, 2.0, s_h_tail)
            call square_calc(body2(1:3, 153+i), body2(1:3, 154+i), body2(1:3, 173+i), 1, 2.0, s_h_tail)
        end if
    end do

    do i = 0, 2
        call square_calc(body2(1:3, 153+i), body2(1:3, 154+i), body2(1:3, 162-i), 1, 2.0, s_h_tail)
        call square_calc(body2(1:3, 154+i), body2(1:3, 161-i), body2(1:3, 162-i), 1, 2.0, s_h_tail)
        ! 奥側
        call square_calc(body2(1:3, 172+i), body2(1:3, 173+i), body2(1:3, 181-i), 1, 2.0, s_h_tail)
        call square_calc(body2(1:3, 173+i), body2(1:3, 180-i), body2(1:3, 181-i), 1, 2.0, s_h_tail)
    end do

    ! 垂直尾翼
    ! osprey_body.dat line 191 to 270
    print *, "vertical tail"
    do i = 0, 2
        do j = 0, 8
            ! 左側
            call square_calc(body2(1:3, 191+20*i+j), body2(1:3, 192+20*i+j), body2(1:3, 211+20*i+j), 1, 2.0, s_v_tail)
            call square_calc(body2(1:3, 192+20*i+j), body2(1:3, 211+20*i+j), body2(1:3, 212+20*i+j), 1, 2.0, s_v_tail)
        end do
    end do
    

    ! 胴体の膨らみ
    ! osprey_body.dat line 463 to 572
    ! i = 0, 3 / j = 0, 8
    print *, "fuselage fukurami"
    do i = 0, 3
        ! 本来はj = 9までだが，9は接合部分（内面）なのでj = 8まで
        do j = 0, 8
            ! print *, "i", i, "j", j
            ! 左側
            call square_calc(body2(1:3, 463+22*i+j), body2(1:3, 464+22*i+j), body2(1:3, 485+22*i+j), 1, 2.0, s_fuselage)
            ! 右側
            call square_calc(body2(1:3, 464+22*i+j), body2(1:3, 485+22*i+j), body2(1:3, 486+22*i+j), 1, 2.0, s_fuselage)
        end do
    end do

    ! 機上の機軸方向 i = 0, 2 / 0, 21
    print *, "body top"
    do i = 0, 2
        do j = 0, 21
            ! 左側
            call square_calc(body2(1:3, 271+4*j+i), body2(1:3, 272+4*j+i), body2(1:3, 275+4*j+i), 1, 2.0, s_fuselage)
            call square_calc(body2(1:3, 272+4*j+i), body2(1:3, 275+4*j+i), body2(1:3, 276+4*j+i), 1, 2.0, s_fuselage)
        end do
    end do

    !! 減算
    ! 胴体との重なり分
    do i = 0, 2
        ! 左側)
        call square_calc(body2(1:3, 271+i), body2(1:3, 272+i), body2(1:3, 359+i), 0, 2.0, s_fuselage)
        call square_calc(body2(1:3, 272+i), body2(1:3, 359+i), body2(1:3, 360+i), 0, 2.0, s_fuselage)
    end do

    ! 主翼
    print *, "main wing"
    do i = 0, 3
        do j = 0, 33
            ! 左側
            call square_calc(wing2(1:3, 1+70*i+j), wing2(1:3, 2+70*i+j), wing2(1:3, 71+70*i+j), 1, 2.0, s_wing)
            call square_calc(wing2(1:3, 2+70*i+j), wing2(1:3, 71+70*i+j), wing2(1:3, 72+70*i+j), 1, 2.0, s_wing)
        end do
    end do

    ! 主翼翼端断面  減算
    do i = 0, 32
        call square_calc(wing2(1:3, 281), wing2(1:3, 282+1*i), wing2(1:3, 283+1*i), 0, 2.0, s_engine)
    end do

    ! 主翼とエンジンの間断面
    do i = 0, 7
        call square_calc(wing2(1:3, 351), wing2(1:3, 352+1*i), wing2(1:3, 353+1*i), 1, 2.0, s_wing)
    end do

    ! 主翼とエンジンの間側面
    do i = 0, 8
        call square_calc(wing2(1:3, 351+i), wing2(1:3, 352+i), wing2(1:3, 371+i), 1, 2.0, s_wing)
        call square_calc(wing2(1:3, 352+i), wing2(1:3, 371+i), wing2(1:3, 372+i), 1, 2.0, s_wing)
    end do
    ! 最後の面
    call square_calc(wing2(1:3, 351), wing2(1:3, 360), wing2(1:3, 380), 1, 2.0, s_wing)
    call square_calc(wing2(1:3, 351), wing2(1:3, 371), wing2(1:3, 380), 1, 2.0, s_wing)    
    ! エンジン
    do i = 0, 9
        do j = 0, 12
            ! 左側
            call square_calc(engine2(1:3, 1+28*i+j), engine2(1:3, 2+28*i+j), engine2(1:3, 29+28*i+j), 1, 2.0, s_engine)
            call square_calc(engine2(1:3, 2+28*i+j), engine2(1:3, 29+28*i+j), engine2(1:3, 30+28*i+j), 1, 2.0, s_engine)
        end do
    end do

    ! エンジン最低面
    do i = 0, 11
        call square_calc(engine2(1:3, 1), engine2(1:3, 2+i), engine2(1:3, 3+i), 1, 2.0, s_engine)
    end do


    ! ! プロペラ i = 0, 2
    do i = 0, 0
        call square_calc(prop2(1:3, 1+16*i), prop2(1:3, 2+16*i), prop2(1:3, 9+16*i), 1, 2.0, s_prop)
        call square_calc(prop2(1:3, 3+16*i), prop2(1:3, 4+16*i), prop2(1:3, 9+16*i), 1, 2.0, s_prop)
        call square_calc(prop2(1:3, 2+16*i), prop2(1:3, 3+16*i), prop2(1:3, 10+16*i), 1, 2.0, s_prop)
        call square_calc(prop2(1:3, 2+16*i), prop2(1:3, 9+16*i), prop2(1:3, 10+16*i), 1, 2.0, s_prop)
        call square_calc(prop2(1:3, 3+16*i), prop2(1:3, 9+16*i), prop2(1:3, 10+16*i), 1, 2.0, s_prop)
        call square_calc(prop2(1:3, 1+16*i), prop2(1:3, 4+16*i), prop2(1:3, 9+16*i), 1, 2.0, s_prop)
    end do

    sum_square = s_fuselage + s_wing + s_v_tail + s_h_tail + s_engine + s_prop
    print *, "s_fuselage: ",  s_fuselage
    print *, "s_wing:     ",  s_wing
    print *, "s_v_tail:   ",  s_v_tail
    print *, "s_h_tail:   ",  s_h_tail
    print *, "s_engine:   ",  s_engine
    print *, "s_propeller:", s_prop
    print *, ""
    print *, "sum square:",  sum_square

    print *, ""
    print *, "s_fuselage: ",  s_fuselage*900/10**(6), "[m2]"
    print *, "s_wing:     ",  s_wing*900/10**(6),     "[m2]"
    print *, "s_v_tail:   ",  s_v_tail*900/10**(6),   "[m2]"
    print *, "s_h_tail:   ",  s_h_tail*900/10**(6),   "[m2]"
    print *, "s_engine:   ",  s_engine*900/10**(6),   "[m2]"
    print *, "s_propeller:", s_prop*900/10**(6),      "[m2]"
    print *, ""
    print *, "sum square:",  sum_square*900/10**(6),  "[m2]"

    call square_calc([1.0, 1.0, 1.0],[0.0, 0.0, 1.0],[0.0, 1.0, 1.0], 1, 6.0, debug)
    print *, debug

end program calc

subroutine square_calc(point1, point2, point3, sign, multiply, part)
    ! ========================
    ! 濡れ面積算出のサブルーチーン
    ! ========================
    ! point1-point3
    ! 3つの3次元のベクトル
    ! ========================
    ! sign: 面積合算の符号
    ! 1 => 現在の面積に加算
    ! 0 => 現在の面積から減算
    ! ========================
    ! part:　部位
    ! ========================
    implicit none
    real, intent(in) :: point1(3, 1), point2(3, 1), point3(3, 1), multiply
    real, intent(inout) :: part
    integer , intent(in) :: sign
    real norm_1, norm_2, dot, sin_theta
    real square
    real, dimension(3, 1) :: vector1, vector2

        square = 0.0   ! initialize
        vector1(1:3, 1) = point2(1:3, 1) - point1(1:3, 1)
        vector2(1:3, 1) = point3(1:3, 1) - point1(1:3, 1)
        norm_1 = sqrt(vector1(1, 1)**2+vector1(2, 1)**2+vector1(3, 1)**2)
        norm_2 = sqrt(vector2(1, 1)**2+vector2(2, 1)**2+vector2(3, 1)**2)
        dot = dot_product(vector1(1:3, 1), vector2(1:3, 1))
        sin_theta = sqrt(1-(dot/(abs(norm_1)*abs(norm_2)))**2)

        if (isnan(sin_theta)) then
            ! print *, "there is a Nan value"
            ! print *, "point1", point1
            ! print *, "point2", point2
            ! print *, "point3", point3
            ! print *, sin_theta
            square = 0.0
        else
            square = 0.5 * abs(norm_1) * abs(norm_2) * sin_theta
            ! print *, ""
            ! print *, "point1   ", point1(1:3, 1)
            ! print *, "point2   ", point2(1:3, 1)
            ! print *, "point3   ", point3(1:3, 1)
            ! print *, "norm    ", norm_1, norm_2
            ! print *, "dot, sin", dot, sin_theta
            ! print *, "square  ", square
            ! print *, "====================="
        end if

        if (multiply == 1.0) then
            ! print *, "okay"
        else if (multiply /= 0.0) then
            square = square * multiply
        else
            print *, "the multiply number is out of range", multiply
            print *, "the process stops"
            stop
        end if

        if (sign == 1) then
            part = part + square
        else if (sign == 0) then
            part = part - square
        else 
            print *, "the sign number is out of range", sign
        end if

end subroutine square_calc
